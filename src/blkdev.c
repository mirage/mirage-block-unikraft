/* SPDX-License-Identifier: BSD-3-Clause */
/*
 * Authors: Simon Kuenzer <simon.kuenzer@neclab.eu>
 *          Fabrice Buoro <fabrice@tarides.com>
 *          Samuel Hym <samuel@tarides.com>
 *
 * Copyright (c) 2013-2017, NEC Europe Ltd., NEC Corporation.
 *               2024-2025, Tarides.
 *               All rights reserved.
*/

#ifdef __Unikraft__

#include "block.h"
#include "result.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "yield.h"

block_t blocks[MAX_BLK_DEVICES] = {0};

static token_t *acquire_token(block_t *block)
{
  token_t *token;

  for (unsigned int i = 0; i < MAX_BLK_TOKENS; i++) {
    const unsigned long pos = 1L << i;
    unsigned long old = block->inflight;
    while ((old & pos) == 0) {
      const unsigned long new = old | pos;
      if (atomic_compare_exchange_strong(&block->inflight, &old, new)) {
        token = &block->tokens[i];
        assert(uk_blkreq_is_done(&token->req));
        return token;
      }
    }
  }
  return NULL;
}

static token_id_t token_id(block_t *block, token_t *token)
{
  return token - block->tokens;
}

static void release_token(block_t *block, token_t *token)
{
  token_id_t id = token_id(block, token);

  unsigned long mask = ~(1L << id);
  block->inflight &= mask;
}

void req_callback(struct uk_blkreq *req, void *cookie)
{
  token_t *token = (token_t*)cookie;
  block_t *block = token->block;

  token_id_t tok_id = token_id(block, token);

  signal_block_request_ready(block->id, tok_id);
}

void queue_callback(struct uk_blkdev *dev, uint16_t queue_id, void *argp)
{
  (void)argp;

  int rc = uk_blkdev_queue_finish_reqs(dev, 0);
  if (rc) {
    uk_pr_err("Error finishing request: %d\n", rc);
  }
}

stoken_id_t block_io(block_t *block, int write, unsigned long sstart,
    unsigned long size, char *buf)
{

  token_t *token = acquire_token(block);
  if (!token) {
    return -1;
  }

  struct uk_blkreq *req = &token->req;
  uk_blkreq_init(req, write ? UK_BLKREQ_WRITE : UK_BLKREQ_READ, sstart, size,
      buf, req_callback, token);
  assert(!uk_blkreq_is_done(&token->req));

  int rc = uk_blkdev_queue_submit_one(block->dev, 0, &token->req);
  if (rc < 0) {
    release_token(block, token);
    return -1;
  }

  return token_id(block, token);
}

// -------------------------------------------------------------------------- //

static block_t *block_init(unsigned int id)
{
  block_t *block = &blocks[id];

  block->tokens = malloc(MAX_BLK_TOKENS * sizeof(token_t));
  if (!block->tokens) {
    return NULL;
  }

  for (int i = 0; i < MAX_BLK_TOKENS; i++) {
    block->tokens[i].block = block;
  }

  block->inflight = 0;
  block->id = id;
  return block;
}

static void block_deinit(block_t *block)
{
  if (block->tokens) {
    free(block->tokens);
  }
  block->inflight = 0;
}

static block_t *block_get(unsigned int id, const char **err)
{
  block_t *block;

  block = block_init(id);
  if (!block) {
    *err = "Failed to allocate memory for blkdev";
    return NULL;
  }

  block->dev = uk_blkdev_get(id);
  if (!block->dev) {
    *err = "Failed to acquire block device";
    block_deinit(block);
    return NULL;
  }

  block->alloc = uk_alloc_get_default();
  if (!block->alloc) {
    *err = "Failed to get default allocator";
    block_deinit(block);
    return NULL;
  }
  return block;
}

static int block_configure(block_t *block, const char **err)
{
  int rc;

  if (uk_blkdev_state_get(block->dev) != UK_BLKDEV_UNCONFIGURED) {
    *err = "Block device not in unconfigured state";
    return -1;
  }

  struct uk_blkdev_info info;
  rc = uk_blkdev_get_info(block->dev, &info);
  if (rc) {
    *err = "Error getting device information";
    return -1;
  }

  struct uk_blkdev_conf conf = { 0 };
  conf.nb_queues = 1;
  rc = uk_blkdev_configure(block->dev, &conf);
  if (rc) {
    *err = "Error configuring device";
    return -1;
  }

  struct uk_blkdev_queue_info q_info;
  rc = uk_blkdev_queue_get_info(block->dev, 0, &q_info);
  if (rc) {
    *err = "Error getting device queue information";
    return -1;
  }

  assert(MAX_BLK_TOKENS < q_info.nb_max);

  struct uk_blkdev_queue_conf q_conf = { 0 };
  q_conf.a = block->alloc;
  q_conf.callback = queue_callback;
  q_conf.callback_cookie = block;
  q_conf.s = uk_sched_current();
  // nb_desc = 0: device default queue length
  rc = uk_blkdev_queue_configure(block->dev, 0, 0, &q_conf);
  if (rc) {
    *err = "Error configuring device queue";
    return -1;
  }

  rc = uk_blkdev_start(block->dev);
  if (rc) {
    *err = "Error starting block device";
    return -1;
  }

  if (uk_blkdev_state_get(block->dev) != UK_BLKDEV_RUNNING) {
    *err = "Block device not in running state";
    return -1;
  }

  const struct uk_blkdev_cap* cap = uk_blkdev_capabilities(block->dev);
  if (cap->mode == O_WRONLY) {
    *err = "Write-only devices are not supported";
    return -1;
  }

  rc = uk_blkdev_queue_intr_enable(block->dev, 0);
  if (rc) {
    *err = "Error enabling interrupt for block device queue";
    return -1;
  }
  return 0;
}

// -------------------------------------------------------------------------- //

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>

value uk_block_init(value v_id)
{
  CAMLparam1(v_id);
  CAMLlocal2(v_result, v_error);
  const char *err = NULL;
  const unsigned id = Int_val(v_id);

  UK_ASSERT(id >= 0);

  block_t *block = block_get(id, &err);
  if (!block) {
    v_result = alloc_result_error(err);
    CAMLreturn(v_result);
  }

  const int rc = block_configure(block, &err);
  if (rc) {
    v_result = alloc_result_error(err);
    CAMLreturn(v_result);
  }

  v_result = alloc_result_ok(Val_ptr(block));
  CAMLreturn(v_result);
}

value uk_block_info(value v_block)
{
  CAMLparam1(v_block);
  CAMLlocal1(v_result);

  block_t *block = (block_t*)Ptr_val(v_block);
  const struct uk_blkdev_cap* cap = uk_blkdev_capabilities(block->dev);

  v_result = caml_alloc(3, 0);
  Store_field(v_result, 0, (cap->mode == O_RDONLY) ? Val_false : Val_true);
                                                           // read_write
  Store_field(v_result, 1, Val_int(cap->ssize));           // sector size
  Store_field(v_result, 2, caml_copy_int64(cap->sectors)); // number of sectors
  CAMLreturn(v_result);
}

// -------------------------------------------------------------------------- //

static stoken_id_t block_read(block_t *block, unsigned long sstart,
        unsigned long size, char *buffer)
{
  return block_io(block, 0, sstart, size, buffer);
}

value uk_block_read(value v_block, value v_sstart, value v_size, value v_buffer,
    value v_offset)
{
  CAMLparam5(v_block, v_sstart, v_size, v_buffer, v_offset);
  CAMLlocal1(v_result);

  block_t *block = (block_t*)Ptr_val(v_block);

  uint8_t *buf = (uint8_t *)Caml_ba_data_val(v_buffer) + Long_val(v_offset);
  unsigned long sstart = Int64_val(v_sstart);
  unsigned long size = Long_val(v_size);

  const stoken_id_t rc = block_read(block, sstart, size, buf);
  if (rc < 0) {
    v_result = alloc_result_error("uk_block_read: error");
    CAMLreturn(v_result);
  }
  token_id_t tokid = (token_id_t)rc;

  v_result = alloc_result_ok(Val_int(tokid));
  CAMLreturn(v_result);
}

// -------------------------------------------------------------------------- //

static stoken_id_t block_write(block_t *block, unsigned long sstart,
    unsigned long size, char *buffer)
{
  return block_io(block, 1, sstart, size, buffer);
}

value uk_block_write(value v_block, value v_sstart, value v_size,
    value v_buffer, value v_offset)
{
  CAMLparam5(v_block, v_sstart, v_size, v_buffer, v_offset);
  CAMLlocal1(v_result);

  block_t *block = (block_t*)Ptr_val(v_block);

  uint8_t *buf = (uint8_t *)Caml_ba_data_val(v_buffer) + Long_val(v_offset);
  unsigned long sstart = Int64_val(v_sstart);
  unsigned long size = Long_val(v_size);

  const stoken_id_t rc = block_write(block, sstart, size, buf);
  if (rc < 0) {
    v_result = alloc_result_error("uk_block_write: error");
    CAMLreturn(v_result);
  }
  token_id_t tokid = (token_id_t)rc;

  v_result = alloc_result_ok(Val_int(tokid));
  CAMLreturn(v_result);
}

// -------------------------------------------------------------------------- //

value uk_complete_io(value v_block, value v_token_id)
{
  CAMLparam2(v_block, v_token_id);
  CAMLlocal1(v_result);

  block_t *block = (block_t*)Ptr_val(v_block);
  token_id_t token_id = Int_val(v_token_id);
  token_t *token = &block->tokens[token_id];
  struct uk_blkreq *request = &token->req;

  assert(uk_blkreq_is_done(request));
  bool success = request->result >= 0;

  release_token(block, token);
  set_block_request_completed(block->id, token_id);

  if (success) {
    CAMLreturn(Val_true);
  }
  CAMLreturn(Val_false);
}

// -------------------------------------------------------------------------- //

value uk_max_tokens(void)
{
  CAMLparam0();

  CAMLreturn(Val_int(MAX_BLK_TOKENS));
}

// -------------------------------------------------------------------------- //

value uk_max_sectors_per_req(value v_block)
{
  CAMLparam1(v_block);
  CAMLlocal1(v_result);

  block_t *block = (block_t*)Ptr_val(v_block);
  int sectors = uk_blkdev_max_sec_per_req(block->dev);
  CAMLreturn(Val_int(sectors));
}

#endif /* __Unikraft__ */
