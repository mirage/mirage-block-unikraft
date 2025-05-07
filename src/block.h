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

#ifndef BLOCK_H
#define BLOCK_H

#include <uk/blkdev.h>
#include <uk/assert.h>
#include <uk/print.h>
#include <uk/alloc.h>

#include <yield.h>

struct token_s;
typedef struct token_s token_t;

typedef unsigned int    token_id_t;
typedef long            stoken_id_t;

struct token_s {
  struct uk_blkreq      req;
  struct block_s        *block;
};

typedef struct block_s {
  struct uk_blkdev      *dev;     // Underlying Unikraft blkdev
  struct uk_alloc       *alloc;
  unsigned int          id;       // Identifier of the Unikraft blkdev
  _Atomic unsigned long inflight; // Bit field marking pending IO requests
                                  // Must be atomic to allow concurrent access
                                  // during the queue callback by the interupts
                                  // thread, and during block_io/complete_io by
                                  // the main thread.
  token_t               *tokens;  // Stores request parameters and result
} block_t;


#endif /* !BLOCK_H */
