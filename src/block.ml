(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Citrix Systems Inc
 * Copyright (c) 2018 Martin Lucina <martin@lucina.net>
 * Copyright (c) 2024-2025 Fabrice Buoro <fabrice@tarides.com>
 * Copyright (c) 2024-2025 Samuel Hym <samuel@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type block_ptr = int

external uk_block_init : int -> (block_ptr, string) result = "uk_block_init"
external uk_block_info : block_ptr -> bool * int * int64 = "uk_block_info"
external uk_max_tokens : unit -> int = "uk_max_tokens"
external uk_max_sectors_per_req : block_ptr -> int = "uk_max_sectors_per_req"

external uk_block_read :
  block_ptr -> int64 -> int -> Cstruct.buffer -> int -> (int, string) result
  = "uk_block_read"

external uk_block_write :
  block_ptr -> int64 -> int -> Cstruct.buffer -> int -> (int, string) result
  = "uk_block_write"

external uk_complete_io : block_ptr -> int -> bool = "uk_complete_io"

open Lwt.Infix

let ( let* ) = Lwt.bind
let src = Logs.Src.create "block" ~doc:"Mirage Unikraft block module"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  id : int;
  handle : block_ptr;
  semaphore : Semaphore.t;
  info : Mirage_block.info;
}

type error =
  [ Mirage_block.error
  | `Invalid_argument
  | `Unspecified_error
  | `Buffer_alignment
  | `Generic_error of string ]

let pp_error ppf = function
  | #Mirage_block.error as e -> Mirage_block.pp_error ppf e
  | `Invalid_argument -> Fmt.string ppf "Invalid argument"
  | `Unspecified_error -> Fmt.string ppf "Unspecified error"
  | `Buffer_alignment ->
      Fmt.string ppf "Invalid argument: buffers must be sector aligned"
  | `Generic_error msg -> Fmt.string ppf msg

type write_error =
  [ Mirage_block.write_error
  | `Invalid_argument
  | `Unspecified_error
  | `Buffer_alignment
  | `Generic_error of string ]

let pp_write_error ppf = function
  | #Mirage_block.write_error as e -> Mirage_block.pp_write_error ppf e
  | `Invalid_argument -> Fmt.string ppf "Invalid argument"
  | `Unspecified_error -> Fmt.string ppf "Unspecified error"
  | `Buffer_alignment ->
      Fmt.string ppf "Invalid argument: buffers must be sector aligned"
  | `Generic_error msg -> Fmt.string ppf msg

let connect devid =
  let aux id =
    match uk_block_init id with
    | Ok handle ->
        let read_write, sector_size, size_sectors = uk_block_info handle in
        let tokens = uk_max_tokens () in
        let semaphore = Semaphore.make tokens in
        let t =
          {
            id;
            handle;
            semaphore;
            info = { read_write; sector_size; size_sectors };
          }
        in
        Lwt.return t
    | Error msg -> Lwt.fail_with msg
  in
  match int_of_string_opt devid with
  | Some id when id >= 0 && id < 63 ->
      Log.info (fun f -> f "Plugging into blkdev %d" id);
      aux id
  | _ ->
      Lwt.fail_with
        (Fmt.str
           "connect(%s): block ids should be integers between 0 and 62 \
           (inclusive) on this platform"
           devid)

let disconnect _t = Lwt.return_unit
let get_info t = Lwt.return t.info

let check_bounds t sector_start buffer =
  let buff_size = Cstruct.length buffer in
  if buff_size mod t.info.sector_size <> 0 then Error `Buffer_alignment
  else
    let end_ =
      Int64.(add sector_start (of_int (buff_size / t.info.sector_size)))
    in
    if end_ > t.info.size_sectors then Error `Invalid_argument else Ok ()

let capped_buffer buffer limit =
  let size = Cstruct.length buffer in
  if size > limit then
    let capped = Cstruct.sub buffer 0 limit in
    let rest = Cstruct.sub buffer limit (size - limit) in
    (capped, Some rest)
  else (buffer, None)

let perform io_op t sector_start buffer =
  let max_size = uk_max_sectors_per_req t.handle * t.info.sector_size in
  let rec aux sector_start buffer =
    let capped, rest = capped_buffer buffer max_size in
    let ssize = Cstruct.length capped / t.info.sector_size in
    let* () = Semaphore.acquire t.semaphore in
    match
      io_op t.handle sector_start ssize capped.Cstruct.buffer capped.Cstruct.off
    with
    | Ok tokid ->
        let* () = Unikraft_os.Main.Uk_engine.wait_for_work_blkdev t.id tokid in
        let ok = uk_complete_io t.handle tokid in
        let* () = Semaphore.release t.semaphore in
        if ok then
          match rest with
          | Some buf -> aux Int64.(add sector_start (of_int ssize)) buf
          | None -> Lwt.return (Ok ())
        else Lwt.return (Error `Unspecified_error)
    | Error msg ->
        let* () = Semaphore.release t.semaphore in
        Lwt.return (Error (`Generic_error msg))
  in
  match check_bounds t sector_start buffer with
  | Ok () -> aux sector_start buffer
  | Error _ as e -> Lwt.return e

let perform io_op t sector_start buffers =
  let rec aux sector_start buffers =
    match buffers with
    | [] -> Lwt.return (Ok ())
    | buf :: tl -> (
        perform io_op t sector_start buf >>= function
        | Ok () ->
            let ssize = Cstruct.length buf / t.info.sector_size in
            aux Int64.(add sector_start (of_int ssize)) tl
        | Error _ as e -> Lwt.return e)
  in
  aux sector_start buffers

let read = perform uk_block_read

let write t sector_start buffers =
  let* info = get_info t in
  if not info.read_write then Lwt.return (Error `Is_read_only)
  else perform uk_block_write t sector_start buffers
