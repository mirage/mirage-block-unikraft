(*
 * Copyright (c) 2024-2025 Virgile Robles <virgile@tarides.com>
 * Copyright (c) 2024-2025 Fabrice Buoro <fabrice@tarides.com>
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

type t = {
  mutex : Lwt_mutex.t;
  mutable count : int;
  nonzero : unit Lwt_condition.t;
}

let make count =
  if count < 0 then raise (Invalid_argument "Invalid initial count");
  { mutex = Lwt_mutex.create (); count; nonzero = Lwt_condition.create () }

let acquire t =
  Lwt_mutex.with_lock t.mutex
    Lwt.(
      fun () ->
        let rec aux () =
          if t.count = 0 then
            Lwt_condition.wait ~mutex:t.mutex t.nonzero >>= aux
          else return_unit
        in
        aux () >>= fun () ->
        t.count <- t.count - 1;
        return_unit)

let release t =
  Lwt_mutex.with_lock t.mutex
    Lwt.(
      fun () ->
        t.count <- t.count + 1;
        Lwt_condition.signal t.nonzero ();
        return_unit)
