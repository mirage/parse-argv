(*
 * Copyright (c) 2016 Citrix Systems Inc
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
 *
 *)

(* Split string into whitespace-separated substrings,
   taking into account quoting *)

type state =
  | Quoted_escaped
  | Quoted
  | Escaped
  | Normal

let chars_to_str chars =
  let chars = List.rev chars in
  [String.init (List.length chars) (List.nth chars)]

(* We keep track of the previous state. If it was [Quoted] we unconditionally
   add the string. [chars_to_str] is only called when in [Normal] state, so
   that means the character before the space or end of input must have been end
   of quote. In that case we want the string even if it's the empty string. *)
let chars_to_str prev_state =
  match prev_state with
  | Quoted -> chars_to_str
  | _ ->
    function
    | [] -> []
    | chars -> chars_to_str chars

let parse s =
  let l = String.length s in
  let rec loop acc curr prev_state state idx =
    if idx = l then
      if state = Normal then
        Ok (List.rev (chars_to_str prev_state curr @ acc))
      else
        Error "bad input line - either escaped or quoted or both"
    else
      match state, String.unsafe_get s idx with
      | Normal, ' ' -> loop (chars_to_str prev_state curr @ acc) [] state state (idx + 1)
      | Escaped, c -> loop acc (c :: curr) state Normal (idx + 1)
      | Quoted_escaped, c -> loop acc (c :: curr) state Quoted (idx + 1)
      | Quoted, '\\' -> loop acc curr state Quoted_escaped (idx + 1)
      | Quoted, '"' -> loop acc curr state Normal (idx + 1)
      | Quoted, c -> loop acc (c :: curr) state Quoted (idx + 1)
      | Normal, '\\' -> loop acc curr state Escaped (idx + 1)
      | Normal, '"' -> loop acc curr state Quoted (idx + 1)
      | Normal, c -> loop acc (c :: curr) state Normal (idx + 1)
  in
  loop [] [] Normal Normal 0
