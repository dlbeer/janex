// Text scanner
// Copyright (C) 2016 Daniel Beer <dlbeer@gmail.com>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

module Janex.Scanner

open System
open System.Text.RegularExpressions

type Scanner = {
    text: string
    mutable i: int }

let scan text = { text = text; i = 0 }

let eof s =
    s.i >= s.text.Length

let peek s = s.text.[s.i]

let advance s n = s.i <- s.i + n

let rcPos (text : string) i =
    let mutable row = 1
    let mutable col = 1
    for j = 0 to (i-1) do
        if text.[j] = '\n' then
            row <- row + 1
            col <- 1
        else
            col <- col + 1
    (row, col)

let accept pattern f =
    let r = new Regex(@"\G(" + pattern + ")")
    fun s ->
        let m = r.Match(s.text, s.i)
        if m.Success && m.Length > 0 then
            let tok = f s.text.[s.i..(s.i+m.Length-1)]
            s.i <- s.i + m.Length
            Some tok
        else
            None

let require name tok s =
    match tok s with
    | None -> failwith ("Expected " + name)
    | Some x -> x

let expect name pattern f =
    accept pattern f |> require name

exception ParseError of string * int * Exception
    with
      override this.Message =
        match this :> exn with
        | ParseError(text, i, ex) ->
          let (r, c) = rcPos text i
          sprintf "Parse error at row %d, col %d (pos %d): %s" r c i ex.Message
        | _ -> Unchecked.defaultof<_>

let parse f text =
    let s = scan text
    try f s
    with ex -> raise(ParseError(text, s.i, ex))
