// CSV parser
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

module Janex.CSV

open System.Text
open System.Collections.Generic

open Scanner

let unquotedCell = accept @"[^,\r\n]*" id

let lineTerm = accept @"\r?\n?" id

let notQuoteChar = accept "[^\"]*" id

let cell s =
    if eof s then ""
    elif peek s = '"' then
        advance s 1
        let b = new StringBuilder()
        let rec chunk () =
            if eof s then failwith "Unterminated quoted string"
            else
              match notQuoteChar s with
              | Some text ->
                b.Append(text) |> ignore
                chunk ()
              | None ->
                advance s 1
                if eof s || peek s <> '"' then ()
                else
                  advance s 1
                  b.Append('"') |> ignore
                  chunk ()
        chunk ()
        b.ToString()
    else
        match unquotedCell s with
        | Some x -> x
        | None -> ""

let row s =
    let out = new List<string>()
    let rec accum () =
        out.Add(cell s)
        if not (eof s) && (peek s = ',') then
            advance s 1
            accum ()
    if eof s then [| |]
    else
      match lineTerm s with
      | Some _ -> [| |]
      | None ->
        accum ()
        lineTerm s |> ignore
        out.ToArray()

let sheet s =
    let out = new List<string[]>()
    while not (eof s) do
        out.Add(row s)
    out.ToArray()
