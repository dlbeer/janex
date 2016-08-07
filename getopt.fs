// Command-line option parser
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

module Janex.Getopt

open System.Collections.Generic

// All options have a long name, an optional short name, and a flag
// which indicates whether they require an argument.
type Option = string * char option * bool

// Parse command-line options
let parse specs (args : string[]) =
    let shortMap = new Dictionary<char, Option>()
    let longMap = new Dictionary<string, Option>()

    for s in specs do
      let (name, short, _) = s
      longMap.[name] <- s
      match short with
      | None -> ()
      | Some c -> shortMap.[c] <- s

    let pairs = new Dictionary<string, string>()
    let nonopt = new List<string>()
    let mutable i = 0

    let consumeOpt name wantArg =
      if wantArg then
        if i >= args.Length then
          failwith ("Option requires an argument: " + name)
        else ()
        pairs.[name] <- args.[i]
        i <- i + 1
      else
        pairs.[name] <- name

    while i < args.Length do
      let a = args.[i]
      i <- i + 1

      if (a.Length > 0) && (a.[0] = '-') then
        if (a.Length > 1) && (a.[1] = '-') then
          let n = a.[2..]
          if n = "" then
            while i < args.Length do
              nonopt.Add(args.[i])
              i <- i + 1
          else
            match longMap.TryGetValue(n) with
            | (false, _) -> failwith ("Unknown long option: " + n)
            | (true, (l, _, w)) -> consumeOpt l w
        else
          for j = 1 to a.Length-1 do
            match shortMap.TryGetValue(a.[j]) with
            | (false, _) -> failwith (sprintf "Unknown short option: %c" a.[j])
            | (true, (l, _, w)) -> consumeOpt l w
      else
        nonopt.Add(a) |> ignore

    (pairs, nonopt.ToArray())
