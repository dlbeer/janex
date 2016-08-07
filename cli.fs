// Command-line interface
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

module Janex.CLI

open System.IO

let helpText = "\
Usage: janex-cli [options] <hosts.nex> <parasites.nex>

Hosts and parasites should be supplied in NEXUS files, each containing
exactly one tree. Output is written to stdout if no output file is
specified.

Host-parasite link matrices are expected to be CSV files, with both row
and column labels. The first row of the file should contain parasite
names; the first column should contain host names. This configuration
can be flipped by specifying the --transpose option. The cells of the
CSV file should contain \"1\" or \"true\" if there is a link between
the corresponding host and parasite.

Options may be any of the following:

  --help                Show this text (short: -?)
  --version             Show version banner
  --out <file.tree>     Write Jane tree file to the specified file
                        (short: -o)
  --links <file.csv>    Read host-parasite link matrix from the given
                        file (short: -l)
  --transpose           Swap rows and columns in the link matrix
                        (short: -t)
"

let loadTree what n =
  try
    File.ReadAllText n |> Scanner.parse Nexus.parseFile |> Nexus.onlyTree
  with
    ex -> failwith (sprintf "%s: %s" what ex.Message)

let run args =
    let (opt, nonopt) =
      Getopt.parse [
        ("help",          Some '?',       false)
        ("version",       None,           false)
        ("out",           Some 'o',       true)
        ("links",         Some 'l',       true)
        ("transpose",     Some 't',       false)
      ] args

    if opt.ContainsKey("help") then
      printf "%s" helpText
    elif opt.ContainsKey("version") then
      printfn "Jane tree file generator, version 0.1"
      printfn "Copyright (C) 2016 Daniel Beer <dlbeer@gmail.com>"
    elif nonopt.Length < 2 then
      failwith "You need to specify a host and parasite input file"
    else
      let h = loadTree "Hosts" nonopt.[0]
      let p = loadTree "Parasites" nonopt.[1]
      let lraw =
        match opt.TryGetValue("links") with
        | (false, _) -> []
        | (true, fn) ->
          File.ReadAllText fn |> Scanner.parse CSV.sheet |> Links.fromTable
      let l =
        if opt.ContainsKey("transpose") then
          [ for (h, p) in lraw -> (p, h) ]
        else lraw
      let o = Jane.toJane h p l
      match opt.TryGetValue("out") with
      | (false, _) -> System.Console.Out.Write(o)
      | (true, fn) -> File.WriteAllText(fn, o)

[<EntryPoint>]
let main args =
    try
      run args
      0
    with ex ->
      System.Console.Error.Write(sprintf "ERROR: %s\n" ex.Message)
      1
