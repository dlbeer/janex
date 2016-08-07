// Jane file
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

module Janex.Jane

open Trees
open System.Text
open System.Collections.Generic

// Flat tree representation
type FlatTree =
  | FlatLeaf of string * float option
  | FlatBranch of int * int * float option

// Flatten a tree
let flatten root =
    let out = new List<FlatTree>()
    let rec visit n =
      match n with
      | Leaf(name, d) -> out.Add(FlatLeaf(name, d))
      | Branch(l, r, d) ->
          let pi = out.Count
          out.Add(FlatLeaf(null, None))
          visit l
          let ri = out.Count
          visit r
          out.[pi] <- FlatBranch(pi+1, ri, d)
    visit root
    out.ToArray()

// Build a mapping of names to indices in the flattened tree
let flatIndex (f : FlatTree[]) =
    let d = new Dictionary<string, int>()
    for i = 0 to f.Length-1 do
        match f.[i] with
        | FlatLeaf(name, _) ->
          if d.ContainsKey(name) then
            failwith ("Duplicate species name: " + name)
          d.[name] <- i
        | _ -> ()
    d

let printTree (out : StringBuilder) name (tree : FlatTree[]) offset =
    out.Append(sprintf "%sTREE\n" name) |> ignore
    for i = 0 to tree.Length - 1 do
        out.Append(
          match tree.[i] with
          | FlatLeaf(name, _) ->
            sprintf "%d\tnull\tnull\n" (i+offset)
          | FlatBranch(l, r, _) ->
            sprintf "%d\t%d\t%d\n" (i+offset)
              (l+offset) (r+offset)) |> ignore
    out.Append(sprintf "\n%sNAMES\n" name) |> ignore
    for i = 0 to tree.Length - 1 do
        out.Append(
          match tree.[i] with
          | FlatLeaf(name, _) -> sprintf "%d\t%s\n" (i+offset) name
          | FlatBranch(l, r, _) ->
            sprintf "%d\t%d\n" (i+offset) (i+offset)) |> ignore

let printDummyRanks (out : StringBuilder) name n offset =
    out.Append(sprintf "%sRANKS\n" name) |> ignore
    for i = 0 to n-1 do
      out.Append(sprintf "%d\t1\n" (i + offset)) |> ignore

let lookup m (n : Dictionary<string, int>) =
    match n.TryGetValue(m) with
    | true, x -> x
    | false, _ -> failwith ("Can't find species name: " + m)

let printJane (out : StringBuilder) host para links =
    printTree out "HOST" host 0
    out.Append('\n') |> ignore
    printTree out "PARASITE" para host.Length
    out.Append('\n') |> ignore

    let hl = flatIndex host
    let pl = flatIndex para

    out.Append("PHI\n") |> ignore
    for (h, p) in links do
        out.Append(
          sprintf "%d\t%d\n" (lookup h hl)
            ((lookup p pl) + host.Length)) |> ignore
    out.Append('\n') |> ignore

    printDummyRanks out "HOST" host.Length 0
    out.Append('\n') |> ignore
    printDummyRanks out "PARASITE" para.Length host.Length

let toJane host para links =
    let out = new StringBuilder()
    printJane out (flatten host) (flatten para) links
    out.ToString()
