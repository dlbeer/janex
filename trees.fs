// Phylogenetic trees
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

module Janex.Trees

open System.Collections.Generic

type Tree =
  | Leaf of string * float option
  | Branch of Tree list * float option

// Obtain the set of names in the tree, and make sure they're unique
let nameSet t =
    let out = new HashSet<string>()
    let rec visit n =
      match n with
      | Leaf(name, _) ->
        if out.Add(name) then ()
        else failwith ("Duplicate species name: " + name)
      | Branch(cs, _) -> List.iter visit cs
    visit t
    out

// Add distance to a given node
let addToNode dp n =
    let addDistance a b =
        match (a, b) with
        | (Some av, Some bv) -> Some (av + bv)
        | (_, _) -> None
    match n with
    | Leaf(name, d) -> Leaf(name, addDistance d dp)
    | Branch(cs, d) -> Branch(cs, addDistance d dp)

// Convert a tree to binary form
let rec binarize t =
    match t with
    | Leaf(_, _) -> t
    | Branch([], d) -> failwith "Empty branch"
    | Branch([s], d) -> addToNode d s
    | Branch(f :: rest, d) -> Branch([f; binarize(Branch(rest, Some 0.0))], d)
