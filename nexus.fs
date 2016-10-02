// NEXUS parser
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

module Janex.Nexus

open System

open Scanner
open Trees

// Basic NEXUS file tokens
let junk = accept @"([ \t\r\n]+|\[[^]]*\])+" id >> ignore

let word = expect "word" @"[0-9A-Za-z\._]+" id

let real =
    expect "real" @"-?[0-9]*(\.[0-9]*)?([Ee][-+]?[0-9]+)?" <|
        fun t -> Convert.ToDouble(t)

let header =
    expect "NEXUS file header" @"#[Nn][Ee][Xx][Uu][Ss][ \t\r]*\n" id

// Newick tree parser
let tree =
    let distance s =
        junk s
        if not (eof s) && (peek s = ':') then
            advance s 1
            junk s
            real s |> Some
        else None
    let rec node s =
        if peek s = '(' then
            advance s 1
            junk s
            tail s [node s]
        else
            let w = word s
            let d = distance s
            Leaf(w, d)
    and tail s accum =
        junk s
        match peek s with
        | ',' ->
            advance s 1
            junk s
            tail s (node s :: accum)
        | ')' ->
            advance s 1
            let d = distance s
            Branch((List.rev accum), d)
        | _ -> failwith "Malformed child list"
    node

let transList s =
    let rec aux accum =
        let k = word s
        junk s
        let v = word s
        junk s
        let na = (k, v) :: accum
        if peek s = ';' then List.rev na
        else
          if peek s <> ',' then failwith "Expected comma" else ()
          advance s 1
          junk s
          aux na
    if peek s = ';' then []
    else aux []

// NEXUS block
type Block = {
    name         : string
    translations : (string * string) list
    trees        : (string * Tree) list }

let ignoreArg = accept @"[^;]*" id >> ignore

let semicolon s =
    junk s
    if peek s <> ';' then failwith "Expected semicolon" else ()
    advance s 1

// Parse a NEXUS block
let parseBlock s =
    let rec lines accum =
        junk s
        let k = word s
        junk s
        match k.ToLower() with
        | "end" ->
          semicolon s
          { accum with trees = List.rev accum.trees }
        | "tree" ->
          if peek s = '*' then
            advance s 1
            junk s
          let n = word s
          junk s
          if peek s <> '=' then failwith "Expected equals sign" else ()
          advance s 1
          junk s
          let t = tree s
          junk s
          semicolon s
          lines { accum with trees = (n, t) :: accum.trees }
        | "translate" ->
          let p = transList s
          semicolon s
          lines { accum with
                    translations = List.append accum.translations p }
        | _ ->
          ignoreArg s
          semicolon s
          lines accum

    if (word s).ToLower() <> "begin" then failwith "Expected begin keyword"
    junk s
    let block = { name = word s; translations = []; trees = [] }
    semicolon s
    lines block

// Parse an entire NEXUS file
let parseFile s =
    let rec blocks accum =
        junk s
        if eof s then List.rev accum
        else blocks (parseBlock s :: accum)
    header s |> ignore
    blocks []

// Given a NEXUS block, obtain a list of translated trees
let extractTrees blk =
    let tab = dict blk.translations
    let lookup n =
        match tab.TryGetValue(n) with
        | true, v -> v
        | false, _ -> n
    let rec mapTree t =
        match t with
        | Leaf(n, d) -> Leaf(lookup n, d)
        | Branch(cs, d) -> Branch(List.map mapTree cs, d)
    [ for n, t in blk.trees -> (n, mapTree t) ]

// Extract all trees from a Jane file
let allTrees blks =
    List.map extractTrees blks |> List.fold List.append []

// Get a single tree from a Jane file
let onlyTree blks =
    match allTrees blks with
    | [(_, t)] -> t
    | _ -> failwith "Multiple trees found in file"
