// Tanglegram checker
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

module Janex.Linter

open System
open System.Collections.Generic

open Trees

type Level = Info | Error
type Message = Level * string

let checkNames what t out =
    let names = new HashSet<string>()
    let rec aux t =
        match t with
        | Leaf(n, _) ->
          if not (names.Add(n)) then
            out (Error, sprintf "Duplicate %s species: %s" what n)
        | Branch(cs, _) -> List.iter aux cs
    aux t
    names

let checkEndpoints what (names : HashSet<string>) eps out =
    let checkList = new HashSet<string>()
    List.iter (fun e ->
      if not (names.Contains(e)) && checkList.Add(e) then
        out (Error, sprintf "Unknown %s species: %s" what e)) eps

let countUniqueEndpoints eps =
    let names = new HashSet<string>()
    List.iter (fun e -> names.Add(e) |> ignore) eps
    names.Count

let checkAllTree (what : string) mt out =
    let r = (
      match mt with
      | None ->
        out (Info, sprintf "No %s tree supplied" (what.ToLower()))
        None
      | Some h ->
        out (Info, sprintf "%s tree: %d species" what (count h))
        checkNames (what.ToLower()) h out |> Some)
    out (Info, "")
    r

let checkAll hosts paras links out =
    let hset = checkAllTree "Host" hosts out
    let pset = checkAllTree "Parasite" paras out

    if links = [] then
      out (Info, "Empty link matrix")
    else
      out (Info, sprintf "Link matrix: %d hosts <--> %d parasites"
                   (List.map fst links |> countUniqueEndpoints)
                   (List.map snd links |> countUniqueEndpoints))

    match hset with
    | None -> ()
    | Some h ->
      checkEndpoints "host" h (List.map fst links) out

    match pset with
    | None -> ()
    | Some p ->
      checkEndpoints "parasite" p (List.map snd links) out

let formatMessage (l, m) =
    sprintf "%s%s" (
      match l with
      | Info ->    ""
      | Error ->   "ERROR: ") m
