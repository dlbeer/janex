// User interface
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

module Janex.GUI

open Trees

open System
open System.Text
open System.IO
open System.Collections.Generic

open System.Drawing
open System.Windows.Forms

type MenuItem =
  | Separator
  | Item of string * (unit -> unit)
  | ItemSC of string * (unit -> unit) * Keys

let buildMenu (menu : MenuStrip) name items =
    let m = new ToolStripMenuItem(name : string)
    for it in items do
        match it with
        | Separator ->
          m.DropDownItems.Add(new ToolStripSeparator()) |> ignore
        | Item(k, f) ->
          let i = new ToolStripMenuItem(k : string)
          i.Click.Add(fun _ -> f ())
          m.DropDownItems.Add(i) |> ignore
        | ItemSC(k, f, sc) ->
          let i = new ToolStripMenuItem(k : string)
          i.Click.Add(fun _ -> f ())
          i.ShortcutKeys <- sc
          m.DropDownItems.Add(i) |> ignore
    menu.Items.Add(m) |> ignore

let showError title (ex : exn) =
    MessageBox.Show(ex.Message, title,
        MessageBoxButtons.OK,
        MessageBoxIcon.Error) |> ignore

let importNexus what cfunc =
    use d = new OpenFileDialog(
                Title = "Import " + what,
                Filter = "NEXUS files|*.nex;*.tre|All files|*.*",
                RestoreDirectory = false,
                ShowHelp = true )
    if d.ShowDialog() = DialogResult.OK then
      try
        File.ReadAllText(d.FileName) |> Scanner.parse Nexus.parseFile |>
          Nexus.onlyTree |> cfunc
      with ex -> showError ("Import " + what) ex

type Dialog() as this =
    inherit Form()

    let menu = new MenuStrip()
    let status =
      new TextBox(
        Multiline = true,
        ReadOnly = true,
        ScrollBars = ScrollBars.Vertical)

    let mutable hosts : Tree option = None
    let mutable parasites : Tree option = None
    let mutable links : (string * string) list = []

    do
        this.Text <- "Jane tree file generator"

        buildMenu menu "&File" [
          Item("Import &hosts...", this.ImportHosts)
          Item("Import &parasites...", this.ImportParasites)
          Item("Import H-P link &matrix...", this.ImportLinks)
          Separator
          Item("&Export Jane tree file...", this.ExportJane)
          Separator
          ItemSC("&Quit", (fun () -> this.DialogResult <- DialogResult.OK),
                 Keys.Control ||| Keys.Q) ]

        buildMenu menu "&Edit" [
          Item("&Transpose H-P link matrix", this.TransposeHP)
          Item("&Swap host/parasite trees", this.SwapTrees)
          Separator
          Item("Clear &hosts", this.ClearHosts)
          Item("Clear &parasites", this.ClearParasites)
          Item("Clear H-P link &matrix", this.ClearLinks)
          Separator
          ItemSC("&Copy", (fun () -> status.Copy()),
                 Keys.Control ||| Keys.C) ]

        buildMenu menu "&Help" [
          Item("&About", this.About) ]

        this.Controls.Add(menu)
        this.Controls.Add(status)
        this.UpdateStatus()

        this.ClientSize <- new Size(600, 400)
        this.UpdateSize()

    override this.OnSizeChanged(e: EventArgs) =
        this.UpdateSize()

    member this.UpdateSize() =
        let w = this.ClientSize.Width
        let y = menu.PreferredSize.Height + menu.Margin.Vertical
        menu.Location <-
          new Point(menu.Margin.Left, menu.Margin.Top)
        menu.Size <-
          new Size(w - menu.Margin.Horizontal, menu.PreferredSize.Height)
        status.Location <-
          new Point(status.Margin.Left, y + status.Margin.Top)
        status.Size <-
          new Size(w - status.Margin.Horizontal,
                   this.ClientSize.Height - status.Margin.Vertical - y)

    member this.ClearHosts() =
        hosts <- None
        this.UpdateStatus()

    member this.ClearParasites() =
        parasites <- None
        this.UpdateStatus()

    member this.ClearLinks() =
        links <- []
        this.UpdateStatus()

    member this.ImportHosts() =
        importNexus "hosts" <| fun h ->
          hosts <- Some h
          this.UpdateStatus()

    member this.ImportParasites() =
        importNexus "parasites" <| fun h ->
          parasites <- Some h
          this.UpdateStatus()

    member this.ImportLinks() =
        use d = new OpenFileDialog(
                    Title = "Import H-P link matrix",
                    Filter = "CSV files (*.csv)|*.csv|All files|*.*",
                    RestoreDirectory = false,
                    ShowHelp = true )
        if d.ShowDialog() = DialogResult.OK then
          try
            links <- File.ReadAllText(d.FileName) |>
              Scanner.parse CSV.sheet |> Janex.Links.fromTable
          with ex -> showError "Import H-P link matrix" ex
        this.UpdateStatus()

    member this.ExportJane() =
        try
          match (hosts, parasites) with
          | (None, _) -> failwith "You need to import a host tree"
          | (_, None) -> failwith "You need to import a parasite tree"
          | (Some h, Some p) ->
            let text = Jane.toJane h p links
            use d = new SaveFileDialog(
                        Title = "Export Jane tree file",
                        Filter = "Tree files (*.tree)|*.tree|All files|*.*",
                        RestoreDirectory = false,
                        OverwritePrompt = true,
                        CreatePrompt = false,
                        ShowHelp = true )
            if d.ShowDialog() = DialogResult.OK then
              File.WriteAllText(d.FileName, text)
        with ex -> showError "Export Jane file" ex

    member this.TransposeHP() =
        links <- [ for (h, p) in links -> (p, h) ]
        this.UpdateStatus()

    member this.SwapTrees() =
        let (h, p) = (hosts, parasites)
        hosts <- p
        parasites <- h
        this.UpdateStatus()

    member this.About() =
        MessageBox.Show(
            "Jane tree file generator, version 0.4\n\n\
             Copyright \u00a9 2016 Daniel Beer <dlbeer@gmail.com>",
            "About",
            MessageBoxButtons.OK,
            MessageBoxIcon.Information) |> ignore

    member this.UpdateStatus() =
        let out = new StringBuilder()
        Linter.checkAll hosts parasites links <| fun m ->
          out.Append(Linter.formatMessage m + "\r\n") |> ignore
        status.Text <- out.ToString()
        status.Select(0, 0)

[<EntryPoint>]
let main args =
    use d = new Dialog()
    d.ShowDialog() |> ignore
    0
