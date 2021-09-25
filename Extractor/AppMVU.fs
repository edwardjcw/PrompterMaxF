namespace Prompter

open System
open Elmish.WPF
open System.Windows

module AppModel =
    type App = {
        ExtractorWindowState: WindowState<ExtractorModel.Model>
        ExtractorModel: ExtractorModel.Model
    }

    type AppMsg = 
        | ExtractorShow
        | ExtractorHide
        | ExtractorClose
        | ExtractorMsg of ExtractorModel.Msg

    let init () = {ExtractorWindowState=WindowState.Closed; ExtractorModel=ExtractorModel.init ()}

module AppUpdate =
    open AppModel
    let update msg m = 
        match msg with
        | ExtractorShow -> {m with ExtractorWindowState=ExtractorModel.init() |> WindowState.Visible}
        | ExtractorHide -> {m with ExtractorWindowState=ExtractorModel.init() |> WindowState.Hidden}
        | ExtractorClose -> {m with ExtractorWindowState=WindowState.Closed}
        | ExtractorMsg msg' -> {m with ExtractorModel=ExtractorUpdate.update msg' m.ExtractorModel}

module AppView =
    open AppModel
    let extractorWindowBindings () = 
        ["Extractor" |> Elmish.WPF.Binding.subModel ((fun m -> m.ExtractorModel), snd, ExtractorMsg, ExtractorView.bindings)] 

    let mainBindings (createExtractorWindow: unit -> #Window) () : Binding<App, AppMsg> list = [
        "ExtractorShow" |> Binding.cmd ExtractorShow
        "ExtractorHide" |> Binding.cmd ExtractorHide
        "ExtractorClose" |> Binding.cmd ExtractorClose
        "ExtractorWindow" |> Binding.subModelWin ((fun m -> m.ExtractorWindowState), fst, id, extractorWindowBindings, createExtractorWindow)
    ]

module AppMain =
    let fail _ = failwith "never called"
    let mainDesignVm = ViewModel.designInstance (AppModel.init ()) (AppView.mainBindings fail ())
    let extractorWindowDesignVm = ViewModel.designInstance (AppModel.init ()) (AppView.extractorWindowBindings ())

    let main mainWindow (createExtractorWindow: Func<#Window>) =
        let createExtractorWindow () = createExtractorWindow.Invoke()
        let bindings = AppView.mainBindings createExtractorWindow
        Program.mkSimpleWpf AppModel.init AppUpdate.update bindings
        |> Program.startElmishLoop ElmConfig.Default mainWindow