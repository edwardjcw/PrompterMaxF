namespace Prompter

open System
open Elmish.WPF
open System.Windows

module AppModel =
    type App = {
        ExtractorWindowState: WindowState<ExtractorModel.Model>
        ExtractorModel: ExtractorModel.Model
        PrompterWindowState: WindowState<PrompterModel.Model>
        PrompterModel: PrompterModel.Model
    }

    type AppMsg = 
        | ExtractorShow
        | ExtractorHide
        | ExtractorClose
        | ExtractorMsg of ExtractorModel.Msg
        | PrompterShow
        | PrompterHide
        | PrompterClose
        | PrompterMsg of PrompterModel.Msg

    let init () = {
        ExtractorWindowState=WindowState.Closed
        ExtractorModel=ExtractorModel.init ()
        PrompterWindowState=WindowState.Closed
        PrompterModel=PrompterModel.init ()
    }

module AppUpdate =
    open AppModel
    let update msg m = 
        match msg with
        | ExtractorShow -> {m with ExtractorWindowState=ExtractorModel.init() |> WindowState.Visible}
        | ExtractorHide -> {m with ExtractorWindowState=ExtractorModel.init() |> WindowState.Hidden}
        | ExtractorClose -> {m with ExtractorWindowState=WindowState.Closed}
        | ExtractorMsg msg' -> {m with ExtractorModel=ExtractorUpdate.update msg' m.ExtractorModel}
        | PrompterShow -> {m with PrompterWindowState=PrompterModel.init() |> WindowState.Visible}
        | PrompterHide -> {m with PrompterWindowState=PrompterModel.init() |> WindowState.Hidden}
        | PrompterClose -> {m with PrompterWindowState=WindowState.Closed}
        | PrompterMsg msg' -> {m with PrompterModel=PrompterUpdate.update msg' m.PrompterModel}

module AppView =
    open AppModel
    let extractorWindowBindings () = 
        ["Extractor" |> Elmish.WPF.Binding.subModel ((fun m -> m.ExtractorModel), snd, ExtractorMsg, ExtractorView.bindings)] 

    let prompterWindowBindings () = 
        ["Prompter" |> Elmish.WPF.Binding.subModel ((fun m -> m.PrompterModel), snd, PrompterMsg, PrompterView.bindings)] 

    let mainBindings (createExtractorWindow: unit -> #Window) (createPrompterWindow: unit -> #Window) () : Binding<App, AppMsg> list = [
        "ExtractorShow" |> Binding.cmd ExtractorShow
        "ExtractorHide" |> Binding.cmd ExtractorHide
        "ExtractorClose" |> Binding.cmd ExtractorClose
        "ExtractorWindow" |> Binding.subModelWin ((fun m -> m.ExtractorWindowState), fst, id, extractorWindowBindings, createExtractorWindow)
        "PrompterShow" |> Binding.cmd PrompterShow
        "PrompterHide" |> Binding.cmd PrompterHide
        "PrompterClose" |> Binding.cmd PrompterClose
        "PrompterWindow" |> Binding.subModelWin ((fun m -> m.PrompterWindowState), fst, id, prompterWindowBindings, createPrompterWindow)
    ]

module AppMain =
    let fail _ = failwith "never called"
    let mainDesignVm = ViewModel.designInstance (AppModel.init ()) (AppView.mainBindings fail fail ())
    let extractorWindowDesignVm = ViewModel.designInstance (AppModel.init ()) (AppView.extractorWindowBindings ())
    let prompterWindowDesignVm = ViewModel.designInstance (AppModel.init ()) (AppView.prompterWindowBindings ())

    let main mainWindow (createExtractorWindow: Func<#Window>) (createPrompterWindow: Func<#Window>) =
        let createExtractorWindow () = createExtractorWindow.Invoke()
        let createPrompterWindow () = createPrompterWindow.Invoke()
        let bindings = AppView.mainBindings createExtractorWindow createPrompterWindow
        Program.mkSimpleWpf AppModel.init AppUpdate.update bindings
        |> Program.startElmishLoop ElmConfig.Default mainWindow