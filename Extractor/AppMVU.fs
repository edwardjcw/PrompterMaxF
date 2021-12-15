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
        | PrompterAction
        | PrompterCommandError of exn

    type AppCmdMsg = | PrompterCommand of PrompterModel.CmdMsg list

    let init () = ({
        ExtractorWindowState=WindowState.Closed
        ExtractorModel=ExtractorModel.init ()
        PrompterWindowState=WindowState.Closed
        PrompterModel=PrompterModel.init () |> fst
    }, [])

module AppUpdate =
    open AppModel
    open Elmish

    let update msg m : App * AppCmdMsg list = 
        match msg with
        | ExtractorShow -> {m with ExtractorWindowState=ExtractorModel.init() |> WindowState.Visible}, []
        | ExtractorHide -> {m with ExtractorWindowState=ExtractorModel.init() |> WindowState.Hidden}, []
        | ExtractorClose -> {m with ExtractorWindowState=WindowState.Closed}, []
        | ExtractorMsg msg' -> {m with ExtractorModel=ExtractorUpdate.update msg' m.ExtractorModel}, []
        | PrompterShow -> {m with PrompterWindowState=PrompterModel.init() |> fst |> WindowState.Visible}, []
        | PrompterHide -> {m with PrompterWindowState=PrompterModel.init() |> fst |> WindowState.Hidden}, []
        | PrompterClose -> {m with PrompterWindowState=WindowState.Closed}, []
        | PrompterMsg msg' -> 
            let updated = PrompterUpdate.update msg' m.PrompterModel
            {m with PrompterModel= updated |> fst}, [PrompterCommand (updated |> snd)]
        | PrompterAction -> m, []
        | PrompterCommandError e -> failwith (e.ToString())

    let prompterTask cmd = async {
        cmd |> List.map (PrompterView.toCmd) |> ignore
        return PrompterAction
    }

    let toCmd (command: AppCmdMsg) : Cmd<AppMsg> =
        match command with
        | PrompterCommand c -> Cmd.OfAsync.either prompterTask c id PrompterCommandError

module AppView =
    open AppModel
    let extractorWindowBindings () = 
        ["Extractor" |> Binding.subModel ((fun m -> m.ExtractorModel), snd, ExtractorMsg, ExtractorView.bindings)] 

    let prompterWindowBindings () = 
        ["Prompter" |> Binding.subModel ((fun m -> m.PrompterModel), snd, PrompterMsg, PrompterView.bindings)] 

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
    let mainDesignVm = ViewModel.designInstance (AppModel.init () |> fst) (AppView.mainBindings fail fail ())
    let extractorWindowDesignVm = ViewModel.designInstance (AppModel.init () |> fst) (AppView.extractorWindowBindings ())
    let prompterWindowDesignVm = ViewModel.designInstance (AppModel.init() |> fst) (AppView.prompterWindowBindings ())

    let main mainWindow (createExtractorWindow: Func<#Window>) (createPrompterWindow: Func<#Window>) =
        let createExtractorWindow () = createExtractorWindow.Invoke()
        let createPrompterWindow () = createPrompterWindow.Invoke()
        let bindings = AppView.mainBindings createExtractorWindow createPrompterWindow
        let config = {ElmConfig.Default with LogConsole = true; Measure = true; LogTrace = true}
        Program.mkProgramWpfWithCmdMsg AppModel.init AppUpdate.update bindings AppUpdate.toCmd
        |> Program.withDebugTrace
        |> Program.startElmishLoop config mainWindow