namespace Prompter

module ExtractorModel =
    type Model = {
        InputPath: string
        OutputPath: string
        Version: uint
        Done: bool
    }

    let init() = {
        InputPath =  @"C:\Users\edwar\Downloads\sherlock3.txt"
        OutputPath = @"C:\Users\edwar\Downloads\fsharpoutputTest.txt"
        Version = 1u
        Done = false
    }

    type Msg = 
        | CreatePrompts
        | SetInputPath of string
        | SetOutputPath of string
        | SetVersion of uint 

module ExtractorUpdate =
    open ExtractorModel
    open Prompter.Extracted
    let update msg m =
        match msg with
        | CreatePrompts -> Extracted m.Version m.InputPath m.OutputPath; {m with Done = true}
        | SetInputPath s -> {m with InputPath = s; Done = false}
        | SetOutputPath s -> {m with OutputPath = s; Done = false}
        | SetVersion v -> {m with Version = v; Done = false}

module ExtractorView =
    open Elmish.WPF
    open ExtractorModel
    let bindings () = [
        "InputPath" |> Binding.twoWay ((fun m -> m.InputPath), (fun newVal _ -> newVal |> SetInputPath))
        "OutputPath" |> Binding.twoWay ((fun m -> m.OutputPath), (fun newVal _ -> newVal |> SetOutputPath))
        "Version" |> Binding.twoWay ((fun m -> uint m.Version), (fun newVal _ -> uint newVal |> SetVersion))
        "Done" |> Binding.oneWay (fun m -> m.Done)
        "CreatePrompts" |> Binding.cmd (fun _ -> CreatePrompts)
    ]

module ExtractorMain =
    open Elmish.WPF
    open ExtractorModel
    open ExtractorUpdate
    open ExtractorView
    let main window = 
        let config = ElmConfig.Default
        Program.mkSimpleWpf init update bindings
        |> Program.startElmishLoop config window