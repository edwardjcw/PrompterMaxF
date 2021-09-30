namespace Prompter

module PrompterModel =
    type Model = {
        MetadataPath: string
        WavDirectory: string
        PreviousText: string
        CurrentText: string
        NextText: string
        PlayButtonText: string
        PlayButtonEnabled: bool
        RecordButtonText: string
        RecordButtonEnabled: bool
        AutoAdvanceChecked: bool
        AutoAdvanceEnabled: bool
        PreviousButtonEnabled: bool
        NextButtonEnabled: bool
        GotoIndexOf: string
        GotoIndexOfEnabled: bool
        GotoOutOf: string
        Prompts: Carrier.Prompts
    }

    let init() = {
        MetadataPath = @"C:\Users\edwar\Downloads\sherlockReadable.txt"
        WavDirectory = @"C:\Users\edwar\Downloads\wavOutput"
        PreviousText = "previous"
        CurrentText = "current"
        NextText = "next"
        PlayButtonText = "Play"
        PlayButtonEnabled = false
        RecordButtonText = "Record"
        RecordButtonEnabled = false
        AutoAdvanceChecked = true
        AutoAdvanceEnabled = false
        PreviousButtonEnabled = false
        NextButtonEnabled = false
        GotoIndexOf = "0"
        GotoIndexOfEnabled = false
        GotoOutOf = " of 000000"
        Prompts = Carrier.emptyPrompts
    }

    type Msg =
        | LoadPrompts
        | PlayAudio
        | RecordAudio
        | PreviousPrompt
        | NextPrompt
        | SetMetadataPath of string
        | SetWavDirectory of string
        | SetAutoAdvanceChecked of bool
        | SetGotoIndexOf of string

module PrompterUpdate =
    open PrompterModel
    open Prompter
    let (|Int|_|) (str:string) =
        match System.Int32.TryParse str with
        | true,int -> Some int
        | _ -> None

    let update msg m =
        match msg with
        | LoadPrompts -> 
            let prompts = m.MetadataPath |> Carrier.prompts m.WavDirectory
            {m with Prompts=prompts}
        | PlayAudio | RecordAudio -> m // not implemented yet
        | PreviousPrompt -> 
            {m with Prompts=Carrier.previous m.Prompts}
        | NextPrompt ->
            {m with Prompts=Carrier.next m.Prompts}
        | SetMetadataPath s -> {m with MetadataPath=s}
        | SetWavDirectory s -> {m with WavDirectory=s}
        | SetAutoAdvanceChecked b -> {m with AutoAdvanceChecked=b}
        | SetGotoIndexOf s -> 
            match s with
            | Int i -> {m with GotoIndexOf=s; Prompts=Carrier.goto i m.Prompts }
            | _ -> m

module PrompterView =
    open Elmish.WPF
    open PrompterModel
    let bindings () = [
        "MetadataPath" |> Binding.twoWay ((fun m -> m.MetadataPath), (fun newVal m -> newVal |> SetMetadataPath))
        "WavDirectory" |> Binding.twoWay ((fun m -> m.WavDirectory), (fun newVal m -> newVal |> SetWavDirectory))
        "LoadPrompts" |> Binding.cmd (fun m -> LoadPrompts)
        "PreviousText" |> Binding.oneWay (fun m -> m.PreviousText)
        "CurrentText" |> Binding.oneWay (fun m -> m.CurrentText)
        "NextText" |> Binding.oneWay (fun m -> m.NextText)
        "PlayButtonText" |> Binding.oneWay (fun m -> m.PlayButtonText)
        "PlayAudio" |> Binding.cmd (fun m -> PlayAudio)
        "PlayButtonEnabled" |> Binding.oneWay (fun m -> m.PlayButtonEnabled)
        "RecordButtonText" |> Binding.oneWay (fun m -> m.RecordButtonText)
        "RecordAudio" |> Binding.cmd (fun m -> RecordAudio)
        "RecordButtonEnabled" |> Binding.oneWay (fun m -> m.RecordButtonEnabled)
        "AutoAdvanceChecked" |> Binding.twoWay ((fun m -> m.AutoAdvanceChecked), fun newVal m -> newVal |> SetAutoAdvanceChecked)
        "AutoAdvanceEnabled" |> Binding.oneWay (fun m -> m.AutoAdvanceEnabled)
        "PreviousPrompt" |> Binding.cmd (fun m -> PreviousPrompt)
        "PreviousButtonEnabled" |> Binding.oneWay (fun m -> m.PreviousButtonEnabled)
        "NextPrompt" |> Binding.cmd (fun m -> NextPrompt)
        "NextButtonEnabled" |> Binding.oneWay (fun m -> m.NextButtonEnabled)
        "GotoIndexOf" |> Binding.twoWay ((fun m -> m.GotoIndexOf), (fun newVal m -> newVal |> SetGotoIndexOf))
        "GotoIndexOfEnabled" |> Binding.oneWay (fun m -> m.GotoIndexOfEnabled)
        "GotoOutOf" |> Binding.oneWay (fun m -> m.GotoOutOf)
    ]

module PrompterMain =
    open Elmish.WPF
    open PrompterModel
    open PrompterUpdate
    open PrompterView
    let main window =
        let config = ElmConfig.Default
        Program.mkSimpleWpf init update bindings
        |> Program.startElmishLoop config window