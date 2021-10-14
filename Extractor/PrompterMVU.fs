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
        AudioPlayback: Player
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
        AudioPlayback = Player.player
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

    // handles showing of the prompts and effects of navigating them
    let promptDisplay (prompts:Carrier.Prompts) m' =
        let previousText = prompts.Current |> Carrier.previousText
        let currentText = prompts.Current |> Carrier.currentText
        let nextText = prompts.Current |> Carrier.nextText

        {m' with 
            Prompts=prompts
            PreviousText = previousText
            CurrentText = currentText
            NextText = nextText
            PlayButtonEnabled = prompts.Current |> Carrier.wavExists
            RecordButtonEnabled = currentText.Trim() <> ""
            AutoAdvanceEnabled = currentText.Trim() <> ""
            PreviousButtonEnabled = prompts.Current |> Carrier.hasPrevious
            NextButtonEnabled = prompts.Current |> Carrier.hasNext
            GotoIndexOf = prompts.Current |> Carrier.currentIndex
            GotoIndexOfEnabled = prompts.Current |> Carrier.hasGotoIndex
            GotoOutOf = $" of {prompts.Prompts.Length}"} 

    let update msg m =
        match msg with
        | LoadPrompts -> 
            m |> promptDisplay (m.MetadataPath |> Carrier.prompts m.WavDirectory)
        | PlayAudio -> m.AudioPlayback.PlayOrStop (m.Prompts.Current |> Carrier.wav) |> ignore; m
        | RecordAudio -> m // not implemented yet
        | PreviousPrompt -> 
            m |> promptDisplay (Carrier.previous m.Prompts)
        | NextPrompt ->
            m |> promptDisplay (Carrier.next m.Prompts)
        | SetMetadataPath s -> {m with MetadataPath=s}
        | SetWavDirectory s -> {m with WavDirectory=s}
        | SetAutoAdvanceChecked b -> {m with AutoAdvanceChecked=b}
        | SetGotoIndexOf s -> 
            match s with
            | Int i -> 
                let prompts = Carrier.goto (i-1) m.Prompts
                if prompts = m.Prompts then m // if it's the same prompt, then either the goto is bad or nothing's changed
                else m |> promptDisplay prompts // only accept a change if goto is good
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
        "PlayButtonText" |> Binding.oneWay (fun m -> m.AudioPlayback.Status)
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