namespace Prompter

module PrompterModel =
    open Elmish
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

    type CmdMsg = | PlayAudio of Player * string option

    type Playing =
        | JustStarted
        | Status
        | Finished
    
    type Msg =
        | LoadPrompts
        | RequestPlayAudio
        | PlayingAudio of Playing
        | PlayFailed of exn
        | RecordAudio
        | PreviousPrompt
        | NextPrompt
        | SetMetadataPath of string
        | SetWavDirectory of string
        | SetAutoAdvanceChecked of bool
        | SetGotoIndexOf of string
        | SetPlayButtonText

    let init() = ({
        MetadataPath = @"C:\Users\edwar\Downloads\sherlockReadable.txt"
        WavDirectory = @"C:\Users\edwar\Downloads\wavOutput"
        PreviousText = "previous"
        CurrentText = "current"
        NextText = "next"
        PlayButtonText = ""
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
    }, 
        [])



module PrompterUpdate =
    open Elmish
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
            m |> promptDisplay (m.MetadataPath |> Carrier.prompts m.WavDirectory), Cmd.none
        | RequestPlayAudio ->
            let play (player:Player, wavFile) =
                player.PlayOrStop wavFile |> Async.Start
                PlayingAudio JustStarted
            let a, s = m.AudioPlayback, (m.Prompts.Current |> Carrier.wav)
            m, Cmd.OfFunc.either play (a, s) id PlayFailed
        | PlayingAudio status ->
            match status with
            | JustStarted -> {m with PlayButtonText="Stop"}, Cmd.ofMsg (PlayingAudio Status)
            | Status ->
                let status = m.AudioPlayback.Status
                let playingStatus = if status = "Play" then Finished else Status 
                m, Cmd.ofMsg (PlayingAudio playingStatus)
            | Finished -> {m with PlayButtonText="Play"}, Cmd.none
        | PlayFailed e -> failwith (e.ToString()) //not implemented yet
        | RecordAudio -> m, Cmd.none // not implemented yet
        | PreviousPrompt -> 
            m |> promptDisplay (Carrier.previous m.Prompts), Cmd.none
        | NextPrompt ->
            m |> promptDisplay (Carrier.next m.Prompts), Cmd.none
        | SetMetadataPath s -> {m with MetadataPath=s}, Cmd.none
        | SetWavDirectory s -> {m with WavDirectory=s}, Cmd.none
        | SetAutoAdvanceChecked b -> {m with AutoAdvanceChecked=b}, Cmd.none
        | SetGotoIndexOf s -> 
            match s with
            | Int i -> 
                let prompts = Carrier.goto (i-1) m.Prompts
                if prompts = m.Prompts then m, [] // if it's the same prompt, then either the goto is bad or nothing's changed
                else m |> promptDisplay prompts, [] // only accept a change if goto is good
            | _ -> m, []
        | SetPlayButtonText -> {m with PlayButtonText=m.AudioPlayback.Status}, Cmd.none

    let playButtonSub _ =
        let sub dispatch =
            let timer = new System.Timers.Timer(500.);
            timer.Elapsed.Add (fun _ -> dispatch (SetPlayButtonText))
            timer.Start()
        Cmd.ofSub sub

    //let buttonUse m = [playButtonSub m]

module PrompterView =
    open Elmish.WPF
    open Elmish
    open PrompterModel
    let bindings () = [
        "MetadataPath" |> Binding.twoWay ((fun m -> m.MetadataPath), (fun newVal _ -> newVal |> SetMetadataPath))
        "WavDirectory" |> Binding.twoWay ((fun m -> m.WavDirectory), (fun newVal _ -> newVal |> SetWavDirectory))
        "LoadPrompts" |> Binding.cmd (fun _ -> LoadPrompts)
        "PreviousText" |> Binding.oneWay (fun m -> m.PreviousText)
        "CurrentText" |> Binding.oneWay (fun m -> m.CurrentText)
        "NextText" |> Binding.oneWay (fun m -> m.NextText)
        "PlayButtonText" |> Binding.oneWay (fun m -> m.AudioPlayback.Status)
        "PlayAudio" |> Binding.cmd (fun _ -> RequestPlayAudio)
        "PlayButtonEnabled" |> Binding.oneWay (fun m -> m.PlayButtonEnabled)
        "RecordButtonText" |> Binding.oneWay (fun m -> m.RecordButtonText)
        "RecordAudio" |> Binding.cmd (fun _ -> RecordAudio)
        "RecordButtonEnabled" |> Binding.oneWay (fun m -> m.RecordButtonEnabled)
        "AutoAdvanceChecked" |> Binding.twoWay ((fun m -> m.AutoAdvanceChecked), fun newVal _ -> newVal |> SetAutoAdvanceChecked)
        "AutoAdvanceEnabled" |> Binding.oneWay (fun m -> m.AutoAdvanceEnabled)
        "PreviousPrompt" |> Binding.cmd (fun _ -> PreviousPrompt)
        "PreviousButtonEnabled" |> Binding.oneWay (fun m -> m.PreviousButtonEnabled)
        "NextPrompt" |> Binding.cmd (fun _ -> NextPrompt)
        "NextButtonEnabled" |> Binding.oneWay (fun m -> m.NextButtonEnabled)
        "GotoIndexOf" |> Binding.twoWay ((fun m -> m.GotoIndexOf), (fun newVal _ -> newVal |> SetGotoIndexOf))
        "GotoIndexOfEnabled" |> Binding.oneWay (fun m -> m.GotoIndexOfEnabled)
        "GotoOutOf" |> Binding.oneWay (fun m -> m.GotoOutOf)
    ]

module PrompterMain =
    open Elmish.WPF
    open Elmish
    open PrompterModel
    open PrompterUpdate
    open PrompterView

    let main window =
        let config = {ElmConfig.Default with LogConsole = true; Measure = true; LogTrace = true}
        Program.mkProgramWpf init update bindings
        |> Program.withSubscription playButtonSub
        |> Program.withDebugTrace
        |> Program.startElmishLoop config window