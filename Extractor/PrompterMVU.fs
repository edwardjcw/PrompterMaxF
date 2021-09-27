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