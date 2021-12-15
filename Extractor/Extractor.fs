namespace Prompter

open System.IO
open System.Speech.Synthesis

type private Last = {stopIndex:int; audioOffset:System.TimeSpan; phraseAudioOffset:System.TimeSpan}
type private Phrase = string 
type private ExtractorState = {last:Last; phrases:Phrase list}

type ExtractorMessage =
    | Replier of AsyncReplyChannel<Phrase list>
    | Progress of SpeakProgressEventArgs
    | Complete of SpeakCompletedEventArgs

type private Extractor(text:string) = 
    let maxPauseLength = 900.0
    let emptyState = {last={stopIndex=0; audioOffset=System.TimeSpan.Zero; phraseAudioOffset=System.TimeSpan.Zero}; phrases=[]}

    let removeNewLines (phrase: string) = phrase.Replace(System.Environment.NewLine, " ")

    let maxLength (difference:System.TimeSpan) =
        // the longer the phrase is running, the less duration difference needed to trigger the end of a phrase
        System.TimeSpan.FromMilliseconds((-0.053 * difference.TotalMilliseconds) + maxPauseLength)
    
    let looper (input: MailboxProcessor<ExtractorMessage>) =
        let rec looper' (state: ExtractorState) = 
            async {
                let! message = input.Receive()
                match message with 
                | Progress arg ->
                    let durationSinceLastAudioOffset = arg.AudioPosition - state.last.audioOffset
                    let durationSinceLastPhraseAudioOffset = arg.AudioPosition - state.last.phraseAudioOffset
                    let updatedLast = {state.last with audioOffset=arg.AudioPosition}
                    // pause isn't long enough to create phrase
                    if durationSinceLastAudioOffset < (maxLength durationSinceLastPhraseAudioOffset) then
                        do! looper' {state with last=updatedLast}
                    // pause is long enough to create phrase
                    else
                        let length = arg.CharacterPosition - state.last.stopIndex - 1
                        let phrase = text.Substring(state.last.stopIndex, if length < 0 then 0 else length).Trim() |> removeNewLines
                        let updatedLast' = {updatedLast with stopIndex=arg.CharacterPosition; phraseAudioOffset=arg.AudioPosition}
                        do! looper' {last=updatedLast'; phrases=phrase::state.phrases}
                | Complete _ ->
                    let phrase = state.last.stopIndex |> text.Substring |> removeNewLines
                    do! looper' {state with phrases=phrase::state.phrases}
                | Replier replyChannel -> replyChannel.Reply((state.phrases) |> List.rev)
            }
        looper' emptyState

    member _.Extracted() =
        let synth = new SpeechSynthesizer()
        let agent = MailboxProcessor<ExtractorMessage>.Start(looper)
        let progressHandler (arg: SpeakProgressEventArgs) = agent.Post (Progress arg)
        let completeHandler (arg: SpeakCompletedEventArgs) = agent.Post (Complete arg)
        synth.SpeakProgress.Add(progressHandler)
        synth.SpeakCompleted.Add(completeHandler)
        synth.SetOutputToNull()
        synth.Speak(text)
        synth.Dispose()
        agent.PostAndReply (Replier)

module Extracted =
    let private TransformedForOutput (version:uint) (texts:Phrase list) =
        texts
        |> List.mapi (fun i text -> (i, text))
        |> List.filter (fun (_, text) -> text |> System.String.IsNullOrEmpty |> not)
        |> List.map (fun (i, text) -> $"EJ{version:D3}-{i:D5}|{text}|{text}")
        |> String.concat System.Environment.NewLine 

    let Extracted (version:uint) (inputPath:string) (outputPath:string) = 
        let text = inputPath |> File.ReadAllText
        let toSave = Extractor(text).Extracted() |> TransformedForOutput version
        File.WriteAllText(outputPath, toSave)
