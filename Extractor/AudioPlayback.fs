namespace Prompter
open NAudio.Wave

type State =
    | Playing
    | Stopped

type Message =
    //| PlaybackStopped of StoppedEventArgs
    | Play of string
    | Stop
    | Replier of AsyncReplyChannel<State>

type private Player() =
    
    let looper (input: MailboxProcessor<Message>) =
        let rec looper' player (reader:AudioFileReader option) (state: State) = 
            async {
                let! message = input.Receive()
                match message, state with
                | Play _, Playing -> do! looper' player reader state
                | Play p, Stopped -> 
                    let reader' = new AudioFileReader(p)
                    let player' = new WaveOutEvent()
                    player'.Init(reader')
                    player'.PlaybackStopped.Add(fun _ -> reader'.Dispose())
                    player'.Play()
                    do! looper' (Some player') (Some reader') Playing
                | Stop, Stopped -> do! looper' player reader state
                | Stop, Playing -> 
                    match player with 
                    | Some player' ->
                        player'.Stop() 
                        do! looper' player reader Stopped
                    | None -> do! looper' player reader Stopped
                | Replier replyChannel, _ -> replyChannel.Reply(state)
            }
        looper' None None Stopped

    let agent = MailboxProcessor<Message>.Start(looper)

    member _.Play (wavPath:string) =
        agent.Post(Play wavPath)

    member _.Stop =
        agent.Post(Stop)

    member _.Status =
        agent.PostAndReply(Replier)