namespace Prompter
open NAudio.Wave

type PlayerState =
    | Playing
    | Stopped

type PlayerMessage =
    | PlaybackStopped of StoppedEventArgs
    | Play of string * MailboxProcessor<PlayerMessage>
    | Stop
    | Replier of AsyncReplyChannel<PlayerState>

type Player() =
    
    let looper (input: MailboxProcessor<PlayerMessage>) =
        let rec looper' player (reader:AudioFileReader option) (state: PlayerState) = 
            async {
                let! message = input.Receive()
                match message, state with
                | Play _, Playing -> do! looper' player reader state
                | Play (p, a), Stopped -> 
                    let reader' = new AudioFileReader(p)
                    let player' = new WaveOutEvent()
                    player'.Init(reader')
                    player'.PlaybackStopped.Add(fun e -> a.Post(PlaybackStopped e))
                    player'.Play()
                    do! looper' (Some player') (Some reader') Playing
                | Stop, Stopped -> do! looper' player reader state
                | Stop, Playing -> 
                    match player with 
                    | Some player' ->
                        player'.Stop() 
                        do! looper' player reader Stopped
                    | None -> do! looper' player reader Stopped
                | PlaybackStopped _, _ -> 
                    match reader with
                    | Some reader' -> 
                        reader'.Dispose()
                        do! looper' player reader Stopped
                    | None -> do! looper' player reader Stopped
                | Replier replyChannel, _ -> replyChannel.Reply(state); do! looper' player reader state
            }
        looper' None None Stopped

    let agent = MailboxProcessor<PlayerMessage>.Start(looper)

    let play (wavPath:string) =
        agent.Post(Play (wavPath, agent))

    let stop () =
        agent.Post(Stop)

    member _.PlayOrStop (wavPath: string option) = async {
            let response = agent.PostAndReply(Replier)
            match response, wavPath with
            | Playing, _ -> stop()
            | Stopped, None -> ()
            | Stopped, Some w -> play(w)
        }

    member _.Status = 
        let response = agent.PostAndReply(Replier)
        match response with
        | Playing -> "Stop"
        | Stopped -> "Play"



module Player =
    let player = Player()
    let playOrStop = player.PlayOrStop
    let status = player.Status