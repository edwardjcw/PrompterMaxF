namespace Prompter

open System.IO

module Prompt =
    type Prompt = {
        Id: int
        Name: string
        Transcription: string
        Normalized: string
        WavPath: string
    }

    let emptyPrompt = {
        Id= -1
        Name=""
        Transcription=""
        Normalized=""
        WavPath=""
    }


    let loadPrompts wavDirectory promptFile=
        let wavConstruction directory filename = Path.Combine(directory, $"{filename}.wav")

        promptFile 
        |> File.ReadAllLines
        |> Array.map (fun rawPrompt -> rawPrompt.Split('|'))
        |> Array.filter (fun parts -> parts.Length = 3)
        |> Array.mapi (fun i parts -> {
            Id=i 
            Name=parts.[0]
            Transcription=parts.[1] 
            Normalized=parts.[2]
            WavPath=(parts.[0] |> wavConstruction wavDirectory)
            })
        |> Array.toList

module Carrier =
    open Prompt

    type Direction =
        | Back
        | Forward
        | Still

    type PromptWindowed = {
        Previous: Prompt;
        Current: Prompt;
        Next: Prompt 
    }

    let emptyPromptWindowed = {Previous=emptyPrompt; Current=emptyPrompt; Next=emptyPrompt}

    type Location = 
        | Beginning of PromptWindowed
        | Middle of PromptWindowed
        | End of PromptWindowed
        | All of PromptWindowed // both Beginning and End
        | Error
        | None

    type Prompts = {
        Prompts: Location list
        Current: Location
    }

    let emptyPrompts = {Prompts=[]; Current=None}

    let addStartEndBuffer prompts = emptyPrompt::prompts@[emptyPrompt]

    let prompted = function
        | [_; c; _] when c = emptyPrompt -> Error
        | [p; c; n] when p = emptyPrompt && n = emptyPrompt -> All {emptyPromptWindowed with Current=c}
        | [p; c; n] when p = emptyPrompt -> Beginning {emptyPromptWindowed with Current=c; Next=n}
        | [p; c; n] when n = emptyPrompt -> End {emptyPromptWindowed with Previous=p; Current=c}
        | [p; c; n] -> Middle {Previous=p; Current=c; Next=n}
        | _ -> None

    let promptsWindowed wavDirectory inputPath =
        inputPath
        |> (loadPrompts wavDirectory)
        |> List.filter (fun p -> p <> emptyPrompt)
        |> List.distinct // get rid of duplicates, just in case
        |> addStartEndBuffer
        |> List.windowed 3
        |> List.map prompted

    // main entry function
    let prompts wavDirectory inputPath = 
        promptsWindowed wavDirectory inputPath
        |> function 
            | [] -> {Prompts=[]; Current=None}
            | f::_ as locations -> {Prompts=locations; Current=f}

    let foundLocation (toFind:Prompt) (prompts:Location list) =
        let finder location =
            match location with
            | None | Error -> false
            | All {Current=c} | Beginning {Current=c} | Middle {Current=c} | End {Current=c} -> c = toFind
        prompts |> List.find finder
            
    let next {Prompts=prompts; Current=location} =
        match location with
        | Error | None | All _ | End _ -> {Prompts=prompts; Current=location}
        | Middle {Next=n} | Beginning {Next=n} -> {Prompts=prompts; Current=foundLocation n prompts}

    let previous {Prompts=prompts; Current=location} =
        match location with
        | Error | None | All _ | Beginning _ -> {Prompts=prompts; Current=location}
        | Middle {Previous=p} | End {Previous=p} -> {Prompts=prompts; Current=foundLocation p prompts}

    // traverses prompts going forward or back to find the new location that matches the index
    let goto index {Prompts=prompts; Current=location} =
        let direction = function
            | Error | None | All _ -> Still
            | Beginning {Current={Id=i}} -> if index <= i then Still else Forward
            | End {Current={Id=i}} -> if index >= i then Still else Back
            | Middle {Current={Id=i}} -> if index = i then Still elif index < i then Back else Forward 
        let rec looper location' = function
            | Still -> {Prompts=prompts; Current=location'}
            | Back ->   let {Prompts=_; Current=location''} = (previous {Prompts=prompts; Current=location'})
                        looper location'' (direction location'')
            | Forward ->    let {Prompts=_; Current=location''} = (next {Prompts=prompts; Current=location'})
                            looper location'' (direction location'')
        looper location (direction location)

    // starts from the beginning and finds the last prompt with a wav file or else returns first prompt
    let lastPromptWithWav {Prompts=prompts; Current=_} =
        let rec looper prompts' result =
            match prompts' with
            | [] -> result
            | f::r ->   match f with
                        | Error | None | All _ -> looper r result 
                        | Beginning {Current=c} | Middle {Current=c} | End {Current=c} ->
                            if c.WavPath |> File.Exists then looper r f
                            else looper r result
        looper prompts (prompts |> List.item 0)

    let previousText = function 
        | Beginning {Previous=p}
        | Middle {Previous=p}
        | End {Previous=p}
        | All {Previous=p} -> p.Normalized
        | Error | None -> ""

    let currentText = function 
        | Beginning {Current=c}
        | Middle {Current=c}
        | End {Current=c}
        | All {Current=c} -> c.Normalized
        | Error | None -> ""

    let nextText = function 
        | Beginning {Next=n}
        | Middle {Next=n}
        | End {Next=n}
        | All {Next=n} -> n.Normalized
        | Error | None -> ""

    let wavExists = function
        | Beginning {Current=c}
        | Middle {Current=c}
        | End {Current=c}
        | All {Current=c} -> c.WavPath |> File.Exists
        | Error | None -> false

    let hasPrevious = function 
        | Beginning _ -> false
        | Middle _ -> true
        | End _ -> true
        | All _ -> false
        | Error | None -> false

    let hasNext = function 
        | Beginning _ -> true
        | Middle _ -> true
        | End _ -> false
        | All _ -> false
        | Error | None -> false

    let currentIndex = function
        | Beginning {Current=c}
        | Middle {Current=c}
        | End {Current=c}
        | All {Current=c} -> (c.Id + 1).ToString() 
        | Error | None -> "0"

    let hasGotoIndex = function
        | Beginning _
        | Middle _
        | End _ -> true
        | All _ -> false 
        | Error | None -> false