module PrompterTest

open NUnit.Framework
open Prompter.Prompt
open Prompter.Carrier

[<SetUp>]
let Setup () =
    ()

let prompt1 = {emptyPrompt with Id=0}
let prompt2 = {emptyPrompt with Id=1}
let prompt3 = {emptyPrompt with Id=2}
let prompt4 = {emptyPrompt with Id=3}
let prompt5 = {emptyPrompt with Id=4}

let location1 = Beginning {Previous=emptyPrompt; Current=prompt1; Next=prompt2}
let location2 = Middle {Previous=prompt1; Current=prompt2; Next=prompt3}
let location3 = Middle {Previous=prompt2; Current=prompt3; Next=prompt4}
let location4 = End {Previous=prompt3; Current=prompt4; Next=emptyPrompt}
let locationListBeginningMiddleEnd = [location1; location2; location3; location4]

let locationAll = All {Previous=emptyPrompt; Current=prompt1; Next=emptyPrompt}
let locationListAll = [locationAll]

let locationListError = [Error]
let locationListNone = [None]

module NextTest =
    let nextTest locationList startCurrent expectedCurrent =
        let initial = {
            Prompts = locationList
            Current = startCurrent
        }
        let expected = {
               Prompts = locationList
               Current = expectedCurrent
        } 
        let actual = next initial 

        Assert.AreEqual(expected, actual)
    
    [<Test>]
    let ``next from beginning moves next`` () =
        nextTest locationListBeginningMiddleEnd location1 location2

    [<Test>]
    let ``next from middle moves next`` () =
        nextTest locationListBeginningMiddleEnd location2 location3

    [<Test>]
    let ``next from end stays on end`` () =
        nextTest locationListBeginningMiddleEnd location4 location4

    [<Test>]
    let ``next from all stays on all`` () =
        nextTest locationListAll locationAll locationAll

    [<Test>]
    let ``next from error stays on error`` () =
        nextTest locationListError Error Error

    [<Test>]
    let ``next from none stays on none`` () =
        nextTest locationListNone None None

module PreviousTest =
    let previousTest locationList startCurrent expectedCurrent =
        let initial = {
            Prompts = locationList
            Current = startCurrent
        }
        let expected = {
               Prompts = locationList
               Current = expectedCurrent
        } 
        let actual = previous initial 

        Assert.AreEqual(expected, actual)

    [<Test>]
    let ``previous from end moves previous`` () =
        previousTest locationListBeginningMiddleEnd location4 location3

    [<Test>]
    let ``previous from middle moves previous`` () =
        previousTest locationListBeginningMiddleEnd location3 location2

    [<Test>]
    let ``previous from beginning stays on beginning`` () =
        previousTest locationListBeginningMiddleEnd location1 location1

    [<Test>]
    let ``previous from all stays on all`` () =
        previousTest locationListAll locationAll locationAll

    [<Test>]
    let ``previous from error stays on error`` () =
        previousTest locationListError Error Error

    [<Test>]
    let ``previous from none stays on none`` () =
        previousTest locationListNone None None

module GotoTest =
    let gotoTest locationList startCurrent expectedCurrent index =
        let initial = {
            Prompts = locationList
            Current = startCurrent
        }
        let expected = {
               Prompts = locationList
               Current = expectedCurrent
        } 
        let actual = goto index initial 

        Assert.AreEqual(expected, actual)

    [<Test>]
    let ``goto from middle backward to start`` () =
        gotoTest locationListBeginningMiddleEnd location3 location1 0
        gotoTest locationListBeginningMiddleEnd location3 location1 -1 // before start

    [<Test>]
    let ``goto from middle forward to end`` () =
        gotoTest locationListBeginningMiddleEnd location2 location4 3
        gotoTest locationListBeginningMiddleEnd location2 location4 4 // after end

    [<Test>]
    let ``goto from same to same`` () =
        gotoTest locationListBeginningMiddleEnd location3 location3 2

    [<Test>]
    let ``goto from all stays on all`` () =
        gotoTest locationListAll locationAll locationAll 1
        gotoTest locationListAll locationAll locationAll 0
        gotoTest locationListAll locationAll locationAll -1

    [<Test>]
    let ``goto from error stays on error`` () =
        gotoTest locationListError Error Error 1
        gotoTest locationListError Error Error 0
        gotoTest locationListError Error Error -1

    [<Test>]
    let ``goto from none stays on none`` () =
        gotoTest locationListNone None None 1
        gotoTest locationListNone None None 0
        gotoTest locationListNone None None -1