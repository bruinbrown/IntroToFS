namespace IntroToFS.Demos

open NUnit.Framework
open FsUnit.TopLevelOperators

[<TestFixture>]
module NumberTests = 
    
    [<Test>]
    let AddingTwoNumbers_TwoBigNumbers_ResultShouldOverflow () =
        ()

    [<Test>]
    let ``5 + 7 should equal 12`` () =
        let result = 5 + 7
        result |> should equal 12