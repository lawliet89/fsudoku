namespace fsudoku.tests

open NUnit.Framework
open fsudoku

[<TestFixture>]
type UtilityTests() = 

    [<Test>]
    member this.``IsValid returns true on values between 0 and 9 and of length 9``() = 
        Assert.IsTrue(fsudoku.program.ValidList [for x in 1..9 do yield (Some x)])

    [<Test>]
    member this.``IsValid returns false on values not between 0 and 9``() = 
        Assert.IsFalse(fsudoku.program.ValidList (Some 999 :: [for x in 1..8 do yield Some(x)]))

    [<Test>]
    member this.``IsValid returns false on a list of length that is not 9``() = 
        Assert.IsFalse(fsudoku.program.ValidList (Some 9 :: [for x in 1..9 do yield (Some x)]))