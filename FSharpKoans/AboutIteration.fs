namespace FSharpKoans
open NUnit.Framework

module ``05: To iterate is human; to recurse, divine`` =
    (*
        The `rec` keyword exposes the function identifier for use inside the function.
        And that's literally all that it does - it has no other purpose whatsoever.
    *)

    [<Test>]
    let ``01 `rec` exposes the name of the function for use inside the function`` () =
        let rec converge d c n =
            match d = c with
            | false ->
                match d < c with
                | true -> converge (d+10) c (n+1)
                | false -> converge (d - 1) c (n+1)
            | true -> n
        converge 3 10 0 |> should equal 4

    [<Test>]
    let ``02 Tail recursion stops a stack overflow from occurring`` () =
        // CHANGE the recursive function to be tail recursive.
        let myfun n =
            let sq = n*n //sq = 144
            let v = sq*sq*sq*sq // v = 429 000 000
            let rec inner count fuck = //count = 144 || fuck = 1
                match count = v with // is count (144) = v (429 000 000)
                | true -> 0 // base case
                | false -> inner (count+1) (count-fuck) // inner (145) (143)
            inner sq 1

        myfun 12 |> should equal 0