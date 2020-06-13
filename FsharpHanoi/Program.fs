open FSharpPlus
open FSharpPlus.Data

let ($) a b = a b

type Step<'T> =
    { Src : 'T
      Dst : 'T
      Height : int }

let cons x y = x :: y
let push x = modify $ cons x

let rec hanoi src bare dst (height : int) =
    match height with
    | 0 -> State $ fun s -> ((), s) // return ()
    | _ ->
        monad {
            do! hanoi src dst bare $ height - 1
            do! push $ { Src = src
                         Dst = dst
                         Height = height }
            do! hanoi bare src dst $ height - 1
        }

[<EntryPoint>]
let main argv =
    let result =
        monad {
            do! hanoi "Left" "Mid" "Right" 3
            do! modify rev
        }
    printfn "%A" $ State.exec result []
    0
