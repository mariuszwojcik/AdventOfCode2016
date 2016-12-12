namespace AdventOfCode

    open System.Text;

    module Day1 =

        type Direction =
        | North
        | East
        | South
        | West
        
        type TurnDirection =
        | Right
        | Left

        type Coords = {
            X: int
            Y: int
            D: Direction
        }

        let turn d td =
            match td with
            | Right ->
                match d with
                | North -> East
                | East -> South
                | South -> West
                | West -> North
            | Left ->
                match d with
                | North -> West
                | West -> South
                | South -> East
                | East -> North

        let parseDirection (s:string) =
            match s with
            | "R" -> Right
            | "L" -> Left
            |_ -> failwith "Unrecognised value."

        let parseStep (s:string) =
            let s' = s.Trim()
            let turn = parseDirection(s'.Substring(0, 1))
            let steps = System.Int32.Parse(s'.Substring(1))
            (turn, steps)

        let move c s =
            //printfn "1: %A | %A" c s
            
            let d' = turn c.D (fst s)
            let s' = snd s
            match d' with
            | North -> { X = c.X + s'; Y = c.Y; D = d' }
            | East -> { X = c.X; Y = c.Y + s'; D = d' }
            | South -> { X = c.X - s'; Y = c.Y; D = d' }
            | West -> { X = c.X; Y = c.Y - s'; D = d' }

        let CalculateDistance (input:string) =
            let c = { X=0; Y=0; D=North}
            
            let c' = 
                input.Split [|','|]
                |> Seq.map parseStep
                |> Seq.fold(fun acc s -> move acc s) c

            System.Math.Abs c'.X + System.Math.Abs c'.Y
