namespace FSharpKoans
open FSharpKoans.Core

//---------------------------------------------------------------
// Getting Started
//
// The F# Koans are a set of exercises designed to get you familiar
// with F#. By the time you're done, you'll have a basic
// understanding of the syntax of F# and learn a little more
// about functional programming in general.
//
// Answering Problems
//
// This is where the fun begins! Each Koan method contains
// an example designed to teach you a lesson about the F# language.
// If you execute the program defined in this project, you will get
// a message that the AssertEquality koan below has failed. Your
// job is to fill in the blank (the __ symbol) to make it pass. Once
// you make the change, re-run the program to make sure the koan
// passes, and continue on to the next failing koan.  With each
// passing koan, you'll learn more about F#, and add another
// weapon to your F# programming arsenal.
//---------------------------------------------------------------

[<Koan(Sort = 1)>]
module KoansExercise =

    type Player = PlayerId of string

    type CardSuit =
        | Clubs     // ♣
        | Diamonds  // ♦
        | Hearts    // ♥
        | Spades    // ♠

    type CardRank =
        | Ace
        | Rank of int
        | Jack
        | Queen
        | King

    type Card = CardSuit * CardRank

    let makeCardC r s =
        let rank =
            match r with
            | 'A' -> Ace
            | '2' -> Rank 2
            | '3' -> Rank 3
            | '4' -> Rank 4
            | '5' -> Rank 5
            | '6' -> Rank 6
            | '7' -> Rank 7
            | '8' -> Rank 8
            | '9' -> Rank 9
            | 'T' -> Rank 10
            | 'J' -> Jack
            | 'Q' -> Queen
            | 'K' -> King
            | _ -> failwith "invalid rank"
        let suit =
            match s with
            | 'C' -> Clubs
            | 'D' -> Diamonds
            | 'H' -> Hearts
            | 'S' -> Spades
            | _ -> failwith "invalid suit"
        suit, rank

    let makeCard (s: string) =
        makeCardC s.[0]  s.[1]

    let makeCards cards =
        cards |> List.map makeCard

    let player1 = PlayerId "Player 1 - Alice"
    let player2 = PlayerId "Player 2 - Bob"

    [<Koan>]
    let BlackJackScore() =
        // Number cards count as their number, the jack, queen, and king ("face cards" or "pictures")
        // count as 10, and aces count as either 1 or 11 according to the player's choice. If the total
        // exceeds 21 points, it busts, and all bets on it immediately lose.
        // When calculating the score, always count an ace as 11 unless this would make the hand bust
        // Use an option as return type, with None meaning bust

        let calculateBlackJackScore cards =

            let add score (suite, rank) =
                match rank with
                | Rank (value) -> value + score
                | Jack | Queen | King -> 10 + score
                | Ace when score <= 10 -> 11 + score
                | Ace -> 1 + score

            let total =
                cards |> List.sortByDescending (fun (s,r) -> r) |> List.fold add 0
            
            if total > 21 then None else Some total

        AssertEquality (Some 13) (calculateBlackJackScore (makeCards ["3H"; "QD" ]))
        AssertEquality (Some 17) (calculateBlackJackScore (makeCards ["JD"; "5H"; "2S" ]))
        AssertEquality (Some 16) (calculateBlackJackScore (makeCards ["9C"; "3H"; "2D"; "2S" ]))
        AssertEquality (Some 21) (calculateBlackJackScore (makeCards ["AS"; "QH" ]))
        AssertEquality (Some 12) (calculateBlackJackScore (makeCards ["AH"; "3C"; "8C" ]))
        AssertEquality None (calculateBlackJackScore (makeCards ["JD"; "4S"; "8H" ]))


    // by setting the sead, we ensure the same random numbers come out
    // which is useful in this testing context
    let rnd = new System.Random(42)

    let deck =
        [ for s in ['C'; 'D'; 'H'; 'S' ] do
            for r in  ['Q'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K' ] do
                yield makeCardC r s ]

    let dealCards() =
        let toBeDelt = new ResizeArray<Card>(deck)
        let getCard() =
            let i = rnd.Next(toBeDelt.Count)
            let card = toBeDelt.[i]
            toBeDelt.RemoveAt(i)
            card

        let rec loop p1Cards p2Cards =
            if toBeDelt.Count > 1 then
                let p1Card = getCard()
                let p2Card = getCard()
                loop (p1Card :: p1Cards) (p2Card :: p2Cards)
            else
                 p1Cards, p2Cards

        loop [] []

    [<Koan>]
    let WarGame() =
        // There are two players.
        // The cards are all dealt equally to each player.
        // Each round, player 1 lays a card down face up at the same time that player 2 lays a card down face up. Whoever has the
        // highest value card, wins both round and take back their card, the other card is put aside.
        // The winning player's cards is added to the bottom of the winner's deck.
        // Aces are high.
        // If both cards are of equal value - both cards are discarded.
        // The player that runs out of cards loses.

        let playGame() =

            let getstrfromcard card =
                match card |> snd with
                | Rank x -> x
                | Jack -> 11
                | Queen -> 12
                | King -> 13
                | Ace -> 14

            let rec playLoop p1Cards p2Cards =

                if p2Cards |> List.length = 0 then player1 else
                if p1Cards |> List.length = 0 then player2 else

                let str1 = p1Cards |> List.head |> getstrfromcard
                let str2 = p2Cards |> List.head |> getstrfromcard

                if str1 = str2 then playLoop p1Cards.Tail p2Cards.Tail else
                
                if str1 > str2 then playLoop (p1Cards.Tail@[p1Cards.Head]) p2Cards.Tail else

                playLoop p1Cards.Tail (p2Cards.Tail@[p2Cards.Head])

            let p1DeltCards, p2DeltCards = dealCards()

            playLoop p1DeltCards p2DeltCards

        AssertEquality player1 (playGame())
        AssertEquality player1 (playGame())
        AssertEquality player2 (playGame())