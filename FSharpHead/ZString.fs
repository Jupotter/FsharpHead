module ZString

open Story
open Type
open Utility

let abbreviationTableLength = 96

let alphabetTable =
    [| [| " "; "?"; "?"; "?"; "?"; "?"; "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z" |]
       [| " "; "?"; "?"; "?"; "?"; "?"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K"; "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W"; "X"; "Y"; "Z" |]
       [| " "; "?"; "?"; "?"; "?"; "?"; "?"; "\n"; "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "."; ","; "!"; "?"; "_"; "#"; "'"; "\""; "/"; "\\"; "-"; ":"; "("; ")" |] |]

type StringState =
    | Alphabet of int
    | Abbrev of AbbreviationNumber
    | Leading
    | Trailing of int

let abbrev0 = Abbrev(Abbreviation 0)
let abbrev32 = Abbrev(Abbreviation 32)
let abbrev64 = Abbrev(Abbreviation 64)
let alphabet0 = Alphabet 0
let alphabet1 = Alphabet 1
let alphabet2 = Alphabet 2

let abbreviationsTableBase story =
    let abbreviationsTableBaseOffset = WordAddress 24
    AbbreviationTableBase(readWord story abbreviationsTableBaseOffset)

let abbreviationZstring story (Abbreviation n) =
    if n < 0 || n >= abbreviationTableLength then
        failwith "bad offset into abbreviation table"
    else
        let baseAddr = firstAbbrevAddr (abbreviationsTableBase story)
        let abbrAddr = incWordAddrBy baseAddr n
        let wordAddr = WordZstring(Story.readWord story abbrAddr)
        decodeWordAddress wordAddr

let rec read story (Zstring address) =
    let rec process_zchar (Zchar zchar) state =
        match (zchar, state) with
        | (1, Alphabet _) -> ("", abbrev0)
        | (2, Alphabet _) -> ("", abbrev32)
        | (3, Alphabet _) -> ("", abbrev64)
        | (4, Alphabet _) -> ("", alphabet1)
        | (5, Alphabet _) -> ("", alphabet2)
        | (6, Alphabet 2) -> ("", Leading)
        | (_, Alphabet a) -> (alphabetTable.[a].[zchar], alphabet0)
        | (_, Abbrev (Abbreviation a)) ->
          let abbrv = Abbreviation (a + zchar) in
          let addr = abbreviationZstring story abbrv in
          let str = read story addr in
          (str, alphabet0)
        | (_, Leading) -> ("", (Trailing zchar))
        | (_, Trailing high) ->
          let s = string (char (high * 32 + zchar)) in
          (s, alphabet0)

    let rec aux acc state1 current_address =
        let zchar_bit_size = size5 in
        let word = Story.readWord story current_address in
        let is_end = fetchBit bit15 word in
        let zchar1 = Zchar (fetchBits bit14 zchar_bit_size word) in
        let zchar2 = Zchar (fetchBits bit9 zchar_bit_size word) in
        let zchar3 = Zchar (fetchBits bit4 zchar_bit_size word) in
        let (text1, state2) = process_zchar zchar1 state1 in
        let (text2, state3) = process_zchar zchar2 state2 in
        let (text3, state_next) = process_zchar zchar3 state3 in
        let new_acc = acc + text1 + text2 + text3 in
        if is_end then new_acc
        else aux new_acc state_next (incWordAddr current_address)
    in aux "" alphabet0 (WordAddress address)      
