module Dictionary

open Type
open Utility

let word_separators_base (Dictionary_base baseAddr) =
  ByteAddress baseAddr

let word_separators_count story =
  let dict_base = Story.dictionary_base story in
  let ws_base = word_separators_base dict_base in
  Story.readByte story ws_base

let entry_base story =
  let dict_base = Story.dictionary_base story in
  let ws_count = word_separators_count story in
  let ws_base = word_separators_base dict_base in
  incByteAddrBy ws_base (ws_count + 1)

let entry_length story =
  Story.readByte story (entry_base story)

let entry_count story =
  let (ByteAddress addr) = incByteAddr (entry_base story) in
  Story.readWord story (WordAddress addr)

(* This is the address of the actual dictionary entries. *)
let table_base story =
  let (ByteAddress addr) = incByteAddrBy (entry_base story) 3 in
  Dictionary_table_base addr

let entry_address story (Dictionary dictionary_number) =
  let (Dictionary_table_base baseAddr) = table_base story in
  let length = entry_length story in
  Dictionary_address (baseAddr + dictionary_number * length)

let entry story dictionary_number =
  let (Dictionary_address addr) = entry_address story dictionary_number in
  ZString.read story (Zstring addr)

let display story =
  let count = entry_count story in
  let to_string i =
    Printf.sprintf "%s " (entry story (Dictionary i)) in
  accumulate_strings_loop to_string 0 count 