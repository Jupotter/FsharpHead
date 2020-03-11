module Object

open Type
open Utility

let default_property_table_size story =
    if Story.v3_or_lower (Story.version story) then 31 else 63

let default_property_table_entry_size = 2

let entry_size story =
    if Story.v3_or_lower (Story.version story) then 9 else 14

let tree_base story =
    let (Object_base baseAddr) = Story.object_table_base story
    let table_size = default_property_table_size story
    Object_tree_base(baseAddr + default_property_table_entry_size * table_size)

let address story (Object obj) =
    let (Object_tree_base tree_base) = tree_base story
    let entry_size = entry_size story
    Object_address(tree_base + (obj - 1) * entry_size)

let parent story obj =
    let (Object_address addr) = address story obj
    if Story.v3_or_lower (Story.version story)
    then Object(Story.readByte story (ByteAddress(addr + 4)))
    else Object(Story.readWord story (WordAddress(addr + 6)))

let sibling story obj =
    let (Object_address addr) = address story obj
    if Story.v3_or_lower (Story.version story)
    then Object(Story.readByte story (ByteAddress(addr + 5)))
    else Object(Story.readWord story (WordAddress(addr + 8)))

let child story obj =
    let (Object_address addr) = address story obj
    if Story.v3_or_lower (Story.version story)
    then Object(Story.readByte story (ByteAddress(addr + 6)))
    else Object(Story.readWord story (WordAddress(addr + 10)))

let property_header_address story obj =
    let object_property_offset =
        if Story.v3_or_lower (Story.version story) then 7 else 12

    let (Object_address addr) = address story obj
    Property_header(Story.readWord story (WordAddress(addr + object_property_offset)))

let name story n =
    let (Property_header addr) = property_header_address story n
    let length = Story.readByte story (ByteAddress addr)
    match length with
    | 0 -> None
    | _ -> Some(ZString.read story (Zstring(addr + 1)))

let count story =
    let (Object_tree_base table_start) = tree_base story
    let (Property_header table_end) = property_header_address story (Object 1)
    let size = entry_size story
    (table_end - table_start) / size

let display_object_table story =
    let count = count story

    let to_string i =
        let current = Object i
        let (Object parent) = parent story current
        let (Object sibling) = sibling story current
        let (Object child) = child story current

        let name =
            match name story current with
            | None -> "<no name>"
            | Some n -> n
        Printf.sprintf "%02x: %02x %02x %02x %s\n" i parent sibling child name
    accumulate_strings_loop to_string 1 (count + 1)

let invalid_object = Object 0

let roots story =
    let rec aux obj acc =
        let current = Object obj
        if current = invalid_object then acc
        else if (parent story current) = invalid_object then aux (obj - 1) (current :: acc)
        else aux (obj - 1) acc
    aux (count story) []

let display_object_tree story =
    let rec aux acc indent obj =
        if obj = invalid_object then
            acc
        else
            let name =
                match name story obj with
                | None -> "<no name>"
                | Some n -> n

            let child = child story obj
            let sibling = sibling story obj

            let object_text =
                Printf.sprintf "%s%s\n" indent name

            let with_object = acc + object_text
            let newIndent = "    " + indent
            let with_children = aux with_object newIndent child
            aux with_children indent sibling

    let to_string obj = aux "" "" obj
    accumulate_strings to_string (roots story)
