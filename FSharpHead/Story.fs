module Story

open Utility
open Type

type T =
    { dynamicMemory: ImmutableBytes.T
      staticMemory: byte array }

let make dynamicA staticA =
    { dynamicMemory = ImmutableBytes.make dynamicA
      staticMemory = staticA }

let readByte story address =
    let dynamicSize = ImmutableBytes.size story.dynamicMemory
    if isInRange address dynamicSize then
        ImmutableBytes.readByte story.dynamicMemory address
    else
        let (MemorySize offset) = dynamicSize
        let staticAddress = decByteAddrBy address offset
        dereferenceString staticAddress story.staticMemory

let readWord story address =
    let high = readByte story (addressOfHighByte address)
    let low = readByte story (addressOfLowByte address)
    256 * high + low

let writeByte story address value =
    let dynamicMemory = ImmutableBytes.writeByte story.dynamicMemory address value
    { story with dynamicMemory = dynamicMemory }

let writeWord story address value =
    let high = (value >>> 8) &&& 0xFF
    let low = value &&& 0xFF
    let story = writeByte story (addressOfHighByte address) high
    writeByte story (addressOfLowByte address) low



let getFile filename = System.IO.File.ReadAllBytes(filename)

let header_size = 64
let static_memory_base_offset = WordAddress 14

let load filename =
  let file = getFile filename in
  let len = Array.length file in
  if len < header_size then
    failwith (Printf.sprintf "%s is not a valid story file" filename)
  else
    let high = dereferenceString (addressOfHighByte static_memory_base_offset) file in
    let low = dereferenceString (addressOfLowByte static_memory_base_offset) file in
    let dynamicLength = high * 256 + low in
    if dynamicLength > len then
      failwith (Printf.sprintf "%s is not a valid story file" filename)
    else 
      let dynamic = Array.sub file 0 dynamicLength in
      let staticA = Array.sub file dynamicLength (len - dynamicLength) in
      make dynamic staticA
