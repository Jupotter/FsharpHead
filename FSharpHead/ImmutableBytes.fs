module ImmutableBytes

open Type
open Utility

type T =
    { originalBytes: byte array
      edits: byte IntMap }

let make bytes =
    { originalBytes = bytes
      edits = Map.empty }

let size bytes = MemorySize bytes.originalBytes.Length

let readByte bytes address =
    if isOutOfRange address (size bytes) then
        failwith "address is out of range"
    else
        let c =
            if Map.containsKey address bytes.edits
            then Map.find address bytes.edits
            else let (ByteAddress addr) = address in bytes.originalBytes.[addr]
        int c

let writeByte bytes address value =
    if isOutOfRange address (size bytes) then
        failwith "address is out of range"
    else
        let edits = Map.add address (byte value) bytes.edits
        { bytes with edits = edits }
