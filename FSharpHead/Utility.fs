module Utility

open Type

let bit0 = BitNumber 0
let bit1 = BitNumber 1
let bit2 = BitNumber 2
let bit3 = BitNumber 3
let bit4 = BitNumber 4
let bit5 = BitNumber 5
let bit6 = BitNumber 6
let bit7 = BitNumber 7
let bit8 = BitNumber 8
let bit9 = BitNumber 9
let bit10 = BitNumber 10
let bit11 = BitNumber 11
let bit12 = BitNumber 12
let bit13 = BitNumber 13
let bit14 = BitNumber 14
let bit15 = BitNumber 15

let size1 = BitSize 1
let size2 = BitSize 2
let size3 = BitSize 3
let size4 = BitSize 4
let size5 = BitSize 5
let size6 = BitSize 6
let size7 = BitSize 7


let fetchBit (BitNumber n) word = (word &&& (1 <<< n)) >>> n = 1

let clearBit (BitNumber n) word = word &&& (~~~(1 <<< n))

let setBit (BitNumber n) word = word ||| (1 <<< n)

let setBitTo n word value =
    if value then setBit n word else clearBit n word

let fetchBits (BitNumber high) (BitSize length) word =
    let mask = ~~~(-1 <<< length)
    (word >>> (high - length + 1)) &&& mask

let isInRange (ByteAddress address) (MemorySize size) = 0 <= address && address < size

let isOutOfRange address size = not (isInRange address size)

let incByteAddrBy (ByteAddress address) offset = ByteAddress(address + offset)

let decByteAddrBy address offset = incByteAddrBy address (0 - offset)

let dereferenceString address (bytes: byte array) =
    if isOutOfRange address (MemorySize bytes.Length) then
        failwith "address out of range"
    else
        let (ByteAddress addr) = address
        int bytes.[addr]

let addressOfHighByte (WordAddress address) = ByteAddress address

let addressOfLowByte (WordAddress address) = ByteAddress(address + 1)

let decodeWordAddress (WordZstring wordAddress) = Zstring (wordAddress * 2)

let wordSize = 2
let incWordAddrBy (WordAddress address) offset =
  WordAddress (address + offset * wordSize)
  
let incWordAddr address =
  incWordAddrBy address 1

let firstAbbrevAddr (AbbreviationTableBase addr) =
  WordAddress addr