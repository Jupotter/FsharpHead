module Type

type BitNumber = BitNumber of int
type BitSize = BitSize of int
type ByteAddress = ByteAddress of int
type MemorySize = MemorySize of int

type IntMap<'a> = Map<ByteAddress,'a>

type WordAddress = WordAddress of int