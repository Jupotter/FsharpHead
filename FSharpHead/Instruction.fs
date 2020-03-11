module Instruction

open Type
open Utility

type opcode_form =
    | Long
    | Short
    | Variable
    | Extended

type operand_count =
    | OP0
    | OP1
    | OP2
    | VAR

type operand_type =
  | Large
  | Small
  | Variable
  | Omitted

(* The tables which follow are maps from the opcode identification number
   to the opcode type; the exact order matters. *)

let one_operand_bytecodes =
    [| OP1_128; OP1_129; OP1_130; OP1_131; OP1_132; OP1_133; OP1_134; OP1_135; OP1_136; OP1_137; OP1_138; OP1_139; OP1_140; OP1_141; OP1_142; OP1_143 |]

let zero_operand_bytecodes =
    [| OP0_176; OP0_177; OP0_178; OP0_179; OP0_180; OP0_181; OP0_182; OP0_183; OP0_184; OP0_185; OP0_186; OP0_187; OP0_188; OP0_189; OP0_190; OP0_191 |]

let two_operand_bytecodes =
    [| ILLEGAL
       OP2_1
       OP2_2
       OP2_3
       OP2_4
       OP2_5
       OP2_6
       OP2_7
       OP2_8
       OP2_9
       OP2_10
       OP2_11
       OP2_12
       OP2_13
       OP2_14
       OP2_15
       OP2_16
       OP2_17
       OP2_18
       OP2_19
       OP2_20
       OP2_21
       OP2_22
       OP2_23
       OP2_24
       OP2_25
       OP2_26
       OP2_27
       OP2_28
       ILLEGAL
       ILLEGAL
       ILLEGAL |]

let var_operand_bytecodes =
    [| VAR_224
       VAR_225
       VAR_226
       VAR_227
       VAR_228
       VAR_229
       VAR_230
       VAR_231
       VAR_232
       VAR_233
       VAR_234
       VAR_235
       VAR_236
       VAR_237
       VAR_238
       VAR_239
       VAR_240
       VAR_241
       VAR_242
       VAR_243
       VAR_244
       VAR_245
       VAR_246
       VAR_247
       VAR_248
       VAR_249
       VAR_250
       VAR_251
       VAR_252
       VAR_253
       VAR_254
       VAR_255 |]

let ext_bytecodes =
    [| EXT_0
       EXT_1
       EXT_2
       EXT_3
       EXT_4
       EXT_5
       EXT_6
       EXT_7
       EXT_8
       EXT_9
       EXT_10
       EXT_11
       EXT_12
       EXT_13
       EXT_14
       ILLEGAL
       EXT_16
       EXT_17
       EXT_18
       EXT_19
       EXT_20
       EXT_21
       EXT_22
       EXT_23
       EXT_24
       EXT_25
       EXT_26
       EXT_27
       EXT_28
       EXT_29
       ILLEGAL
       ILLEGAL |]


let decode address story =
    let read_byte = Story.readByte story

    let decode_form address =
        let byte = read_byte address
        match fetchBits bit7 size2 byte with
        | 3 -> opcode_form.Variable
        | 2 ->
            if byte = 190 then Extended else Short
        | _ -> Long

    let decode_op_count address form =
        let b = read_byte address
        match form with
        | Short ->
            if fetchBits bit5 size2 b = 3 then OP0 else OP1
        | Long -> OP2
        | opcode_form.Variable ->
            if fetchBit bit5 b then VAR else OP2
        | Extended -> VAR

    let decode_opcode address form op_count =
        let b = read_byte address
        match (form, op_count) with
        | (Extended, _) ->
            let maximum_extended = 29
            let ext = read_byte (incByteAddr address)
            if ext > maximum_extended then ILLEGAL else ext_bytecodes.[ext]
        | (_, OP0) -> zero_operand_bytecodes.[fetchBits bit3 size4 b]
        | (_, OP1) -> one_operand_bytecodes.[fetchBits bit3 size4 b]
        | (_, OP2) -> two_operand_bytecodes.[fetchBits bit4 size5 b]
        | (_, VAR) -> var_operand_bytecodes.[fetchBits bit4 size5 b]

    let get_opcode_length form =
        match form with
        | Extended -> 2
        | _ -> 1 in
        
    let decode_types n =
        match n with
        | 0 -> Large
        | 1 -> Small
        | 2 -> Variable
        | _ -> Omitted in

    decode_op_count address (decode_form address)
