open Type;
open Utility;

let () = 
  let versionAddress = ByteAddress 0 in
  let story = Story.load "minizork.z3" in
  let version = Story.readByte story versionAddress in
  Printf.printf "%d\n" version