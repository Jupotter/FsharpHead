open Type;


let () = 
  let story = Story.load "minizork.z3" in
  let zstring = Zstring 0xb106 in
  let text = ZString.read story zstring in
  Printf.printf "%s\n" text;
  let dict = Dictionary.display story in
  Printf.printf "%s\n" dict;
  let tree = Object.display_object_tree story in
  Printf.printf "%s\n" tree