(* let %test_unit _ = 
  let dir = Filename.dirname "/home/fxdx/Desktop/minilucy/test/examples/ " in 
  let file = Filename.concat dir "ex001.lus" in 
  let _ = Minilucy.Load.load file in () *)

let test_parse () = 
  let dir = Filename.dirname "/home/fxdx/Desktop/minilucy/test/examples/ " in 
  let file = Filename.concat dir "ex001.lus" in 
  let f = Minilucy.Load.load file in 
  Format.printf "\n";
  Minilucy.Pp_untyped.pp_file Format.std_formatter f


let () = 
  test_parse ();