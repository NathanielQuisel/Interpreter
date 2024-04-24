(* Honor code comes here:

   First Name: Nathaniel
   Last Name: Quisel
   BU ID: U83615241

   I pledge that this program represents my own program code and that I have
   coded on my own. I received help from no one in designing and debugging my
   program. I have read the course syllabus of CS 320 and have read the sections
   on Collaboration and Academic Misconduct. I also understand that I may be
   asked to meet the instructor or the TF for a follow up interview on Zoom. I
   may be asked to explain my solution in person and may also ask you to solve a
   related problem. *)

(*NOTE: There are no restrictions on what you can use*)


(*Writing a line to a file*)
let write_file_example (file_path: string) : unit =
  let fp = open_out file_path in
  let () = Printf.fprintf fp "writing this line!" in
    close_out fp

(*When it comes to parsing src, you should find it useful that fold_left can be
  defined and used with strings (like String.fold_left in new OCaml version).
  See String.get. These are suggestions though and you are welcome
  to use what you want :)  *)

type digit =
  |1
  |2
  |3
  |4
  |5
  |6
  |7
  |8
  |9
  |0


let interpreter (src : string) (output_file_path: string): unit =
  failwith "Unimplemented"