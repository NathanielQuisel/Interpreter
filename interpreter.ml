
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



type com = 
  | Quit
  | Push
  | Pop
  | Add
  | Sub
  | Mul
  | Div
  | Swap
  | Neg
  | Concat
  | And
  | Or
  | Not
  | Equal
  | Lte
  | Local
  | Global
  | Begin
  | IfThen

type const =
  | String of string
  | Int of int
  | Left of const
  | Right of const
  | Tuple of const list
  | SimpleClosure of (string*string) 
  (*function name, arg name*)

type env_elem = 
 | Var of (string*const)
 | Closure of ((string*string*string list) list*env_elem list)
 (*list of mutual functions (name,param_name,code), corresponding environment*)
type env = env_elem list
type state = (const list*env*env) (*stack,local,global*)

let interpreter (src: string) (output_file_path: string): unit =

  let rec write_to_file (stack: const list) (file: out_channel) : unit =
    
    let rec create_string (c: const): string =
      match c with
      | Left(s) ->
        "Left " ^ create_string(s)
      | Right(s) ->
        "Right " ^ create_string(s)
      | Int(i) ->
        string_of_int(i)
      | String(s) ->
        s
      | Tuple(clist) ->
        "(" ^ handle_tuple(clist)
      | SimpleClosure(fun_name,arg) ->
        "Clo (" ^ fun_name ^ ", " ^ arg ^ ")"
    and handle_tuple (tuple: const list): string =
      match tuple with
      | [] -> ")"
      | hd::[] -> create_string hd ^ ")"
      | hd::tl -> (create_string hd) ^ ", " ^ (handle_tuple tl)
    in
    match stack with
    | [] -> close_out file
    | hd::tl -> 
      let () = Printf.fprintf file "%s\n" (create_string(hd)) in
      write_to_file tl file
  in

  let pop_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | hd::tl -> tl
  in
  
  let intop_helper (stack: const list) (f: int->int->int): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | _::[] -> [String("\"Error\"")]
    | first::second::tl ->
      (match (first,second) with
      | (Int(a), Int(b)) -> Int(f a b)::tl
      | (_,_) -> [String("\"Error\"")]
      )
  in

  let division_helper (stack: const list) : const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | _::[] -> [String("\"Error\"")]
    | first::second::tl ->
      (match (first,second) with
      | (Int(a), Int(b)) -> 
        if b = 0 then
          [String("\"Error\"")]
        else 
          Int(a/b)::tl
      | (_,_) -> [String("\"Error\"")]
      )
  in

  let swap_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | _::[] -> [String("\"Error\"")]
    | first::second::tl -> second::first::tl
  in

  let neg_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | hd::tl -> 
      (match hd with
      | Int(a) -> Int(-a)::tl
      | _ -> [String("\"Error\"")]
      )
  in

  let remove_char (s: string) (start: int): string =
    String.sub s start (String.length(s)-1)
  in

  let is_proper_string (s: string) : bool =
    let string_helper (c: char): bool =
      ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    in
    let new_s = remove_char (remove_char s 1) 0 in
    String.for_all string_helper new_s
  in

  let concat_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | _::[] -> [String("\"Error\"")]
    | first::second::tl -> 
      (match (first,second) with
      | (String(a),String(b)) -> String(String.cat (remove_char a 0) (remove_char b 1))::tl
      | (_,_) -> [String("\"Error\"")]
      )
  in

  let rec get_value_of_var (e: env) (key: string): const option =
    let rec handle_closure (fun_list: (string*string*string list) list) (key: string): const option =
      match fun_list with
      | [] -> None
      | (name,arg,code)::tl ->
        if name = key then
          Some(SimpleClosure(name,arg))
        else
          handle_closure tl key
    in
    match e with
    | [] -> None
    | hd::tl -> 
      (match hd with
      | Closure(cl,_) -> 
        (match handle_closure cl key with
        | Some(c) -> Some(c)
        | None -> get_value_of_var tl key)
      | Var(s,value) ->
        if s = key then
          Some(value)
        else
          get_value_of_var tl key
      )
  in

  let push_helper (value: string) (stack: const list) (local: env) (global: env): const list =
    if (value.[0] = '\"' && (is_proper_string value)) then
      String(value)::stack
    else
      match Stdlib.int_of_string_opt value with
      | Some(i) -> Int(i)::stack
      | None -> 
        (match get_value_of_var local value with
        | Some(c) -> c::stack
        | None -> 
          (match get_value_of_var global value with
          | Some(c) -> c::stack
          | None -> [String("\"Error\"")]))
  in

  let is_boolean (b: int) : bool =
    (b = 1 || b = 0)
  in

  let and_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | _::[] -> [String("\"Error\"")]
    | first::second::tl ->
      (match (first,second) with
      | (Int(a), Int(b)) ->
        if is_boolean(a) && is_boolean(b) then
          Int(a*b)::tl
        else
          [String("\"Error\"")]
      | (_,_) -> [String("\"Error\"")]
      )
  in

  let or_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | _::[] -> [String("\"Error\"")]
    | first::second::tl ->
      (match (first,second) with
      | (Int(a), Int(b)) ->
        if is_boolean(a) && is_boolean(b) then
          if a+b >= 1 then
            Int(1)::tl
          else
            Int(0)::tl
        else
          [String("\"Error\"")]
      | (_,_) -> [String("\"Error\"")]
      )
  in
  
  let not_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | first::tl -> 
      (match first with
      | Int(a) -> 
        if is_boolean a then
          if a = 1 then
            Int(0)::tl
          else
            Int(1)::tl
        else
          [String("\"Error\"")]
      | _ -> [String("\"Error\"")])
  in

  let equal_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | _::[] -> [String("\"Error\"")]
    | first::second::tl ->
      (match (first,second) with
      | (Int(a), Int(b)) ->
        if a = b then
          Int(1)::tl
        else
          Int(0)::tl
      | (_,_) -> [String("\"Error\"")]
      )
  in

  let lte_helper (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | _::[] -> [String("\"Error\"")]
    | first::second::tl ->
      (match (first,second) with
      | (Int(a), Int(b)) ->
        if a <= b then
          Int(1)::tl
        else
          Int(0)::tl
      | (_,_) -> [String("\"Error\"")]
      )
  in

  let is_name (name: string): bool =
    let name_helper (c: char): bool =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c = '_') || (c >= '0' && c <= '9')
    in
    if name.[0] >= 'a' && name.[0] <= 'z' then
      String.for_all name_helper name
    else
      false
  in

  let rec create_env (name: string) (value: const) (environ: env) (been_added: bool): env =
    match environ with
    | [] -> 
      if been_added then
        []
      else
        Var(name,value)::[]
    | hd::tl ->
      (match hd with
      | Closure(c) -> Closure(c)::(create_env name value tl false)
      | Var(n,v) -> 
        if n = name then
          Var(name,value)::(create_env name value tl true)
        else
          Var(n,v)::(create_env name value tl false))
  in

  let local_helper (name: string) (stack: const list) (local: env) (global: env): state =
    if is_name name then
      match stack with
      | [] -> ([String("\"Error\"")], local, global)
      | first::tl -> (tl,(create_env name first local false),global)
    else
      ([String("\"Error\"")], local, global)
  in

  let global_helper (name: string) (stack: const list) (local: env) (global: env): state =
    if is_name name then
      match stack with
      | [] -> ([String("\"Error\"")], local, global)
      | first::tl -> (tl,local,(create_env name first global false))
    else
      ([String("\"Error\"")], local, global)
  in

  let ifthen_helper (stack: const list): (bool*const list) = (*figure out how to skip to the right part*)
    match stack with
    | [] -> (false,[String("\"Error\"")])
    | first::tl -> 
      (match first with
      | Int(a) -> 
        if is_boolean a then
          if a = 1 then
            (true,tl)
          else
            (false,tl)
        else
          (false,[String("\"Error\"")])
      | _ -> (false,[String("\"Error\"")]))
  in

  let rec skip_until_end (file: string list) (extraEnd: int): string list =
    match file with
    | [] -> ["ERROR"] (* this is a janky way to get it to give error always*)
    | hd::tl -> 
      (match hd with
      | "End" -> 
        if extraEnd = 0 then
          tl
        else
          skip_until_end tl (extraEnd-1)
      | "Begin" -> skip_until_end tl (extraEnd+1)
      | "CaseLeft" -> skip_until_end tl (extraEnd+1)
      | "IfThen" -> skip_until_end tl (extraEnd+1)
      | _ -> let command = 
        (match String.split_on_char ' ' hd with
        | [] -> "Error"
        | hd::tl -> hd
        ) in
        (match command with
        | "Fun" -> skip_until_end tl (extraEnd+1)
        | _ -> skip_until_end tl extraEnd))
  in

  let rec skip_until_else (file: string list) (extraElse: int): string list =
    match file with
    | [] -> ["ERROR"] (* this is a janky way to get it to give error always*)
    | hd::tl -> 
      (match hd with
      | "Else" -> 
        if extraElse = 0 then
          tl
        else
          skip_until_else tl (extraElse-1)
      | "IfThen" -> skip_until_else tl (extraElse+1)
      | _ -> skip_until_else tl extraElse)
  in

  let rec skip_until_right (file: string list) (extraRight: int): string list =
    match file with
    | [] -> ["ERROR"] (* this is a janky way to get it to give error always*)
    | hd::tl -> 
      (match hd with
      | "Right" -> 
        if extraRight = 0 then
          tl
        else
          skip_until_right tl (extraRight-1)
      | "CaseLeft" -> skip_until_right tl (extraRight+1)
      | _ -> skip_until_right tl extraRight)
  in

  let inj_helper (isLeft: bool) (stack: const list): const list =
    match stack with
    | [] -> [String("\"Error\"")]
    | hd::tl ->
      if isLeft then
        Left(hd)::tl
      else
        Right(hd)::tl
  in

  let caseLeft_helper (stack: const list): (bool*const list) = 
    match stack with
    | [] -> (false,[String("\"Error\"")])
    | first::tl -> 
      (match first with
      | Left(c) ->
        (true,c::tl)
      | Right(c) ->
        (false,c::tl)
      | _ -> (false,[String("\"Error\"")]))
  in

  let tuple_helper (value: string) (stack: const list): const list =
    let rec remove_elems (number: int) (stack: const list): const list =
      match stack with
      | [] -> [String("\"Error\"")]
      | hd::tl -> 
        if number = 1 then
          tl
        else
          remove_elems (number-1) tl
    in
    let rec create_tuple (num_consts: int) (stack: const list): const =
      match stack with
      | [] -> String("\"Error\"")
      | hd::tl -> 
        if num_consts-1 = 0 then
          Tuple([hd])
        else
          (match create_tuple (num_consts-1) tl with
          | Tuple(c) -> Tuple(hd::c)
          | _ -> String("\"Error\""))
    in
    match int_of_string_opt value with
    | None -> [String("\"Error\"")]
    | Some(i) ->
      if i = 0 then
        Tuple([])::stack
      else if i > 0 then
        (match create_tuple i stack with
        | Tuple(c) -> Tuple(List.rev c)::(remove_elems i stack)
        | _ -> [String("\"Error\"")])
      else
        [String("\"Error\"")]
  in

  let get_helper (value: string) (stack: const list): const list =
    match int_of_string_opt value with
    | None -> [String("\"Error\"")]
    | Some(i) -> 
      (match stack with
      | [] -> [String("\"Error\"")]
      | hd::tl ->
        (match hd with
        | Tuple(cl) ->
          (match List.nth_opt cl i with
          | None -> [String("\"Error\"")]
          | Some(c) -> c::stack)
        | _ -> [String("\"Error\"")]))
  in

  let rec find_fun_code (file: string list) (extraEnds: int): string list =
    match file with
    | [] -> ["\"Error\""]
    | hd::tl -> 
      (match hd with
      | "Begin" -> hd::(find_fun_code tl (extraEnds+1))
      | "CaseLeft" -> hd::(find_fun_code tl (extraEnds+1))
      | "IfThen" -> hd::(find_fun_code tl (extraEnds+1))
      | "End" -> 
        if extraEnds = 0 then
          []
        else
          hd::(find_fun_code tl (extraEnds-1))
      | _ -> 
        let command = 
          (match String.split_on_char ' ' hd with
          | [] -> "Error"
          | hd::tl -> hd
          ) in
        (match command with
        | "Fun" -> hd::(find_fun_code tl (extraEnds+1))
        | "Mut" -> 
          if extraEnds = 0 then
            []
          else
            hd::(find_fun_code tl extraEnds)
        | _ -> hd::find_fun_code tl extraEnds))
  in

  let rec next_mut (file: string list) (extraEnds: int): string list =
    match file with
    | [] -> ["\"Error\""]
    | hd::tl -> 
      (match hd with
      | "Begin" -> next_mut tl (extraEnds+1)
      | "CaseLeft" -> next_mut tl (extraEnds+1)
      | "IfThen" -> next_mut tl (extraEnds+1)
      | "End" -> 
        if extraEnds = 0 then
          tl
        else
          next_mut tl (extraEnds-1)
      | _ -> 
        let command = 
          (match String.split_on_char ' ' hd with
          | [] -> "Error"
          | hd::tl -> hd
          ) in
        (match command with
        | "Fun" -> next_mut tl (extraEnds+1)
        | "Mut" -> 
          if extraEnds = 0 then
            file
          else
            next_mut tl extraEnds
        | _ -> next_mut tl extraEnds))
  in

  let rec muts_helper (file: string list): (string*string*string list) list =
    match file with
    | [] -> []
    | hd::tl -> 
      (match String.split_on_char ' ' hd with
      | command::name::param::[] -> 
        if command = "Mut" then
          (name,param,(find_fun_code tl 0))::(muts_helper (next_mut tl 0))
        else
          []
      | _::_::_::_::_ -> []
      | _::_::[] -> []
      | _::[] -> []
      | [] -> []
      )
  in

  let fun_helper (fun_name: string) (arg_name: string) (file: string list) (local: env): env =
    let muts_file = 
      match file with
      | [] -> []
      | hd::tl -> next_mut tl 0
    in
    Closure((fun_name,arg_name,(find_fun_code file 0))::(muts_helper muts_file),local)::local
  in

  let rec get_fun_info (local: env) (key: string): env_elem option =
    let rec in_closure (funs: (string*string*string list) list) (key: string): bool =
      match funs with
      | [] -> false
      | (name,_,_)::tl ->
        if name = key then
          true
        else
          in_closure tl key
    in
    match local with
    | [] -> None
    | Var(_)::tl -> get_fun_info tl key
    | Closure(funs,stored_local)::tl ->
      if in_closure funs key then
        Some(Closure(funs,stored_local))
      else
        get_fun_info tl key
  in

  let call_helper (stack: const list) (local: env): (const list*env*string list) option =
    let rec basic_fun_info (funs: (string*string*string list) list) (key: string): string list option =
      match funs with
      | [] -> None
      | (name,arg_name,code)::tl ->
        if name = key then
          Some(code)
        else
          basic_fun_info tl key
    in
    (*edited stack, new local env, code to execute*)
    match stack with
    | [] -> None
    | _::[] -> None
    | SimpleClosure(fun_name,arg_name)::arg_val::tl ->
      (match get_fun_info local fun_name with
      | None -> None
      | Some(Var(_)) -> None
      | Some(Closure(funs,stored_env)) -> 
        (match basic_fun_info funs fun_name with
        | None -> None
        | Some(code) -> Some(tl,(Var(arg_name,arg_val)::(Closure(funs,stored_env)::stored_env)),code)))
    | _::_::_ -> None
  in

  let rec skip_until_blocker (file: string list): string list =
    match file with
    | [] -> []
    | hd::tl -> 
      if hd = "Blocker" then
        tl
      else
        skip_until_blocker tl
  in
  

  let rec run (file: string list) (stl: (state*(bool*int)*bool) list): unit =
    match stl with
    | [] -> ()
    | (st,(inBegin,extraEnds),inFun)::infos ->
    (match st with
    | (stack, local, global) ->
      if stack = [String("\"Error\"")] then
        write_to_file stack (open_out output_file_path)
      else
        (match file with
        | [] -> ()
        | hd::tl -> 
          if hd = "Quit" then
            write_to_file stack (open_out output_file_path) (*write all of the stack onto the output file*)
          else
            let parts = String.split_on_char ' ' hd in
            (match parts with
              | [] -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos)
              | command::[] -> 
                (match command with
                | "Pop" -> run tl ((((pop_helper stack),local,global),(inBegin,extraEnds),inFun)::infos)
                | "Add" -> run tl ((((intop_helper stack (+)),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Sub" -> run tl ((((intop_helper stack (-)),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Mul" -> run tl ((((intop_helper stack ( * )),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Div" -> run tl ((((division_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Swap" -> run tl ((((swap_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Neg" -> run tl ((((neg_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Concat" -> run tl ((((concat_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "And" -> run tl ((((and_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Or" -> run tl ((((or_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Not" -> run tl ((((not_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Equal" -> run tl ((((equal_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Lte" -> run tl ((((lte_helper stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Else" -> run (skip_until_end tl 0) (((stack, local, global),(inBegin,extraEnds),inFun)::infos) 
                | "End" ->
                  if inBegin then
                    if extraEnds > 0 then
                      run tl (((stack,local,global),(inBegin,extraEnds-1),inFun)::infos)
                    else
                      (match infos with
                      | [] -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos) 
                      | ((old_stack,old_local,_),(wasInBegin,old_extraEnds),wasInFun)::old_states ->
                        (match stack with
                        | [] -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos) 
                        | hd::consts -> run tl (((hd::old_stack,old_local,global),(wasInBegin,old_extraEnds),wasInFun)::infos)))
                  else 
                    run tl (((stack,local,global),(inBegin,extraEnds),inFun)::infos) 
                | "IfThen" -> 
                  (match ifthen_helper stack with
                  | (true,s) -> run tl (((s,local,global),(inBegin,extraEnds),inFun)::infos) 
                  | (false,s) -> 
                    if inBegin then
                      run (skip_until_else tl 0) (((s,local,global),(inBegin,extraEnds+1),inFun)::infos)
                    else
                      run (skip_until_else tl 0) (((s,local,global),(inBegin,extraEnds),inFun)::infos))  
                | "Begin" -> run tl ((([],local,global),(true,0),false)::stl)
                  (* run (skip_until_end tl 0) (((begin_helper tl stack local global),(inBegin,extraEnds),inFun)::infos)  *)
                | "InjL" -> run tl ((((inj_helper true stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "InjR" -> run tl ((((inj_helper false stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "CaseLeft" -> 
                  (match caseLeft_helper stack with
                    | (true,s) -> run tl (((s,local,global),(inBegin,extraEnds),inFun)::infos) 
                    | (false,s) -> 
                      if inBegin then
                        run (skip_until_right tl 0) (((s,local,global),(inBegin,extraEnds+1),inFun)::infos)
                      else
                        run (skip_until_right tl 0) (((s,local,global),(inBegin,extraEnds),inFun)::infos))
                | "Right" -> run (skip_until_end tl 0) (((stack, local, global),(inBegin,extraEnds),inFun)::infos) 
                | "Call" -> 
                  (match call_helper stack local with
                  | None -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos)
                  | Some(new_stack,new_local,code) -> run (code @ ("Blocker"::tl)) ((([],new_local,global),(false,0),true)::(((new_stack,local,global),(inBegin,extraEnds),inFun)::infos))
                  ) (*add something to make sure functions have return statements*)
                | "Return" -> 
                  if inFun then
                    (match infos with
                      | [] -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos) 
                      | ((old_stack,old_local,_),(wasInBegin,old_extraEnds),wasInFun)::old_infos ->
                        (match stack with
                        | [] -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos) 
                        | hd::consts -> run (skip_until_blocker tl) (((hd::old_stack,old_local,global),(wasInBegin,old_extraEnds),wasInFun)::old_infos)))
                  else
                    run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos)
                | _ -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos) )
              | command::arg::[] ->
                (match command with
                | "Tuple" -> run tl ((((tuple_helper arg stack),local,global),(inBegin,extraEnds),inFun)::infos)
                | "Get" -> run tl ((((get_helper arg stack),local,global),(inBegin,extraEnds),inFun)::infos) 
                | "Local" -> run tl (((local_helper arg stack local global),(inBegin,extraEnds),inFun)::infos) (*add function to check if proper name*)
                | "Global" -> run tl (((global_helper arg stack local global),(inBegin,extraEnds),inFun)::infos) 
                | "Push" -> run tl ((((push_helper arg stack local global),local,global),(inBegin,extraEnds),inFun)::infos) 
                | _ -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos))
              | command::name::param::[] -> 
                (match command with
                | "Fun" -> run (skip_until_end tl 0) (((stack,(fun_helper name param tl local),global),(inBegin,extraEnds),inFun)::infos) 
                | _ -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos) )
              | _::_::_::_ -> run tl ((([String("\"Error\"")],local,global),(inBegin,extraEnds),inFun)::infos) )))
            
  in
  run (String.split_on_char '\n' src) ((([],[],[]),(false,0),false)::[])

