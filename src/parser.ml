let (clauses : (int * bool) list list ref) = ref []
let nbvars = ref 0
let nbcls = ref 0

let parsed_clause c = nbcls := !nbcls + 1; clauses := List.rev c :: !clauses

let parsed_atom n negated acc = nbvars := max n !nbvars; (n, negated) :: acc

let rec skip_line cin = if (input_char cin) <> '\n' then skip_line cin

let to_digit c = match c with
  | '0' -> 0  | '1' -> 1  | '2' -> 2  | '3' -> 3  | '4' -> 4
  | '5' -> 5  | '6' -> 6  | '7' -> 7  | '8' -> 8  | '9' -> 9
  | _   -> Format.printf "(%c)@." c; assert false

let rec skip_spaces c cin =
  if c <> ' ' then c else skip_spaces (input_char cin) cin

let read_sign c cin =
  if c = '-' then true, skip_spaces (input_char cin) cin
  else if c = '+' then false, skip_spaces (input_char cin) cin else false, c

let rec parse_int n cin =
  try
    let c = input_char cin in
    if c = ' ' || c = '\n' || c = '\t' then n else parse_int (n * 10 + (to_digit c)) cin
  with End_of_file -> n

let read_dims c1 cin =
  let c2 = input_char cin in
  let c3 = input_char cin in
  let c4 = input_char cin in
  match c1,c2,c3,c4 with
    |'c', 'n' ,'f' ,' ' ->
      let c = skip_spaces (input_char cin) cin in
      let nbv = parse_int (to_digit c) cin in
      let c = skip_spaces (input_char cin) cin in
      let nbc = parse_int (to_digit c) cin in
      (* given dimensions are ignored *)
      ignore (nbc, nbv)
    | _ -> assert false

let rec read_line negated c cin acc =
  let n = parse_int (to_digit c) cin in
  if n = 0 then parsed_clause acc
  else
    let c = skip_spaces (input_char cin) cin in
    let acc = parsed_atom n negated acc in
    let negated, c = read_sign c cin in
    read_line negated c cin acc

let main cin =
  try
    while true do
      match input_char cin with
        | 'c'  -> skip_line cin
        | '\n' -> ()
        | 'p'  -> read_dims (skip_spaces (input_char cin) cin) cin;
        |  c   ->
          let c = skip_spaces c cin in
          let negated, c = read_sign c cin in
          ignore(read_line negated c cin []);
    done
  with End_of_file -> ()

let parse file =
  let cin = open_in file in
  main cin;
  close_in cin;
  let l = !clauses in
  let nbv = !nbvars in
  let nbc = !nbcls in
  clauses := [];
  nbvars  := 0;
  nbcls   := 0;
  List.rev l, nbv, nbc
