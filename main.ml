type 'a result =
  | Success of 'a
  | Failure of string
  
let explode s =
  let rec expl i l =
    if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let implode l = String.concat "" (List.map (String.make 1) l);;

(*
  let implode l =
    let result = Bytes.create (List.length l) in 
    let rec imp i = function
      | [] -> result
      | c :: l -> Bytes.set result i c; imp (i + 1) l in
    imp 0 l;;


  let pchar charToMatch str =
    match explode str with
    | [] -> Failure "Empty"
    | h::t -> if h = charToMatch
        then Success(charToMatch, implode t)
        else Failure "Not Found";;
  *)

let pchar charToMatch str =
  match explode str with
  | [] -> Failure "Empty"
  | h::t -> match compare h charToMatch with
    | 0 -> Success(charToMatch, implode t)
    | _ -> Failure "Not Found";;
             
             
let inputZBC = "ABC";; 
pchar 'A' inputZBC;;

let parseB = pchar 'B';;

parseB "BC";;
