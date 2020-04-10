type 'a result =
  | Success of 'a
  | Failure of string
  
type 't parser = Parser of (string -> ('t * string) result)

let explode s =
  let rec expl i l =
    if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let implode l = String.concat "" (List.map (String.make 1) l);;

let pchar charToMatch =
  let ret input= 
    match explode input with
    | [] -> Failure "Empty"
    | h::t -> match compare h charToMatch with
      | 0 -> Success(charToMatch, implode t)
      | _ -> Failure "Not Found" in
  Parser ret;;
  
let res v =
  Parser (fun x -> Success(v, x))

let item =
  let ret inp = match explode inp with
    | [] -> Failure "Empty"
    | h::t -> Success(h, implode t) in
  Parser ret;;
                
let zero =
  Parser (fun x -> Failure "Zero Parser")

let run parser input = 
  let (Parser innerFn) = parser in
  innerFn input;; 

let parse (Parser p) inp = p inp;;

let (>>=) p f =
  Parser (fun inp ->
      match parse p inp with
      | Success(result', input') -> parse (f result') input'
      | Failure error -> Failure error) 

let (<|>) p f =
  Parser (fun inp ->
      match parse p inp with
      | Success(_) as res -> res
      | Failure _ -> parse f inp)
    
let (let*) = (>>=);; 
  
(* p <*> f applies the parsers p and f in sequence
and returns the results in a tuple. *)
let (<*>) p f =
  (*p1 >>= fun a ->
    p2 >>= fun b -> preturn (a, b)*)
  let* x = p in
  let* y = f in
  res (x,y) 
    
(* p *> f applies the parsers p and f in sequence
and returns the result of f *)
let ( *>) p f = p >>= fun _ -> f 
  
(* p <* f applies the parsers p and f in sequence
and returns the result of p *)
let ( <*) p f = p >>= fun x -> f >>= fun _ -> res x
      
let sat pred =
  let* x = item in
  if pred x then res x else zero;;

let charParser x = sat ((=) x);;

let xx = charParser 'A';;
let yy = charParser 'B';;
parse (yy <|> xx) "ABC";;

let parseAB = xx <* yy;;
parse parseAB "ABC";;

  (*
    let inputZBC = "A";; 
    let parseA = pchar 'A';;
    run parseA "ABC";;

    let parseAny = res 'A';;
    run parseAny "ABC";;
  
    let itemParse = item;;
    run itemParse "ABC";;

    let zeroParser = zero;;
    run zeroParser "ABC";;
*)
