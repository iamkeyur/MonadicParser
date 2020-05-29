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
  (* p >>= fun a ->
     f >>= fun b -> 
     res (a, b) *)
  let* x = p in
  let* y = f in
  res (x,y) 
    
(* p *> f applies the parsers p and f in sequence
and returns the result of f *)
let ( *>) p f = p >>= fun _ -> f 
  
(* p <* f applies the parsers p and f in sequence
and returns the result of p *)
let ( <*) p f = p >>= fun x -> f >>= fun _ -> res x
    
let (<~>) x xs = x >>= fun r -> xs >>= fun rs -> res (r :: rs)

(* map function f on result produced by parser p*)    
let (|>>) p f = p >>= fun x -> res (f x) 
    
    
let pipe2 p1 p2 f = 
  let* x = p1 in
  let* y = p2 in
  res (f x y)
    
(*  
  let rec many p = 
    (p >>= fun r -> many p |>> fun rs -> r::rs) <|> res []
*)
    
(* alternative implementation of many *)
let rec many p = 
  (let* x = p in
   let* y = many p in
   res (x::y)) <|> res []
  
let many1 x = x <~> many x

let sat pred =
  let* x = item in
  if pred x then res x else zero;;

let charParser x = sat ((=) x);;

(*
  let xx = charParser '1';;
  let yy = charParser '2';; 
  parse (xx <*> yy |>> fun (x, y) -> x::y::[]) "123";; 
  parse (pipe2 xx yy (fun x y -> x::y::[])) "123";;
*)
let xx = charParser 'A';;
let yy = charParser 'B';; 
parse (xx >>= fun _ -> yy) "ABC";; 
let tt = many xx;;
parse tt "B";;
parse (many1 xx) "B";;
