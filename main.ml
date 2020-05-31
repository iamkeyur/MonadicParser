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
  
let opt p = (p |>> (fun x -> Some x)) <|> res None
let optional p = (p >>= fun _ -> res ()) <|> res ();;

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

let rec reduce l op = match l with
  | [] -> res []
  | h::t -> pipe2 h (reduce t op) op
              
let anyOf chars = explode chars
                  |> fun l -> sat (fun x -> List.mem x l)

let spaces = many (anyOf " \n\r");;

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false;;
let is_digit = function '0' .. '9' -> true | _ -> false;;

let digit = sat is_digit;;
parse digit "1";;
parse spaces "\n\n";;

let xx = charParser 'A';;
let yy = charParser 'B';; 
let zz = charParser 'C';;

let ll = xx::yy::zz::[];;

let cons head tail = head::tail;;

let rec seq l = match l with
  | [] -> res []
  | h::t -> pipe2 h (seq t) cons
  
let string s = explode s
               |> List.map(fun c -> charParser c)
               |> seq
               |>> implode;;

parse (string "Patel") "PatelKeyur";;

let tt = many xx;;
parse tt "B";;
parse (many1 xx) "B";;

(* RFC ISO8601 Duration *)
let point = charParser '.';;
let parseDecimal = ((many digit |>> implode) <*> (optional point) *> (many digit |>> implode)) 
                   |>> fun (x, y) -> match y with
                   | "" -> x
                   | _ -> String.concat "." [x ; y];;
parse parseDecimal "11.2332323";;
  
type durSecond = DurSecond of int
type durMinute = DurMinute of int * durSecond option
type durHour   = DurHour of int * durMinute option 
type durTime   = DurTimeHour of durHour 
               | DurTimeMinute of durMinute 
               | DurTimeSecond of durSecond
type durDay = DurDay of int
type durWeek = DurWeek of int
type durMonth = DurMonth of int * durDay option
type durYear = DurYear of int * durMonth option 
type durDate   = DurDateDay of durDay  * durTime option
               | DurDateMonth of durMonth * durTime option
               | DurDateYear  of durYear  * durTime option 
type duration  = DurationDate of durDate
               | DurationTime of durTime
               | DurationWeek of durWeek            

let durSecond = parseDecimal <* charParser 'S' 
                |>> int_of_string 
                |>> (fun x -> DurSecond x);;

let durMinute = ((parseDecimal <* charParser 'M') 
                 |>> int_of_string) 
                <*> (opt durSecond) 
                |>> fun (x,y) -> DurMinute(x,y);;

let durHour = ((parseDecimal <* charParser 'H')
               |>> int_of_string)
              <*> opt durMinute
              |>> fun (x,y) -> DurHour(x,y);;

let durTime = ((charParser 'T') 
               *> ((durHour |>> (fun x -> DurTimeHour x))
                   <|> (durMinute |>> (fun x -> DurTimeMinute x))
                   <|> (durSecond |>> (fun x -> DurTimeSecond x))));;
parse durTime "T12M5S";;
