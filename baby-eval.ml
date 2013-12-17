type expr = If of expr * expr * expr
	  | Bool of bool
	  | Int of int;;

exception Predicate_not_bool;;
  
let rec eval exp env =
  match exp with
  | If (pred, cons, alt) -> eval_if pred cons alt env
  | Int x -> (Int x)
  | Bool b -> (Bool b)

and  eval_if pred cons alt env =
  match(eval pred env) with
  | Bool true -> (eval cons env)
  | Bool false -> (eval alt env)
  | _ -> raise Predicate_not_bool;;
  
let statement = If (Bool false, Int 7, Bool true);;

(eval statement ());;


