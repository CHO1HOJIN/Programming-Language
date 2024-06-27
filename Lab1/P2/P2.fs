module P2

// (Note) Do NOT change the definition of the following type and exception.
type Exp =
    Num of int
  | Add of Exp * Exp
  | Sub of Exp * Exp
  | Mul of Exp * Exp
  | Div of Exp * Exp

exception DivByZero

/// Return the integer value represented by the expression 'e'. If there is any
/// division-by-zero case, raise the 'DivByZero' exception.
let rec eval (e: Exp) : int =
  match e with
  | Num x -> x
  | Add (x,y) -> (eval x) + (eval y)
  | Sub (x,y) -> (eval x) - (eval y)
  | Mul (x,y) -> (eval x) * (eval y)
  | Div (x,y) ->  let k = eval y
                  match k with
                  | 0 -> raise(DivByZero) 
                  | _ -> (eval x) / (eval y)