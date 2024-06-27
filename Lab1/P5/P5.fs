module P5

/// For int list 'l' that contains decimal digits (0~9), return the integer that
/// is represented by this list. For example, "digitsToInt [1; 3; 2]" must
/// return 132 as a result. When the input list is empty, just return 0.
let rec digitsToInt (l: int list) : int =
  let k = List.fold(fun f n -> ((n + f)*10)) 0 l in k / 10