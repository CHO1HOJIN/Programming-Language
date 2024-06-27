module P6

/// From list 'l', find the element that appears most frequently in the list,
/// and return how many times it appears. If the input list is empty, return 0.

let m = ref Map.empty<string, int>

let rec countMostFrequent (l: List<'a>) : int =
  match l with
  | [] ->
    let x = Map.fold (fun acc _ v -> max acc v) 0 !m
    m:= Map.empty<string,int>
    x
  | hd::tl ->
    let x = Map.tryFind (string hd) !m 
    match x with
    | Some value ->
      let y = Map.find (string hd) !m
      m := Map.add (string hd) (y+1) !m
      countMostFrequent tl
    | None ->
      m := Map.add (string hd) 1 !m
      countMostFrequent tl