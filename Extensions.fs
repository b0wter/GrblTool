module List

let compare (xs: 'a list) (ys: 'b list) =
    List.fold2 (fun acc x y -> acc && (x = y))