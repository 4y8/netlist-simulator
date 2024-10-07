let rec map_opt f = function
    [] -> []
  | hd :: tl -> match f hd with None -> map_opt f tl
                              | Some x -> x :: map_opt f tl
