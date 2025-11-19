let option a b c f =
  match (a, b, c) with
  | Some x, Some y, Some z -> Some(f x y z)
  | _ -> None


let rec map2 f lst1 lst2 =
  match (lst1, lst2) with
  | ([], []) -> []
  | (h1::t1, h2::t2) -> (f h1 h2) :: (map2 f t1 t2)
  | _ -> invalid_arg "lists have diff lengths"


let rec find p lst =
  match lst with
  | [] -> raise Not_found
  | h::t -> if p h then h else find p t 


let rec find_opt p lst =
  match lst with
  | [] -> None
  | h::t -> if p h then Some h else find_opt p t


let rec concat lsts =
  match lsts with
  | [] -> []
  | h::t -> h @ concat t


let cartesian lst1 lst2 = 
  concat (List.map (fun x -> List.map (fun y -> (x, y)) lst2) lst1)


let rec cartesianN lsts = 
  match lsts with
  | [] -> [[]]
  | h::t -> 
    let tls = cartesianN t in
    concat (List.map (fun x -> List.map (fun y -> x::y) tls) h)


let rec concat_map lst ~f =
  match lst with
  | [] -> []
  | h::t ->
    let new_lst = f h in 
    new_lst @ concat_map t ~f


let fold_left f acc lst =
  List.fold_right (fun x acc' -> f acc' x) (List.rev lst) acc

let fold_right f lst acc =
  List.fold_left (fun acc' x -> f x acc') acc (List.rev lst)