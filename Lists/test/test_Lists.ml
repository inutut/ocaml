open Lists

let%test "opt with all Some" =
  option (Some 1) (Some 2) (Some 3) (fun x y z -> x + y + z) = Some 6

let%test "opt with None" =
  option (Some 1) None (Some 3) (fun x y z -> x + y + z) = None

let%test "map2 with equal lngths" =
  map2 (fun x y -> x + y) [1; 2; 3] [4; 5; 6] = [5; 7; 9]

let%test "map2 with diff lngths" =
  try
    let _ = map2 (fun x y -> x + y) [1; 2] [4; 5; 6] in
    false
  with
  | Invalid_argument _ -> true
  | _ -> false


let%test "find with existing element" =
  find (fun x -> x = 2) [1; 2; 3] = 2

let%test "find with non-existing element" =
  try
    let _ = find (fun x -> x = 4) [1; 5; 10] in
    false
  with Not_found -> true

let%test "find_opt with non-existing element" =
  find_opt (fun x -> x = 4) [1; 2; 3] = None

let%test "cartesian" =
  cartesian [1; 2] ['a'; 'b'] = [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')]

let%test "cartesianN" =
  cartesianN [[1; 2; 3]; [10; 20]] = [[1; 10]; [1; 20]; [2; 10]; [2; 20]; [3; 10]; [3; 20]]

let%test "concat" =
  concat [[1; 2]; [3; 4]] = [1; 2; 3; 4]

let%test "concat_map" =
  concat_map [1; 2] ~f:(fun x -> [x; x + 1]) = [1; 2; 2; 3]

let%test "fold_left" =
  fold_left (fun acc x -> acc + x) 0 [1; 2; 3] = 6

let%test "fold_right" =
  fold_right (fun x acc -> x + acc) [1; 2; 3] 0 = 6