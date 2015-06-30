module List =
  let last xs =
    (List.rev >> List.head) xs

  let init xs =
    (List.rev >> List.tail >> List.rev) xs

  let take m xs =
    xs |> List.mapi (fun i x -> if i < m then Some x else None) |> List.choose id

  let skip m xs =
    xs |> List.mapi (fun i x -> if i >= m then Some x else None) |> List.choose id

  let split m xs =
    take m xs, skip m xs

module PrefixNetwork =
  type NW<'a> = ('a list -> 'a list) -> 'a list -> 'a list

  let pplus [a; b] =
    [a; a + b]

  let delF [a; b] =
    let m = max a b
    [m; m + 1]

  let rec ser comp xs =
    match xs with
      | [] -> []
      | [x] -> [x]
      | x :: y :: xs ->
        let [c1; c2] = comp [x; y]
        c1 :: ser comp (c2 :: xs)

  let rec fan comp xs =
    match xs with
      | [] -> []
      | x :: xs -> x :: List.map (comp x) xs

  let rec skl comp xs =
    match xs with
      | [] -> []
      | [x] -> [x]
      | _ ->
        let lxs, rxs = List.split ((List.length xs + 1) / 2) xs
        let lys = skl comp lxs
        let rys = skl comp rxs
        let rys' = fan comp ((List.last lys) :: rys)
        (List.init lys) @ rys';

  let buf1 comp xs =
    match xs with
      | [] -> []
      | _ -> (comp [List.head xs]) @ List.tail xs

  let rec bufall comp xs =
    match xs with
      | [] -> []
      | _ -> buf1 comp (List.head xs :: bufall comp (List.tail xs))
