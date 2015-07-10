module ParallelPrefix

(* We don't actually use these, but they're helpful for error finging. *)
type Fan<'a> = 'a list -> 'a list
type PP<'a> = Fan<'a> -> 'a list -> 'a list

let mkFan f xs =
  match xs with
    | [] -> []
    | x :: xs -> x :: (List.map (f x) xs)

let rec ser f xs =
  match xs with
    | [] -> []
    | [x] -> xs
    | x :: y :: xs ->
      let [x'; y'] = f [x; y]
      x' :: ser f (y' :: xs)

let delay ds =
  match ds with
    | [] -> []
    | [d] -> ds
    | ds -> let dm = List.max ds + 1 in [ for i in 1..List.length ds -> dm ]

(* Just to avoid writing new 0-arrays all the time... *)
let checkDelay p n =
  p delay [ for i in 1..n -> 0 ]

(* For later visualization of networks. *)
module Net =
  type Net = { fans: (int * int list) list; wire: int; phase: int }

  let fans n = n.fans
  let wire n = n.wire
  let phase n = n.phase

  let netsz n =
    [ for i in 1..n -> { fans = []; wire = i; phase = 0 } ]

  let netFan is =
    match is with
    | [] -> []
    | [x] -> [x]
    | x :: xs ->
      let ph = (List.map phase >> List.max) is
      let j = { fans = (ph, List.map wire is) :: fans x; wire = wire x; phase = ph + 1 }
      j :: List.map (fun k -> { fans = k.fans; wire = k.wire; phase = ph + 1 }) xs

  let getNets f n =
    f netFan (netsz n)

(* Some functions on lists which the standard library does not provide. *)
module PList =
  let last xs =
    (List.rev >> List.head) xs

  let init xs =
    (List.rev >> List.tail >> List.rev) xs

  let take m xs =
    xs |> List.mapi (fun i x -> if i < m then Some x else None) |> List.choose id

  let skip m xs =
    xs |> List.mapi (fun i x -> if i >= m then Some x else None) |> List.choose id

  (* We could also use List.partition for this, but it'd be a bit more clumsy. *)
  let splitAt m xs =
    take m xs, skip m xs

  let toLasts f xs =
    List.map (fun (l, r) -> l @ [r]) <| List.zip (List.map init xs) (f (List.map last xs))

  let toTail f xs =
    List.head xs :: f (List.tail xs)

  let toLast f xs =
    init xs @ [f (last xs)]

  let toInit f xs =
    f (init xs) @ [last xs]

open PList

(* Simple Sklansky construction. *)
let rec skl f xs =
  match xs with
    | [] -> []
    | [x] -> xs
    | xs ->
      let lxs, rxs = splitAt ((List.length xs) / 2) xs
      let lys, rys = skl f lxs, skl f rxs (* Recursive computation. *)
      init lys @ f (last lys :: rys) (* Combination and propagation to the bottom right. *)

(* Brent-Kung pattern *)
let rec split ds xs =
  match ds, xs with
    | [], _ -> []
    | _, [] -> []
    | d :: ds, xs ->
      let lxs, rxs = splitAt d xs
      lxs :: split ds rxs

let shift xs =
  match xs with
    | [] -> []
    | x :: xs ->
      x - 1 :: init xs @ [last xs + 1]

(* Haskell's f . g corresponds to F#'s f << g. *)
let build0 ws p f =
  split ws >> List.map (ser f) >>
  toInit (toLasts (p f)) >> List.concat >>
  split (shift ws) >> toTail (List.map f) >> List.concat

let twos n =
  List.replicate n (n / 2) @ [n % 2]

let rec bK0 f xs =
  match xs with
    | [] -> []
    | [x] -> xs
    | xs -> build0 (twos (List.length xs)) bK0 f xs
