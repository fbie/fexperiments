#if INTERACTIVE
#else
module PrefixNetwork
#endif

(* We don't actually use these, but they're helpful for error finding. *)
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

let check0 p n =
  let l = [ for i in 1..n -> [i] ]
  p (mkFan (@)) l = List.tail (List.scan (@) [] l)

(* Some functions on lists which the standard library does not provide. *)
module PList =
  let last xs =
    (List.rev >> List.head) xs

  let init xs =
    (List.rev >> List.tail >> List.rev) xs

  (* Robust, tail-recursive list splitting. *)
  let splitAt m xs =
    let rec split m lxs rxs =
      match rxs with
        | []                 -> List.rev lxs, rxs
        | x :: xs when m = 0 -> List.rev lxs, rxs
        | x :: xs            -> split (m - 1) (x :: lxs) xs
    match xs with
      | [] -> [], []
      | x :: xs -> split m [x] xs

  let toLast f xs =
    init xs @ [f (last xs)]

  let toTail f xs =
    List.head xs :: f (List.tail xs)

  let toInit f xs =
    f (init xs) @ [last xs]

open PList

(* Simple Sklansky construction. TODO: Formulate as tail-recursive. *)
let rec skl f xs =
  match xs with
    | [] -> []
    | [x] -> xs
    | xs ->
      let lxs, rxs = splitAt ((List.length xs) / 2) xs
      let lys, rys = skl f lxs, skl f rxs (* Recursive computation. *)
      init lys @ f (last lys :: rys) (* Combination and propagation to the bottom right. *)

(* Slow and prone to stack overflows. *)
let rec split2 ds xs =
  match ds, xs with
    | [], _ -> [xs]
    | d :: ds, xs ->
      let lxs, rxs = splitAt d xs
      lxs :: split2 ds rxs

(* TODO: No stack overflows since tail-recusrive, but still slow. *)
let split ds xs =
  let rec spl ds xs ss =
    match ds, xs with
      | [], _ -> List.rev ss
      | d :: ds, xs ->
        let lxs, rxs = splitAt d xs
        spl ds rxs (lxs :: ss)
  spl ds xs []

let shift xs =
  match xs with
    | [] -> []
    | x :: xs ->
      x - 1 :: init xs @ [last xs + 1]

(* Haskell's f . g corresponds to F#'s f << g. *)
let build0 ws p f =
  split ws >> List.map (ser f) >>
  toInit (toLast (p f)) >> List.concat >>
  split (shift ws) >> toTail (List.map f) >> List.concat

let twos n =
  List.replicate n (n / 2) @ [n % 2]

(* Brent-Kung pattern *)
let rec bK0 f xs =
  match xs with
    | [] -> []
    | [x] -> xs
    | xs -> build0 (twos (List.length xs)) bK0 f xs
