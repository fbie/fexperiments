#if INTERACTIVE
#else
module PrefixNetworks
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

let wdFan wds =
  match wds with
    | [] -> []
    | [wd] -> wds
    | _ ->
      let ws, ds = List.unzip wds
      List.map (fun w -> w, List.max ds + 1) ws

let zdel n =
  [ for i in 1..n -> (i, 0) ]

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
      j :: List.map (fun k -> { fans = fans k; wire = wire k; phase = ph + 1 }) xs

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

  (* Tiggers a bug in Mono:
       Stack overflow in unmanaged: IP: 0x109768bcb, fault addr: 0x7fff55e62ff8
       error FS0193: internal error: Object reference not set to an instance of an object*)
  let rec splitAt0 m xs =
    match xs with
      | [] -> [], []
      | x :: xs when m = 0 -> [x], xs
      | x :: xs ->
        let lxs, rxs = splitAt0 (m - 1) xs
        x :: lxs, rxs

  (* Triggers the same bug when used in skl. *)
  let splitAt1 m xs =
    let rec split m lxs rxs =
      match rxs with
        | []                 -> List.rev lxs, rxs
        | x :: xs when m = 0 -> List.rev lxs, rxs
        | x :: xs            -> split (m - 1) (x :: lxs) xs
    match xs with
      | [] -> [], []
      | x :: xs -> split m [x] xs

  let take m =
    List.mapi (fun i x -> if i < m then Some x else None) >> List.choose id

  let skip m =
    List.mapi (fun i x -> if i >= m then Some x else None) >> List.choose id

  (* O(2n), but at least does not crash or stack-overflow. *)
  let splitAt2 m xs =
    take m xs, skip m xs

  (* For now, use the O(2n) version. *)
  let splitAt = splitAt2

  let toLast f xs =
    init xs @ [f (last xs)]

  (* Applys f to the list of all last elements
     of the lists in xs and puts the resulting
     elements back again. *)
  let toLasts f xs =
    [ for is, l in List.zip (List.map init xs) (f (List.map last xs)) -> is @ [l] ]

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
      let lxs, rxs = splitAt2 ((List.length xs) / 2) xs
      let lys, rys = skl f lxs, skl f rxs (* Recursive computation. *)
      init lys @ f (last lys :: rys) (* Combination and propagation to the bottom right. *)

let rec sklPar f xs =
  match xs with
    | [] -> []
    | [x] -> xs
    | xs ->
      let lxs, rxs = splitAt2 ((List.length xs) / 2) xs
      let [| lys; rys |] = Array.Parallel.map (sklPar f) [| lxs; rxs |]
      init lys @ f (last lys :: rys) (* Combination and propagation to the bottom right. *)

(* Slow and prone to stack-overflow. *)
let rec split0 ds xs =
  match ds with
    | [] -> []
    | d :: ds ->
      let lxs, rxs = splitAt d xs
      lxs :: split0 ds rxs

(* No stack overflows since tail-recursive, but still slow. *)
let split1 ds xs =
  let rec spl ds xs ss =
    match ds with
      | [] -> List.rev ss
      | d :: ds ->
        let lxs, rxs = splitAt d xs
        spl ds rxs (lxs :: ss)
  spl ds xs []

let split = split1

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
  List.replicate (n / 2) 2 @ [n % 2]

(* Brent-Kung pattern *)
let rec bk0 f xs =
  match xs with
    | [] -> []
    | [x] -> xs
    | xs -> build0 (twos (List.length xs)) bk0 f xs

let rec twos' = function
  | 1 -> [1]
  | 2 -> [1; 1]
  | n -> 2 :: twos' (n - 2)

let rec bk1 f xs =
  match xs with
    | [] -> []
    | [x] -> xs
    | xs -> build0 (twos' (List.length xs)) bk1 f xs

module Benchmark =
  let test pp n =
    pp (mkFan (@)) [ for i in 1..n -> [i] ] |> ignore

  let time f =
    let t = System.Diagnostics.Stopwatch.StartNew()
    f()
    t.Elapsed.TotalMilliseconds

  let benchmark m f =
    let es = List.init (m + 3) (fun _ -> time f) |> List.tail |> List.tail |> List.tail
    let avg = List.average es
    let stdev = List.map (fun x -> (x - avg) ** 2.0) es |> List.average
    avg, stdev
