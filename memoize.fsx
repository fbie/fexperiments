open System.Collections.Generic
(* Some functions on Dictionary that have a functional looks. *)
module Dict =
  let emptyDict() =
    printfn "Creating empty dictionary..."
    new Dictionary<_,_>()

  let get k (d : Dictionary<_,_>) =
    match d.TryGetValue k with
      | true, v -> Some v
      | _ -> None

  let put k v (d : Dictionary<_,_>) =
    d.[k] <- v; v

(* Typical implementation of fib. *)
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)

(* Doesn't work for recursive calls. *)
let memoize f =
  let d = Dict.emptyDict()
  fun k ->
    match Dict.get k d with
      | Some v -> v
      | None -> Dict.put k (f k) d

let rec fix f x =
  f (fix f) x

let fib' f n =
  match n with
    | 0 | 1 -> 1
    | _ -> f (n - 1) + f (n - 2)

(* No memoization, instead hundreds of new dictionary allocations... *)
let fibmem =
  fix (memoize >> fib')

let rec fix' f x =
  lazy f (fix' f) x

let force (l: Lazy<_>) =
  l.Value

let fib'' f n =
  match n with
    | 0 | 1 -> 1
    | _ -> force (f (n - 2)) + force (f (n - 1))

(* The same using lazy computations. Still no memoization. *)
let fibmem' =
    fix' (memoize >> fib'')

(* For partial application. *)
let (!!) xs n =
  Seq.nth n xs

(* Only int -> 'a memoization. *)
let memoize' f =
  let m = Seq.initInfinite (fun n -> printfn "Computing f %d"n; f n) |> Seq.cache
  (!!) m

(* Also doesn't work, no difference to the above approaches. *)
let fibmem'' =
  fix (memoize' >> fib')

(* But this does! Or does it? *)
let fibmem''' =
  memoize' (fix fib')

(* And so does this, which also is more general. *)
let memoizeDict d f k =
  match Dict.get k d with
    | Some v -> v
    | None -> Dict.put k (f k) d

let memoize'' f =
  let d = Dict.emptyDict()
  fix (f >> fun f n -> printfn "Computing f %d"n; memoizeDict d f n)

let fibmem'''' =
  memoize'' fib'
