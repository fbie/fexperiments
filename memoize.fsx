
(* Some functions on Dictionary that have a functional looks. *)
module Cache =
  type Cache<'k, 'v> = System.Collections.Concurrent.ConcurrentDictionary<'k, 'v>

  let empty() =
    printfn "Creating empty cache..."
    new Cache<_,_>()

  let get (d : Cache<_,_>) k  =
    match d.TryGetValue k with
      | true, v -> Some v
      | _ -> None

  let put (d : Cache<_,_>) k v =
    d.[k] <- v; v

(* Typical implementation of fib. *)
let rec fib0 = function
  | 0 | 1 -> 1
  | n -> fib0 (n - 1) + fib0 (n - 2)

(* Doesn't work for recursive calls. *)
let memoize0 f =
  let d = Cache.empty() (* Initializes a new cache on every call. *)
  fun k ->
    match Cache.get d k with
      | Some v -> v
      | None -> Cache.put d k (f k)

let rec fix f x =
  f (fix f) x

let fib1 f n =
  match n with
    | 0 | 1 -> 1
    | _ -> f (n - 1) + f (n - 2)

(* Uses uint64 to handle larger n. *)
let fib2 f n =
  match n with
    | 0UL | 1UL -> 1UL
    | _ -> f (n - 1UL) + f (n - 2UL)

(* No memoization, instead hundreds of new dictionary allocations... *)
let fibmem0 =
  fix (memoize0 >> fib1)

let rec fix0 f x =
  lazy f (fix0 f) x

let force (l: Lazy<_>) =
  l.Value

let fib3 f n =
  match n with
    | 0 | 1 -> 1
    | _ -> force (f (n - 2)) + force (f (n - 1))

(* The same using lazy computations. Still no memoization. *)
let fibmem1 =
    fix0 (memoize0 >> fib3)

(* For partial application. *)
let (!!) xs n =
  Seq.nth n xs

(* Only int -> 'a. *)
let memoize1 f =
  (!!) (Seq.initInfinite (fun n -> printfn "Computing f %d"n; f n) |> Seq.cache)

(* Also doesn't work, no difference to the above approaches. *)
let fibmem2 =
  fix0 (memoize1 >> fib3)

(* But this does, because Seq.cache side-effects. *)
let fibmem3 =
  memoize0 (fix fib2)

(* And so does this, which also is more general. *)
let getOrComp d f k =
  match Cache.get d k with
    | Some v -> v
    | None -> printfn "Computing f %d" k; Cache.put d (f k) k

let memoize f =
  fix (f >> getOrComp (Cache.empty()))

let fibmem4 (x: uint64) =
  (memoize fib2) x
