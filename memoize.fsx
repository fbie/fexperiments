
(* Some functions on Dictionary that have a functional looks. *)
module Cache =
  type Cache<'k, 'v> = System.Collections.Concurrent.ConcurrentDictionary<'k, 'v>

  let empty() =
    printfn "Creating empty cache..."
    new Cache<_,_>()

  let get (d: Cache<_,_>) k  =
    match d.TryGetValue k with
      | true, v -> Some v
      | _ -> None

  let put (d: Cache<_,_>) k v =
    d.[k] <- v; v

  let putOrGet (d: Cache<'k,'v>) (k: 'k) (v: 'v) =
    d.GetOrAdd(k, v)

  let putOrCall (d: Cache<'k, Lazy<'v>>) (k: 'k) (f: 'k -> 'v) =
    d.GetOrAdd(k, lazy f k)

(* Typical implementation of fib. *)
let rec fib0 = function
  | 0 | 1 -> 1
  | n -> fib0 (n - 1) + fib0 (n - 2)

(* Does, of course, not work for recursive calls. *)
let memoize0 f =
  let d = Cache.empty() (* Initializes a new cache on every call. *)
  fun k ->
    match Cache.get d k with
      | Some v -> v
      | None -> printfn "Computing f %d" k; Cache.put d (f k) k

let rec fix f x =
  f (fix f) x

(* Continuation based implementation of fib. *)
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
  fix (memoize0 >> fib2)

(* For partial application. *)
let (!!) xs n =
  Seq.nth n xs

(* Only int -> 'a. *)
let memoize1 f =
  (!!) (Seq.initInfinite (fun n -> printfn "Computing f %d"n; f n) |> Seq.cache)

(* Also doesn't work, no difference to the above approaches. *)
let fibmem1 =
  fix (memoize1 >> fib1)

let fibmem2 =
  memoize0 (fix fib2)

(* And so does this, which also is more general. *)
let memoize d f k =
  match Cache.get d k with
    | Some v -> v
    | None -> printfn "Computing f %d" k; Cache.put d (f k) k

(* Something is odd here, some small numbers are recomputed. *)
let fibmem3 =
  fix (fib2 >> memoize (Cache.empty()))
