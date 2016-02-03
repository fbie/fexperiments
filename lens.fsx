(* The general lens type. *)
type Lens<'a, 'b> = { get : 'a -> 'b; set : 'a -> 'b -> 'a }

(* Access get and set functions more functionally. *)
let get l = l.get
let set l = l.set

(* Lenses for pairs. *)
let fstLens = { get = fst; set = fun (a, b) c -> (c, b) }
let sndLens = { get = snd; set = fun (a, b) c -> (a, c) }

(* Composition of lenses. *)
let compGet f g = get f >> get g
let compSet f g a c = set f a (set g (get f a) c)

(* Get and update nested data structures via composed lense. *)
let compLens f g = { get = compGet f g; set = compSet f g }
let (>>) f g = compLens f g
let (<<) g f = f >> g

let fstSnd<'a, 'b> = fstLens >> sndLens
let sndFst<'a, 'b> = sndLens >> fstLens
let fstFst<'a, 'b> = fstLens >> fstLens
let sndSnd<'a, 'b> = sndLens >> sndLens

(* Check that lens rules hold. *)
set fstLens (1, 2) 3 = set fstLens (set fstLens (1, 2) 4) 3
get fstLens (set fstLens (1, 2) 42) = 42
