(* I did not quite get the buzz about lenses, so I
   had to see myself, why composition of lenses
   makes sense and how it works. *)

(* The general lens type. *)
type Lens<'a, 'b> = { get : 'a -> 'b; set : 'a -> 'b -> 'a }

(* Access get and set functions more functional. *)
let get l = l.get
let set l = l.set

(* Lenses for pairs. *)
let fstLens = { get = fst; set = fun (a, b) c -> (c, b) }
let sndLens = { get = snd; set = fun (a, b) c -> (a, c) }

(* Composition of lenses. *)
let compGet f g = get f >> get g
let compSet f g a c = set f a (set g (get f a) c) (* Get sub-structure, set it, set it in the main structure. *)

(* Get and update nested data structures via composed lense. *)
let compLens f g = { get = compGet f g; set = compSet f g }

let fstSnd<'a, 'b> = compLens fstLens sndLens
let sndFst<'a, 'b> = compLens sndLens fstLens
let fstFst<'a, 'b> = compLens fstLens fstLens
let sndSnd<'a, 'b> = compLens sndLens sndLens

(* Check that lens rules hold. *)
set fstLens (1, 2) 3 = set fstLens (set fstLens (1, 2) 4) 3
get fstLens (set fstLens (1, 2) 42) = 42
