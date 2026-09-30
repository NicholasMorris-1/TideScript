
(* This unit shadows the dependency's Bigraph wrapper module. *)
module Big = Bigraph__Big
module Ctrl = Bigraph__Ctrl
module Face = Bigraph__Face


let ion_a = Big.ion (Face.make ["a"]) (Ctrl.make "A" 1)

let ion_b = Big.ion Face.empty (Ctrl.make "B" 0)

let nest_1 = Big.nest ion_a Big.one

let nest_2 = Big.nest ion_b Big.one

let par_1 = Big.par (Big.nest ion_a Big.one) (Big.nest ion_b Big.one)

let tens_1 = Big.tens (Big.nest ion_a Big.one) (Big.nest ion_b Big.one)

let f = Face.make ["x"]

let id_f = Big.id (Big.Inter (1,f))

let b =
  Big.nest
      (Big.ion (Face.make [ "a" ]) (Ctrl.make "A" 1))
      (Big.nest
         (Big.ion Face.empty (Ctrl.make "Snd" 0))
         (Big.par
            (Big.nest
               (Big.ion (Face.make [ "a"; "v_a" ]) (Ctrl.make "M" 2))
               Big.one)
            (Big.nest
               (Big.ion Face.empty (Ctrl.make "Rcv" 0))
               (Big.nest (Big.ion Face.empty (Ctrl.make "Fun" 0)) Big.one))));;
