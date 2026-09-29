
(* This unit shadows the dependency's Bigraph wrapper module. *)
module Big = Bigraph__Big
module Ctrl = Bigraph__Ctrl
module Face = Bigraph__Face


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
