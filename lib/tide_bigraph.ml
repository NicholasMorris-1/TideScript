(* This unit shadows the dependency's Bigraph wrapper module. *)
module Big = Bigraph__Big
module Ctrl = Bigraph__Ctrl
module Face = Bigraph__Face
module Solver = Bigraph__Solver
module Brs = Bigraph__Brs
module Fun = Bigraph__Fun



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


module S = Solver.Make_SAT (Solver.MC)

let s0 =
    Big.nest
      (Big.ion (Face.make [ "ch" ]) (Ctrl.make "Server" 1))
      (Big.par
         (Big.nest (Big.ion (Face.make [ "ch" ]) (Ctrl.make "Client" 1)) Big.one)
         (Big.par
            (Big.nest (Big.ion (Face.make [ "ch" ]) (Ctrl.make "Client" 1)) Big.one)
            (Big.nest (Big.ion Face.empty (Ctrl.make "Idle" 0)) Big.one)))

let pattern =
  Big.ion (Face.make [ "ch" ]) (Ctrl.make "Client" 1)

let result = S.occurs ~target:s0 ~pattern


module R = Brs.Make(S)

let rdx =
  Big.ion (Face.make [ "ch" ]) (Ctrl.make "Client" 1)

 let rct =
    Big.ion (Face.make [ "ch" ]) (Ctrl.make "Active" 0)

 let rule =
    R.parse_react_unsafe ~name:"activate" ~lhs:rdx ~rhs:rct
      () (Some (Fun.of_list [ (0, 0) ]))

let validity_result = R.is_valid_react rule

let (stepped, total) = R.step s0 [ rule ]
