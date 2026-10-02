(* This unit shadows the dependency's Bigraph wrapper module. *)
module Big = Bigraph__Big
module Ctrl = Bigraph__Ctrl
module Face = Bigraph__Face
module Solver = Bigraph__Solver
module Brs = Bigraph__Brs
module Fun = Bigraph__Fun
module Ts = Bigraph__Ts
module Tikz = Bigraph__Tikz
module Pbrs = Bigraph__Pbrs
module Sbrs = Bigraph__Sbrs
module TS = Bigraph__Ts





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

let r2 = R.apply s0 [ rule ]

let r3 = R.fix s0 [ rule ]

let pc = R.P_class [ rule ]

let (ts, stats) =
  try R.bfs ~s0:s0 ~priorities:[pc] ~predicates:[]
        ~max:50 (fun _ _ -> ())
  with R.MAX (g, s) -> (g,s)

module WP = Pbrs.Make (S)
let wr =
    WP.parse_react_unsafe ~name:"tick" ~lhs:rdx ~rhs:rct
      7.0
      (Some (Fun.of_list [ (0, 0) ]))

module WS = Sbrs.Make (S)
let sr =
    WS.parse_react_unsafe ~name:"decay" ~lhs:rdx ~rhs:rct
      1.0
      (Some (Fun.of_list [ (0, 0) ]))


let output_to_dot bigraph filename =
  let oc = open_out filename in
  Big.to_dot bigraph  "b" |> output_string oc;
  close_out oc

let save_dot_to_dir bigraph dir filename =
  let path = Filename.concat dir filename in
  let oc = open_out path in
  Big.to_dot bigraph "b" |> output_string oc;
  close_out oc


let output_to_tikz bigraph filename =
  let oc = open_out filename in
  Tikz.big_to_tikz bigraph  |> output_string oc;
  close_out oc

let save_tikz_to_dir bigraph dir filename =
  let path = Filename.concat dir filename in
  let oc = open_out path in
  Tikz.big_to_tikz bigraph |> output_string oc;
  close_out oc


