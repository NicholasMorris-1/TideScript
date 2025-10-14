
%{
  open Types
%}

%token <float> FLOAT
%token <int> NUMERAL
%token <string> ID
%token <string> PEPID
%token <string> MOLID
%token <char> CODE
%token PEPTIDE
%token MOLECULE
%token SOLVENT
%token SOLUTION
%token AASOLUTION
%token RESIN
%token RV
%token PROTOCOL
%token DISPENSE
%token FIND
%token LOCATION
%token IN
%token MIX
%token COMBINE
%token AND
%token ADD
%token AT
%token TO
%token AGITATE
%token CHANGETEMP
%token RETURN
%token STOP
%token WAIT
%token PLUS
%token FOR
%token MINUTES
%token HOURS
%token EQ
%token MM
%token MGML
%token GML
%token COMMA
%token ML
%token GMOL
%token MG
%token C
%token L
%token CALCULATE_AVERAGE_MASS
%token GENERATE_SMILES
%token PRINT
%token CALL
%token VOID
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRAC
%token RBRAC
%token EQUAL
%token LT
%token GT
%token <string>STRING

%token SEMICOLON
%token EOF

/* Precedence */
%left SEMICOLON

/* Top level rule */
%start toplevel
%type <Types.expression> toplevel

%%

/* Grammar */

toplevel: e = expression EOF
  { e }
;

argument:
  | v = ID {StringArg(v)}
  | x = FLOAT {FloatArg(x)}
  | ML {StringArg("mL")}
  | L {StringArg("L")}
  | MM {StringArg("mM")}
  | MGML {StringArg("mg/mL")}


argument_list:
  | arg = argument rest = argument_list {arg :: rest}
  |  { [] }


//arglist:
  //| var = argument rest = arglist {Arglist(var, rest)}
//  |  { EmptyArglist }

sollist:
  |   var = ID LPAREN conc = FLOAT cu = conc_unit RPAREN  rest = sollist {Sollist(var, conc, cu, rest)}
  |  { EmptySollist }

solvnlist:
  | var = ID rest = solvnlist {Solvnlist(var, rest)}
  |  { EmptySolvnlist }

solution_construction:
  | LBRAC args1 = sollist RBRAC IN LBRAC args2 = solvnlist RBRAC
      { fun var -> Solution (var, args1, args2) }
  | COMBINE var2 = ID AND var3 = ID
    { fun var -> Combine (var, var2, var3) }
  | MIX var2 = ID LPAREN eq1 = FLOAT EQ RPAREN PLUS var3 = ID LPAREN eq2 = FLOAT EQ RPAREN AT vol = volume_type unit = volume_unit
    { fun var -> Mix (var, var2, var3, eq1, eq2, vol, unit) }
  | CALL var2 = ID args = argument_list {fun var -> Call_solution_2(var, var2, args) }

conc_unit:
  | MM { Millimolar }
  | MGML { MgPerMl }
  | id = ID { ConcUnitParam id }

return_type:
  | VOID { VoidType }
  | SOLUTION { SolutionType }

volume_type:
  | v = FLOAT { Volume v }
  | id = ID { VolumeParam id }

volume_unit:
  | ML { Milliliters }
  | L { Liters }
  | id = ID { VolumeUnitParam id }



expression:
  | e1 = expression SEMICOLON e2 = expression {Sequence (e1, e2)}
  | PEPTIDE var = ID EQUAL LT var2 = PEPID GT {Addpeptide (var, var2)}
  | MOLECULE var = ID EQUAL LPAREN var2 = MOLID RPAREN {Addmolecule (var, var2)}
  | SOLVENT var = ID {Solvent var}
  | SOLUTION var = ID EQUAL sc = solution_construction { sc var }
  | AASOLUTION LPAREN var2 = CODE RPAREN var = ID EQUAL  LBRAC args1 = sollist RBRAC IN LBRAC args2 = solvnlist RBRAC
      { AASolution (var, var2, args1, args2) }
  | RESIN var1 = ID EQUAL LBRACE  var2 = FLOAT GMOL COMMA var3 = ID RBRACE {AddResin (var1, var2, var3)}
  | RV var1 = ID EQUAL LBRACE var2 = ID COMMA var3 = FLOAT MG RBRACE {AddRV (var1, var2, var3)}
  //| CALCULATE_AVERAGE_MASS LT var = PEPID GT {CalculateAverageMass (var)}
  //| GENERATE_SMILES LT var = PEPID GT {GenerateSmiles (var)}
  | PROTOCOL  LT ret = return_type GT var = ID LPAREN args = argument_list RPAREN LBRACE body = expression RBRACE   {Protocol (var, ret, args, body)}
  //| PROTOCOL LT SOLUTION  GT var = ID args = arglist LBRACE body = expression RBRACE   {Protocol (var, args, body)}
  | RETURN var = ID {Return(var)}
  | CHANGETEMP var = ID TO temp = FLOAT C {ChangeTemp(var, temp)}
  | AGITATE var = ID {Agitate(var)}
  | STOP AGITATE var = ID {Deagitate(var)}
  | WAIT FOR var = NUMERAL HOURS {Wait(var)}
  | DISPENSE var = ID {Dispense var}
  | PRINT {Print}
  | CALL var = ID args = argument_list  {Call_void(var, args)}
