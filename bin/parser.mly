
%{
  open Types
%}

%token <float> FLOAT
%token <int> NUMERAL
%token <string> ID
%token <string> PEPID
%token <string> MOLID
%token PEPTIDE
%token MOLECULE
%token SOLVENT
%token SOLUTION
%token PROTOCOL
%token DISPENSE
%token FIND
%token LOCATION
%token IN
%token COMBINE
%token AND
%token AT
%token AGITATE
%token STOP
%token WAIT
%token FOR
%token MINUTES
%token HOURS
%token MM
%token CALCULATE_AVERAGE_MASS
%token GENERATE_SMILES
%token PRINT
%token CALL
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

arglist:
| var = ID rest = arglist {Arglist(var, rest)}
|  { EmptyArglist }

sollist:
|   var = ID LPAREN conc = FLOAT MM RPAREN  rest = sollist {Sollist(var, conc, rest)}
|  { EmptySollist }

solvnlist:
| var = ID rest = solvnlist {Solvnlist(var, rest)}
|  { EmptySolvnlist }




expression: 
| e1 = expression SEMICOLON e2 = expression {Sequence (e1, e2)}
| PEPTIDE var = ID EQUAL LT var2 = PEPID GT {Addpeptide (var, var2)}
| MOLECULE var = ID EQUAL LPAREN var2 = MOLID RPAREN {Addmolecule (var, var2)}
| SOLVENT var = ID {Solvent var}
| SOLUTION var = ID EQUAL LBRAC args1 = sollist RBRAC
     IN LBRAC args2= solvnlist RBRAC {Solution (var, args1, args2)}
| var = ID EQUAL COMBINE var2 = ID AND var3 = ID {Combine(var, var2, var3)}
| CALCULATE_AVERAGE_MASS LT var = PEPID GT {CalculateAverageMass (var)}
| GENERATE_SMILES LT var = PEPID GT {GenerateSmiles (var)} 
| PROTOCOL var = ID args = arglist LBRACE body = expression RBRACE   {Protocol (var, args, body)}
| AGITATE var = ID {Agitate(var)}
| STOP AGITATE var = ID {Deagitate(var)}
| WAIT FOR var = NUMERAL HOURS {Wait(var)}
| DISPENSE var = ID {Dispense var}
| var = ID EQUAL FIND LOCATION {FindLocation(var)}
| PRINT {Print}
| CALL var = ID args = list(ID)  {Call(var, args)}
