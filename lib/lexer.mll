{
	open Parser
} 



rule token = parse 
	| [' ' '\t' '\r'] { token lexbuf }
  | '\n'            { Lexing.new_line lexbuf; token lexbuf }
  | ['0'-'9']+      {NUMERAL (int_of_string (Lexing.lexeme lexbuf))}
  | ['-']? ['0'-'9']+ '.' ['0'-'9']* { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
  | ['-']? '.' ['0'-'9']+ { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
  | ['-']? ['0'-'9']+ { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
  | '#' [^ '\n' '\r']*     { token lexbuf }  (* Skip comments *)
  | "peptide"       { PEPTIDE }
  | "molecule"      { MOLECULE }
  | "solvent"       { SOLVENT }
  | "solution"      {SOLUTION}
  | "aa_solution"   {AASOLUTION}
  | "resin"         {RESIN}
  | "rv"            {RV}
  | "in"            {IN}
  | "mM"            {MM}
  | "mg"            {MG}
  | "mgML"          {MGML}
  | "C"             {C}
  | "g/Ml"          {GML}
  | "mL"            {ML}
  | "g/mol"         {GMOL}
  | "L"             {L}
  | "eq"            {EQ}
  | "calculate_average_mass"  {CALCULATE_AVERAGE_MASS}
  | "generate_smiles"      {GENERATE_SMILES}
  | "protocol"      {PROTOCOL}
  | "dispense"      {DISPENSE}
  | "find"          {FIND} 
  | "location"      {LOCATION}
  | "combine"       {COMBINE}
  | "mix"           {MIX}
  | "and"           {AND}
  | "add"           {ADD}
  | "at"            {AT}
  | "to"            {TO}
  | "agitate"       {AGITATE}
  | "changeTemp"    {CHANGETEMP}
  | "stop"          {STOP}
  | "wait"          {WAIT}
  | "for"           {FOR}
  | "minutes"       {MINUTES}
  | "hours"         {HOURS}
  | "print"         {PRINT}
  | "call"          {CALL}
  | "void"          {VOID}
  | "return"        {RETURN}
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | ';' 		    { SEMICOLON }
  | '+'             {PLUS}
  | '='             { EQUAL }
  | "<"             { LT }
  | ">"             { GT }
  | "{"             { LBRACE }
  | "}"             { RBRACE}
  | "["             {LBRAC}
  | "]"             {RBRAC}
  | ","             {COMMA}
  | ['a'-'z' '_']*     { ID (Lexing.lexeme lexbuf) }
  | ['A'- 'Z']*     { PEPID (Lexing.lexeme lexbuf) }
  | ['a'-'z' 'A'-'Z' '0'-'9']*   { MOLID (Lexing.lexeme lexbuf) }
  | ['A'-'Z'] as c      {CODE c}
  | eof             { EOF }
