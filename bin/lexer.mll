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
  | "in"            {IN}
  | "mM"            {MM}
  | "calculate_average_mass"  {CALCULATE_AVERAGE_MASS}
  | "generate_smiles"      {GENERATE_SMILES}
  | "protocol"      {PROTOCOL}
  | "dispense"      {DISPENSE}
  | "find"          {FIND} 
  | "location"      {LOCATION}
  | "combine"       {COMBINE}
  | "and"           {AND}
  | "at"            {AT}
  | "agitate"          {AGITATE}
  | "stop"          {STOP}
  | "wait"           {WAIT}
  | "for"           {FOR}
  | "minutes"       {MINUTES}
  | "hours"         {HOURS}
  | "print"         {PRINT}
  | "call"          {CALL}
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | ';' 		    { SEMICOLON }
  | '='             { EQUAL }
  | "<"             { LT }
  | ">"             { GT }
  | "{"             { LBRACE }
  | "}"             { RBRACE}
  | "["             {LBRAC}
  | "]"             {RBRAC}
  | ['a'-'z']*     { ID (Lexing.lexeme lexbuf) }
  | ['A'- 'Z']*     { PEPID (Lexing.lexeme lexbuf) }
  | ['a'-'z' 'A'-'Z' '0'-'9']*   { MOLID (Lexing.lexeme lexbuf) }
  | eof             { EOF }
