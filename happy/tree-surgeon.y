{
module Main where
}

%name calc
%tokentype { Token }
%error { parseError }

%token
      '|'             { TokenOr }
      'isChildOf'     { TokenIsChildOf $$ }

%%

Exp:

