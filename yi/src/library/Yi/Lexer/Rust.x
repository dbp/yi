-- -*- haskell -*- 
--
--  A Rust lexer
--
--  This is based on the Ruby lexer.

{
{-# OPTIONS -w  #-}
module Yi.Lexer.Rust ( initState, alexScanToken ) where

import Yi.Lexer.Alex
import qualified Yi.Syntax
import Yi.Style

}

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}\:]

$ascdigit     = 0-9
$unidigit     = [] -- TODO
$digit        = [$ascdigit $unidigit]

$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol    = [] -- TODO
$symbol       = [$ascsymbol $unisymbol] # [$special \_]

$large        = [A-Z \xc0-\xd6 \xd8-\xde]
$small        = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha        = [$small $large]

$graphic      = [$small $large $symbol $digit $special \"\']

$nonzerodigit = 1-9
$octit        = 0-7
$hexit        = [0-9 A-F a-f]
$idchar       = [$alpha $digit]
$symchar      = [$symbol]
$nl           = [\n\r]

$longintegersuffix = [lL]

@builtinValues =
    true|false|Some|None|Left|Right|Ok|Err

@builtinVars = EXIT_FAILURE|EXIT_SUCCESS|RAND_MAX|EOF|SEEK_SET|SEEK_CUR|SEEK_END|_IOFBF|_IONBF|_IOLBF|BUFSIZ|FOPEN_MAX|FILENAME_MAX|L_tmpnam|TMP_MAX|O_RDONLY|O_WRONLY|O_RDWR|O_APPEND|O_CREAT|O_EXCL|O_TRUNC|S_IFIFO|S_IFCHR|S_IFBLK|S_IFDIR|S_IFREG|S_IFMT|S_IEXEC|S_IWRITE|S_IREAD|S_IRWXU|S_IXUSR|S_IWUSR|S_IRUSR|F_OK|R_OK|W_OK|X_OK|STDIN_FILENO|STDOUT_FILENO|STDERR_FILENO

-- @builtinConstants =

@importst =
    extern mod
  | use

@reservedid = 
    @builtinVars
  |as|assert|break|claim|const|copy|Copy|do|drop|else|extern|fail|for|if|impl|in|let|log|loop|match|mod|module|move|mut|Owned|priv|pub|pure|ref|return|unchecked|unsafe|use|while|mod|Send|static|trait|class|struct|enum|type

@compop =
  "<=" | ">=" | "==" | "<" | ">" | "<>"

@infarithop =
  "+" | "-" | "*" | "/" | "//" | "%" | "&" | "|" | "^" | ">>" | "<<" | "**"

-- This is separated so the infix operators above can be used with the augmented assignment form"
@prefarithop = "~"

@assignop = @infarithop? "="

@reservedop = 
  @compop | @prefarithop | @assignop

@varid  = $alpha $idchar*
@varsym = $symbol+

@digits = $nonzerodigit $digit*
@octits = "0"  $octit
@hexits = "0x" $hexit

@integer     = @digits | @octits | @hexits
@longinteger = @integer $longintegersuffix
@exponent    = [eE] [\-\+] @integer
@number      = @integer | @longinteger
@predicates  = $small* \?

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @number)
@gap     = \\ $whitechar+ \\

@shortstring = $graphic # [\"\'\\] | " " | @escape | @gap
@longstring  = @shortstring | $nl

main :-

<0> {
 $white+                                        { c defaultStyle }

 "#"[^\n]*                                      { c commentStyle }
 $special                                       { c defaultStyle }
 @importst                                      { c importStyle  }

 @reservedid                                    { c keywordStyle }
 @builtinValues                                 { c typeStyle }
 @predicates                                    { c operatorStyle }

 -- classes and modules
 [A-Z] $alpha*                                  { c builtinStyle }
 @varid                                         { c variableStyle }
 @reservedop                                    { c operatorStyle }

 @number @exponent?
   | @number \. @number? @exponent?             { c numberStyle }

 -- symbols and raw strings
 :[$small]+                                     { c stringStyle }
 \%q\{ @longstring* \}
 | \' @shortstring* \'                          { c stringStyle }

 -- interpolated strings
 \%\{ @longstring* \}
 | \%\/ @longstring* \/
 | \%Q\{ @longstring* \}
 | \" @shortstring* \"                          { c stringStyle }

 .                                              { c operatorStyle }
}


{

type HlState = Int
type Token = StyleName

stateToInit x = 0

initState :: HlState
initState = 0

#include "common.hsinc"
}
