type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val cmtDepth = ref 0
val builder  = ref ""
val unclose  = ref false
val strpos   = ref 0
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = 
    let val pos = hd(!linePos) 
    in 
        if !unclose = true
        then (ErrorMsg.error pos ("Unclosed string "); Tokens.EOF(pos,pos))
        else if !cmtDepth <> 0
        then (ErrorMsg.error pos ("Unclosed comment "); Tokens.EOF(pos,pos))
        else Tokens.EOF(pos,pos)
    end

fun get_int s = 
    case (Int.fromString s) of
       SOME(i) => i
     | NONE => 0

%% 
IDENTIFIER = [a-zA-Z]([0-9a-zA-Z_])*; 
DIGITS = [0-9];
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
QUOTE = [\"];
NONQUOTE = [^\"];
DDD = [0-9][0-9][0-9];

%s COMMENT STRING ESCPSTRING;
%%

<INITIAL>\n     => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>\t     => (continue());
<INITIAL>" "    => (continue());
<INITIAL>while  => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>for    => (Tokens.FOR(yypos, yypos+3));
<INITIAL>to     => (Tokens.TO(yypos, yypos+2));
<INITIAL>break  => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>let    => (Tokens.LET(yypos, yypos+3));
<INITIAL>in     => (Tokens.IN(yypos, yypos+2));
<INITIAL>end    => (Tokens.END(yypos, yypos+3));
<INITIAL>function => (Tokens.FUNCTION(yypos, yypos+8));
<INITIAL>var    => (Tokens.VAR(yypos,yypos+3));
<INITIAL>type   => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>array  => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>if     => (Tokens.IF(yypos, yypos+2));
<INITIAL>then   => (Tokens.THEN(yypos, yypos+4));
<INITIAL>else   => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>do     => (Tokens.DO(yypos, yypos+2));
<INITIAL>of     => (Tokens.OF(yypos, yypos+2));
<INITIAL>nil    => (Tokens.NIL(yypos, yypos+3)); 
<INITIAL>","    => (Tokens.COMMA(yypos, yypos+1));
<INITIAL>":"    => (Tokens.COLON(yypos, yypos+1));
<INITIAL>";"    => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>"("    => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")"    => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>"["    => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]"    => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"{"    => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"}"    => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL>"."    => (Tokens.DOT(yypos, yypos+1));
<INITIAL>"+"    => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-"    => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"*"    => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"/"    => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"="    => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"<>"   => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>">="   => (Tokens.GE(yypos, yypos+2));
<INITIAL>">"    => (Tokens.GT(yypos, yypos+1));
<INITIAL>"<="   => (Tokens.LE(yypos, yypos+2));
<INITIAL>"<"    => (Tokens.LT(yypos, yypos+1));
<INITIAL>"&"    => (Tokens.AND(yypos, yypos+1));
<INITIAL>"|"    => (Tokens.OR(yypos, yypos+1));
<INITIAL>":="   => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL>{IDENTIFIER} => (Tokens.ID(yytext, yypos, yypos+size yytext));
<INITIAL>{DIGITS}+ => (Tokens.INT(get_int yytext, yypos, yypos+size yytext));

<INITIAL>"/*" => (cmtDepth := !cmtDepth+1; YYBEGIN COMMENT; continue());
<COMMENT>"/*" => (cmtDepth := !cmtDepth+1; continue());
<COMMENT>"*/" => (cmtDepth := !cmtDepth-1; if (!cmtDepth = 0) then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT>\n   => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>. => (continue());

<INITIAL>{QUOTE} => (YYBEGIN STRING; builder := ""; strpos := yypos; unclose := true; continue());
<STRING>{QUOTE}  => (YYBEGIN INITIAL; unclose := false; Tokens.STRING(!builder, !strpos, yypos+1));
<STRING>(\\n|\\t|\\\^c|\\{DDD}|\\\"|\\\\) => (builder := !builder ^ valOf(String.fromString yytext); continue());
<STRING>[\\]   => (YYBEGIN ESCPSTRING; continue());
<ESCPSTRING>[\n] => (lineNum := !lineNum+1; linePos := yypos+1 :: !linePos; continue());
<ESCPSTRING>[\ |\t|\f] => (continue()); 
<ESCPSTRING>[\\] => (YYBEGIN STRING; continue());
<ESCPSTRING>. => (ErrorMsg.error yypos ("Illegal escape string " ^ yytext); YYBEGIN STRING; continue());
<STRING>(\\.) => (ErrorMsg.error yypos ("Illegal escape character " ^ yytext); continue());
<STRING>. => (builder := !builder ^ yytext; continue());

<INITIAL>. => (ErrorMsg.error yypos ("Illegal character " ^ yytext); continue());
