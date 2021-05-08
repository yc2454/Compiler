signature TRANSLATE = 
sig
    type level
    type access 
    type exp

    val outermost : level
    val newLevel : {parent: level, name: Temp.label,  formals: bool list} -> level 
    val formals: level -> access list
    val allocLocal: level -> bool -> access 

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

    val trSimpleVar : access * level -> exp
    val trIfElse : (exp * exp * exp) -> exp
    val trIf : (exp * exp) -> exp
    val trMakeRec : exp list -> exp
    val trMakeArray : exp -> exp -> exp
    val trFieldSelect : exp -> int -> exp
    val trArraySub : exp -> exp -> exp
    val trWhile : exp -> exp -> Temp.label -> exp
    val trFor : (exp * exp * exp * exp * Temp.label) -> exp
    val trCall : (Temp.label * exp list * level * level * bool) -> exp
    val trAssign : exp -> exp -> exp
    val trError : exp
    val trArith : exp -> exp -> Absyn.oper -> exp
    val trCompare : exp -> exp -> Absyn.oper -> exp
    val trCompareString : exp -> exp -> Absyn.oper -> exp
    val trInt : int -> exp
    val trBreak : Temp.label -> exp
    val trString : Temp.label -> string -> exp
    val trUnit : exp
    val trLet : (exp list * exp) -> exp
    val trSeq : exp list -> exp
    (* val trFunDec : (exp * Temp.label) -> exp *)

    val procEntryExit : {level: level, body: exp} -> unit

    type frag = MipsFrame.frag

    val getResult : unit -> frag list

    val reset : unit -> unit
    
    (* val frags : fraglist *)

end