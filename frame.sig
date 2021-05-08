signature FRAME = 
sig 
    type frame
    datatype access = InFrame of int | InReg of Temp.temp
    type register = string

    val newFrame : {name: Temp.label, formals: bool list} -> frame 
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access

    val argregs : Temp.temp list
    val calleesaves : Temp.temp list
    val callersaves : Temp.temp list
    val specialregs : Temp.temp list
    val numRegs : int
    val regs : Temp.temp list
    val registers : register list 

    val fp : Temp.temp
    val rv : Temp.temp
    val sp : Temp.temp 
    val ra : Temp.temp
    val wordSize: int
    val exp : access -> Tree.exp -> Tree.exp
    val externalCall : string * Tree.exp list -> Tree.exp
    val tempMap : register Temp.Table.table
    val printTemp : Temp.temp -> string
    val string : Temp.label * string -> string
    val regEq : register * register -> bool

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
    
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list ->
                         {prolog:string, body:Assem.instr list, epilog:string}

end