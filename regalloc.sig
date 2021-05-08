signature REG_ALLOC =
sig
  (* structure Frame : FRAME *)
  type allocation = MipsFrame.register Temp.Table.table 
  val alloc : Assem.instr list * MipsFrame.frame ->
                Assem.instr list * allocation
end