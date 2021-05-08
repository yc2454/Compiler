structure MipsFrame : FRAME =
struct

    datatype access = InFrame of int | InReg of Temp.temp
    type frame = {name : Temp.label, acc : access list, numLocal : int ref, instrs : Tree.stm list}
    type register = string

    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

    (* the 32 registers of MIPS *)
    val zero = Temp.newtemp()

    val at = Temp.newtemp()

    val rv = Temp.newtemp()
    val v1 = Temp.newtemp()

    val a0 = Temp.newtemp()
    val a1 = Temp.newtemp()
    val a2 = Temp.newtemp()
    val a3 = Temp.newtemp()

    val argregs = [a0, a1, a2, a3]

    val t0 = Temp.newtemp()
    val t1 = Temp.newtemp()
    val t2 = Temp.newtemp()
    val t3 = Temp.newtemp()
    val t4 = Temp.newtemp()
    val t5 = Temp.newtemp()
    val t6 = Temp.newtemp()
    val t7 = Temp.newtemp()

    val s0 = Temp.newtemp()
    val s1 = Temp.newtemp()
    val s2 = Temp.newtemp()
    val s3 = Temp.newtemp()
    val s4 = Temp.newtemp()
    val s5 = Temp.newtemp()
    val s6 = Temp.newtemp()
    val s7 = Temp.newtemp()

    val t8 = Temp.newtemp()
    val t9 = Temp.newtemp()

    val callersaves = [t0, t1, t2, t3, t4, t5, t6, t7, t8]
    val calleesaves = [s0, s1, s2, s3, s4, s5, s6, s7]

    val k0 = Temp.newtemp()
    val k1 = Temp.newtemp()

    val gp = Temp.newtemp()
    val sp = Temp.newtemp()
    val fp = Temp.newtemp()
    val ra = Temp.newtemp()

    val specialregs = [rv, v1, gp, sp, fp, ra]

    (* the locations of all the formals,
       instructions required to implement the "view shift,"
       the number of locals allocated so far,
       and the l a b e l at which the function's machine code is to begin *)

    val wordSize = 4

    val regs = [rv, v1] @ argregs @ [t0, t1, t2, t3, t4, t5, t6, t7] @
                calleesaves @ [t8, t9] @ [gp, sp, fp, ra]
    val reg_names : register list = 
        ["$v0", "$v1", "$a0", "$a1", "$a2", "$a3",
          "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$s0",
          "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7", "$t8", "$t9",
          "$gp", "$sp", "$fp", "$ra"]
    
    val registers : register list = ["$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", 
                                     "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
                                     "$t8", "$v0", "$v1", "$a0", "$a1", "$a2", 
                                     "$a3", "$sp", "$fp", "$ra"]

    fun regEq (r1, r2) = r1 = r2
    
    val numRegs = 24

    fun buildRegMap (temps, names) = 
      case (temps, names) of
         ([], []) => Temp.Table.empty
       | (t::tt, n::nn) => Temp.Table.enter(buildRegMap (tt, nn), t, n)
       | _ => Temp.Table.empty

    val tempMap = buildRegMap (regs, reg_names)

    fun exp (InFrame(k))= (fn (temp) => Tree.MEM(Tree.BINOP(Tree.PLUS, temp, Tree.CONST k)))
      | exp (InReg(temp)) = (fn (_) => Tree.TEMP temp)

    (* val frameSize = 20000 *)

    fun newFrame {name: Temp.label, formals: bool list} = 
        let
          val n = List.length formals
          val num_local = ref 0
          (* val _ = print ("num formals: " ^ (Int.toString (List.length formals)) ^ "\n") *)

          fun buildAcc (formals, escOff, regs) = 
            case formals of
               true :: ff => (num_local := (!num_local + 1); 
                              InFrame(escOff) :: (buildAcc (ff, escOff - wordSize, regs)))
             | false :: ff => 
                (case regs of
                   r :: rr => InReg(Temp.newtemp()) :: (buildAcc (ff, escOff, rr))
                 | [] => InFrame(escOff) :: (buildAcc (ff, escOff, regs)))
             | [] => []
          
          val formalsAcc = buildAcc (formals, ~4, argregs)

          fun buildInstr (acc, reg) = Tree.MOVE ((exp acc) (Tree.TEMP fp), Tree.TEMP reg)
          val instructions = ListPair.map buildInstr (formalsAcc, argregs)

        in
          {name=name, acc=formalsAcc, numLocal=num_local, instrs=instructions}
        end
        
    fun name {name, acc, numLocal, instrs} = name

    fun formals {name, acc, numLocal, instrs} = acc

    fun allocLocal {name, acc, numLocal, instrs} esc = 
      let
        fun getOffset numLocal = ~(4 * numLocal)
      in
        (numLocal := (!numLocal + 1);
        if esc then InFrame((getOffset (!numLocal)))
            else InReg(Temp.newtemp()))
      end

    fun externalCall(s, args) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args)

    fun printTemp temp = 
      case (Temp.Table.look (tempMap, temp)) of
         SOME reg => (reg)
       | NONE => Temp.makestring temp

    fun string(lab, str) = (Symbol.name lab) ^ ": \n" ^ ".word " ^ 
                           (Int.toString (String.size str)) ^ "\n.asciiz " ^ "\"" ^ str ^ "\"\n"

    fun seq [a, b] = Tree.SEQ(a, b)
      | seq [a] = a
      | seq (a :: l) = Tree.SEQ(a, seq l)
      | seq [] = Tree.EXP(Tree.CONST 0)
    
    fun procEntryExit1({name, acc, numLocal, instrs=view_shift}, body) = 
      let

        val to_be_saved = [gp, ra] @ calleesaves
        
        fun do_callee_save ([], instrs, accs) = (instrs, accs)
          | do_callee_save ((r :: rr), instrs, accs) = 
          let
            val acc = allocLocal {name=name, acc=acc, numLocal=numLocal, instrs=view_shift} true
            val instr = Tree.MOVE ((exp acc) (Tree.TEMP fp), Tree.TEMP r)
          in
            do_callee_save (rr, (instr :: instrs), (acc :: accs))
          end
        
        val (callee_save_save, accs) = do_callee_save (to_be_saved, [], [])

        fun do_callee_restore ([], [], instrs) = instrs
          | do_callee_restore ((r :: rr), (a :: aa), instrs) = 
            do_callee_restore (rr, aa, (Tree.MOVE (Tree.TEMP r, (exp a) (Tree.TEMP fp))) :: instrs)
          | do_callee_restore (_, _, instrs) = instrs

        val callee_save_restore = do_callee_restore (to_be_saved, (rev accs), [])

      in
        seq (callee_save_save @ view_shift @ [body] @ callee_save_restore)
      end

    fun procEntryExit2(frame, body) = 
      body @
      [Assem.OPER{assem="",
              src=specialregs,
              dst=[], jump=SOME[]}]
    
    fun procEntryExit3({name, acc, numLocal, instrs}, body) = 
      let
        val frameSize = (!numLocal + 12) * wordSize (* 12 is the number of callee-saved regs *)
        (* val exit_instr = 
          if Symbol.name name = "main" then "li $v0, 10\nsyscall\n" else "jr $ra\n" *)
      in
        {prolog = Symbol.name name ^ ":\n" ^ 
                  "sw $fp, 0($sp)\n" ^
                  "move $fp, $sp\n" ^ 
                  "addiu $sp, $sp, -" ^ (Int.toString frameSize) ^ "\n",
         body = body,
         epilog = "addiu $sp, $sp, " ^ (Int.toString frameSize) ^ "\n" ^ 
                  "lw $fp, 0($sp)\n" ^ 
                  "jr $ra\n"}
      end
      
end