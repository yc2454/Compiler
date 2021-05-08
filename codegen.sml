signature CODEGEN = 
sig
  val codegen : MipsFrame.frame -> Tree.stm -> Assem.instr list 
end

structure MipsGen : CODEGEN =
struct
                    
  fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
    let
      val ilist = ref (nil : Assem.instr list)

      fun emit x = ilist := x :: !ilist

      fun result(gen) = let val t = Temp.newtemp() in gen t; t end

      fun int2str n = if n >= 0 then Int.toString n else ("-" ^ Int.toString (~n))

      val calldefs = [MipsFrame.rv, MipsFrame.ra] @ MipsFrame.argregs @ MipsFrame.callersaves
      
      fun relop2str Tree.EQ = "beq"
        | relop2str Tree.NE = "bne"
        | relop2str Tree.LT = "blt"
        | relop2str Tree.LE = "ble"
        | relop2str Tree.GT = "bgt"
        | relop2str Tree.GE = "bge"
        | relop2str _ = "this relop should not be in Tiger!"
      
      fun munchStm (Tree.SEQ(a, b)) = (munchStm a; munchStm b)

        | munchStm (Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, e1, Tree.CONST i)), e2)) =
          emit (Assem.OPER {assem="sw `s1, " ^ (int2str i) ^ "(`s0)\n",
                            src=[munchExp e1, munchExp e2],
                            dst=[], jump=NONE})
        | munchStm (Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST i, e1)), e2)) =
          emit (Assem.OPER {assem="sw `s1, " ^ (int2str i) ^ "(`s0)\n",
                            src=[munchExp e1, munchExp e2],
                            dst=[], jump=NONE})
        | munchStm (Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.MINUS, e1, Tree.CONST i)), e2)) =
          emit (Assem.OPER {assem="sw `s1, " ^ (int2str (~i)) ^ "(`s0)\n",
                            src=[munchExp e1, munchExp e2],
                            dst=[], jump=NONE})
        (* | munchStm (Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.MINUS, Tree.CONST i, e1)), e2)) =
          emit (Assem.OPER {assem="sw `s1, " ^ (int2str ~i) ^ "(`s0)\n",
                            src=[munchExp e1, munchExp e2],
                            dst=[], jump=NONE}) *)
        | munchStm (Tree.MOVE(Tree.MEM(e1), e2)) =
          emit (Assem.OPER {assem="sw `s1, 0(`s0)\n", src=[munchExp e1, munchExp e2],
                            dst=[], jump=NONE})

        | munchStm (Tree.MOVE(Tree.TEMP i, Tree.CONST n)) =
          emit (Assem.OPER {assem="li `d0, " ^ (int2str n) ^ "\n", src=[],
                            dst=[i], jump=NONE})

        | munchStm (Tree.MOVE(Tree.TEMP i, Tree.NAME lab)) =
          emit (Assem.OPER {assem="la `d0, " ^ (Symbol.name lab) ^ "\n", src=[],
                            dst=[i], jump=NONE})

        | munchStm (Tree.MOVE(Tree.TEMP i1, Tree.TEMP i2)) =
          emit (Assem.MOVE {assem="move `d0, `s0\n", src=i2,
                            dst=i1})

        | munchStm (Tree.MOVE(Tree.TEMP i, Tree.MEM(Tree.BINOP(Tree.PLUS, e, Tree.CONST n)))) =
          emit (Assem.OPER {assem="lw `d0, " ^ (int2str n) ^ "(`s0)\n", src=[munchExp e],
                            dst=[i], jump=NONE})
        | munchStm (Tree.MOVE(Tree.TEMP i, Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST n, e)))) =
          emit (Assem.OPER {assem="lw `d0, " ^ (int2str n) ^ "(`s0)\n", src=[munchExp e],
                            dst=[i], jump=NONE})
        | munchStm (Tree.MOVE(Tree.TEMP i, Tree.MEM(Tree.BINOP(Tree.MINUS, e, Tree.CONST n)))) =
          emit (Assem.OPER {assem="lw `d0, " ^ (int2str (~n)) ^ "(`s0)\n", src=[munchExp e],
                            dst=[i], jump=NONE})
        (* | munchStm (Tree.MOVE(Tree.TEMP i, Tree.MEM(Tree.BINOP(Tree.MINUS, Tree.CONST i, e)))) =
          emit (Assem.OPER {assem="lw `d0, " ^ (int2str ~i) ^ "(`s0)\n", src=[munchExp e],
                            dst=[i], jump=NONE}) *)
        
        | munchStm (Tree.MOVE(Tree.TEMP i, e2)) =
          emit (Assem.MOVE {assem="move `d0, `s0\n", src=munchExp e2,
                            dst=i})
        
        | munchStm (Tree.JUMP(Tree.NAME(lab), labs)) = 
          emit (Assem.OPER {assem="j " ^ (Symbol.name lab) ^ "\n",
                            src=[], dst=[], jump=SOME labs})
        
        | munchStm (Tree.JUMP(Tree.TEMP i, labs)) = 
          emit (Assem.OPER {assem="jr `s0\n",
                            src=[i], dst=[], jump=SOME labs})

        | munchStm (Tree.JUMP(e, labs)) = 
          emit (Assem.OPER {assem="jr `s0\n",
                            src=[munchExp e], dst=[], jump=SOME labs})

        | munchStm (Tree.CJUMP (Tree.GE, e1, Tree.CONST 0, lab1, lab2)) = 
          emit (Assem.OPER {assem="bgez `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})
        
        | munchStm (Tree.CJUMP (Tree.GE, Tree.CONST 0, e1, lab1, lab2)) = 
          emit (Assem.OPER {assem="bltz `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})

        | munchStm (Tree.CJUMP (Tree.GT, e1, Tree.CONST 0, lab1, lab2)) = 
          emit (Assem.OPER {assem="bgtz `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})
        
        | munchStm (Tree.CJUMP (Tree.GT, Tree.CONST 0, e1, lab1, lab2)) = 
          emit (Assem.OPER {assem="blez `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})

        | munchStm (Tree.CJUMP (Tree.LE, e1, Tree.CONST 0, lab1, lab2)) = 
          emit (Assem.OPER {assem="blez `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})
        
        | munchStm (Tree.CJUMP (Tree.LE, Tree.CONST 0, e1, lab1, lab2)) = 
          emit (Assem.OPER {assem="bgtz `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})

        | munchStm (Tree.CJUMP (Tree.LT, e1, Tree.CONST 0, lab1, lab2)) = 
          emit (Assem.OPER {assem="bltz `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})
        
        | munchStm (Tree.CJUMP (Tree.LT, Tree.CONST 0, e1, lab1, lab2)) = 
          emit (Assem.OPER {assem="bgez `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})

        | munchStm (Tree.CJUMP (Tree.EQ, e1, Tree.CONST 0, lab1, lab2)) = 
          emit (Assem.OPER {assem="beqz `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})
        
        | munchStm (Tree.CJUMP (Tree.EQ, Tree.CONST 0, e1, lab1, lab2)) = 
          emit (Assem.OPER {assem="beqz `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})

        | munchStm (Tree.CJUMP (Tree.NE, e1, Tree.CONST 0, lab1, lab2)) = 
          emit (Assem.OPER {assem="bnez `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})
        
        | munchStm (Tree.CJUMP (Tree.NE, Tree.CONST 0, e1, lab1, lab2)) = 
          emit (Assem.OPER {assem="bnez `s0, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1], dst=[], jump=SOME [lab1, lab2]})
        
        | munchStm (Tree.CJUMP (rel, e1, e2, lab1, lab2)) = 
          emit (Assem.OPER {assem=(relop2str rel) ^ " `s0, `s1, " ^ (Symbol.name lab1 ^ "\n"),
                            src=[munchExp e1, munchExp e2], dst=[], jump=SOME [lab1, lab2]})

        | munchStm (Tree.EXP(Tree.CALL (Tree.NAME f, args))) = 
          emit (Assem.OPER {assem="jal " ^ (Symbol.name f) ^ "\n",
                            src=(munchArgs (0, args)),
                            dst=calldefs, jump=NONE})
        
        | munchStm (Tree.EXP(Tree.CALL (e, args))) = 
          emit (Assem.OPER {assem="jal `s0\n",
                            src=(munchExp e) :: (munchArgs (0, args)),
                            dst=calldefs, jump=NONE})

        | munchStm (Tree.EXP(e)) = 
          (munchExp e; ())

        | munchStm (Tree.LABEL lab) = 
          emit (Assem.LABEL{assem=(Symbol.name lab) ^ ":\n", lab=lab})
        
        | munchStm (Tree.BOUNDSL idx) = 
          let
            val l = Temp.newlabel()
          in
            (emit (Assem.OPER {assem="bltz `s0, exit\n", src=[munchExp idx], dst=[],
                              jump=SOME [Temp.namedlabel "exit", l]});
             emit (Assem.LABEL{assem=(Symbol.name l) ^ ":\n", lab=l}))
          end
          
        
        | munchStm (Tree.BOUNDSH (idx, arr)) = 
          let
            val size = Tree.MEM (Tree.BINOP (Tree.MINUS, arr, Tree.CONST MipsFrame.wordSize))
            val l = Temp.newlabel()
          in
            (emit (Assem.OPER {assem="bgt `s0, `s1, exit\n", 
                              src=[munchExp idx, munchExp size], dst=[],
                              jump=SOME [Temp.namedlabel "exit", l]});
             emit (Assem.LABEL{assem=(Symbol.name l) ^ ":\n", lab=l}))
          end

      and munchArgs (n, args) = 
        case args of
           [] => []
         | a :: aa => 
           let
             val areg = List.nth (MipsFrame.argregs, n)
           in
             (munchStm(Tree.MOVE(Tree.TEMP areg, Tree.TEMP(munchExp a))); 
              areg :: munchArgs(n+1, aa))
           end

      and munchExp (Tree.MEM(Tree.BINOP(Tree.PLUS, e1, Tree.CONST i))) = 
          result (fn r => 
                    emit (Assem.OPER {assem="lw `d0, " ^ (int2str i) ^ "(`s0)\n",
                                      src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST i, e1))) = 
          result (fn r => 
                    emit (Assem.OPER {assem="lw `d0, " ^ (int2str i) ^ "(`s0)\n",
                                      src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (Tree.MEM(e1)) = 
          result (fn r => 
                    emit (Assem.OPER {assem="lw `d0, 0(`s0)\n",
                                      src=[munchExp e1], dst=[r], jump=NONE}))

        | munchExp (Tree.BINOP(Tree.PLUS, e1, Tree.CONST i)) = 
          result (fn r => 
                    emit (Assem.OPER {assem="addi `d0, `s0, " ^ (int2str i) ^ "\n",
                                      src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (Tree.BINOP(Tree.PLUS, Tree.CONST i, e1)) = 
          result (fn r => 
                    emit (Assem.OPER {assem="addi `d0, `s0, " ^ (int2str i) ^ "\n",
                                      src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (Tree.BINOP(Tree.PLUS, e1, e2)) = 
          result (fn r => 
                    emit (Assem.OPER {assem="add `d0, `s0, `s1\n",
                                      src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

        | munchExp (Tree.BINOP(Tree.MINUS, e1, Tree.CONST i)) = 
          result (fn r => 
                    emit (Assem.OPER {assem="addi `d0, `s0, -" ^ (int2str i) ^ "\n",
                                      src=[munchExp e1], dst=[r], jump=NONE}))
        | munchExp (Tree.BINOP(Tree.MINUS, e1, e2)) = 
          result (fn r => 
                    emit (Assem.OPER {assem="sub `d0, `s0, `s1\n",
                                      src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

        | munchExp (Tree.BINOP(Tree.MUL, e1, e2)) = 
          result (fn r => 
                    emit (Assem.OPER {assem="mul `d0, `s0, `s1\n",
                                      src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))
        
        | munchExp (Tree.BINOP(Tree.DIV, e1, e2)) = 
          result (fn r => 
                    emit (Assem.OPER {assem="div `d0, `s0, `s1\n",
                                      src=[munchExp e1, munchExp e2], dst=[r], jump=NONE}))

        | munchExp (Tree.CONST i) = 
          result (fn r => 
                    emit (Assem.OPER {assem="addi `d0, $zero, " ^ (int2str i) ^ "\n",
                                      src=[], dst=[r], jump=NONE}))
        
        | munchExp (Tree.NAME lab) = 
          result (fn r => 
                    emit (Assem.OPER {assem="la `d0, " ^ (Symbol.name lab) ^ "\n", src=[],
                                      dst=[r], jump=NONE}))

        | munchExp (Tree.TEMP t) = t

        | munchExp (Tree.CALL (Tree.NAME f, args)) = 
          (emit (Assem.OPER {assem="jal " ^ (Symbol.name f) ^ "\n",
                            src=(munchArgs (0, args)),
                            dst=calldefs, jump=NONE});
          MipsFrame.rv)

        | munchExp (Tree.CALL (e, args)) = 
          (emit (Assem.OPER {assem="jal `s0\n",
                            src=(munchArgs (0, args)),
                            dst=calldefs, jump=NONE});
          MipsFrame.rv)

      (* fun codegen () = 
        MipsFrame.procEntryExit2(frame, rev(!list)) *)
    in
      munchStm stm;
      rev(!ilist)
    end
end
