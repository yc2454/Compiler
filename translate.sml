structure Translate : TRANSLATE =
struct
    datatype level = Top | Lev of {parent : level, frame : MipsFrame.frame} * unit ref

    type access = level * MipsFrame.access

    datatype exp = 
        Ex of Tree.exp 
      | Nx of Tree.stm 
      | Cx of Temp.label * Temp.label -> Tree.stm

    val outermost = Top

    type frag = MipsFrame.frag

    val frags : frag list ref = ref nil

    fun reset () = frags := nil

    fun newLevel {parent, name, formals} : level = 
      let
        val formals' = true :: formals
        val frame = MipsFrame.newFrame {name=name, formals=formals'}
      in
        Lev ({parent=parent, frame=frame}, ref ())
      end

    fun formals lev = 
      let
        val Lev ({parent, frame}, uniq) = lev
        (* val (_ :: faccs) = MipsFrame.formals frame *)
      in
        List.map (fn acc => (lev, acc)) (List.tl (MipsFrame.formals frame))
      end

    fun allocLocal lev esc = 
      case lev of
        Lev({parent=_, frame=frame}, _) => (lev, MipsFrame.allocLocal frame esc)
    
    fun seq [a, b] = Tree.SEQ(a, b)
      | seq [a] = a
      | seq (a :: l) = Tree.SEQ(a, seq l)
      | seq [] = Tree.EXP(Tree.CONST 0)

    fun unEx (Ex e) = e
      | unEx (Cx genstm) =
        let 
            val r = Temp.newtemp()
            val t = Temp.newlabel() and f = Temp.newlabel()
        in 
            Tree.ESEQ(seq[Tree.MOVE(Tree.TEMP r, Tree.CONST 1), genstm(t,f),
                          Tree.LABEL f,
                          Tree.MOVE(Tree.TEMP r, Tree.CONST 0), 
                          Tree.LABEL t],
                    Tree.TEMP r)
        end
      | unEx (Nx s) = Tree.ESEQ(s, Tree.CONST 0)
  
  fun unNx (Nx s) = s
    | unNx (Ex e) = Tree.EXP(e)
    | unNx (Cx genstm) = 
      let
        val t = Temp.newlabel() 
      in
        (genstm(t, t); Tree.LABEL t)
      end
  
  fun unCx (Cx genstm) = genstm
    | unCx (Nx _) = raise ErrorMsg.Error
    | unCx (Ex e) = 
      case e of
         Tree.CONST 0 => (fn (t, f) => Tree.LABEL(f))
       | Tree.CONST 1 => (fn (t, f) => Tree.LABEL(t))
       | _ => (fn (t, f) => Tree.CJUMP(Tree.EQ, e, Tree.CONST 0, f, t))

  fun trInt i = Ex (Tree.CONST i)

  fun trBreak label = Nx (Tree.JUMP (Tree.NAME label, [label]))

  fun trSimpleVar (acc, use_lev) = 
    let
      val (Lev ({parent=def_parent, frame=def_frame}, def), facc) = acc
      
      (* facc: offset in original env *)
      
      (* val _ = 
        case facc of
           MipsFrame.InFrame (k) => print ("in mem: " ^ (Int.toString k) ^ "\n")
         | _ => () *)

      fun findStaticLinks (cur_lev, sl) = 
        case cur_lev of
           Top => sl
         | Lev ({parent=cur_parent, frame=cur_frame}, cur) => 
           if cur = def then (MipsFrame.exp facc sl) else 
           findStaticLinks (cur_parent, MipsFrame.exp (List.hd (MipsFrame.formals cur_frame)) sl)

      val simpleVarExp = findStaticLinks (use_lev, Tree.TEMP (MipsFrame.fp))
      
    in
      Ex simpleVarExp
    end
  
    fun trIfElse (e1, e2, e3) = 
      let
        val ce1 = unCx e1
        val ee2 = unEx e2
        val ee3 = unEx e3
        val t = Temp.newlabel() and f = Temp.newlabel()
        val r = Temp.newtemp()
        val join = Temp.newlabel()
      in
        Ex (Tree.ESEQ(seq([ce1(t, f), Tree.LABEL(t), Tree.MOVE(Tree.TEMP r, ee2), 
                       Tree.JUMP(Tree.NAME join, [join]),
                       Tree.LABEL(f), 
                       Tree.MOVE(Tree.TEMP r, ee3),
                       Tree.LABEL(join)]),
                    Tree.TEMP r))
      end

    fun trIf (e1, e2) = 
      let
        val ce1 = unCx e1
        val ee2 = unEx e2
        val t = Temp.newlabel() and f = Temp.newlabel()
        val r = Temp.newtemp()
      in
        Ex (Tree.ESEQ(seq([ce1(t, f), Tree.LABEL(t), 
                       Tree.MOVE(Tree.TEMP r, ee2), 
                       Tree.LABEL(f)]),
                    Tree.TEMP r))
      end
    
    fun trMakeRec inits = 
      let
        val inits' = List.map unEx inits
        val len = List.length inits
        val r = Temp.newtemp()
        val mem_size = Tree.BINOP(Tree.MUL, Tree.CONST MipsFrame.wordSize, Tree.CONST len)
        val mem_alloc = Tree.MOVE(Tree.TEMP r, 
          Tree.CALL(Tree.NAME(Temp.namedlabel("tig_allocRecord")), [mem_size]))
        fun st ([], _) = []
          | st (e::ee, idx) = 
            Tree.MOVE(Tree.MEM (Tree.BINOP(Tree.PLUS, Tree.CONST idx, Tree.TEMP r)), e) :: 
            st (ee, idx + 4)
      in
        Ex (Tree.ESEQ(seq(mem_alloc::(st (inits', 0))), Tree.TEMP r))
      end

    fun trMakeArray sizeExp initExp = 
      (* Ex (MipsFrame.externalCall ("initArray", [unEx sizeExp, unEx initExp])) *)
      let
        val init' = unEx initExp
        val size = unEx sizeExp
        val r = Temp.newtemp()
        val mem_size = Tree.BINOP(Tree.PLUS, Tree.CONST 1, size)
        val mem_alloc = Tree.MOVE(Tree.TEMP r, 
          MipsFrame.externalCall ("tig_initArray", [mem_size, unEx initExp]))
        val record_size = Tree.MOVE(Tree.MEM (Tree.TEMP r), Tree.BINOP(Tree.MINUS, size, Tree.CONST 1))
        val r_return = Temp.newtemp()
        val move_return = Tree.MOVE(Tree.TEMP r_return, 
          Tree.BINOP(Tree.PLUS, Tree.TEMP r, Tree.CONST MipsFrame.wordSize))
      in
        Ex (Tree.ESEQ(seq([mem_alloc, record_size, move_return]), 
                           Tree.TEMP r_return))
      end

    fun trFieldSelect a idx = 
      let
        val ea = unEx a
      in
        Ex (Tree.MEM(Tree.BINOP(Tree.PLUS, ea, 
        Tree.BINOP(Tree.MUL, Tree.CONST idx, Tree.CONST MipsFrame.wordSize))))
      end
    
    fun trArraySub a idxExp = 
      let
        val ea = unEx a
        val ei = unEx idxExp
        val bounds_check = seq([Tree.BOUNDSL(ei), Tree.BOUNDSH(ei, ea)])
      in
        Ex (Tree.ESEQ(bounds_check, (Tree.MEM(Tree.BINOP(Tree.PLUS, ea, 
            Tree.BINOP(Tree.MUL, ei, Tree.CONST MipsFrame.wordSize))))))
      end

    fun trWhile condExp body done = 
      let
        val test = Temp.newlabel()
        val start_body = Temp.newlabel()
      in
        Nx (seq([Tree.LABEL test, 
                 Tree.CJUMP(Tree.EQ, unEx condExp, Tree.CONST 0, done, start_body), 
                 Tree.LABEL start_body, 
                 unNx body, 
                 Tree.JUMP(Tree.NAME test, [test]),
                 Tree.LABEL done]))
      end

    fun trAssign varExp valExp = 
      let
        val evar = unEx varExp
        val eval = unEx valExp
      in
        Nx (Tree.MOVE(evar, eval))
      end
    
    fun trFor (iter, loExp, hiExp, body, done) = 
      let
        val tempH = Temp.newtemp()
        val l1 = Temp.newlabel()
        val l2 = Temp.newlabel()
        val x = unEx iter
        val lo = unEx loExp
        val hi = unEx hiExp
        val xpp = Tree.MOVE(x, Tree.BINOP(Tree.PLUS, x, Tree.CONST 1))
        val body_stm = unNx body
      in
        Nx (seq[unNx (trAssign(iter)(loExp)),
                Tree.MOVE(Tree.TEMP tempH, hi), 
                Tree.CJUMP(Tree.LE, x, Tree.TEMP tempH, l2, done),
                Tree.LABEL l1, xpp,
                Tree.LABEL l2, body_stm,
                Tree.CJUMP(Tree.LT, x, Tree.TEMP tempH, l1, done),
                Tree.LABEL done])
      end

    val trError = Ex (Tree.CONST 0)

    val trUnit = Nx (Tree.EXP(Tree.CONST 0))
    
    fun trCall (label_f, argsExp, caller_lev, callee_lev, isproc) = 
      let
        fun compareLevel (Top, Top) = true
          | compareLevel (Lev ({parent=p1, frame=f1}, l1), Lev ({parent=p2, frame=f2}, l2)) = l1 = l2
          | compareLevel _ = false
        val argsExp' = List.map unEx argsExp
        val (caller_parent, caller) = 
          case caller_lev of
             Top => (Top, ref ())
           | Lev ({parent=caller_parent, frame=caller_frame}, caller) => (caller_parent, caller)
        val caller_sl = Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST ~4, Tree.TEMP MipsFrame.fp))
      in
        case callee_lev of
          Top => if isproc then 
                  Nx (Tree.EXP (MipsFrame.externalCall(Symbol.name label_f, argsExp')))
                  else
                  Ex (MipsFrame.externalCall(Symbol.name label_f, argsExp'))
        | Lev ({parent=callee_parent, frame=callee_frame}, callee) => 
            (let
              fun findStaticLinks (cur_lev, sl) = 
                if compareLevel (cur_lev, callee_parent) then sl
                else (case cur_lev of
                   Top => sl
                 | Lev ({parent=cur_parent, frame=cur_frame}, _) => 
                   findStaticLinks (cur_parent, Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST ~4, sl))))

              val sl = 
                if compareLevel (caller_lev, callee_lev) then caller_sl 
                else if compareLevel (caller_lev, callee_parent) then Tree.TEMP MipsFrame.fp
                else findStaticLinks (caller_parent, caller_sl)

              val argsExp'' = sl::argsExp'
            in
              if isproc then 
              Nx (Tree.EXP(Tree.CALL(Tree.NAME label_f, argsExp'')))
              else
              Ex (Tree.CALL(Tree.NAME label_f, argsExp''))
            end)
      end

    fun trString lab lit = 
      ((frags := (MipsFrame.STRING (lab, lit) :: !frags)); Ex (Tree.NAME(lab)))

    fun trArith l r Absyn.PlusOp = Ex (Tree.BINOP(Tree.PLUS, unEx l, unEx r))
      | trArith l r Absyn.MinusOp = Ex (Tree.BINOP(Tree.MINUS, unEx l, unEx r))
      | trArith l r Absyn.TimesOp = Ex (Tree.BINOP(Tree.MUL, unEx l, unEx r))
      | trArith l r Absyn.DivideOp = Ex (Tree.BINOP(Tree.DIV, unEx l, unEx r))
      | trArith l r _ = trError
    
    fun trCompare l r Absyn.GtOp = Cx (fn (t, f) => Tree.CJUMP(Tree.GT, unEx l, unEx r, t, f))
      | trCompare l r Absyn.GeOp = Cx (fn (t, f) => Tree.CJUMP(Tree.GE, unEx l, unEx r, t, f))
      | trCompare l r Absyn.LtOp = Cx (fn (t, f) => Tree.CJUMP(Tree.LT, unEx l, unEx r, t, f))
      | trCompare l r Absyn.LeOp = Cx (fn (t, f) => Tree.CJUMP(Tree.LE, unEx l, unEx r, t, f))
      | trCompare l r Absyn.EqOp = Cx (fn (t, f) => Tree.CJUMP(Tree.EQ, unEx l, unEx r, t, f))
      | trCompare l r Absyn.NeqOp = Cx (fn (t, f) => Tree.CJUMP(Tree.NE, unEx l, unEx r, t, f))
      | trCompare l r _ = trError

    fun trCompareString l r rel = 
      let
        val res = MipsFrame.externalCall ("tig_stringEqual", [unEx l, unEx r])
      in
        case rel of
           Absyn.EqOp => Cx (fn (t, f) => Tree.CJUMP(Tree.EQ, res, Tree.CONST 1, t, f))
         | Absyn.NeqOp => Cx (fn (t, f) => Tree.CJUMP(Tree.NE, res, Tree.CONST 0, t, f))
         | _ => trError
      end

    fun trLet (assignments, body) = 
      case assignments of
         [] => body
       | _ => 
         let
           val ass_stms = List.map unNx assignments
         in
           Ex (Tree.ESEQ(seq ass_stms, unEx body))
         end

    fun trSeq [] = trUnit
      | trSeq exps = 
        Ex (Tree.ESEQ(seq (List.take ((map unNx exps), (List.length exps) - 1)), 
            unEx (List.last exps)))

    (* no label? *)
    (* fun trFunDec (bodyExp, label) = 
      Ex (Tree.ESEQ(seq([unNx bodyExp]), 
          unEx bodyExp)) *)

    fun procEntryExit {level, body} = 
      let
        val proc_frame = 
          case level of
             Top => MipsFrame.newFrame{name=Temp.newlabel(), formals=[]}
           | Lev ({parent, frame}, _) => frame
        (* val _ =
          case body of
            Nx _ => print "Nx\n"
          | Ex _ => print "Ex\n"
          | _ => print "neither\n" *)
        val proc_body = MipsFrame.procEntryExit1(proc_frame, Tree.MOVE(Tree.TEMP MipsFrame.rv, unEx(body)))
      in
        frags := (MipsFrame.PROC {body=proc_body, frame=proc_frame} :: (!frags))
      end

    fun getResult () = !frags

end