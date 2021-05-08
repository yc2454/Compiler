structure A = Absyn
structure T = Types
structure E = ErrorMsg
structure Tr = Translate

signature SEMANT =
sig
    type venv
    type tenv
    type expty
    val transProg : A.exp -> Tr.frag list
    (* val transExp: venv * tenv * A.exp -> expty
    val transDec: venv * tenv * A.dec -> {venv: venv, tenv: tenv} 
    val transTy: tenv * A.ty -> T.ty *)
end

structure Semant :> SEMANT =
struct

  type venv = Env.enventry Symbol.table 
  type tenv = T.ty Symbol.table
  type expty = {exp: Tr.exp, ty: T.ty}

  fun printTy ty = 
    case ty of
       T.INT => "int"
     | T.STRING => "string"
     | T.RECORD ([], _) => "RCD END"
     | T.RECORD (((sym, fty)::tail), _) => "(" ^ (Symbol.name sym) ^ ", " ^ (printTy fty) ^ "), " ^ (printTy (T.RECORD (tail, ref ())))
     | T.ARRAY (ty, _) => "array: " ^ (printTy ty)
     | T.NIL => "nil"
     | T.NAME (sym, _) => "name " ^ (Symbol.name sym)
     | T.UNIT => "unit"

  fun transExp (venv, tenv, level, break) = 
    let
      fun trexp (A.OpExp {left, oper=A.GtOp, right, pos}) = 
            let
              val {exp=l, ty} = trexp left
              val {exp=r, ty} = trexp right
              val genstm = Tr.trCompare l r A.GtOp
            in
              {exp=genstm, ty=checkCompare(left, right, pos)}
            end
        | trexp (A.OpExp {left, oper=A.GeOp, right, pos}) = 
            let
              val {exp=l, ty} = trexp left
              val {exp=r, ty} = trexp right
              val genstm = Tr.trCompare l r A.GeOp
            in
              {exp=genstm, ty=checkCompare(left, right, pos)}
            end
        | trexp (A.OpExp {left, oper=A.LtOp, right, pos}) = 
            let
              val {exp=l, ty} = trexp left
              val {exp=r, ty} = trexp right
              val genstm = Tr.trCompare l r A.LtOp
            in
              {exp=genstm, ty=checkCompare(left, right, pos)}
            end
        | trexp (A.OpExp {left, oper=A.LeOp, right, pos}) = 
            let
              val {exp=l, ty} = trexp left
              val {exp=r, ty} = trexp right
              val genstm = Tr.trCompare l r A.LeOp
            in
              {exp=genstm, ty=checkCompare(left, right, pos)}
            end
        | trexp (A.OpExp {left, oper=A.EqOp, right, pos}) =
            let
              val {exp=l, ty=lTy} = trexp left
              val {exp=r, ty=rTy} = trexp right
              val resTy = checkEquality(left, right, pos)
              val genstm = 
                if lTy = T.STRING then Tr.trCompareString l r A.EqOp else
                Tr.trCompare l r A.EqOp
            in
              {exp=genstm, ty=resTy}
            end
        | trexp (A.OpExp {left, oper=A.NeqOp, right, pos}) =
            let
              val {exp=l, ty=lTy} = trexp left
              val {exp=r, ty=rTy} = trexp right
              val resTy = checkEquality(left, right, pos)
              val genstm = 
                if lTy = T.STRING then Tr.trCompareString l r A.NeqOp else
                Tr.trCompare l r A.NeqOp
            in
              {exp=genstm, ty=resTy}
            end
        | trexp (A.OpExp {left, oper, right, pos}) = 
            let
              val {exp=l, ty} = trexp left
              val {exp=r, ty} = trexp right
              val res = Tr.trArith(l)(r)(oper)
            in
              {exp=res, ty=checkArith(left, right, pos)}
            end
        | trexp (A.RecordExp {fields, typ, pos}) = 
            let
                val recordTypeOp = Symbol.look(tenv, typ)
                val inits_tr = List.map (fn (sym, exp, pos) => trexp exp) fields
                val inits = List.map (fn {exp, ty} => exp) inits_tr
                val rcdExp = Tr.trMakeRec inits
            in
                (case recordTypeOp of
                  NONE => (E.error pos ("undefined record type: " ^ (Symbol.name typ)); 
                            {exp=rcdExp, ty=T.RECORD([], ref ())})
                (* | SOME (T.RECORD(symTyList, uniq)) => 
                    (checkRecord (fields, symTyList, pos); 
                    {exp=(), ty=T.RECORD(symTyList, uniq)}) *)
                | SOME (t) => 
                  (case (actual_ty ([], t, pos)) of
                     (T.RECORD(symTyList, uniq)) => 
                      (checkRecord (fields, symTyList, pos); 
                      {exp=rcdExp, ty=T.RECORD(symTyList, uniq)})
                   | _ => (E.error pos "record type required"; 
                            {exp=rcdExp, ty=T.RECORD([], ref ())})))
            end
        | trexp (A.CallExp {func, args, pos}) = 
            let
                val funEntryOp = Symbol.look(venv, func)
                fun checkArgs (args, (formals : T.ty list), pos) = 
                case (args, formals) of
                    ([], []) => ()
                    | (e::ee, fTy::ffTy) => 
                    (checkExpType ((trexp e), fTy, pos);
                     checkArgs (ee, ffTy, pos))
                    | _ => E.error pos "wrong argument number";
                val argsExpTy = List.map trexp args
                val argsExp = List.map (fn {exp, ty} => exp) argsExpTy
            in
                (case funEntryOp of
                  NONE => (E.error pos ("undefined function: " ^ (Symbol.name func)); 
                    {exp=Tr.trError, ty=T.INT})
                | SOME (Env.FunEntry{level=lDef, label, formals, result}) => 
                  ((checkArgs (args, formals, pos));
                  (case result of
                     T.UNIT => {exp=Tr.trCall (label, argsExp, level, lDef, true), ty=result}
                   | _ => {exp=Tr.trCall (label, argsExp, level, lDef, false), ty=result}))
                | _ => (E.error pos ("function type required"); 
                    {exp=Tr.trError, ty=T.INT}))
            end
        | trexp (A.IfExp {test, then', else', pos}) = 
            let
                val {exp=thenExp, ty=thenTy} = (trexp then')
                val {exp=condExp, ty} = trexp test
            in
                ((checkInt (trexp test, pos));
                (case else' of
                  NONE => (if thenTy = T.UNIT then {exp=Tr.trIf(condExp, thenExp), ty=thenTy} else 
                    (E.error pos "if-then must produce no value"; {exp=Tr.trError, ty=T.UNIT}))
                | SOME (expElse) => 
                  let
                    val {exp=elseExp, ty} = (trexp expElse)
                  in
                    ((checkExpType (trexp expElse, thenTy, pos));
                                    {exp=Tr.trIfElse(condExp, thenExp, elseExp), ty=thenTy})
                  end))
            end
        | trexp (A.ArrayExp {typ, size, init, pos}) = 
            let
                val arrayTyOp = Symbol.look (tenv, typ)
                val {exp=sizeExp, ty=sizeTy} = trexp size
                val {exp=initExp, ty=initTy} = trexp init
                val arrExp = Tr.trMakeArray(sizeExp)(initExp)
            in
                case arrayTyOp of
                  NONE => ((E.error pos ("undefined array type: " ^ (Symbol.name typ))); 
                            {exp=arrExp, ty=T.ARRAY (T.INT, ref ())})
                | SOME t => 
                  (case (actual_ty ([], t, pos)) of
                     T.ARRAY(eltTy, uniq) => ((checkInt (trexp size, pos)); 
                                    (checkExpType (trexp init, eltTy, pos));
                                    {exp=arrExp, ty=T.ARRAY (eltTy, uniq)})
                   | _ => ((E.error pos ("array type required.")); 
                            {exp=arrExp, ty=T.ARRAY (T.INT, ref ())}))
            end
        | trexp (A.SeqExp expPosList) = 
            let
              val exps = map (fn (e, pos) => getExp (trexp e)) expPosList
              val last_ty = 
                if List.null expPosList then T.UNIT else 
                getTy (trexp (#1(List.last expPosList)))
            in
              {exp=Tr.trSeq exps, ty=last_ty}
            end
        | trexp (A.WhileExp {test, body, pos}) = 
          let
            val done = Temp.newlabel()
            val {exp=bodyExp, ty=bodyTy} = transExp(venv, tenv, level, SOME done) body
            (* val _ = (print ("body type is: " ^ (printTy bodyTy)); print " \n") *)
            val {exp=condExp, ty=condTy} = trexp test
            val whileExp = Tr.trWhile(condExp)(bodyExp)(done)
          in
            (if (checkSameType (condTy, T.INT, pos) andalso checkSameType(bodyTy, T.UNIT, pos)) 
             then {exp=whileExp, ty=T.UNIT}
             else if (not (checkSameType (condTy, T.INT, pos))) 
                  then (E.error pos "bad condition type"; {exp=whileExp, ty=T.UNIT})
                  else (E.error pos "body type not unit"; {exp=whileExp, ty=T.UNIT}))
          end
        | trexp (A.NilExp) = {exp=Tr.trError, ty=T.NIL}
        | trexp (A.IntExp i) = {exp=Tr.trInt i, ty=T.INT}
        | trexp (A.StringExp (s, p)) = 
          let
            val lab = Temp.newlabel()
          in
            {exp=Tr.trString lab s, ty=T.STRING}
          end
        | trexp (A.AssignExp {var, exp, pos}) = 
            let
                val {exp=varExp, ty=varTy} = trvar var
                val {exp=valExp, ty=valTy} = trexp exp
                val assExp = Tr.trAssign varExp valExp
            in
                (if checkSameType(varTy, valTy, pos) then {exp=assExp, ty=T.UNIT}
                 else (E.error pos "assigning wrong type of value to the variable"; 
                       {exp=assExp, ty=T.UNIT}))
            end
        | trexp (A.LetExp {decs, body, pos}) = 
            let
                val {venv=venv', tenv=tenv', exp=assExpList} = transDec(venv, tenv, decs, level, break)
                val {exp=bodyExp, ty=bodyTy} = transExp (venv', tenv', level, break) body
            in
                {exp=Tr.trLet (assExpList, bodyExp), ty=bodyTy}
            end
        | trexp (A.BreakExp(pos)) = 
          (case break of
             NONE => (E.error pos "Break outside loop"; {exp=Tr.trError, ty=T.UNIT})
           | SOME label => {exp=Tr.trBreak label, ty=T.UNIT})
        | trexp (A.ForExp {var, escape, lo, hi, body, pos}) = 
            let
              val {exp=loExp, ty=loTy} = trexp lo
              val acc = Tr.allocLocal(level)(!escape)
              val venvv = Symbol.enter (venv, var, Env.VarEntry{access=acc, ty=loTy})
              val {exp=hiExp, ty=hiTy} = trexp hi
              val done = Temp.newlabel()
              val {exp=bodyExp, ty=bodyTy} = transExp(venvv, tenv, level, SOME done) body
              val varExp = Tr.trSimpleVar(acc, level)
              val forExp = Tr.trFor(varExp, loExp, hiExp, bodyExp, done)
            in
               (checkInt (trexp hi, pos); checkInt (trexp lo, pos); checkAssign (var, body);
               (case bodyTy of
                  T.UNIT => {exp=forExp, ty=bodyTy}
                | _ => ((E.error pos "for loop body must produce no value"); 
                            {exp=forExp, ty=bodyTy})))
            end
        | trexp (A.VarExp(var)) = trvar var
      
      and trvar (A.SimpleVar (id, pos)) = 
            (case Symbol.look (venv, id) of
                  NONE => (E.error pos ("undefined variable " ^ (Symbol.name id));
                        {exp=Tr.trError, ty=T.INT})
                | SOME (Env.VarEntry{access=acc, ty=ty}) => 
                  {exp=Tr.trSimpleVar (acc, level), ty=(actual_ty ([], ty, pos))}
                | _ => (E.error pos ("variable required");
                        {exp=Tr.trError, ty=T.INT}))

        | trvar (A.FieldVar (v, id, pos)) = 
            let
                val {exp=rcdExp, ty=rcdTy} = trvar v
            in
                case rcdTy of
                  T.RECORD (symTyList, _) => 
                  let
                    val (ty, idx) = searchField(0, symTyList, id, pos)
                    val sel_field = Tr.trFieldSelect(rcdExp)(idx)
                  in
                    {exp=sel_field, ty=ty}
                  end
                | _ => (E.error pos ("record type required");
                        {exp=Tr.trError, ty=T.INT})
            end

        | trvar (A.SubscriptVar (v, exp, pos)) = 
            let
                val {exp=arrExp, ty=arrayTy} = trvar v
                val {exp=idxExp, ty=idxTy} = trexp exp
                val arr_elt = Tr.trArraySub(arrExp)(idxExp)
            in
                case arrayTy of
                    T.ARRAY (ty, _) => 
                    ((checkInt (trexp exp, pos));
                      {exp=arr_elt, ty=(actual_ty ([], ty, pos))})
                | _ => (E.error pos ("array type required");
                        {exp=arr_elt, ty=T.INT})
            end

      and searchField (idx, symTyList, id, pos) = 
        case symTyList of
           [] => (E.error pos "the required field does not exist"; 
                  (T.INT, idx))
         | (sym, ty)::tail => if (Symbol.name sym) = (Symbol.name id) then 
                              ((actual_ty ([], ty, pos)), idx)
                                else (searchField (idx + 1, tail, id, pos))

      and actual_ty (seen, ty, pos) = 
        case ty of
           T.NAME (sym, ref (SOME(T.NAME (symm, ref (NONE))))) =>
           if (List.all (fn x => x = symm) seen) then (E.error pos "circular naming"; T.INT)
           else (E.error pos "empty name"; T.INT)
         | T.NAME (sym, ref (SOME(T.NAME (symm, ref (SOME(t)))))) =>
           actual_ty (symm::seen, t, pos)
         | T.NAME (sym, ref (SOME(t))) => t
         | T.NAME (sym, ref (NONE)) => T.INT
         | _ => ty

      and checkInt ({exp, ty}, pos) = 
        case ty of
           T.INT => ()
         | _ => E.error pos "integer required"

      and checkUnit ({exp, ty}, pos) = 
        case ty of
           T.UNIT => ()
         | _ => E.error pos "expression required to produce no value"

      and checkString ({exp, ty}, pos) = 
        case ty of
           T.STRING => ()
         | _ => E.error pos "string required"
      
      and checkArith (left, right, pos) = 
        (checkInt (trexp left, pos); checkInt (trexp right, pos);
        T.INT)

      and checkCompare (left, right, pos) = 
        case (trexp left) of
              {exp, ty=T.STRING} => ((checkString (trexp right, pos)); T.INT)
            | {exp, ty=T.INT} => ((checkInt (trexp right, pos)); T.INT)
            | _ => (E.error pos "int or string required"; T.INT)
      
      and checkSameArray (left, right, pos) = 
        case (left, right) of
           (A.ArrayExp{typ=lty, size=ls, init=li, pos=lp}, 
           A.ArrayExp{typ=rty, size=rs, init=ri, pos=rp}) => 
            (checkSymbol (lty, rty, pos))
         | _ => E.error pos "array required"

      and checkSymbol (sym, targetSym, pos) = 
        case String.compare((Symbol.name sym), (Symbol.name targetSym)) of
           EQUAL => ()
         | _ => E.error pos "used wrong symbol in record"

      and checkSameType (ty, targetTy, pos) = 
        let
          val t = actual_ty ([], ty, pos)
          val tt = actual_ty ([], targetTy, pos)
          (* val pt = print ((printTy t) ^ "\n")
          val ptt = print ((printTy tt) ^ "\n") *)
        in
          case tt of
            T.INT => t = T.INT 
          | T.STRING => t = T.STRING 
          | T.UNIT => t = T.UNIT 
          | T.RECORD (_, uniqtt) => t = T.NIL orelse 
            (case t of
              T.RECORD (_, uniqt) => uniqt = uniqtt
            | _ => false)
          | T.NAME _ => checkSameType (t, tt, pos)
          | T.ARRAY (_, uniqtt) => 
            (case t of
                T.ARRAY (_, uniqt) => uniqt = uniqtt
              | _ => false)
          | T.NIL => t = T.NIL
        end
      
      and checkExpType ({exp, ty}, (targetTy : T.ty), pos) = 
        if checkSameType (ty, targetTy, pos) then () else 
            E.error pos ("wrong expression type. Should be " ^ 
                (printTy targetTy) ^ " but is " ^ (printTy ty))
      
      and checkRecord (exp, typ, pos) = 
        case (exp, typ) of
            ([], []) => ()
          | ((esymbol, eexp, epos)::ee, (tsymbol, tty)::tt) => 
            (checkSymbol (esymbol, tsymbol, epos); 
            checkExpType ((trexp eexp), tty, epos);
            checkRecord (ee, tt, pos))
          | _ => E.error pos "wrong record field number"

      and checkSameRecord (left, right, pos) = 
        case (left, right) of
           (A.RecordExp{fields=lfd, typ=lty, pos=lpos}, 
           A.RecordExp{fields=rfd, typ=rty, pos=rpos}) => 
            (checkSymbol (lty, rty, pos))
         | (_, A.NilExp) => ()
         | _ => E.error pos "record required"

      and checkEquality (left, right, pos) = 
        case (trexp left) of
            {exp, ty=T.STRING} => ((checkString (trexp right, pos)); T.INT)
          | {exp, ty=T.INT} => ((checkInt (trexp right, pos)); T.INT)
          | {exp, ty=T.RECORD _} => ((checkSameRecord (left, right, pos)); T.INT)
          | {exp, ty=T.ARRAY _} => ((checkSameArray (left, right, pos)); T.INT)
          | {exp, ty=T.NIL} => 
            (case (trexp right) of
               {exp, ty=T.RECORD _} => T.INT
             | _ => (E.error pos "checking equality between nil and non-record"; T.INT))
          | _ => (E.error pos "unfit type for equality checking"; T.INT)
      and checkAssign (iter, exp) = 
        case exp of
           A.AssignExp {var, exp, pos} => 
           (case var of
               A.SimpleVar (sym, pos) => 
               (if ((Symbol.name sym) = (Symbol.name iter)) then 
                E.error pos "iterator variable was assigned to"
                else ()))
         | A.SeqExp(expPosList) => 
            (case expPosList of
                [] => ()
              | (e, pos)::[] => checkAssign (iter, e)
              | (e, pos)::el => 
                (checkAssign (iter, e); checkAssign (iter, A.SeqExp (el))))
         | _ => ()
      and getExp {exp, ty} = exp
      and getTy {exp, ty} = ty
    in
      trexp
    end

and transDec (venv, tenv, (decs), level, break) = 
  let
    fun checkSameType (ty, targetTy, pos) = 
      let
        val t = actual_ty ([], ty, pos)
        val tt = actual_ty ([], targetTy, pos)
        (* val pt = print ((printTy t) ^ "\n")
        val ptt = print ((printTy tt) ^ "\n") *)
      in
        case tt of
          T.INT => t = T.INT 
        | T.STRING => t = T.STRING 
        | T.UNIT => t = T.UNIT 
        | T.RECORD (_, uniqtt) => t = T.NIL orelse 
          (case t of
             T.RECORD (_, uniqt) => uniqt = uniqtt
           | _ => false)
        | T.NAME _ => checkSameType (t, tt, pos)
        | T.ARRAY (_, uniqtt) => 
          (case t of
              T.ARRAY (_, uniqt) => uniqt = uniqtt
            | _ => false)
        | T.NIL => t = T.NIL
      end

    and actual_ty (seen, ty, pos) = 
      case ty of
          T.NAME (sym, ref (SOME(T.NAME (symm, ref (NONE))))) =>
          if (List.all (fn x => x = symm) seen) then (E.error pos "circular naming"; T.INT)
          else (E.error pos "empty name"; T.INT)
        | T.NAME (sym, ref (SOME(T.NAME (symm, ref (SOME(t)))))) =>
          actual_ty (symm::seen, t, pos)
        | T.NAME (sym, ref (SOME(t))) => t
        | T.NAME (sym, ref (NONE)) => T.INT
        | _ => ty

    fun trdec (venv, tenv, A.VarDec{name, escape, typ, init, pos}) = 
      let
        val {exp=initExp, ty=initTy} = transExp(venv, tenv, level, break) init
        val acc = Tr.allocLocal(level)(!escape)
      in
        (* see whether there is a typ given *)
        case typ of
           NONE => 
           (case initTy of
              T.NIL => (E.error pos "nil not constrained in record"; 
              {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry{access=acc, ty=initTy})})
            | _ => {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry{access=acc, ty=initTy})})
         | SOME (sym, pos) => 
            let
              val typOp = Symbol.look (tenv, sym)
            in
              (* see whether the typ has been defined *)
              case typOp of
                 NONE => (E.error pos ("undefined type: " ^ (Symbol.name sym)); 
                            {tenv=tenv, venv=venv})
               | SOME (typTy) => 
                 (* only update venv when init matches typ *)
                 (case (actual_ty ([], typTy, pos), actual_ty ([], initTy, pos)) of
                    (T.RECORD _, T.NIL) => {tenv=tenv, 
                        venv=Symbol.enter(venv, name, Env.VarEntry{access=acc, ty=typTy})}
                  | (_, T.NIL) => (E.error pos "nil not constrained in record"; 
                    {tenv=tenv, 
                      venv=Symbol.enter(venv, name, Env.VarEntry{access=acc, ty=typTy})})
                  | (typTy, initTy) => if checkSameType (typTy, initTy, pos) then 
                        {tenv=tenv, 
                        venv=(Symbol.enter(venv, name, Env.VarEntry{access=acc, ty=initTy}))} 
                        else (E.error pos ("type unmatch: typ: " ^ (printTy typTy) ^
                              " init: " ^ (printTy initTy)); 
                          {tenv=tenv, venv=venv}))
            end
      end
    | trdec (venv, tenv, A.TypeDec(tyDecList)) = 
      let
        fun enterHeaders (seen, tenv, tyDecList) = 
          case tyDecList of
             [] => tenv
           | {name=name, ty, pos} :: tdList => 
              if (List.all (fn x => (Symbol.name x) <> (Symbol.name name)) seen) then
                enterHeaders (name::seen, Symbol.enter (tenv, name, T.NAME(name, ref NONE)), tdList)
              else
                (E.error pos "two types with same name"; 
                enterHeaders (seen, Symbol.enter (tenv, name, T.NAME(name, ref NONE)), tdList))

        val tenv' = enterHeaders ([], tenv, tyDecList)
        val tenv'' =
          foldl (fn ({name, ty, pos}, ten) =>
            (case Symbol.look(ten,name) of
              SOME(T.NAME(sym, tef)) =>
                (tef := SOME(transTy(ten, ty)); ten) | _ => ten)) tenv' tyDecList

        fun checkCircle (seen, tyOp, pos) = 
          case tyOp of
            SOME (T.NAME (sym, ref (SOME(T.NAME (symm, ref (SOME(t))))))) =>
              if (List.all (fn x => (Symbol.name x) <> (Symbol.name symm)) seen) then 
                checkCircle (symm::seen, SOME (t), pos) else true
          | SOME (T.NAME (sym, ref (SOME(t)))) => false
          | SOME (T.NAME (sym, ref (NONE))) => false
          | NONE => false 
          | _ => false

        fun checkCircleList [] = ()
          | checkCircleList ({name, ty, pos=p}::tdList) = 
            if checkCircle ([], Symbol.look (tenv'', name), p) then 
              E.error p "circular naming" else checkCircleList tdList

      in
        ((checkCircleList tyDecList); {venv=venv, tenv=tenv''})
      end

    | trdec (venv, tenv, A.FunctionDec(funDecList)) = 
      let
        fun transParam {name, escape, typ, pos} = 
          case Symbol.look (tenv, typ) of
             NONE => (E.error pos "undefined type in function parameters"; 
                        {name=name, escape=escape, ty=T.INT})
           | SOME (t) => {name=name, escape=escape, ty=t}

        fun params2formals params = 
          case params of
             [] => []
           | {name, escape, typ=typ, pos=pos} :: tail=> 
             (case Symbol.look (tenv, typ) of
                NONE => (E.error pos "undefined type in parameters"; 
                        (T.INT :: params2formals tail))
              | SOME (t) => t :: (params2formals tail))

        fun enterHeaders (seen, venv, funDecList) = 
          case funDecList of
             [] => venv
           | {name=name, params=params, result=rt, body, pos} :: fdList => 
             let
               val func_label = Temp.newlabel()
               val fms = List.map (fn ({name, escape=esc, typ, pos}) => !esc) params
               val new_level = Tr.newLevel {parent=level, name=func_label, formals=fms}
             in
               if (List.all (fn x => (Symbol.name x) <> (Symbol.name name)) seen) then
                case rt of
                  NONE => 
                  enterHeaders (name::seen, Symbol.enter (venv, name,
                  Env.FunEntry({level=new_level, label=func_label, 
                    formals=(params2formals params), result=T.UNIT})), fdList)
                | SOME (sym, rtPos) => 
                  (case Symbol.look (tenv, sym) of
                      NONE => (E.error rtPos "function result type undefined"; 
                              enterHeaders (name::seen, venv, fdList))
                    | SOME (t) => enterHeaders (name::seen, Symbol.enter (venv, name, 
                                  Env.FunEntry{level=new_level, label=func_label, 
                                  formals=(params2formals params), result=t}), fdList))
              else
                (E.error pos "two functions with same name";
                (case rt of
                  NONE => 
                  enterHeaders (seen, Symbol.enter (venv, name,
                  Env.FunEntry({level=new_level, label=func_label, 
                  formals=(params2formals params), result=T.UNIT})), fdList)
                | SOME (sym, rtPos) => 
                  (case Symbol.look (tenv, sym) of
                      NONE => (E.error rtPos "function result type undefined"; 
                              enterHeaders (seen, venv, fdList))
                    | SOME (t) => enterHeaders (seen, Symbol.enter (venv, name, 
                                  Env.FunEntry{level=new_level, label=func_label, 
                                  formals=(params2formals params), result=t}), fdList))))
             end
        
        val venv' = enterHeaders ([], venv, funDecList)

        fun processBodies (venv, tenv, funDecList) = 
          case funDecList of
             [] => {venv=venv, tenv=tenv}
           | {name=name, params=params, result=rt, body=body, pos=pos} :: fdList => 
             (let
                val SOME (Env.FunEntry {level=cur_level, label=cur_label, formals=cur_formals, 
                    result=cur_results}) = Symbol.look (venv', name)
                val params' = map transParam params
                (* enter the parameters into venv *)
                val param_acc_assoc = ListPair.zip (params', Tr.formals cur_level)
                fun enterParam (({name, escape, ty}, acc), venv) = 
                  Symbol.enter (venv, name, Env.VarEntry{access=acc, ty=ty})
                val venv'' = foldl enterParam venv param_acc_assoc
                (* process body *)
                val {exp=bodyExp, ty=bodyTy} = transExp(venv'', tenv, cur_level, break) body
                (* val funExp = Tr.trFunDec(bodyExp, cur_label) *)
                
             in
              (* check that the result type matches the body type *)
               case rt of
                  NONE => (if checkSameType (bodyTy, T.UNIT, pos) then 
                  (Tr.procEntryExit {level=cur_level, body=bodyExp}; processBodies (venv, tenv, fdList))
                  else (E.error pos "procedure returns value"; 
                   Tr.procEntryExit {level=cur_level, body=bodyExp}; processBodies (venv, tenv, fdList)))
                | SOME (sym, rtPos) => 
                  (case Symbol.look (tenv, sym) of
                    NONE => (E.error rtPos "function result type undefined"; 
                              processBodies (venv, tenv, fdList))
                  | SOME (t) => 
                    if (checkSameType (bodyTy, t, pos)) then 
                      (Tr.procEntryExit {level=cur_level, body=bodyExp}; processBodies (venv, tenv, fdList)) 
                      else
                      (E.error rtPos "body type doesn't match declaration";
                      Tr.procEntryExit {level=cur_level, body=bodyExp};
                      processBodies (venv, tenv, fdList)))
             end)
      in
        processBodies (venv', tenv, funDecList)
      end

    fun getNewEnv (venv, tenv, []) = {venv=venv, tenv=tenv}
      | getNewEnv (venv, tenv, dec :: decList) =
        let
          val {venv=venv', tenv=tenv'} = trdec (venv, tenv, dec)
        in
          getNewEnv (venv', tenv', decList)
        end
    
    val {venv=new_venv, tenv=new_tenv} = getNewEnv (venv, tenv, decs)
      
    fun getVarInits (A.VarDec{name, escape, typ, init, pos} :: decList) = 
        let
          val {exp=initExp, ty=initTy} = 
            transExp(new_venv, new_tenv, level, break) init

          fun makeDecExp name =
            case Symbol.look (new_venv, name) of
                SOME (Env.VarEntry{access=acc, ty}) => 
                Tr.trAssign(Tr.trSimpleVar(acc, level))(initExp)
              | _ => (Tr.trError)
          
          val decExp = makeDecExp name
        in
          decExp :: (getVarInits decList)
        end
      | getVarInits (_ :: decList) = getVarInits decList
      | getVarInits [] = []
    
    val exps = getVarInits decs

  in
    {venv=new_venv, tenv=new_tenv, exp=exps}
  end

and transTy (tenv, ty) = 
  let
    fun trty (A.NameTy(sym, pos)) = 
      (case Symbol.look (tenv, sym) of
          NONE => (E.error pos "undefined name type"; T.INT)
        | SOME (t) => t)
      | trty (A.RecordTy(fldList)) = 
        let
          fun exp2ty fldList = 
            case fldList of
                [] => []
              | {name=name, escape, typ=typ, pos=pos} :: tail => 
                (case Symbol.look (tenv, typ) of
                    NONE => (E.error pos "undefined field type"; 
                            (name, T.INT) :: (exp2ty tail))
                  | SOME (t) => (name, t) :: (exp2ty tail))
          val fieldTy = exp2ty fldList
        in
          T.RECORD (fieldTy, ref ())
        end
      | trty (A.ArrayTy(sym, pos)) = 
        case Symbol.look (tenv, sym) of
            NONE => (E.error pos "undefined element type for array"; T.INT)
          | SOME (t) => T.ARRAY(t, ref ())
  in
      trty ty
  end

fun transProg prog = 
  let
    val _ = Tr.reset ()
    val main = Tr.newLevel{parent=Tr.outermost, 
        name=Temp.namedlabel "tig_main", formals=[]}
    val {exp, ty} = (FindEscape.findEscape prog; 
                     transExp (Env.base_venv, Env.base_tenv, main, NONE) prog)
  in
    (Tr.procEntryExit{level=main, body=exp};
     Tr.getResult())
  end

end
