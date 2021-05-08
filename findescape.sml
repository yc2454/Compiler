(* structure Absyn = Absyn *)

structure FindEscape: sig val findEscape: Absyn.exp -> unit end =
struct

    type depth = int

    type escEnv = (depth * bool ref) Symbol.table

    fun traverseVar((env : escEnv), (d : depth), Absyn.SimpleVar(sym, pos)) = 
      let
        val escape = Symbol.look (env, sym)
      in
        case escape of
           NONE => print "undefined variable; unable to find escape\n"
         | SOME (dep, r) => if dep < d then r := true else ()
      end
      | traverseVar((env : escEnv), (d : depth), Absyn.FieldVar(var, symbol, _)) = traverseVar (env, d, var)
      | traverseVar((env : escEnv), (d : depth), Absyn.SubscriptVar(var, exp, _)) = traverseVar (env, d, var)
      
    and traverseExp((env : escEnv), (d : depth), s : Absyn.exp) : unit = 
      let
        fun traexp (Absyn.OpExp {left, oper, right, pos}) = (traexp left; traexp right)
          | traexp (Absyn.RecordExp {fields, typ, pos}) = 
            let
              fun trafields ((_, exp, _) :: t) = (traexp exp; trafields t)
                | trafields [] = ()
            in
              trafields fields
            end
          | traexp (Absyn.CallExp {func, args, pos}) = app traexp args
          | traexp (Absyn.IfExp {test, then', else', pos}) = 
            (case else' of
               NONE => (traexp test; traexp then')
             | SOME else'' => (traexp test; traexp then'; traexp else''))
          | traexp (Absyn.ArrayExp {typ, size, init, pos}) = (traexp size; traexp init)
          | traexp (Absyn.SeqExp expPosList) = 
            let
              val expList = List.map (fn (exp, _) => exp) expPosList
            in
              app traexp expList
            end
          | traexp (Absyn.WhileExp {test, body, pos}) = (traexp test; traexp body)
          | traexp (Absyn.NilExp) = ()
          | traexp (Absyn.IntExp i) = ()
          | traexp (Absyn.StringExp (s, p)) = ()
          | traexp (Absyn.AssignExp {var, exp, pos}) = (traverseVar (env, d, var); traexp exp)
          | traexp (Absyn.LetExp {decs, body, pos}) = 
            let
              val new_env = traverseDecs (env, d, decs)
            in
              traverseExp (new_env, d, body)
            end
          | traexp (Absyn.BreakExp(pos)) = ()
          | traexp (Absyn.ForExp {var, escape, lo, hi, body, pos}) = 
            let
              val limit = Symbol.symbol "limit"
              val for_decs = [Absyn.VarDec{name=var, escape=ref true,
                              typ=NONE, init=lo, pos=pos}, 
                              Absyn.VarDec{name=limit, escape=ref true,
                              typ=NONE, init=hi, pos=pos}]
              val for_test = Absyn.OpExp{left=Absyn.VarExp(Absyn.SimpleVar(var, pos)), oper=Absyn.LeOp, 
                             right=Absyn.VarExp(Absyn.SimpleVar(limit, pos)), pos=pos}
              val for_body = Absyn.SeqExp [(body, pos), 
                            (Absyn.AssignExp{var=Absyn.SimpleVar(var, pos), 
                              exp=Absyn.OpExp{left=Absyn.VarExp(Absyn.SimpleVar(var, pos)), oper=Absyn.PlusOp, 
                              right=Absyn.IntExp(1), pos=pos}, pos=pos}, pos)]
              val let_while = Absyn.LetExp {decs=for_decs, body=for_body, pos=pos}
            in
              (traexp lo; traexp hi; traexp let_while)
            end
          | traexp (Absyn.VarExp(var)) = traverseVar (env, d, var)
      in
        traexp s
      end

    and traverseDecs(env, d, s : Absyn.dec list) : escEnv = 
      let
        fun tradec (eenv, d, Absyn.VarDec{name, escape, typ, init, pos}) = 
          (escape := false; Symbol.enter (eenv, name, (d, escape)))
          | tradec (eenv, d, Absyn.FunctionDec(funDecList)) = 
            let
                fun traparams (eenv, ({name, escape, typ, pos} :: tail)) = 
                  traparams ((escape := false; Symbol.enter (eenv, name, (d+1, escape))), tail)
                | traparams (eenv, []) = eenv
            in
                (case funDecList of
                {name=name, params=params, result=rt, body, pos} :: fdList => 
                (let
                   val fun_env = traparams (eenv, params)
                 in
                   (traverseExp (fun_env, d+1, body); 
                    tradec (env, d, Absyn.FunctionDec(fdList)))
                 end)
            | [] => eenv)
            end
          | tradec (eenv, d, Absyn.TypeDec(tyDecList)) = eenv
      in
        case s of
           dec :: decs => traverseDecs (tradec (env, d, dec), d, decs)
         | [] => env
      end

    fun findEscape(prog: Absyn.exp) : unit = traverseExp (Symbol.empty, 0, prog)

end