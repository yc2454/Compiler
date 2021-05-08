structure R = Translate

signature ENV = sig
    type access
    (* type ty *)
    datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                      | FunEntry of {level: Translate.level, 
                                     label: Temp.label,
                                     formals: Types.ty list, 
                                     result: Types.ty}
    val base_tenv : Types.ty Symbol.table (* predefined types*)
    val base_venv : enventry Symbol.table (* predefinedfunctions*) 
end

structure Env :> ENV =
struct
  fun funList2Table entryL nameL = case (entryL, nameL) of
     ([], []) => Symbol.empty
   | (e::ee, n::nn) => Symbol.enter((funList2Table ee nn), (Symbol.symbol n), e)
   | _ => Symbol.empty

  type access = unit
  (* type ty = Types.ty *)

  datatype enventry = VarEntry of {access: Translate.access, ty: Types.ty}
                    | FunEntry of {level: Translate.level, 
                                   label: Temp.label,
                                   formals: Types.ty list, 
                                   result: Types.ty}

  val printEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_print", 
                              formals=[Types.STRING], result=Types.UNIT})
  val flushEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_flush",
                              formals=[], result=Types.UNIT})
  val getcharEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_getchar", 
                                formals=[], result=Types.STRING})
  val ordEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_ord",
                            formals=[Types.STRING], result=Types.INT})
  val chrEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_chr",
                            formals=[Types.INT], result=Types.STRING})
  val sizeEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_size",
                              formals=[Types.STRING], result=Types.INT})
  val subtringEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_substring",
                                formals=[Types.STRING, Types.INT, Types.INT], result=Types.STRING})
  val concatEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_concat",
                                formals=[Types.STRING, Types.STRING], result=Types.STRING})
  val notEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_not",
                            formals=[Types.INT], result=Types.INT})
  val exitEntry = FunEntry({level=R.outermost, label=Temp.namedlabel "tig_exit",
                              formals=[Types.INT], result=Types.UNIT})

  val preFuncs = [printEntry, flushEntry, getcharEntry, ordEntry, chrEntry, 
                    sizeEntry, subtringEntry, concatEntry, notEntry, exitEntry]
  val preFuncNames = ["print", "flush", "getchar", "ord", "chr", "size", 
                        "substring", "not", "exit"]
  val preFuncTable = funList2Table preFuncs preFuncNames

  val base_venv = preFuncTable
  val base_tenv = Symbol.enter(Symbol.enter(Symbol.empty, (Symbol.symbol "int"), Types.INT), (Symbol.symbol "string"), Types.STRING)

end