type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EsqExp of stm * exp

val prog =
    CompoundStm (AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
                  CompoundStm (AssignStm ("b", EsqExp(PrintStm[IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
                                                       OpExp(NumExp 10, Times, IdExp "a"))),
                                PrintStm[IdExp "b"]))

fun maxargs (s:stm) : int =
    case s of
        CompoundStm (s1, s2) => Int.max(maxargs(s1),
                                        maxargs(s2))
      | AssignStm (_, e)     => maxargsexp(e)
      | PrintStm es          => Int.max(List.length es,
                                        maxargsexplist(es))
and maxargsexp (e:exp) : int =
    case e of
        OpExp (e1, _, e2) => Int.max(maxargsexp e1,
                                     maxargsexp e2)
      | EsqExp (s, e3)    => Int.max(maxargs s,
                                     maxargsexp e3)
      | _                 => 0
and maxargsexplist (es : exp list) =
    case es of
        nil         => 0
      | (e :: rest) => Int.max(maxargsexp e,
                               maxargsexplist rest)

val printStmInProg = maxargs(prog)

fun fst (a, b) = a;

type table = (id * int) list

fun lookup(t:table, i:id) : int =
    case t of
        nil => 0
      | ((j,v) :: rest) => if i = j then v else lookup(rest, i)

fun update (i:id, v:int, t:table) : table = (i,v) :: t

fun interp (s:stm) : table =
    let val emptyTable : table = []

        fun interpStm (s:stm, t:table) : table =
            case s of
                CompoundStm (s1, s2) => interpStm(s2, interpStm(s1, t))
              | AssignStm (i, e)     => let val (v, t') = interpExp(e, t)
                                        in
                                            update(i, v, t')
                                        end
              | PrintStm (e :: es)   => let val (v, t') = interpExp(e, t)
                                            val vals    = List.map (fn x => fst (interpExp(x, t'))) (e :: es);
                                        in
                                            List.map (fn x => print("Printing: " ^ Int.toString x ^ "\n")) vals;
                                            t'
                                        end
              | PrintStm nil         => t

        and interpExp (e:exp, t:table) : (int * table) =
            case e of
                IdExp i               => (lookup(t,i), t)
              | NumExp i              => (i, t)
              | OpExp (e1, binOp, e2) => let val (a, t1) = interpExp(e1, t)
                                             val (b, t2) = interpExp(e2, t)
                                         in case binOp of
                                                Plus  => (a + b, t)
                                              | Minus => (a - b, t)
                                              | Times => (a * b, t)
                                              | Div   => (a div b, t)
                                         end
              | EsqExp (s, e1)        => interpExp(e1, interpStm(s,t))
    in
        interpStm(s, emptyTable)
    end

val result = interp(prog)
