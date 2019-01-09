package amyc
package codegen

import analyzer._
import ast.{Identifier, SymbolicTreeModule}
import ast.SymbolicTreeModule.{And => AmyAnd, Call => AmyCall, Div => AmyDiv, Or => AmyOr, _}
import utils.{Context, Pipeline}
import wasm._
import Instructions._
import Utils._

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[(Program, SymbolTable), Module] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v

    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect { case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) =>
        cgFunction(fd, name, false) :: getLocalFun(fd.localDefs, name)
      }.flatten :::
        // Generate code for the "main" function, which contains the module expression
        optExpr.toList.map { expr =>
          val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), List(), expr, isInlined = false, isLocal = false)
          cgFunction(mainFd, name, true)
        }
    }

    def getLocalFun(fd: List[FunDef], name: Identifier): List[Function] = {
      fd match {
        case head :: tail => cgFunction(head, name, false) :: getLocalFun(head.localDefs, name) ::: getLocalFun(tail, name)
        case head :: Nil => cgFunction(head, name, false) :: getLocalFun(head.localDefs, name)
        case Nil => List()
      }
    }


    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the wasm function name from a combination of
      // module and function name, since we put everything in the same wasm module.
      val name = fullName(owner, fd.name)
      Function(name, fd.params.size, isMain){ lh =>
        val locals = fd.paramNames.zipWithIndex.toMap
        val body = cgExpr(fd.body)(locals, lh)
        if (isMain) {
          body <:> Drop // Main functions do not return a value,
          // so we need to drop the value generated by their body
        } else {
          body
        }
      }
    }

    // Generate code for an expression expr.
    // Additional arguments are a mapping from identifiers (parameters and variables) to
    // their index in the wasm local variables, and a LocalsHandler which will generate
    // fresh local slots as required.
    def cgExpr(expr: Expr)(implicit locals: Map[Identifier, Int], lh: LocalsHandler): Code = {
      expr match{
        case Match(scrut, cases) => {
          def matchAndBind(idx: Int, pat: SymbolicTreeModule.Pattern): (Code, Map[Identifier, Int]) = {
            pat match{
              case WildcardPattern() => (Const(1), locals)
              case IdPattern(name) =>
                val newFreshLocal = lh.getFreshLocal()
                (GetLocal(idx) <:> SetLocal(newFreshLocal) <:> Const(1), locals + (name -> newFreshLocal))
              case LiteralPattern(lit) => (GetLocal(idx) <:> cgExpr(lit) <:> Eq, locals)
              case CaseClassPattern(constr, args) =>
                val sConstr = table.getConstructor(constr).get
                val handledPattern = args.zipWithIndex.map{case (pattern, index) =>
                  val scrutFreshId = lh.getFreshLocal()
                  val (matchBindCode, newBindings) = matchAndBind(scrutFreshId, pattern)
                  val code = GetLocal(idx) <:>
                    adtField(index) <:>
                    Load <:>
                    SetLocal(scrutFreshId) <:>
                    matchBindCode
                  (code, newBindings)
                }

                val code = GetLocal(idx) <:>
                  Load <:>
                  Const(sConstr.index) <:>
                  Eq <:>
                  handledPattern.map(_._1) <:>
                  List.fill(args.size)(And)

                val newEnv = handledPattern.map(_._2).foldLeft(locals){case (acc, curr) => acc ++ curr}

                (code, newEnv)
            }
          }

          val freshId = lh.getFreshLocal()
          val casesCode = cases.map { caze =>
            val (code, newBindings) = matchAndBind(freshId, caze.pat)
            code <:> If_i32 <:> cgExpr(caze.expr)(newBindings, lh) <:> Else
          }

          cgExpr(scrut) <:>
            SetLocal(freshId) <:>
            casesCode <:>
            mkString("Error: no match found.") <:>
            Unreachable <:>
            List.fill(casesCode.size)(End)
        }

        //Variables
        case Variable(name) => GetLocal(locals(name))

        //Literals
        case IntLiteral(int) => Const(int)
        case BooleanLiteral(bool) => if (bool) Const(1) else Const(0)
        case StringLiteral(str)  => mkString(str)
        case UnitLiteral() => Const(0)

        // Binary operators
        case Plus(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Add
        case Minus(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Sub
        case Times(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Mul
        case AmyDiv(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Div
        case Mod(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Rem
        case LessThan(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Lt_s
        case LessEquals(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Le_s
        case Equals(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Eq
        case AmyAnd(lhs, rhs) => cgExpr(lhs) <:> If_i32 <:> cgExpr(rhs) <:> Else <:> Const(0) <:> End
        case AmyOr(lhs, rhs) => cgExpr(lhs) <:> If_i32 <:> Const(1) <:> Else <:> cgExpr(rhs) <:> End
        case Concat(lhs, rhs) => cgExpr(lhs) <:> cgExpr(rhs) <:> Call("String_concat")

        // Unary operators
        case Not(value) => cgExpr(value) <:> Eqz
        case Neg(value) => cgExpr(value) <:> Const(-1) <:> Mul

        // Function/ type constructor call
        case AmyCall(qname, args) =>
          val constr = table.getFunction(qname)
          constr match{
            case Some(funSig) => args.map(cgExpr) <:> Call(fullName(funSig.owner, qname))
            case None =>
              val constructor = table.getConstructor(qname).get
              val freshId = lh.getFreshLocal()

              GetGlobal(memoryBoundary) <:>
                SetLocal(freshId) <:>
                GetGlobal(memoryBoundary) <:>
                Const(constructor.index) <:>
                Store <:>
                GetGlobal(memoryBoundary) <:>
                adtField(args.size) <:>
                SetGlobal(memoryBoundary) <:>
                args.zipWithIndex.map { case (arg, index) =>
                  GetLocal(freshId) <:>
                    adtField(index) <:>
                    cgExpr(arg) <:>
                    Store
                } <:>
                GetLocal(freshId)

          }

        // The ; operator
        case Sequence(e1, e2) => cgExpr(e1) <:> Drop <:> cgExpr(e2)

        // Local variable definition
        case Let(df, value, body) =>
          val freshId = lh.getFreshLocal()
          cgExpr(value) <:> SetLocal(freshId) <:> cgExpr(body)(locals + (df.name -> freshId), lh)

        // If-then-else
        case Ite(cond, thenn, elze) => cgExpr(cond) <:> If_i32 <:> cgExpr(thenn) <:> Else <:> cgExpr(elze) <:> End

        // Represents a computational error; prints its message, then exits
        case Error(msg) => cgExpr(msg) <:> Call("Std_printString") <:> Unreachable
      }
    }

    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }
}