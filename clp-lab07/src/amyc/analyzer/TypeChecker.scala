package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        
        case Equals(lhs, rhs) =>
          val typeVar = TypeVariable.fresh()
          genConstraints(lhs, typeVar) ++ genConstraints(rhs, typeVar) ++ topLevelConstraint(BooleanType)
        
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            pat match{
              case WildcardPattern() => (Nil, Map())
              case IdPattern(name) => (Nil, Map(name -> scrutExpected))
              case LiteralPattern(lit) => (genConstraints(lit, scrutExpected), Map())
              case CaseClassPattern(constr, args) =>
                val sConstr = table.getConstructor(constr)
                if(sConstr.isDefined){
                  val handledPattern = args.zip(sConstr.get.argTypes).map { case (exprIn, exprType) => handlePattern(exprIn, exprType) }
                  val init = genConstraints(Variable(constr), scrutExpected)(env + (constr -> sConstr.get.retType))
                  handledPattern.foldLeft(init, Map[Identifier, Type]()) {
                    (acc, elem) => {
                      (acc._1 ++ elem._1, acc._2 ++ elem._2)
                    }
                  }
                }
                else
                  fatal("Class not found", pat)
            }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, scrutExpected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))


        //Variables
        case Variable(name) => topLevelConstraint(env(name))

        //Literals
        case BooleanLiteral(_) => topLevelConstraint(BooleanType)
        case StringLiteral(_)  => topLevelConstraint(StringType)
        case UnitLiteral() => topLevelConstraint(UnitType)

        // Binary operators
        case Plus(lhs, rhs) => genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Minus(lhs, rhs) => genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Times(lhs, rhs) => genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Div(lhs, rhs) => genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case Mod(lhs, rhs) => genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(IntType)
        case LessThan(lhs, rhs) => genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)
        case LessEquals(lhs, rhs) => genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType) ++ topLevelConstraint(BooleanType)
        case And(lhs, rhs) => genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)
        case Or(lhs, rhs) => genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType) ++ topLevelConstraint(BooleanType)
        case Concat(lhs, rhs) => genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType) ++ topLevelConstraint(StringType)

        // Unary operators
        case Not(value) => genConstraints(value, BooleanType) ++ topLevelConstraint(BooleanType)
        case Neg(value) => genConstraints(value, IntType) ++ topLevelConstraint(IntType)

        // Function/ type constructor call
        case Call(qname, args) =>
          val constr = table.getFunction(qname)
          val sig = if (constr.isDefined) constr.get else table.getConstructor(qname).get
          val constraints = args.zip(sig.argTypes).map { case (exprIn, exprType) => genConstraints(exprIn, exprType)}
          constraints.reduce(_ ++ _) ++ topLevelConstraint(sig.retType)

        // The ; operator
        case Sequence(e1, e2) => genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, expected)

        // Local variable definition
        case Let(df, value, body) => genConstraints(value, df.tt.tpe) ++ genConstraints(body, expected) (env + (df.name -> df.tt.tpe))

        // If-then-else
        case Ite(cond, thenn, elze) => genConstraints(cond, BooleanType) ++ genConstraints(thenn, expected) ++ genConstraints(elze, expected)

        // Represents a computational error; prints its message, then exits
        case Error(msg) => genConstraints(msg, StringType)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          expected match{
            case s1: TypeVariable => solveConstraints(subst_*(more, s1.id, found))
            case _ =>
              if(found != expected)
                fatal("Type  not found.", pos)
              else
                solveConstraints(more)
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body, _) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
