package amyc
package analyzer

import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach { case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }

    modNames.keys.toList foreach table.addModule


    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    p.modules.foreach(_.defs.groupBy(_.name)
                            .foreach{ case (name, defs) => if (defs.size > 1) fatal(s"Two modules named $name in program", defs.head.position)})

    // Step 3: Discover types and add them to symbol table
    p.modules.foreach{module => module.defs.foreach{ case N.AbstractClassDef(name) => table.addType(module.name, name)
                                                     case _ => Nil}}

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach{module => module.defs.foreach{ case caseClassDef @ N.CaseClassDef(name, fields, parent) =>
                                                       val toSymbolic = fields.map(field => transformType(field, module.name))
                                                       val parentId = table.getType(module.name, parent).getOrElse(fatal(s"Could not find $parent", caseClassDef))
                                                       table.addConstructor(module.name, name, toSymbolic, parentId)
                                                     case _ => Nil}}

    // Step 5: Discover functions signatures, add them to table
    p.modules.foreach{module => module.defs.foreach{ case N.FunDef(name, params, retType, _, isInlined) =>
                                                       val paramsToSymbolic = params.map(param => transformType(param.tt, module.name))
                                                       val retToSymbolic = transformType(retType, module.name)
                                                       table.addFunction(module.name, name, paramsToSymbolic, retToSymbolic, isInlined)
                                                     case _ => Nil}}

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    
    // This part is split into three transform functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case abstractClassDef @ N.AbstractClassDef(name) =>
        val nameS = table.getType(module, name).getOrElse(fatal(s"Could not find $name", abstractClassDef))
        S.AbstractClassDef(nameS)
      case caseClassDef @ N.CaseClassDef(name, fields, parent) =>
        val (id, _) = table.getConstructor(module, name).getOrElse(fatal(s"Could not find $name", caseClassDef))
        val sFields = fields.map(field => S.TypeTree(transformType(field, module)))
        val sParent = table.getType(module, parent).getOrElse(fatal(s"Could not find $name", caseClassDef))
        S.CaseClassDef(id, sFields, sParent)
      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }}.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body, isInlined) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (nameIn, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $nameIn in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(nameIn, tt), tpe) =>
        val s = Identifier.fresh(nameIn)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map())),
        isInlined
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match{
              case wildPat @ N.WildcardPattern() => (S.WildcardPattern().setPos(wildPat), List())
              case idPat @ N.IdPattern(name) =>
                val constSig = table.getConstructor(module, name)
                if(locals.get(name).isDefined)
                  fatal(s"Identifier $name already defined", pat)
                else if (constSig.isDefined && constSig.get._2.argTypes.isEmpty)
                  warning(s"'$name' was called. Did you mean '$name()'?", pat)
                val id = Identifier.fresh(name)
                (S.IdPattern(id).setPos(idPat), (name, id) :: List())
              case litPat @ N.LiteralPattern(lit) => lit match {
                case N.IntLiteral(value) => (S.LiteralPattern(S.IntLiteral(value)).setPos(litPat), List())
                case N.BooleanLiteral(value) => (S.LiteralPattern(S.BooleanLiteral(value)).setPos(litPat), List())
                case N.StringLiteral(value)  => (S.LiteralPattern(S.StringLiteral(value)).setPos(litPat), List())
                case N.UnitLiteral() => (S.LiteralPattern(S.UnitLiteral()).setPos(litPat), List())
              }
              case caseClassPat @ N.CaseClassPattern(constr, args) =>
                val sConstr = table.getConstructor(constr.module.getOrElse(module), constr.name).getOrElse(fatal("Cannot resolve " + constr.name + ".", caseClassPat))
                if(sConstr._2.argTypes.size != args.size)
                  fatal(s"Number of arguments for case class ${constr.name} does not match.", caseClassPat)
                val sArgs = args.map(transformPattern)
                sArgs.flatMap(_._2).groupBy(_._1).foreach{ case (name, ids) => if(ids.size > 1) fatal(s"Two modules named $name in program", caseClassPat)}
                (S.CaseClassPattern(sConstr._1, sArgs.map(_._1)).setPos(caseClassPat), sArgs.flatMap(_._2))
            }
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            val sRhs = transformExpr(rhs)(module, (params, locals ++ moreLocals))
            S.MatchCase(newPat, sRhs)
          }

          S.Match(transformExpr(scrut), cases.map(transformCase))

        //Variables
        case variable @ N.Variable(name) => params.get(name) match{
          case Some(value) => S.Variable(value)
          case None => locals.get(name) match{
            case Some(value) => S.Variable(value)
            case None => fatal(s"Variable  $name in module $module is unknown", variable)
          }
        }

        //Literals
        case N.IntLiteral(value) => S.IntLiteral(value)
        case N.BooleanLiteral(value) => S.BooleanLiteral(value)
        case N.StringLiteral(value)  => S.StringLiteral(value)
        case N.UnitLiteral() => S.UnitLiteral()

        // Binary operators
        case N.Plus(lhs, rhs) => S.Plus(transformExpr(lhs), transformExpr(rhs))
        case N.Minus(lhs, rhs) => S.Minus(transformExpr(lhs), transformExpr(rhs))
        case N.Times(lhs, rhs) => S.Times(transformExpr(lhs), transformExpr(rhs))
        case N.Div(lhs, rhs) => S.Div(transformExpr(lhs), transformExpr(rhs))
        case N.Mod(lhs, rhs) => S.Mod(transformExpr(lhs), transformExpr(rhs))
        case N.LessThan(lhs, rhs) => S.LessThan(transformExpr(lhs), transformExpr(rhs))
        case N.LessEquals(lhs, rhs) => S.LessEquals(transformExpr(lhs), transformExpr(rhs))
        case N.And(lhs, rhs) => S.And(transformExpr(lhs), transformExpr(rhs))
        case N.Or(lhs, rhs) => S.Or(transformExpr(lhs), transformExpr(rhs))
        case N.Equals(lhs, rhs) => S.Equals(transformExpr(lhs), transformExpr(rhs))
        case N.Concat(lhs, rhs) => S.Concat(transformExpr(lhs), transformExpr(rhs))

        // Unary operators
        case N.Not(e) => S.Not(transformExpr(e))
        case N.Neg(e) => S.Neg(transformExpr(e))

        // Function/ type constructor call
        case call @ N.Call(qname, args) =>
          table.getFunction(qname.module.getOrElse(module), qname.name) match{
            case Some((id, paramsFunc)) =>
              if (paramsFunc.argTypes.size != args.size) {
                fatal(s"Arguments number mismatch", call)
              }
              S.Call(id, args.map(transformExpr))
            case None =>
              table.getConstructor(qname.module.getOrElse(module), qname.name) match {
                case Some((id, paramsIn)) =>
                  if (paramsIn.argTypes.size != args.size) {
                    fatal(s"Arguments number mismatch", call)
                  }
                  S.Call(id, args map transformExpr)
                case None => fatal("Cannot resolve " + qname.name + ".", call)
              }
          }
        // The ; operator
        case N.Sequence(e1, e2) => S.Sequence(transformExpr(e1), transformExpr(e2))
        // Local variable definition
        case let @ N.Let(df, value, body) =>
          if (locals.get(df.name).isDefined)
            fatal("Two variables have the same name " + df.name , df)
          else if (params.contains(df.name))
            warning("Suspicious shadowing detected for variable " + df.name, df)
          val id = Identifier.fresh(df.name)
          val sDf = S.ParamDef(id, S.TypeTree(transformType(df.tt, module)).setPos(let))
          S.Let(sDf, transformExpr(value), transformExpr(body)(module, (params, locals + (df.name -> id))))
        // If-then-else
        case N.Ite(cond, thenn, elze) => S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
        // Represents a computational error; prints its message, then exits
        case N.Error(msg) => S.Error(transformExpr(msg))
      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
