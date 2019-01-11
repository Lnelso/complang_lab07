package amyc
package parsing

import amyc.utils._
import scala.collection.mutable.HashMap
import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._



// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  override def constructDef0(pTree: NodeOrLeaf[Token]): ClassOrFunDef = {
    pTree match {
      case Node('AbstractClassDef ::= _, List(Leaf(abs), _, name)) =>
        AbstractClassDef(constructName(name)._1).setPos(abs)
      case Node('CaseClassDef ::= _, List(Leaf(cse), _, name, _, params, _, _, parent)) =>
        CaseClassDef(
          constructName(name)._1,
          constructList(params, constructParam, hasComma = true).map(_.tt),
          constructName(parent)._1
        ).setPos(cse)
      case Node('FunDef ::= (INLINE() :: _), List(_, Leaf(df), name, _, params, _, _, retType, _, _, listFunDefLocal, body, _)) =>
        val constructedParams = constructList(params, constructParam, hasComma = true)
        val constructedName = constructName(name)._1
        val constrBody = constructExpr(body, cstFolding = true)
        innerBodyRecur += (constructedName -> innerBodyCalls.toList)
        innerBodyCalls.clear()

        val constrFunDefLoc = constructFunDefLocal(listFunDefLocal, cstFolding = true)

        val shouldInline = shouldInlineRecur(constructedName, "", 0, 6)

        innerBodyRecur.clear()

        val fd = FunDef(
                   constructedName,
                   constructedParams,
                   constructType(retType),
                   constrFunDefLoc,
                   constrBody,
                   isInlined = true,
                   isLocal = false
                 ).setPos(df)
        inlinedFunctions += (fd.name -> (fd, body))
        fd

      case Node('FunDef ::= _, List(Leaf(df), name, _, params, _, _, retType, _, _, listFunDefLocal, body, _)) =>

        val constructedParams = constructList(params, constructParam, hasComma = true)
        val constructedName = constructName(name)._1
        val constrBody = constructExpr(body)
        innerBodyRecur += (constructedName -> innerBodyCalls.toList)
        innerBodyCalls.clear()
        val constrFunDefLoc = constructFunDefLocal(listFunDefLocal)

        innerBodyRecur.clear()

        val fd = FunDef(
          constructedName,
          constructedParams,
          constructType(retType),
          constrFunDefLoc,
          constructExpr(body, false),
          isInlined = false,
          isLocal = false
        ).setPos(df)

        fd
    }
  }

  def shouldInlineRecur(start: Name, current: Name, actualDepth: Int, maxDepth: Int): Boolean = {
    if (actualDepth >= maxDepth) {
      true
    } else {
      val a = innerBodyRecur(start)
      a match {
        case head :: tail => if (current == start) false else a.map(e => shouldInlineRecur(start, e, actualDepth + 1, maxDepth)).reduce((e1, e2) => e1 && e2)
        case Nil => true
      }
    }
  }

  override def constructQname(ptree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    ptree match {
      case Node('QName ::= _, List(id, qnameInner)) =>
        qnameInner match {
          case Node('QNameInner ::= (DOT() :: _), List(_, id2)) =>
            val (module, pos) = constructName(id)
            val (name, _) = constructName(id2)
            (QualifiedName(Some(module), name), pos)
          case Node('QNameInner ::= _, _) =>
            val (name, pos) = constructName(id)
            (QualifiedName(None, name), pos)
        }
    }
  }

  def constructFunDefLocal(ptree: NodeOrLeaf[Token], cstFolding: Boolean = false): List[FunDef] ={
    ptree match {
      case Node('FunDefLocal ::= List('FunDef, 'FunDefLocal), List(localFun, moreLocalFun)) =>
        constructFunDef(localFun) :: constructFunDefLocal(moreLocalFun)

      case Node('FunDefLocal ::= _, List(localFun)) =>
        List(constructFunDef(localFun))

      case Node('FunDefLocal ::= _, List()) => List()
    }
  }

  def constructFunDef(ptree: NodeOrLeaf[Token]): FunDef = {
    ptree match {
      case Node('FunDef ::= (INLINE() :: _), List(_, Leaf(df), name, _, params, _, _, retType, _, _, listFunDefLocal, body, _)) =>
        val constructedParams = constructList(params, constructParam, hasComma = true, true)
        val constructedName = constructName(name)._1
        val constrBody = constructExpr(body, cstFolding = true)

        innerBodyRecur += (constructedName -> innerBodyCalls.toList)
        innerBodyCalls.clear()

        val constrFunDefLoc = constructFunDefLocal(listFunDefLocal)

        val shouldInline = shouldInlineRecur(constructedName, "", 0, 6)

        val fd = FunDef(
          constructedName,
          constructedParams,
          constructType(retType),
          constrFunDefLoc,
          constrBody,
          isInlined = true,
          isLocal = true
        ).setPos(df)
        inlinedFunctions += (fd.name -> (fd, body))
        fd

      case Node('FunDef ::= _, List(Leaf(df), name, _, params, _, _, retType, _, _, listFunDefLocal, body, _)) =>
        val constructedParams = constructList(params, constructParam, hasComma = true)
        val constructedName = constructName(name)._1
        val constrBody = constructExpr(body)
        innerBodyRecur += (constructedName -> innerBodyCalls.toList)
        innerBodyCalls.clear()
        val constrFunDefLoc = constructFunDefLocal(listFunDefLocal)

        val shouldInline = shouldInlineRecur(constructedName, "", 0, 6)


        val fd = FunDef(
          constructedName,
          constructedParams,
          constructType(retType),
          constrFunDefLoc,
          constructExpr(body, shouldInline),
          isInlined = shouldInline,
          isLocal = true
        ).setPos(df)

        if(shouldInline){
          inlinedFunctions += (fd.name -> (fd, body))
        }
        fd
    }
  }

  override def constructExpr(ptree: NodeOrLeaf[Token], cstFolding: Boolean = false): Expr = {
    ptree match{
      case Node('Expr ::= (VAL() :: _), List(Leaf(vt), param, _, value, _, body)) =>
        Let(constructParam(param), constructExprPR2(value, cstFolding), constructExpr(body)).setPos(vt)
      case Node('Expr ::= List('PR2, _), List(pr2, seq)) =>
        seq match {
          case Node('PR1Seq ::= _, List(_, expr)) =>
            val pr2Expr = constructExprPR2(pr2, cstFolding)
            Sequence(pr2Expr, constructExpr(expr)).setPos(pr2Expr)
          case Node(_, List()) =>
            constructExprPR2(pr2, cstFolding)
        }
    }
  }

  def constructCases(cases: NodeOrLeaf[Token], acc: List[MatchCase]): List[MatchCase] = {
    cases match {
      case Node('Cases ::= _, List(caseIN, next)) => constructCases(next, acc :+ constructCase(caseIN))
      case Node('CasesInner ::= List('Cases), List(casesIN)) => constructCases(casesIN, acc)
      case Node('CasesInner ::= _, _) => acc
    }
  }

  def constructExprPR2(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match {
      case Node('PR2 ::= _, List(pr3, pr2Seq)) =>
        pr2Seq match {
          case Node('PR2Seq ::= _, List(Leaf(pos), _, cases, _)) =>
            Match(constructExprPR3(pr3, cstFolding), constructCases(cases, List())).setPos(pos)
          case Node(_, _) =>
            constructExprPR3(pr3, cstFolding)
        }
    }
  }

  def constructExprPR3(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match {
      case Node('PR3 ::= _, List(pr4, n3Seq)) => constructOpExpr(constructExprPR4(pr4, cstFolding), n3Seq, cstFolding)
    }
  }
  def constructExprPR4(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match {
      case Node('PR4 ::= _, List(pr5, n4Seq)) => constructOpExpr(constructExprPR5(pr5, cstFolding), n4Seq, cstFolding)
    }
  }
  def constructExprPR5(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match {
      case Node('PR5 ::= _, List(pr6, n5Seq)) => constructOpExpr(constructExprPR6(pr6, cstFolding), n5Seq, cstFolding)
    }
  }
  def constructExprPR6(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match {
      case Node('PR6 ::= _, List(pr7, n6Seq)) => constructOpExpr(constructExprPR7(pr7, cstFolding), n6Seq, cstFolding)
    }
  }
  def constructExprPR7(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match {
      case Node('PR7 ::= _, List(pr8, n7Seq)) => constructOpExpr(constructExprPR8(pr8, cstFolding), n7Seq, cstFolding)
    }
  }
  def constructExprPR8(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match {
      case Node('PR8 ::= _, List(pr9, n8Seq)) => constructOpExpr(constructExprPR9(pr9, cstFolding), n8Seq, cstFolding)
    }
  }

  def constructExprPR9(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match{
      case Node('PR9 ::= (MINUS() :: _), List(Leaf(m), pr10)) =>
        val expr10 = constructExprPR10(pr10, cstFolding)
        if(cstFolding){
          expr10 match{
            case IntLiteral(value) => IntLiteral(-value).setPos(m)
            case _ => expr10.setPos(m)
          }
        }
        else
          Neg(expr10).setPos(m)
      case Node('PR9 ::= (BANG() :: _), List(Leaf(b), pr10)) =>
        val expr10 = constructExprPR10(pr10, cstFolding)
        if(cstFolding){
          expr10 match{
            case BooleanLiteral(value) => BooleanLiteral(!value).setPos(b)
            case _ => expr10.setPos(b)
          }
        }
        else
          Not(expr10).setPos(b)
      case Node('PR9 ::= List('PR10), List(pr10)) => constructExprPR10(pr10, cstFolding)
    }
  }

  def constructExprPR10(ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {
    ptree match{
      case Node('PR10 ::= (LPAREN() :: _), List(Leaf(p), seq)) =>
        seq match{
          case Node('Closure ::= _, List(_)) => UnitLiteral().setPos(p)
          case Node('Closure ::= _, List(expr, _)) => constructExpr(expr).setPos(p)
        }

      case Node('PR10 ::= List('Literal), List(lit)) => constructLiteral(lit)

      case Node('PR10 ::= List('Error), List(error)) =>
        error match {
          case Node('Error ::= _, List(Leaf(ert), _, msg, _)) =>
            Error(constructExpr(msg)).setPos(ert)
        }

      case Node('PR10 ::= List('If), List(ifStm)) =>
        ifStm match {
          case Node('If ::= _, List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)) =>
            val constructedCond = constructExpr(cond, cstFolding)
            constructedCond match{
              case BooleanLiteral(value) => if(value) constructExpr(thenn, cstFolding).setPos(it) else constructExpr(elze, cstFolding).setPos(it)
              case _ => Ite(constructedCond, constructExpr(thenn, cstFolding), constructExpr(elze, cstFolding)).setPos(it)
            }
        }

      case Node('PR10 ::= List('Call), List(call)) =>
        call match {
          case Node('Call ::= _, List(id, seq)) =>
            seq match {
              case Node('CallInner ::= (DOT() :: _), List(_, idIN, _, args, _)) =>
                val (module, pos) = constructName(id)
                val (name, _) = constructName(idIN)
                val qname = QualifiedName(Some(module), name)

                innerBodyCalls += name

                if(inlinedFunctions.get(name).isDefined){
                  val myargs = constructList(args, constructExpr, hasComma = true, cstFolding = true)
                  val nameWithExpr = inlinedFunctions(name)._1.paramNames.zip(myargs)
                  nameWithExpr.foreach(x =>  inlinedLocals += (x._1 -> x._2))
                  constructExpr(inlinedFunctions(name)._2, true).setPos(pos)
                }
                else {
                  val myargs = constructList(args, constructExpr, hasComma = true)
                  Call(qname, myargs).setPos(pos)
                }

              case Node('CallInner ::= (LPAREN() :: _), List(_, args, _)) =>
                val (name, pos) = constructName(id)
                val qname = QualifiedName(None, name)

                innerBodyCalls += name

                if(inlinedFunctions.get(name).isDefined){
                  val myargs = constructList(args, constructExpr, hasComma = true, cstFolding = true)
                  val nameWithExpr = inlinedFunctions(name)._1.paramNames.zip(myargs)
                  nameWithExpr.foreach(x =>  inlinedLocals += (x._1 -> x._2))
                  constructExpr(inlinedFunctions(name)._2, true).setPos(pos)
                }
                else{
                  val myargs = constructList(args, constructExpr, hasComma = true)
                  Call(qname, myargs).setPos(pos)
                }

              case Node('CallInner ::= _, _) =>
                val (name, pos) = constructName(id)

                if(inlinedLocals.exists(x => x._1 == name))
                  inlinedLocals(name)
                else
                  Variable(name).setPos(pos)

            }
        }
    }
  }

  override def constructPattern(pTree: NodeOrLeaf[Token], cstFolding: Boolean = false): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= (LPAREN() :: _), Leaf(lpar) :: _) =>
        LiteralPattern(UnitLiteral()).setPos(lpar)
      case Node('Pattern ::= List('IdPattern), List(idPat)) =>
        idPat match {
          case Node('IdPattern ::= _, List(id, idPatNext)) =>
            idPatNext match {
              case Node('IdPatternInner ::= ('QNameInner :: _), List(qnameNext, _, patterns, _)) =>
                qnameNext match {
                  case Node('QNameInner ::= (DOT() :: _), List(_, idIN)) =>
                    val (module, pos) = constructName(id)
                    val (name, _) = constructName(idIN)
                    val qname = QualifiedName(Some(module), name)
                    val mypatterns = constructList(patterns, constructPattern, hasComma = true)
                    CaseClassPattern(qname, mypatterns).setPos(pos)
                  case Node('QNameInner ::= _, _) =>
                    val (name, pos) = constructName(id)
                    val qname = QualifiedName(None, name)
                    val mypatterns = constructList(patterns, constructPattern, hasComma = true)
                    CaseClassPattern(qname, mypatterns).setPos(pos)
                }
              case Node('IdPatternInner ::= _, _) =>
                val (name, pos) = constructName(id)
                IdPattern(name).setPos(pos)
            }
        }
    }
  }

  override def constructOp(ptree: NodeOrLeaf[Token]) = {
    ptree match {
      case Node(_, List(Leaf(t))) =>
        tokenToExpr(t)
      case Leaf(t) =>
        tokenToExpr(t)
    }
  }

  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token], cstFolding: Boolean): Expr = {

    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('PR3Seq, 'PR4Seq, 'PR5Seq, 'PR6Seq, 'PR7Seq, 'PR8Seq) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = nextOpd match {
              case Node('PR2 ::= _, _) => constructExprPR2(nextOpd, cstFolding)
              case Node('PR3 ::= _, _) => constructExprPR3(nextOpd, cstFolding)
              case Node('PR4 ::= _, _) => constructExprPR4(nextOpd, cstFolding)
              case Node('PR5 ::= _, _) => constructExprPR5(nextOpd, cstFolding)
              case Node('PR6 ::= _, _) => constructExprPR6(nextOpd, cstFolding)
              case Node('PR7 ::= _, _) => constructExprPR7(nextOpd, cstFolding)
              case Node('PR8 ::= _, _) => constructExprPR8(nextOpd, cstFolding)
              case Node('PR9 ::= _, _) => constructExprPR9(nextOpd, cstFolding)
              case Node('PR10 ::= _, _) => constructExprPR10(nextOpd, cstFolding)
            }

            if(cstFolding) constantFolding(leftopd, nextAtom, op, suf)
            else constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, cstFolding)
        }
    }
  }

  def constantFolding(leftopd: Expr, nextAtom: Expr, op: NodeOrLeaf[Token], suf: NodeOrLeaf[Token]): Expr = {
    val opConstructed = constructOp(op)
    opConstructed match {
      case Plus => (leftopd, nextAtom) match {
        case (IntLiteral(_), IntLiteral(_)) => constructOpExpr(IntLiteral(leftopd.asInt + nextAtom.asInt).setPos(leftopd), suf, true)
        case (IntLiteral(_), Plus(lhs1: IntLiteral, rhs1)) => constructOpExpr(Plus(IntLiteral(leftopd.asInt + lhs1.asInt), rhs1).setPos(leftopd), suf, true)
        case (IntLiteral(_), Minus(lhs1: IntLiteral, rhs1)) => constructOpExpr(Minus(IntLiteral(leftopd.asInt + lhs1.asInt), rhs1).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case Minus => (leftopd, nextAtom) match {
        case (IntLiteral(_), IntLiteral(_)) => constructOpExpr(IntLiteral(leftopd.asInt - nextAtom.asInt).setPos(leftopd), suf, true)
        case (IntLiteral(_), Plus(lhs1: IntLiteral, rhs1)) => constructOpExpr(Plus(IntLiteral(leftopd.asInt - lhs1.asInt), rhs1).setPos(leftopd), suf, true)
        case (IntLiteral(_), Minus(lhs1: IntLiteral, rhs1)) => constructOpExpr(Minus(IntLiteral(leftopd.asInt - lhs1.asInt), rhs1).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case Times => (leftopd, nextAtom) match{
        case (IntLiteral(_), IntLiteral(_)) => constructOpExpr(IntLiteral(leftopd.asInt * nextAtom.asInt).setPos(leftopd), suf, true)
        case (IntLiteral(_), Times(lhs1: IntLiteral, rhs1)) => constructOpExpr(Times(IntLiteral(leftopd.asInt * lhs1.asInt), rhs1).setPos(leftopd), suf, true)
        case (IntLiteral(_), Div(lhs1: IntLiteral, rhs1)) => constructOpExpr(Div(IntLiteral(leftopd.asInt * lhs1.asInt), rhs1).setPos(leftopd), suf, true)
        case (IntLiteral(_), Mod(lhs1: IntLiteral, rhs1)) => constructOpExpr(Mod(IntLiteral(leftopd.asInt * lhs1.asInt), rhs1).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case Div => (leftopd, nextAtom) match{
        case (IntLiteral(_), IntLiteral(_)) =>
          if (nextAtom.asInt != 0)
            constructOpExpr(IntLiteral(leftopd.asInt / nextAtom.asInt).setPos(leftopd), suf, true)
          else
            Error(StringLiteral("Error: dividing by zero")).setPos(leftopd)
        case (IntLiteral(_), Times(lhs1: IntLiteral, rhs1)) =>
          if (lhs1.asInt != 0)
            constructOpExpr(Times(IntLiteral(leftopd.asInt / lhs1.asInt).setPos(leftopd), rhs1), suf, true)
          else
            Error(StringLiteral("Error: dividing by zero")).setPos(leftopd)
        case (IntLiteral(_), Div(lhs1: IntLiteral, rhs1)) =>
          if (lhs1.asInt != 0)
            constructOpExpr(Div(IntLiteral(leftopd.asInt / lhs1.asInt), rhs1).setPos(leftopd), suf, true)
          else
            Error(StringLiteral("Error: dividing by zero")).setPos(leftopd)
        case (IntLiteral(_), Mod(lhs1: IntLiteral, rhs1)) =>
          if (lhs1.asInt != 0)
            constructOpExpr(Mod(IntLiteral(leftopd.asInt / lhs1.asInt), rhs1).setPos(leftopd), suf, true)
          else
            Error(StringLiteral("Error: dividing by zero")).setPos(leftopd)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case Mod => (leftopd, nextAtom) match{
        case (IntLiteral(_), IntLiteral(_)) =>
          if (nextAtom.asInt != 0)
            constructOpExpr(IntLiteral(leftopd.asInt % nextAtom.asInt).setPos(leftopd), suf, true)
          else
            Error(StringLiteral("Error: dividing by zero")).setPos(leftopd)
        case (IntLiteral(_), Times(lhs1: IntLiteral, rhs1)) =>
          if (lhs1.asInt != 0)
            constructOpExpr(Times(IntLiteral(leftopd.asInt % lhs1.asInt), rhs1).setPos(leftopd), suf, true)
          else
            Error(StringLiteral("Error: dividing by zero")).setPos(leftopd)
        case (IntLiteral(_), Div(lhs1: IntLiteral, rhs1)) =>
          if (lhs1.asInt != 0)
            constructOpExpr(Div(IntLiteral(leftopd.asInt % lhs1.asInt), rhs1).setPos(leftopd), suf, true)
          else
            Error(StringLiteral("Error: dividing by zero")).setPos(leftopd)
        case (IntLiteral(_), Mod(lhs1: IntLiteral, rhs1)) =>
          if (lhs1.asInt != 0)
            constructOpExpr(Mod(IntLiteral(leftopd.asInt % lhs1.asInt), rhs1).setPos(leftopd), suf, true)
          else
            Error(StringLiteral("Error: dividing by zero")).setPos(leftopd)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case LessThan => (leftopd, nextAtom) match{
        case (IntLiteral(_), IntLiteral(_)) => constructOpExpr(BooleanLiteral(leftopd.asInt < nextAtom.asInt).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case LessEquals => (leftopd, nextAtom) match{
        case (IntLiteral(_), IntLiteral(_)) => constructOpExpr(BooleanLiteral(leftopd.asInt < nextAtom.asInt).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case And => (leftopd, nextAtom) match{
        case (BooleanLiteral(_), BooleanLiteral(_)) => constructOpExpr(BooleanLiteral(leftopd.asBoolean && nextAtom.asBoolean).setPos(leftopd), suf, true)
        case (BooleanLiteral(_), And(lhs1: BooleanLiteral, rhs1)) => constructOpExpr(And(BooleanLiteral(leftopd.asBoolean && lhs1.asBoolean), rhs1).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case Or => (leftopd, nextAtom) match{
        case (BooleanLiteral(_), BooleanLiteral(_)) => constructOpExpr(BooleanLiteral(leftopd.asBoolean || nextAtom.asBoolean).setPos(leftopd), suf, true)
        case (BooleanLiteral(_), Or(lhs1: BooleanLiteral, rhs1)) => constructOpExpr(Or(BooleanLiteral(leftopd.asBoolean || lhs1.asBoolean), rhs1).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case Equals => ((leftopd, nextAtom): @unchecked) match {
        case (IntLiteral(v1), IntLiteral(v2)) => constructOpExpr(BooleanLiteral(v1 == v2).setPos(leftopd), suf, true)
        case (StringLiteral(v1), StringLiteral(v2)) => constructOpExpr(BooleanLiteral(v1 eq v2).setPos(leftopd), suf, true)
        case (BooleanLiteral(v1), BooleanLiteral(v2)) => constructOpExpr(BooleanLiteral(v1 == v2).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case Concat =>  (leftopd, nextAtom) match{
        case (StringLiteral(_), StringLiteral(_)) => constructOpExpr(StringLiteral(leftopd.asString + nextAtom.asString).setPos(leftopd), suf, true)
        case (StringLiteral(_), Concat(lhs1, rhs1)) => constructOpExpr(Concat(StringLiteral(leftopd.asString + lhs1.asString), rhs1).setPos(leftopd), suf, true)
        case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
      }

      case _ => constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf, true)
    }
  }
}