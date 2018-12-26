package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

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

  override def constructExpr(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match{
      case Node('Expr ::= (VAL() :: _), List(Leaf(vt), param, _, value, _, body)) =>
        Let(constructParam(param), constructExprPR2(value), constructExpr(body)).setPos(vt)
      case Node('Expr ::= List('PR2, _), List(pr2, seq)) =>
        seq match {
          case Node('PR1Seq ::= _, List(_, expr)) =>
            val pr2Expr = constructExprPR2(pr2)
            Sequence(pr2Expr, constructExpr(expr)).setPos(pr2Expr)
          case Node(_, List()) =>
            constructExprPR2(pr2)
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

  def constructExprPR2(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('PR2 ::= _, List(pr3, pr2Seq)) =>
        pr2Seq match {
          case Node('PR2Seq ::= _, List(Leaf(pos), _, cases, _)) =>
            Match(constructExprPR3(pr3), constructCases(cases, List())).setPos(pos)
          case Node(_, _) =>
            constructExprPR3(pr3)
        }
    }
  }

  def constructExprPR3(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('PR3 ::= _, List(pr4, n3Seq)) => constructOpExpr(constructExprPR4(pr4), n3Seq)
    }
  }
  def constructExprPR4(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('PR4 ::= _, List(pr5, n4Seq)) => constructOpExpr(constructExprPR5(pr5), n4Seq)
    }
  }
  def constructExprPR5(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('PR5 ::= _, List(pr6, n5Seq)) => constructOpExpr(constructExprPR6(pr6), n5Seq)
    }
  }
  def constructExprPR6(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('PR6 ::= _, List(pr7, n6Seq)) => constructOpExpr(constructExprPR7(pr7), n6Seq)
    }
  }
  def constructExprPR7(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('PR7 ::= _, List(pr8, n7Seq)) => constructOpExpr(constructExprPR8(pr8), n7Seq)
    }
  }
  def constructExprPR8(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node('PR8 ::= _, List(pr9, n8Seq)) => constructOpExpr(constructExprPR9(pr9), n8Seq)
    }
  }

  def constructExprPR9(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match{
      case Node('PR9 ::= (MINUS() :: _), List(Leaf(m), pr10)) => Neg(constructExprPR10(pr10)).setPos(m)
      case Node('PR9 ::= (BANG() :: _), List(Leaf(b), pr10)) => Not(constructExprPR10(pr10)).setPos(b)
      case Node('PR9 ::= List('PR10), List(pr10)) => constructExprPR10(pr10)
    }
  }

  def constructExprPR10(ptree: NodeOrLeaf[Token]): Expr = {
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
            Ite(constructExpr(cond), constructExpr(thenn), constructExpr(elze)).setPos(it)
        }

      case Node('PR10 ::= List('Call), List(call)) =>
        call match {
          case Node('Call ::= _, List(id, seq)) =>
            seq match {
              case Node('CallInner ::= (DOT() :: _), List(_, idIN, _, args, _)) =>
                val (module, pos) = constructName(id)
                val (name, _) = constructName(idIN)
                val myargs = constructList(args, constructExpr, hasComma = true)
                val qname = QualifiedName(Some(module), name)
                Call(qname, myargs).setPos(pos)
              case Node('CallInner ::= (LPAREN() :: _), List(_, args, _)) =>
                val (name, pos) = constructName(id)
                val qname = QualifiedName(None, name)
                val myargs = constructList(args, constructExpr, hasComma = true)
                Call(qname, myargs).setPos(pos)
              case Node('CallInner ::= _, _) =>
                val (name, pos) = constructName(id)
                Variable(name).setPos(pos)
            }
        }
    }
  }

  override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
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
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('PR3Seq, 'PR4Seq, 'PR5Seq, 'PR6Seq, 'PR7Seq, 'PR8Seq) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = nextOpd match {
              case Node('PR2 ::= _, _) => constructExprPR2(nextOpd)
              case Node('PR3 ::= _, _) => constructExprPR3(nextOpd)
              case Node('PR4 ::= _, _) => constructExprPR4(nextOpd)
              case Node('PR5 ::= _, _) => constructExprPR5(nextOpd)
              case Node('PR6 ::= _, _) => constructExprPR6(nextOpd)
              case Node('PR7 ::= _, _) => constructExprPR7(nextOpd)
              case Node('PR8 ::= _, _) => constructExprPR8(nextOpd)
              case Node('PR9 ::= _, _) => constructExprPR9(nextOpd)
              case Node('PR10 ::= _, _) => constructExprPR10(nextOpd)
            }
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }
}