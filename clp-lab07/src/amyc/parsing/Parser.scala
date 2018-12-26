package amyc
package parsing

import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarDSL._
import grammarcomp.grammar.GrammarUtils.InLL1
import grammarcomp.grammar._
import grammarcomp.parsing._
import amyc.utils._
import ast.NominalTreeModule._
import Tokens._

// The parser for Amy
// Absorbs tokens from the Lexer and then uses grammarcomp to generate parse trees.
// Defines two different grammars, a naive one which does not obey operator precedence (for demonstration purposes)
// and an LL1 grammar that implements the true syntax of Amy
object Parser extends Pipeline[Stream[Token], Program] {

  /* This grammar does not implement the correct syntax of Amy and is not LL1
   * It is given as an example
   */
  val amyGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id | 'Id ~ DOT() ~ 'Id,
    'Expr ::= 'Id | 'Literal | 'Expr ~ 'BinOp ~ 'Expr | BANG() ~ 'Expr | MINUS() ~ 'Expr |
              'QName ~ LPAREN() ~ 'Args ~ RPAREN() | 'Expr ~ SEMICOLON() ~ 'Expr |
              VAL() ~ 'Param ~ EQSIGN() ~ 'Expr ~ SEMICOLON() ~ 'Expr |
              IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              'Expr ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'Expr ~ RPAREN(),
    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
               AND() | OR() | EQUALS() | CONCAT(),
    'Cases ::= 'Case | 'Case ~ 'Cases,
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  // You can start from the example above and work your way from there.
  // Make sure you use the warning (see `run` below) that tells you which part is not in LL1.
  lazy val amyGrammarLL1 = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,

    'QName ::= 'Id ~ 'QNameInner,
    'QNameInner ::= DOT() ~ 'Id | epsilon(),

    //Define the order of priority among the different expressions
    'Expr ::= VAL() ~ 'Param ~ EQSIGN() ~ 'PR2 ~ SEMICOLON() ~'Expr | 'PR2 ~ 'PR1Seq,  // val
    'PR1Seq ::= SEMICOLON() ~ 'Expr  | epsilon(),

    'PR2 ::= 'PR3 ~ 'PR2Seq,                                                         // match
    'PR2Seq ::= MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() | epsilon(),

    'PR3 ::= 'PR4 ~ 'PR3Seq,                                                         // ||
    'PR3Seq ::= OR() ~ 'PR3 | epsilon(),

    'PR4 ::= 'PR5 ~ 'PR4Seq,                                                         // &&
    'PR4Seq ::= AND() ~ 'PR4 | epsilon(),

    'PR5 ::= 'PR6 ~ 'PR5Seq,                                                         // ==
    'PR5Seq ::= EQUALS() ~ 'PR5 | epsilon(),

    'PR6 ::= 'PR7 ~ 'PR6Seq,                                                         // <, <=
    'PR6Seq ::= LESSEQUALS() ~ 'PR6 | LESSTHAN() ~ 'PR6 | epsilon(),

    'PR7 ::= 'PR8 ~ 'PR7Seq,                                                         // +, -, ++
    'PR7Seq ::= PLUS() ~ 'PR7 | MINUS() ~ 'PR7 | CONCAT() ~ 'PR7 | epsilon(),

    'PR8 ::= 'PR9 ~ 'PR8Seq,                                                         // *, /, %
    'PR8Seq ::= TIMES() ~ 'PR8 | DIV() ~ 'PR8 | MOD() ~ 'PR8 | epsilon(),

    'PR9 ::=  MINUS() ~ 'PR10 | BANG() ~ 'PR10 | 'PR10,                                      // -, !

    'PR10 ::= LPAREN() ~ 'Closure | 'Literal | 'Error | 'If | 'Call,                 // error, if, calls, parenthesized expr, lit

    'Closure ::= RPAREN() | 'Expr ~ RPAREN(),
    'Error ::= ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN(),
    'If ::= IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Call ::= 'Id ~ 'CallInner,
    'CallInner ::= DOT() ~ 'Id ~ LPAREN() ~ 'Args ~ RPAREN() | LPAREN() ~ 'Args ~ RPAREN()| epsilon(),

    'Literal ::= TRUE() | FALSE() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
      AND() | OR() | EQUALS() | CONCAT(),

    'Cases ::= 'Case ~ 'CasesInner,
    'CasesInner ::= 'Cases | epsilon(),
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,

    'IdPattern ::= 'Id ~ 'IdPatternInner,
    'IdPatternInner ::= 'QNameInner ~ LPAREN() ~ 'Patterns ~ RPAREN() | epsilon(),
    'Pattern ::= UNDERSCORE() | 'Literal | 'IdPattern | LPAREN() ~ RPAREN(),        // [Id .]?Id ( Patterns ) | Id | Literal | _

    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  def run(ctx: Context)(tokens: Stream[Token]): Program = {
    //val (grammar, constructor) = (amyGrammar, new ASTConstructor)
    val (grammar, constructor) = (amyGrammarLL1, new ASTConstructorLL1)

    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()

    GrammarUtils.isLL1WithFeedback(grammar) match {
      case InLL1() =>
        // info("Grammar is in LL1")
      case other =>
        warning(other)
    }

    val feedback = ParseTreeUtils.parseWithTrees(grammar, tokens.toList)
    feedback match {
      case s: Success[Token] =>
        constructor.constructProgram(s.parseTrees.head)
      case err@LL1Error(_, Some(tok)) =>
        fatal(s"Parsing failed: $err", tok.obj.position)
      case err =>
        fatal(s"Parsing failed: $err")
    }
  }

}