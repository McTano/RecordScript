
package ammonite
package $file
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.repl.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}


object SExpression{
/*<script>*/import scala.util.matching.Regex

object Solution {
     val intmatch: Regex = """^(-?\d+)(.*)""".r
     val varmatch: Regex = """^([a-z]\w*)(.*)""".r
    

    def evaluate(expression: String): Int = {
        interpret(parse(tokenize(expression)), NullContext)
    }

    def interpret(expression: Expression, context: Context): Int = {
        expression match {
            case Num(n) => n
            case Var(v) => context.lookup(v) match {
                case None => throw new RuntimeException(s"variable $v not found in context: $context")
                case Some(value) => value
            }
            case LetExpr(binders, targetExpression) => 
                interpret(targetExpression,
                    binders.foldLeft(context)((context, binder) =>
                        context.extend(binder._1.name, interpret(binder._2, context))))
            case MultExpr(lhs, rhs) => 
                interpret(lhs, context) * interpret(rhs, context)
            case AddExpr(lhs, rhs) => interpret(lhs, context) + interpret(rhs, context)
            case _ => throw new RuntimeException(s"expression `$expression not recognized`")
        }
    }

    def parse(tokens: List[Token]): Expression = {
        parseHead(tokens) match {
            case (t, Nil) => t
            case (t, tokens) => throw new RuntimeException(s"expected a single expression, got ${tokens}")
            case _ => throw new RuntimeException("unexpected case")
        }
    }


    def parseHead(tokens: List[Token]): (Expression, List[Token]) = {
        tokens match {
         case Nil => throw new Error("no tokens in list")
         case Num(n) :: rest => (Num(n), rest)
         case Var(x) :: rest => (Var(x), rest)
         case OpenParen :: rest => parseList(rest)
         case CloseParen :: rest => throw new RuntimeException("Unexpected close paren encountered. Expecting Expression ${tokens}")
         case _ => throw new RuntimeException(s"Unrecognized pattern of tokens: ${tokens}")
        }
    }

    def parseList(tokens: List[Token]): (Expression, List[Token]) = {
        tokens match {  
            case Let :: rest => parseLet(rest)
            case Add :: rest => parseAdd(rest)
            case Mult :: rest => parseMult(rest)
            case _ => throw new RuntimeException(s"List Expression does not start with a keyword: ${tokens}")
            }
    }

    def parseTwo(tokens: List[Token]): (Expression, Expression, List[Token]) = {
        parseHead(tokens) match {
            case (e1, tail1) => parseHead(tail1) match {
                case (e2, tail2) => tail2 match {
                    case CloseParen :: tail3 => (e1, e2, tail3)
                    case _ => throw new RuntimeException(s"Expected two subexpressions followed by a parenthesis, got ${tokens}")
                }
            }
        }
    }

    def parseAdd(tokens: List[Token]): (AddExpr, List[Token]) = {
        parseTwo(tokens) match {
            case (e1, e2, tail) => (AddExpr(e1, e2), tail)
        }
    }

    def parseMult(tokens: List[Token]): (MultExpr, List[Token]) = {
        parseTwo(tokens) match {
            case (e1, e2, tail) => (MultExpr(e1, e2), tail)
        }
    }


    def parseLet(tokens: List[Token]): (LetExpr, List[Token]) = {
        val (bindings, targetExpression, rest) = parseBindings(tokens)
        (LetExpr(bindings, targetExpression), rest)
    }

    def parseBindings(tokens: List[Token]): (List[(Var, Expression)], Expression, List[Token]) = {
        parseHead(tokens) match {
            case (targetExpression, CloseParen :: rest) => (Nil, targetExpression, rest)
            case (v: Var, rest1) => parseHead(rest1) match {
                case (e: Expression, rest2) => parseBindings(rest2) match {
                    case (bindings, targetExpression, rest3) 
                        => {((v, e) :: bindings, targetExpression, rest3)}
                }
            }

            case _ => throw new RuntimeException("unexpected case")
        }
    }


    def tokenize(str: String): List[Token] = {
        str match {
            // ignore spaces
            case s" $rest" => tokenize(rest)
            case s"($rest" => OpenParen :: tokenize(rest)
            case s")$rest" => CloseParen :: tokenize(rest)
            case s"let$rest" => Let :: tokenize(rest)
            case s"mult$rest" => Mult :: tokenize(rest)
            case s"add$rest" => Add :: tokenize(rest)
            case intmatch(num, rest) => Num(num.toInt) :: tokenize(rest)
            case varmatch(name, rest) => Var(name) :: tokenize(rest)
            case "" => Nil
            case _ => throw new RuntimeException(s"can't parse : ```\n${str}\n```")
        }
    }
}

sealed trait Expression
case class LetExpr(binders: List[(Var, Expression)], targetExpression: Expression) extends Expression
case class AddExpr(e1: Expression, e2: Expression) extends Expression
case class MultExpr(e1: Expression, e2: Expression) extends Expression

sealed trait Token
case class Num(n: Int) extends Token with Expression
case class Var(name: String) extends Token with Expression

sealed trait Syntax extends Token
case object `OpenParen` extends Syntax
case object `CloseParen` extends Syntax

sealed trait Keyword extends Token
case object Let extends Keyword
case object Add extends Keyword
case object Mult extends Keyword





sealed trait Context {
    def extend(b: String, v: Int): Context = {
        new Binding(this, b, v)
    }
    def lookup(v: String): Option[Int]
}
case object NullContext extends Context {
    def lookup(v: String): Option[Int] = {
        None
    }
}
case class Binding (outerContext: Context, variable: String, value: Int) extends Context {
    def lookup(v: String): Option[Int] = {
        // println(s"looking up ${v} in context ${this}")
        if (v == variable) {
            Some(value)
        } else {
            outerContext.lookup(v)
        }
    }

    override def toString() = {
        s"""|{
            |  $variable = $value,
            |  outerContext: {
            |    $outerContext
            |  }
            |}""".stripMargin
    }
}


// println(Solution.evaluate("(add 1 2)"))
// println(Solution.evaluate("(let x 2 (mult x (let x 3 y 4 (add x y))))"))
// println(Solution.evaluate("(let x 3 x 2 x)"))
// println(Solution.evaluate("-12"))
// println(Solution.evaluate("(let var 78 b 77 (let c 33 (add c (mult var 66))))"))/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "SExpression"
  /*</generated>*/
}
