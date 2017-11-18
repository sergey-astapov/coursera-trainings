package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map(x => {
      def proc: Double = {
        eval(x._1, x._2.apply(), namedExpressions - x._1)
      }

      (x._1, Var(proc))
    })
  }

  def eval(name: String, expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def _eval(expr: Expr): Double = expr match {
      case Literal(v) => v
      case Ref(n) if n == name => Double.NaN
      case Ref(n) if references.contains(n) => _eval(references(n).apply())
      case Ref(_) => Double.NaN
      case Plus(a, b) => _eval(a) + _eval(b)
      case Minus(a, b) => _eval(a) - _eval(b)
      case Times(a, b) => _eval(a) * _eval(b)
      case Divide(a, b) => _eval(a) / _eval(b)
    }

    _eval(expr)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
