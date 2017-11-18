package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    def delta = {
      val _b = b.apply()
      val _a = a.apply()
      val _c = c.apply()
      _b * _b - 4 * _a * _c
    }

    Var(delta)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def solution = {
      val _a = a.apply()
      val _b = b.apply()
      val _d = delta.apply() match {
        case x if x < 0 => 0
        case x => math.sqrt(x)
      }
      Set(root(_a, _b, _d), root(_a, _b, -_d))
    }

    Var(solution)
  }

  private def root(a: Double, b: Double, delta: Double) = (b + delta) / (2 * a)
}
