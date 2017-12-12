import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  object Boundaries {
    def apply(minX: Float, maxX: Float, minY: Float, maxY: Float): Boundaries = {
      val b = new Boundaries()
      b.minX = minX
      b.maxX = maxX
      b.minY = minY
      b.maxY = maxY
      b
    }

    private def apply(bodies: Seq[Body], opt: Option[Boundaries]): Option[Boundaries] = (bodies, opt) match {
      case (h :: tail, Some(b)) =>
        apply(tail, Some(Boundaries(
          Math.min(h.x, b.minX),
          Math.max(h.x, b.maxX),
          Math.min(h.y, b.minY),
          Math.max(h.y, b.maxY))))
      case (h :: tail, _) =>
        apply(tail, Some(Boundaries(h.x, h.x, h.y, h.y)))
      case (Nil, b) => b
    }

    def apply(bodies: Seq[Body]): Boundaries = apply(bodies, None).getOrElse(throw new IllegalStateException())
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad

    def halfSize: Float = size / 2
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0f
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = nw.centerX + nw.halfSize
    val centerY: Float = nw.centerY + nw.halfSize
    val size: Float = 2 * nw.size
    private val quads = List(nw, ne, sw, se)
    val mass: Float = quads.map(_.mass).sum
    val massX: Float = quads.map(q => q.massX * q.mass).foldLeft(0f)(_ + _) / mass
    val massY: Float = quads.map(q => q.massY * q.mass).foldLeft(0f)(_ + _) / mass
    val total: Int = quads.map(_.total).sum

    def insert(b: Body): Fork = {
      quads.
        map(q => (q, distance(q.centerX, q.centerY, b.x, b.y))).
        reduce((q1, q2) => if (q1._2 <= q2._2) q1 else q2) match {
        case (quad, _) => quad match {
          case q if q eq nw => Fork(nw.insert(b), ne, sw, se)
          case q if q eq ne => Fork(nw, ne.insert(b), sw, se)
          case q if q eq sw => Fork(nw, ne, sw.insert(b), se)
          case q if q eq se => Fork(nw, ne, sw, se.insert(b))
        }
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val (mass, massX, massY) = {
      val m = bodies.map(_.mass).foldLeft(0f)(_ + _)
      val mx = bodies.map(b => b.mass * b.x).foldLeft(0f)(_ + _) / m
      val my = bodies.map(b => b.mass * b.y).foldLeft(0f)(_ + _) / m
      (m, mx, my)
    }
    val total: Int = bodies.size
    def insert(b: Body): Quad = if (size == minimumSize) {
      val bound = Boundaries(bodies)
      val fork = Fork(
        Empty(bound.centerX - bound.width / 4, centerY - bound.height / 4, bound.size / 2),
        Empty(bound.centerX + bound.width / 4, centerY - bound.height / 4, bound.size / 2),
        Empty(bound.centerX - bound.width / 4, centerY + bound.height / 4, bound.size / 2),
        Empty(bound.centerX + bound.width / 4, centerY + bound.height / 4, bound.size / 2))
      bodies.foreach(fork.insert)
      fork
    } else {
      val newBodies = bodies :+ b
      val bound = Boundaries(newBodies)
      Leaf(bound.centerX, bound.centerY, bound.size, newBodies)
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case f @ Fork(nw, ne, sw, se) =>
          if (quad.size / distance(f.centerX, f.centerY, x, y) < theta) {
            List(nw, ne, sw, se).foreach(traverse)
          } else {
            addForce(f.mass, f.massX, f.massY)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

    override def toString: String = s"mass: $mass, x: $x, y: $y, xspeed: $xspeed, yspeed: $yspeed"

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- matrix.indices) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      val sectorX = ((((b.x min boundaries.maxX) max boundaries.minX) - boundaries.minX) / sectorSize).toInt
      val sectorY = ((((b.y min boundaries.maxY) max boundaries.minY) - boundaries.minY) / sectorSize).toInt
      this(sectorX, sectorY) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- matrix.indices)
        matrix(i) = matrix(i).combine(that.matrix(i))
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
