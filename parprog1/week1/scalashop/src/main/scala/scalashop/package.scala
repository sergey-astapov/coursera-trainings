
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = radius match {
    case 0 => src(x, y)
    case _ =>
      val acc = Array(0, 0, 0, 0) // 0 - red, 1 - green, 2 - blue, 3 - alpha
      var num = 0
      for (i <- clamp(x - radius, 0, x) to clamp(x + radius, x, src.width - 1);
           j <- clamp(y - radius, 0, y) to clamp(y + radius, y, src.height - 1)) {
        val p = src(i, j)
        acc(0) = acc(0) + red(p)
        acc(1) = acc(1) + green(p)
        acc(2) = acc(2) + blue(p)
        acc(3) = acc(3) + alpha(p)
        num = num + 1
      }
      rgba(acc(0) / num, acc(1) / num, acc(2) / num, acc(3) / num)
  }

}
