package algos.impl.levenstein

import scala.math._

object LevensteinDistance {
  def minimum(x: Int*): Int = x.min
  def LevensteinDistancePerecntMatch(a: String, b: String): Int = {
    val ms = (a.toLowerCase()).length
    val ns = (b.toLowerCase()).length
    val distance = Array.tabulate(ms + 1, ns + 1){(i ,j) => if (i == 0) j else if (j == 0) i else 0} 
      for (m <- 1 to ms; n <- 1 to ns) {
      val dis = if (a(m - 1) == b(n - 1)) 0 else 1
      distance(m)(n) = minimum(distance(m - 1)(n) + 1, //deletion
                               distance(m)(n - 1) + 1, //insertion
                               distance(m - 1)(n - 1) + dis) //substitution
    }
    (distance(ms)(ns) * 100) / max(a.length, b.length)
  }
