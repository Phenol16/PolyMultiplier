package core

object CoreSchoolbookRef {
  def mask(width: Int): BigInt = {
    require(width > 0, "mask width must be positive")
    (BigInt(1) << width) - 1
  }

  def toSigned(x: BigInt, width: Int): BigInt = {
    require(width > 0, "signed width must be positive")
    val m = BigInt(1) << width
    val half = BigInt(1) << (width - 1)
    val y = x & (m - 1)
    if (y >= half) y - m else y
  }

  def schoolbookNegacyclic(
      a: Seq[BigInt],
      b: Seq[BigInt],
      n: Int,
      aW: Int,
      bW: Int,
      outW: Int
  ): Seq[BigInt] = {
    require(a.length == n, s"a length must be $n")
    require(b.length == n, s"b length must be $n")

    val qMask = mask(outW)
    val c = Array.fill(n)(BigInt(0))

    for (i <- 0 until n) {
      val ai = toSigned(a(i), aW)
      for (j <- 0 until n) {
        val bj = toSigned(b(j), bW)
        val rawIdx = i + j
        if (rawIdx < n) {
          c(rawIdx) = c(rawIdx) + ai * bj
        } else {
          c(rawIdx - n) = c(rawIdx - n) - ai * bj
        }
      }
    }

    c.map(_ & qMask).toSeq
  }
}
