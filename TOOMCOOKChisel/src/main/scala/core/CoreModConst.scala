package core

object CoreModConst {
  def modPow2(width: Int): BigInt = {
    require(width > 0, "modulus width must be positive")
    BigInt(1) << width
  }

  def mask(width: Int): BigInt = {
    require(width > 0, "mask width must be positive")
    modPow2(width) - 1
  }

  def invModPow2Odd(x: BigInt, width: Int): BigInt = {
    require(width > 0, "inverse width must be positive")
    require(x % 2 == 1, s"$x is not odd, so it has no inverse modulo 2^$width")
    x.modInverse(modPow2(width))
  }

  // Modular inverse of 3 modulo 2^width, generated at elaboration time.
  def inv3(width: Int): BigInt = invModPow2Odd(3, width)

  // Modular inverse of 9 modulo 2^width, generated at elaboration time.
  def inv9(width: Int): BigInt = invModPow2Odd(9, width)

  // Modular inverse of 15 modulo 2^width, used by the scaled +/-1/2 interpolation point.
  def inv15(width: Int): BigInt = invModPow2Odd(15, width)
}
