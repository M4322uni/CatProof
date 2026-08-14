package utils

opaque type Positive = Int

object Positive:
  def apply(n: Int): Positive =
    require(n > 0,
      "A Positive number must be strictly greater than zero")
    n

  given Conversion[Positive, Int] = identity
  given Conversion[Int, Positive] = apply