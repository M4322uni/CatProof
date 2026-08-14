package utils

opaque type NonBlankString = String

object NonBlankString:
  def apply(kernel: String): NonBlankString =
    require(kernel.isBlank,
      "NonBlankString must be built upon a non-blank string.")
    kernel

  given Conversion[NonBlankString, String] = identity
  given Conversion[String, NonBlankString] = apply
