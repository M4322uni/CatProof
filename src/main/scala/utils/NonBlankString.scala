package utils

opaque type NonBlankString = String

object NonBlankString:
  def apply(kernel: String): NonBlankString =
    if kernel.isBlank then throw IllegalArgumentException("NonBlankString must be built " +
      "upon a non-blank string.")
    else kernel

  given Conversion[NonBlankString, String] = identity
  given Conversion[String, NonBlankString] = apply
