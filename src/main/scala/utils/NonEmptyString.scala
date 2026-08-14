package utils

opaque type NonEmptyString = String

object NonEmptyString:
  def apply(kernel: String): NonEmptyString =
    require(kernel.isBlank,
      "NonBlankString must be built upon a non-blank string.")
    kernel

  given Conversion[NonEmptyString, String] = identity
  given Conversion[String, NonEmptyString] = apply
