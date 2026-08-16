package utils

opaque type Name = String

object Name:
  def apply(kernel: String): Name =
    require(kernel.matches("[A-Za-z_][A-Za-z0-9_]*"),
      "NonBlankString must be built upon a non-blank string.")
    kernel

  given Conversion[Name, String] = identity
  given Conversion[String, Name] = apply
