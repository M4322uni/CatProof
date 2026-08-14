package utils

opaque type MorphismName = String

object MorphismName:
  def apply(kernel: String): MorphismName =
    require(kernel.matches("[a-z]([a-z]|_[a-z])*"),
      "The name for a morphism must consist " +
      "of lowercase letters, optionally separated by underscores.")
    kernel
    
  given Conversion[MorphismName, String] = identity
  given Conversion[String, MorphismName] = apply