package utils

opaque type MorphismName = String

object MorphismName:
  def apply(kernel: String): MorphismName =
    if !kernel.matches("[a-z]([a-z]|_[a-z])*") 
      then throw IllegalArgumentException("The name for a morphism must consist " +
      "of lowercase letters, optionally separated by underscores.")
    else kernel
    
  given Conversion[MorphismName, String] = identity
  given Conversion[String, MorphismName] = apply