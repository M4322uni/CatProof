package utils

opaque type ObjectName = String

object ObjectName:
  def apply(kernel: String): ObjectName =
    require(kernel.matches("[A-Z]([A-Z]|_[A-Z])*"),
      "The name for an object must consist " + 
      "of uppercase letters, optionally separated by underscores.")
    kernel

  given Conversion[ObjectName, String] = identity
  given Conversion[String, ObjectName] = apply