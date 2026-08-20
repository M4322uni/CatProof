package logic.derivation

import utils.Name

class Diagram(private val name: Name,
              cat: Cat,
              adjacency: Set[(Obj, Set[(Name, Obj)])]):

  override def equals(obj: Any): Boolean =
    obj match
      case casted: Diagram => name == casted.name
      case _ => false

  override def hashCode(): Int = name.hashCode

class Obj(name: Name)

case class Cat(name: Name)