package logic.derivation

import logic.derivation.structures.Category
import utils.Name

class Diagram(val name: Name,
              cat: Category,
              adjacency: Set[(Vertex, Set[(Name, Vertex)])]):

  override def equals(obj: Any): Boolean =
    obj match
      case casted: Diagram => name == casted.name
      case _ => false

  override def hashCode(): Int = name.hashCode

class Vertex(name: Name)