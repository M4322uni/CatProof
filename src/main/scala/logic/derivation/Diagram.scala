package logic.derivation

import logic.derivation.semantics.Category
import utils.Name

class Diagram(val name: Name,
              val cat: Category,
              val adjacency: Map[Name, Set[(Name, Name)]]):
  
  def nodes: Int = adjacency.size

  override def equals(obj: Any): Boolean =
    obj match
      case casted: Diagram => name == casted.name
      case _ => false

  override def hashCode(): Int = name.hashCode