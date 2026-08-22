package logic.derivation

import logic.derivation.semantics.{Category, Morphism, Object}
import utils.Name

class Diagram(val name: Name,
              val cat: Category,
              val adjacency: Map[Object, Set[(Morphism, Object)]]):
  
  def nodes: Int = adjacency.size

  override def equals(obj: Any): Boolean =
    obj match
      case casted: Diagram => name == casted.name
      case _ => false

  override def hashCode(): Int = name.hashCode