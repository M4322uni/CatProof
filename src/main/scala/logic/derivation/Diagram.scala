package logic.derivation

import logic.derivation.Diagram.Node
import logic.derivation.semantics.{Category, Morphism, Object}
import utils.Name

class Diagram(val name: Name,
              val cat: Category,
              val adjacency: Map[Node, Set[(Morphism, Node)]]):

  def nodes: Int = adjacency.size

  override def equals(obj: Any): Boolean =
    obj match
      case casted: Diagram => name == casted.name
      case _ => false

  override def hashCode(): Int = name.hashCode

object Diagram:

  class Node(val id: Int, val content: Object):

    override def equals(obj: Any): Boolean =
      obj match
        case casted: Node => id == casted.id
        case _ => false

    override def hashCode(): Int = id.hashCode