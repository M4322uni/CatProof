package logic.derivation.procedure.graph

import logic.derivation.procedure.{Condition, DerivationError, Rule}
import utils.Positive

class DerivationNode(
//                     context: Set[Condition],
                     rule: Rule,
                     attachments: Vector[Option[DerivationNode]],
                     ):

  def attach(other: DerivationNode, pos: Positive): DerivationNode =
    rule.pre match
      case vect: Vector[?] =>
        attachments(pos - 1) match
          case Some(_) =>
            throw DerivationError(s"there are more then one derivations of ${vect(pos-1)} in the provided proof")
          case _ => DerivationNode(rule, attachments.updated(pos-1, Some(other))) //TODO: try-catch
      case _ => throw DerivationError(s"$rule has no antecedent, it cant have an attachment")

object DerivationNode:

  private def apply(rule: Rule,
                    attachments: Vector[Option[DerivationNode]]): DerivationNode =
    new DerivationNode(rule, attachments)

  def apply(rule: Rule): DerivationNode =
    rule.pre match
      case vect: Vector[?] =>
        apply(rule, Vector.fill(vect.size)(None))
      case _ => apply(rule, Vector.empty)