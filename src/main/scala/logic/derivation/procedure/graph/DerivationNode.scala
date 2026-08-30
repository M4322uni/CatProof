package logic.derivation.procedure.graph

import logic.derivation.procedure.{DerivationError, RuleResult}
import utils.Positive

class DerivationNode private(
                              ruleResult: RuleResult,
                              attachments: Vector[Option[DerivationNode]],
                     ):

  def attach(other: DerivationNode, pos: Int): DerivationNode =
    ruleResult.pre match
      case Some(vect) =>
        attachments(pos - 1) match
          case Some(_) =>
            throw DerivationError(s"there are more then one derivations of ${vect(pos)} in the provided proof")
          case _ => DerivationNode(ruleResult, attachments.updated(pos, Some(other))) //TODO: try-catch
      case _ => throw DerivationError(s"$ruleResult has no antecedent, it cant have an attachment")

object DerivationNode:

  private def apply(ruleResult: RuleResult,
                    attachments: Vector[Option[DerivationNode]]): DerivationNode =
    new DerivationNode(ruleResult, attachments)

  def apply(ruleResult: RuleResult): DerivationNode =
    ruleResult.pre match
      case Some(vect) =>
        apply(ruleResult, Vector.fill(vect.size)(None))
      case _ => apply(ruleResult, Vector.empty)