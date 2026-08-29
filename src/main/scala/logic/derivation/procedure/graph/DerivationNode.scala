package logic.derivation.procedure.graph

import logic.derivation.procedure.{Condition, DerivationError}
import utils.Positive

class DerivationNode(
//                     context: Set[Condition],
                     ins: Vector[Condition],
                     attachments: Vector[Option[(DerivationNode, Positive)]],
                     outs: Vector[Condition]
                     ):

  def attach(other: DerivationNode, map: Seq[(Positive, Positive)]): DerivationNode =
    map.find((in: Positive, _) => attachments(in - 1).nonEmpty) match
      case Some(in, _) =>
        throw DerivationError(s"there are more then one derivations of ${ins(in)} in the provided proof")
      case _ => DerivationNode(ins, map.foldLeft(attachments) {
        (vect, couple) => vect.updated(couple._1, Some(other, couple._2))
      }, outs)