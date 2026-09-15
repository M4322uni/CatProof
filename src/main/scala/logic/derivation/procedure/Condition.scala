package logic.derivation.procedure

import utils.Name
import logic.derivation.semantics.*

//TODO: tidy up

type TypeSubj = Object | Morphism

enum Condition:
  case Equation(p: ECheck)
  case TypeJudgement(p: TCheck)

  override def toString: String =
    this match
      case Equation(ECheck(lhs, rhs)) => s"$lhs = $rhs"
      case TypeJudgement(TCheck(subj, ttype)) => s"$subj: $ttype"

case class ECheck(lhs: Construction, rhs: Construction):
  (lhs, rhs) match
    case (_: Category, _: Category)
         | (_: Morphism, _: Morphism)
         | (_: Object, _: Object) =>
    case _ => throw SemanticError(s"$lhs and $rhs are not of the same type")

case class TCheck(lhs: TypeSubj, rhs: RestrictType):
  (lhs, rhs) match
    case (_: Object, _: ObjectType)
      | (_: Morphism, _: MorphismType) =>
    case _ => throw SemanticError(s"$lhs cannot be of type $rhs")