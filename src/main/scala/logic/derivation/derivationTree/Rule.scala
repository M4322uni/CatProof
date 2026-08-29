package logic.derivation.derivationTree

import utils.Name
import logic.derivation.semantics.*

//TODO: tidy up

enum Condition:
  case Equation(p: Check)
  case TypeJudgement(subj: Object | Morphism | Name, ttype: Type)

case class Check(lhs: Construction, rhs: Construction):
  (lhs, rhs) match
    case (_: Category, _: Category)
         | (_: Morphism, _: Morphism)
         | (_: Object, _: Object) =>
    case _ => throw SemanticError(s"$lhs and $rhs are not of the same type")

case class Branch(antecedentExp: Set[Condition], consequent: Condition)
