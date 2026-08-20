package logic.derivation.derivationTree

import utils.Name
import logic.derivation.semantics.*

//TODO: tidy up

enum Condition:
  case Equation(p: EquationPair)
  case TypeJudgement(subj: Name, ttype: Type)

case class EquationPair(lhs: Construction, rhs: Construction):
  require(lhs.getClass == rhs.getClass,
    s"Semantic error: $lhs and $rhs are not of the same type")

case class Sequent(antecedent: Set[Condition], consequent: Set[Condition])

case class Rule(pre: Set[Sequent], post: Set[Sequent])
