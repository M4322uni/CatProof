package logic.derivation.derivationTree

enum Condition:
  case Equation
  case TypeJudgement

case class Sequent(antecedent: Set[Condition], consequent: Set[Condition])

case class Rule(pre: Set[Sequent], post: Set[Sequent])
