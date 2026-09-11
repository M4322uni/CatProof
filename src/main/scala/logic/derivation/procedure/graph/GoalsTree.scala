package logic.derivation.procedure.graph

import logic.derivation.procedure.Condition

enum GoalsTree:
  case Leaf(open: (Set[Condition], Condition))
  case Fork(open: (Set[Condition], Condition), subGoals: Seq[GoalsTree])
