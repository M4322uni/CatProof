package logic.derivation.procedure.graph

import logic.derivation.procedure.Condition
import utils.Positive

enum GoalsTree:
  case Leaf(line: Positive, idx: Option[Int], open: (Set[Condition], Condition))
  case Fork(line: Positive, idx: Option[Int], open: (Set[Condition], Condition), subGoals: Seq[GoalsTree])

  override def toString: String = recString()

  private def recString(level: Int = 0): String =
    "\t" * level + (this match
      case Leaf(line, Some(idx), (pre, post)) => s"$line-${idx+1}: $pre ⊢ $post" //prettyprint for sets and conds
      case Leaf(line, None, (pre, post)) => s"$line: $pre ⊢ $post"
      case Fork(line, Some(idx), (pre, post), subs) => s"$line-${idx+1}: $pre ⊢ $post\n"
        + subs.map { _.recString(level+1) }.mkString("\n")
      case Fork(line, None, (pre, post), subs) => s"$line: $pre ⊢ $post\n"
        + subs.map { _.recString(level+1) }.mkString("\n"))
