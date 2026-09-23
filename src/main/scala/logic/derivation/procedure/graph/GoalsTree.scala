package logic.derivation.procedure.graph

import logic.derivation.Proof.Mode
import logic.derivation.Proof.Mode.*
import logic.derivation.procedure.Condition
import logic.derivation.procedure.Condition.*
import logic.derivation.procedure.graph.GoalsTree.*
import logic.derivation.procedure.graph.GoalsTree.Quality.*
import utils.Positive

enum GoalsTree:
  case Leaf(line: Positive, idx: Option[Int],
            open: (Set[Condition], Condition), closed: Boolean)
  case Fork(line: Positive, idx: Option[Int], open: (Set[Condition], Condition),
            subGoals: Seq[GoalsTree], closed: Boolean)

  def recString(mode: Mode, context: Vector[Set[Condition]],
                prefix: String = "    ", connector: String = ""): Seq[(Quality, String)] =
    val (line, idx, (pre, post), subs, closed) = this match
      case Leaf(line, idx, open, closed) =>
        (line, idx, open, Seq.empty[GoalsTree], closed)
      case Fork(line, idx, open, subs, closed) =>
        (line, idx, open, subs, closed)

    val label = idx.fold(s"$line")(i => s"$line-${i+1}")
    val childPrefix = prefix + (connector match
      case "" => ""
      case "└── " => "    "
      case _ => "│   ")

    Seq(
      (BASELINE, prefix + connector),
      (if closed then PROVEN else UNPROVEN, s"$label: ${setPrint(pre, mode, context)} ⊢ $post")
    ) ++ subs.zipWithIndex.flatMap { (sub, i) =>
      Seq((BASELINE, "\n")) ++ sub.recString(
        mode, context,
        childPrefix, if i == subs.size - 1 then "└── " else "├── "
      )
    }

object GoalsTree:

  enum Quality:
    case BASELINE
    case PROVEN
    case UNPROVEN

  private def setPrint(s: Set[Condition], mode: Mode,
                       context: Vector[Set[Condition]]): String =

    def simplePrint(s: Set[Condition]): String =
      if s.isEmpty then "∅" else s.mkString(", ")

    mode match
      case VERBOSE =>
        simplePrint(s)
      case TYPINGS =>
        simplePrint(s.collect {
          case casted: TypeJudgement => casted
        })
      case EQUATIONS =>
        simplePrint(s.collect {
          case casted: Equation => casted
        })
      case SYMBOLIC => ???
