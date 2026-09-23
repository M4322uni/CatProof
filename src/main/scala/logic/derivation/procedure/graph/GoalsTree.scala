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

  def recString(mode: Mode, contexts: List[Set[Condition]],
                prefix: String = "    ", connector: String = ""):
                (Seq[(Quality, String)], List[Set[Condition]]) =
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

    val (firstS, firstC) = setPrint(pre, mode, contexts)
    val (subS, subC) = subs.zipWithIndex.foldLeft(Seq.empty[(Quality, String)], firstC) {
      (t1: (Seq[(Quality, String)], List[Set[Condition]]),
       t2: (GoalsTree, Int)) =>
        val (rSeq, rCont) = t1
        val (newGoal, idx) = t2
        val (str: Seq[(Quality, String)], nContexts: List[Set[Condition]]) =
          newGoal.recString(
            mode, rCont,
            childPrefix, if idx == subs.size - 1 then "└── " else "├── "
          )
        ( rSeq ++ Seq((BASELINE, "\n")) ++ str, nContexts )
    }

    ( Seq(
      (BASELINE, prefix + connector),
      (if closed then PROVEN else UNPROVEN, s"$label: $firstS ⊢ $post")
    ) ++ subS, subC )

object GoalsTree:

  enum Quality:
    case BASELINE
    case PROVEN
    case UNPROVEN

  private[derivation] def simplePrint(s: Set[Condition]): String =
    if s.isEmpty then "∅" else s.mkString(", ")

  private[derivation] def subscript(n: Int): String =
    n.toString.map(digit => "₀₁₂₃₄₅₆₇₈₉"(digit - '0'))

  private def setPrint(s: Set[Condition], mode: Mode,
                       contexts: List[Set[Condition]]): (String, List[Set[Condition]]) =
    mode match
      case VERBOSE =>
        (simplePrint(s), contexts)
      case TYPINGS =>
        (simplePrint(s.collect {
          case casted: TypeJudgement => casted
        }), contexts)
      case EQUATIONS =>
        (simplePrint(s.collect {
          case casted: Equation => casted
        }), contexts)
      case SYMBOLIC =>
        contexts.indexOf(s) match
          case -1 =>
            val nContexts: List[Set[Condition]] =
              contexts :+ s
            (s"Γ${subscript(nContexts.length)}", nContexts)
          case els =>
            (s"Γ${subscript(els+1)}", contexts)
