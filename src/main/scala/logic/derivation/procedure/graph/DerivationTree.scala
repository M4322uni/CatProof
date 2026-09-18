package logic.derivation.procedure.graph

import logic.derivation.procedure.graph.DerivationTree.GoalType
import logic.derivation.procedure.graph.DerivationTree.GoalType.*
import logic.derivation.procedure.graph.GoalsTree.{Fork, Leaf}
import logic.derivation.procedure.{Condition, DerivationError}
import logic.derivation.semantics.ProofStep
import utils.Positive

class DerivationTree private(
                               mainGoals: Map[Positive, Vector[Condition]],
                               nodes: Map[Positive, RuleResult],
                               attachments: Map[(Positive, Int), Positive],
                               objectives: Map[(Positive, Int), GoalType]
                             ):

  def solve(context: Seq[Condition]): Seq[GoalsTree] =
    val contextSet = context.toSet
    mainGoals.flatMap {
      (p: Positive, vect: Vector[Condition]) => vect.indices.map {
        (i: Int) => constructGraph((p, i), contextSet)
      }
    }.collect {
      case casted: GoalsTree => casted
    }.toSeq

  private def constructGraph(goal: (Positive, Int), context: Set[Condition]): GoalsTree | Unit =
    val (line, idx) = goal

    def getIdx(got: Option[RuleResult]): Option[Int] =
      got match
        case Some(RuleResult(vect: Vector[(Set[Condition], Condition)], 
          _)) => if vect.size > 1
          then Some(idx) else None
        case None => if mainGoals(goal._1).size > 1
          then Some(idx) else None
        case _ => None

    attachments.get(goal) match
      case Some(p) => nodes(p) match
        case RuleResult(vect: Vector[(Set[Condition], Condition)], 
          _) => vect.indices.map {
          (i: Int) =>
            constructGraph((p, i), context ++ vect(i)._1)
        }.collect {
          case casted: GoalsTree => casted
        } match
          case seq if seq.isEmpty => ()
          case seq =>
            val got = nodes.get(goal._1)
            Fork(line, getIdx(got), got match
              case Some(RuleResult(vect: Vector[(Set[Condition], Condition)], _)) =>
                val (_, post) = vect(goal._2)
                (context, post)
              case Some(RuleResult(_, _)) => throw DerivationError("a derivation tree was created" +
                "with an invalid starting point")
              case _ => (context, mainGoals(goal._1)(goal._2)), seq)
        case RuleResult(cond: Boolean, post) => if cond || context.contains(post)
          then () else throw DerivationError(s"wrong derivation for ${goal._1}-${goal._2+1}")// check
      case None =>
        val got = nodes.get(goal._1)
        Leaf(line, getIdx(got), got match
          case Some(RuleResult(vect: Vector[(Set[Condition], Condition)], _)) =>
            val (_, post) = vect(goal._2)
            (context, post)
          case Some(RuleResult(_, _)) => throw DerivationError("a derivation tree was created" +
            "with an invalid starting point")
          case _ => (context, mainGoals(goal._1)(goal._2)))

  def extend(step: (Positive, ProofStep)): DerivationTree =
    val (stepLine, ProofStep(rule, attach, subst)) = step
    val ruleResult = RuleResult(rule, subst)
    objectives.get(attach) match
      case None => throw DerivationError(s"${attach._1}-${attach._2} is not a goal at line $stepLine")
      case Some(MAIN_GOAL) => mainGoals(attach._1) match
        case vect if vect(attach._2) == ruleResult.post =>
        case _ => throw DerivationError(s"the proof step at line $stepLine is invalid")
      case Some(SUBGOAL) => nodes(attach._1) match
        case RuleResult(vect: Vector[(Set[Condition], Condition)], 
          _) if vect(attach._2)._2 == ruleResult.post =>
        case _ => throw DerivationError(s"the proof step at line $stepLine is invalid")
    val nodes2 = nodes + (stepLine -> ruleResult)
    val attachments2 = attachments + (attach -> stepLine)
    val objectives2 = (objectives - attach) ++ ( 0 until (ruleResult.pre match
      case vect: Vector[(Set[Condition], Condition)] => vect.size
      case _ => 0) ).map { (stepLine, _) -> SUBGOAL }.toMap
    DerivationTree(mainGoals, nodes2, attachments2, objectives2)
  
object DerivationTree:

  enum GoalType:
    case MAIN_GOAL
    case SUBGOAL

  class UnificationError
    extends IllegalArgumentException("Unification failed")
  
  private def apply(mainGoals: Map[Positive, Vector[Condition]],
                    nodes: Map[Positive, RuleResult],
                    attachments: Map[(Positive, Int), Positive],
                    objectives: Map[(Positive, Int), GoalType]): DerivationTree =
    new DerivationTree(mainGoals, nodes, attachments, objectives)

  def apply(goalsMap: Map[Positive, Vector[Condition]]): DerivationTree =
    DerivationTree(goalsMap, Map(), Map(), goalsMap.keys.flatMap {
      line => goalsMap(line).indices.map{ (line, _) -> MAIN_GOAL } }.toMap)