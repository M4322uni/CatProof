package logic.derivation.procedure.graph

import logic.derivation.procedure.{Condition, DerivationError}
import logic.derivation.procedure.graph.DerivationGraph.Goal
import logic.derivation.procedure.graph.DerivationGraph.Goal.*
import logic.parsing.ProofStep
import utils.Positive

class DerivationGraph private(
                               mainGoals: Map[Positive, Seq[Condition]],
                               nodes: Map[Positive, RuleResult],
                               attachments: Map[(Positive, Int), Positive],
                               objectives: Set[(Positive, Int)]
                             ):
  
  def solve(context: Seq[Condition]): GoalsGraph = ???

  def extend(step: (Positive, ProofStep)): DerivationGraph =
    val (stepLine, ProofStep(rule, attach)) = step
    val ruleResult: RuleResult = ??? // translate rule
    if !objectives.contains(attach) then
      throw DerivationError(s"${attach._1}-${attach._2} is not a goal at line $stepLine")

    val nodes2 = nodes + (stepLine -> ruleResult)
    val attachments2 = attachments + (attach -> stepLine)
    val objectives2 = (objectives - attach) ++ ( 0 until (ruleResult.pre match
      case Some(vect) => vect.size
      case _ => 0) ).map { stepLine -> _ }.toSet
    DerivationGraph(mainGoals, nodes2, attachments2, objectives2)
  
object DerivationGraph:

  enum Goal:
    case MAIN_GOAL
    case SUBGOAL
  
  private def apply(mainGoals: Map[Positive, Seq[Condition]],
                    nodes: Map[Positive, RuleResult],
                    attachments: Map[(Positive, Int), Positive],
                    objectives: Set[(Positive, Int)]): DerivationGraph =
    new DerivationGraph(mainGoals, nodes, attachments, objectives)

  def apply(goalsMap: Map[Positive, Seq[Condition]]): DerivationGraph =
    DerivationGraph(goalsMap, Map(), Map(), goalsMap.keys.flatMap {
      line => goalsMap(line).indices.map(line -> _) }.toSet)