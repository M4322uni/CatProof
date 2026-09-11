package logic.derivation.procedure.graph

import logic.derivation.procedure.graph.DerivationGraph.GoalType
import logic.derivation.procedure.graph.DerivationGraph.GoalType.*
import logic.derivation.procedure.graph.GoalsTree.{Fork, Leaf}
import logic.derivation.procedure.{Condition, DerivationError}
import logic.derivation.semantics.ProofStep
import utils.Positive

class DerivationGraph private(
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
    attachments.get(goal) match
      case Some(p) => nodes(p) match
        case RuleResult(Some(vect), _) => vect.indices.map {
          (i: Int) =>
            constructGraph((p, i), nodes(p) match
              case RuleResult(Some(vect), _) => context ++ vect(i)._1
              case RuleResult(None, _) => throw DerivationError("a derivation tree was created" +
                "with an invalid starting point"))
        }.collect {
          case casted: GoalsTree => casted
        } match
          case seq if seq.isEmpty => ()
          case seq => Fork(nodes(goal._1) match
            case RuleResult(Some(vect), _) => vect(goal._2)
            case RuleResult(None, _) => throw DerivationError("a derivation tree was created" +
              "with an invalid starting point"), seq)
        case RuleResult(None, post) => if context.contains(post)
          then () else throw DerivationError(s"wrong derivation for $goal")// check
      case None => Leaf(nodes(goal._1) match
        case RuleResult(Some(vect), _) => vect(goal._2)
        case RuleResult(None, _) => throw DerivationError("a derivation tree was created" +
          "with an invalid starting point"))

  def extend(step: (Positive, ProofStep)): DerivationGraph =
    val (stepLine, ProofStep(rule, attach, subst)) = step
    val ruleResult = RuleResult(rule, subst)
    objectives.get(attach) match
      case None => throw DerivationError(s"${attach._1}-${attach._2} is not a goal at line $stepLine")
      case Some(MAIN_GOAL) => mainGoals(attach._1) match
        case vect if vect(attach._2) == ruleResult.post =>
        case _ => throw DerivationError(s"the proof step at line $stepLine is invalid")
      case Some(SUBGOAL) => nodes(attach._1) match
        case RuleResult(Some(vect), _) if vect(attach._2)._2 == ruleResult.post =>
        case _ => throw DerivationError(s"the proof step at line $stepLine is invalid")
    val nodes2 = nodes + (stepLine -> ruleResult)
    val attachments2 = attachments + (attach -> stepLine)
    val objectives2 = (objectives - attach) ++ ( 0 until (ruleResult.pre match
      case Some(vect) => vect.size
      case _ => 0) ).map { (stepLine, _) -> SUBGOAL }.toMap
    DerivationGraph(mainGoals, nodes2, attachments2, objectives2)
  
object DerivationGraph:

  enum GoalType:
    case MAIN_GOAL
    case SUBGOAL

  class UnificationError
    extends IllegalArgumentException("Unification failed")
  
  private def apply(mainGoals: Map[Positive, Vector[Condition]],
                    nodes: Map[Positive, RuleResult],
                    attachments: Map[(Positive, Int), Positive],
                    objectives: Map[(Positive, Int), GoalType]): DerivationGraph =
    new DerivationGraph(mainGoals, nodes, attachments, objectives)

  def apply(goalsMap: Map[Positive, Vector[Condition]]): DerivationGraph =
    DerivationGraph(goalsMap, Map(), Map(), goalsMap.keys.flatMap {
      line => goalsMap(line).indices.map{ (line, _) -> MAIN_GOAL } }.toMap)