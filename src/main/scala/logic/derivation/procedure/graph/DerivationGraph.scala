package logic.derivation.procedure.graph

import logic.derivation.procedure.{Condition, DerivationError}
import logic.derivation.semantics.{Category, Construction, Morphism, Object, Parameter}
import logic.derivation.semantics.Category.*
import logic.derivation.semantics.Morphism.*
import logic.parsing.ProofStep
import utils.Positive

import scala.annotation.tailrec

class DerivationGraph private(
                               mainGoals: Map[Positive, Seq[Condition]],
                               nodes: Map[Positive, RuleResult],
                               attachments: Map[(Positive, Int), Positive],
                               objectives: Set[(Positive, Int)]
                             ):

  def solve(context: Seq[Condition]): GoalsGraph =
    ???

  def extend(step: (Positive, ProofStep)): DerivationGraph =
    val (stepLine, ProofStep(rule, attach)) = step
    val ruleResult: RuleResult = translate(rule)
    if !objectives.contains(attach) then
      throw DerivationError(s"${attach._1}-${attach._2} is not a goal at line $stepLine")

    val nodes2 = nodes + (stepLine -> ruleResult)
    val attachments2 = attachments + (attach -> stepLine)
    val objectives2 = (objectives - attach) ++ ( 0 until (ruleResult.pre match
      case Some(vect) => vect.size
      case _ => 0) ).map { stepLine -> _ }.toSet
    DerivationGraph(mainGoals, nodes2, attachments2, objectives2)
  
object DerivationGraph:

  class UnificationError
    extends IllegalArgumentException("Unification failed")
  
  private def apply(mainGoals: Map[Positive, Seq[Condition]],
                    nodes: Map[Positive, RuleResult],
                    attachments: Map[(Positive, Int), Positive],
                    objectives: Set[(Positive, Int)]): DerivationGraph =
    new DerivationGraph(mainGoals, nodes, attachments, objectives)

  def apply(goalsMap: Map[Positive, Seq[Condition]]): DerivationGraph =
    DerivationGraph(goalsMap, Map(), Map(), goalsMap.keys.flatMap {
      line => goalsMap(line).indices.map(line -> _) }.toSet)

  def unify(left: Condition, right: Condition): (Condition, Map[Parameter, Construction]) =
    ???

  private def unifyCategories(left: Category,
                              right: Category): (Category, Map[Parameter, Construction]) =
    left match
      case Category.X(param1) => ( right, right match
        case Category.X(param2) if param1 == param2 => Map()
        case _ => Map(param1 -> right) )
      case Category.Base(name1) => right match
        case Category.X(param) => (left, Map(param -> left))
        case Category.Base(name2) => if name1 == name2 then (left, Map())
          else throw UnificationError()

  private def unifyMorphisms(left: Morphism,
                            right: Morphism): (Morphism, Map[Parameter, Construction]) =
    left match
      case Morphism.X(param1) => ( right, right match
        case Morphism.X(param2) if param1 == param2 => Map()
        case _ => Map(param1 -> right) )
      case Identity(obj1) => right match
        case Identity(obj2) =>
          val (res, map) = unifyObjects(obj1, obj2)
          (Identity(res), map)
        case Morphism.X(param) => (left, Map(param -> left))
        case _ => throw UnificationError()

  private def unifyObjects(left: Object,
                           right: Object): (Object, Map[Parameter, Construction]) =
    ???