package logic.derivation.procedure.graph

import logic.derivation.procedure.{Condition, RuleResult}
import logic.parsing.ProofStep
import utils.Positive

class DerivationGraph private(
                               staringGoals: Map[Positive, Condition],
                               nodes: Map[Positive, RuleResult],
                               attachments: Map[(Positive, Int), Positive],
                               objectives: Set[Positive]
                             ):


  def extend(step: (Positive, ProofStep)): DerivationGraph =
    val (line, ProofStep(rule, attach)) = step
    //translate rule
    ???
  
object DerivationGraph:
  
  ???