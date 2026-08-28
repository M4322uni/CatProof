package logic.derivation

import logic.derivation.derivationTree.Condition
import logic.parsing.*
import logic.parsing.Formula.Include
import logic.derivation.semantics.*
import utils.*

type ProofResult = Set[Condition] | Tree | String //TODO

class Proof(body: String, diagrams: Seq[Diagram]):
  
  def apply(): ProofResult =
    def includes(formulas: Seq[(Positive, Formula)]): Map[Name, Diagram] =
      formulas.collect {
        case (_, Include(name)) => name -> (diagrams.find(_.name == name) match
          case Some(v) => v
          case _ => throw IllegalArgumentException(s"Parser error: diagram $name not found"))
      }.toMap

    val tst @ Tree(assumptions, goals, proof) = Parser(body)()
    val assIncludes: Map[Name, Diagram] = includes(assumptions)
    val goalIncludes: Map[Name, Diagram] = includes(goals)
    val (context, extablishedTypes) = translateFormulaList(assIncludes, Map(), assumptions)
    val (goalsMap, _) = translateFormulaList(goalIncludes, extablishedTypes, goals)
    // create roots of derivation Tree
    // create context
    // run
    val result: ProofResult = createProof(context, goalsMap.flatMap(_._2).toSet, proof)
    result
    
object Proof:
  
  def apply(body: String, diagrams: Seq[Diagram]) =
    new Proof(body, diagrams)