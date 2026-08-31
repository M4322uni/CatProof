package logic.derivation

import logic.derivation.procedure.{Condition, createProof}
import logic.parsing.*
import logic.parsing.Formula.Include
import logic.derivation.semantics.*
import utils.*

type ProofResult = Set[Condition] | Tree | String //TODO

class Proof(body: String, diagrams: Seq[Diagram]):
  
  def apply(): ProofResult =
    def includes(formulas: Seq[(Positive, Formula) | Formula]): Map[Name, Diagram] =
      
      def mapDiagram(name: Name): (Name, Diagram) =
        name -> (diagrams.find(_.name == name) match
          case Some(v) => v
          case _ => throw IllegalArgumentException(s"Parser error: diagram $name not found"))
      
      formulas.collect {
        case (_, Include(name)) => mapDiagram(name)
        case Include(name) => mapDiagram(name)
      }.toMap

    val tst @ Tree(assumptions, goals, proof) = Parser(body)()
    val assIncludes: Map[Name, Diagram] = includes(assumptions)
    val goalIncludes: Map[Name, Diagram] = includes(goals)
    val (context, establishedTypes) = translateAssList(assIncludes, Map(), assumptions)
    val (goalsMap, _) = translateGoalList(goalIncludes, establishedTypes, goals)
    val steps = proof.map { translateProofStep(establishedTypes, _) }
    // create roots of derivation Tree
    // create context
    // run
//    val result: ProofResult = createProof(context, goalsMap, proof)
//    result
    context.toSet
    
object Proof:
  
  def apply(body: String, diagrams: Seq[Diagram]) =
    new Proof(body, diagrams)