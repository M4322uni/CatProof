package logic.derivation

import logic.parsing.*
import logic.parsing.Formula.Include
import utils.*

type ProofResult = Tree | String //TODO

class Proof(body: String, diagrams: Seq[Diagram]):
  
  def apply(): ProofResult =
    def includes(formulas: Seq[(Positive, Formula)]): Set[Name] =
      formulas.collect {
        case (_, Include(name)) => name
      }.toSet

    def map(includes: Set[Name]): Map[Name, Diagram] =
      diagrams.collect {
        case diag if includes.contains(diag.name)
        => diag.name -> diag
      }.toMap

    val Tree(assumptions, goals, proof) = Parser(body)()
    val assIncludes: Set[Name] = includes(assumptions)
    val goalIncludes: Set[Name] = includes(assumptions)
    val assMap: Map[Name, Diagram] = map(assIncludes)
    val goalMap: Map[Name, Diagram] = map(goalIncludes)

    "ciao"
    
object Proof:
  
  def apply(body: String, diagrams: Seq[Diagram]) =
    new Proof(body, diagrams)