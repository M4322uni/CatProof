package logic.derivation

import logic.derivation.derivationTree.Condition
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

    def map(includes: Set[Name]): Set[Diagram] =
      diagrams.collect {
        case diag if includes.contains(diag.name)
        => diag
      }.toSet

    val tst @ Tree(assumptions, goals, proof) = Parser(body)()
    val assIncludes: Set[Name] = includes(assumptions)
    val goalIncludes: Set[Name] = includes(goals)
    val assSet: Set[Diagram] = map(assIncludes)
    val goalSet: Set[Diagram] = map(goalIncludes)
    val context: Map[Positive, Set[Condition]] = ???
    // create roots of derivation Tree
    // create context
    // run

    tst
    
object Proof:
  
  def apply(body: String, diagrams: Seq[Diagram]) =
    new Proof(body, diagrams)