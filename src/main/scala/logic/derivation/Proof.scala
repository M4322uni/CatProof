package logic.derivation

import logic.derivation.procedure.graph.GoalsTree.{Fork, Leaf, Quality}
import logic.derivation.procedure.graph.GoalsTree.Quality.*
import logic.derivation.procedure.graph.{DerivationTree, GoalsTree}
import logic.parsing.*
import logic.parsing.Formula.Include
import logic.derivation.semantics.*
import utils.*

class Proof(body: String, diagrams: Seq[Diagram]):

  def apply(): Seq[(Quality, String)] =
    def includes(formulas: Iterable[(Positive, Formula) | Formula]): Map[Name, Diagram] =
      
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
    val startingTree = DerivationTree(goalsMap)
    val finalTree = steps.foldLeft(startingTree) { (graph, step) => graph.extend(step) }
    val objectives = finalTree.solve(context)

    printout(objectives)

  private def printout(objectives: Seq[GoalsTree]): Seq[(Quality, String)] =
    objectives match
      case seq if seq.isEmpty => Seq((BASELINE, "Waiting for user input..."))
      case _ =>
        Seq( (BASELINE, "Derivation tree:\n") ) ++ (
        objectives.reverse
          .map {
            _.recString()
          }
          ++ (
          if objectives.forall {
            case Leaf(_, _, _, true) => true
            case Fork(_, _, _, _, true) => true
            case _ => false
          }
          then Seq(Seq((BASELINE, "    QED")))
          else Seq()
          ))
        .reduce {
          (s1: Seq[(Quality, String)], s2: Seq[(Quality, String)]) =>
            s1 ++ Seq((BASELINE, "\n\n    ---\n\n")) ++ s2
        }
    
object Proof:
  
  def apply(body: String, diagrams: Seq[Diagram]) =
    new Proof(body, diagrams)