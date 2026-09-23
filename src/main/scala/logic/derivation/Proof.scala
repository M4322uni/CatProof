package logic.derivation

import logic.derivation.Proof.Mode
import logic.derivation.Proof.Mode.SYMBOLIC
import logic.derivation.procedure.Condition
import logic.derivation.procedure.graph.GoalsTree.{Fork, Leaf, Quality}
import logic.derivation.procedure.graph.GoalsTree.Quality.*
import logic.derivation.procedure.graph.{DerivationTree, GoalsTree}
import logic.parsing.*
import logic.parsing.Formula.Include
import logic.derivation.semantics.*
import utils.*

class Proof(body: String, diagrams: Seq[Diagram]):

  def apply(mode: Mode): Seq[(Quality, String)] =
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

    printout(objectives, mode)

  private def printout(objectives: Seq[GoalsTree], mode: Mode): Seq[(Quality, String)] =
    objectives match
      case seq if seq.isEmpty => Seq((BASELINE, "Waiting for user input..."))
      case _ =>
        val (fSeq, fContexts) = objectives.reverse
          .foldLeft(Seq.empty[Seq[(Quality, String)]], List.empty[Set[Condition]]) {
            (t1: (Seq[Seq[(Quality, String)]], List[Set[Condition]]),
              goal: GoalsTree) =>
              val (seq, contexts) = t1
              val (nSeq, nContexts) = goal.recString(mode, contexts)
              (seq ++ Seq(nSeq), nContexts)
          }

        Seq( (BASELINE, "Derivation tree:\n") ) ++ (
        fSeq ++ (
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
        ++ Seq( (BASELINE, "\n\n") )
        ++ ( if mode == SYMBOLIC then printGammas(fContexts) else Seq() )

  private def printGammas(gammas: List[Set[Condition]]): Seq[(Quality, String)] =

    Seq( (BASELINE, "Contexts:\n") )
    ++ gammas.indices.map {
      (idx: Int) =>
        (BASELINE,
          s"    Γ${GoalsTree.subscript(idx+1)} = ${GoalsTree.simplePrint(gammas(idx))}\n")
    }
    
object Proof:

  enum Mode:
    case VERBOSE
    case TYPINGS
    case EQUATIONS
    case SYMBOLIC

  def apply(body: String, diagrams: Seq[Diagram]) =
    new Proof(body, diagrams)
