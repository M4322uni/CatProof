package logic.derivation.procedure.graph

import logic.derivation.procedure.graph.DerivationTree.GoalType
import logic.derivation.procedure.graph.DerivationTree.GoalType.*
import logic.derivation.procedure.graph.GoalsTree.{Fork, Leaf}
import logic.derivation.procedure.{Condition, DerivationError}
import logic.derivation.semantics.{Construction, ProofStep}
import logic.derivation.semantics.ProofStep.*
import logic.parsing.Rule
import utils.{Name, Positive}

class DerivationTree private(
                              mainGoals: Map[Positive, Vector[Condition]],
                              nodes: Map[Positive, RuleConstruct],
                              attachments: Map[(Positive, Int), Positive],
                              objectives: Map[(Positive, Int), GoalType],
                              repeats: Set[(Positive, Int)]
                             ):

  def solve(context: Seq[Condition]): Seq[GoalsTree] =
    if cyclic then
      throw DerivationError("the proof is cyclic")
    val contextSet = context.toSet
    mainGoals.flatMap {
      (p: Positive, vect: Vector[Condition]) => vect.indices.map {
        (i: Int) => constructGraph((p, i), contextSet)
      }
    }.toSeq

  private def cyclic: Boolean =

    def cyclicR(node: Positive,
               visited: Map[Positive, Int]): (Boolean, Map[Positive, Int]) =
      val (cycle, visited2, exit) = visited(node) match
        case 0 => (false, visited.updated(node, 1), false)
        case 1 => (true, visited, true)
        case 2 => (false, visited, true)
      if exit then (cycle, visited2)
      else
        val neighbors: Seq[Int] = nodes(node).pre match
          case vect: Vector[(Set[Condition], Condition)] => vect.indices
          case _ => Seq.empty
        val (rCycle, rVisited) = neighbors.foldLeft((cycle, visited2)) {
          (tuple: (Boolean, Map[Positive, Int]), idx: Int) =>
            val (sCycle, sVisited) = tuple
            if sCycle then tuple
            else
              attachments.get(node, idx) match
                case Some(value) => cyclicR(value, sVisited)
                case None => tuple
        }
        (rCycle, rVisited.updated(node, 2))

    val keyVal: Iterable[(Positive, Int)] = mainGoals.keys.flatMap {
      (key: Positive) =>
        mainGoals(key).indices.map { (key, _) }
    }
    keyVal.foldLeft((false, nodes.keys.map {
      _ -> 0
    }.toMap)) {
      (tuple: (Boolean, Map[Positive, Int]), id: (Positive, Int)) =>
        val (sCycle, sVisited) = tuple
        if sCycle then tuple
        else
          attachments.get(id) match
            case Some(value) => cyclicR(value, sVisited)
            case None => tuple
    }._1

  private def constructGraph(goal: (Positive, Int), context: Set[Condition]): GoalsTree =

    def getIdx(got: Option[RuleConstruct], idx: Int): Option[Int] =
      got match
        case Some(RuleConstruct(vect: Vector[(Set[Condition], Condition)],
          _)) => if vect.size > 1
          then Some(idx) else None
        case None => if mainGoals(goal._1).size > 1
          then Some(idx) else None
        case _ => None

    def getOpen(got: Option[RuleConstruct]): (Set[Condition], Condition) =
      got match
        case Some(RuleConstruct(vect: Vector[(Set[Condition], Condition)], _)) =>
          val (_, post) = vect(goal._2)
          (context, post)
        case Some(RuleConstruct(_, _)) => throw DerivationError("a derivation tree was created " +
          "with an invalid starting point")
        case _ => (context, mainGoals(goal._1)(goal._2))

    val (line, idx) = goal
    val got = nodes.get(goal._1)
    attachments.get(goal) match
      case Some(p) => nodes(p) match
        case RuleConstruct(vect: Vector[(Set[Condition], Condition)], _) =>
          val seq: Seq[GoalsTree] = vect.indices.map {
            (i: Int) =>
              constructGraph((p, i), context ++ vect(i)._1)
          }
          val cCond: Boolean = seq.forall {
            case Fork(_, _, _, _, true) => true
            case Leaf(_, _, _, true) => true
            case _ => false
          }
          Fork(line, getIdx(got, idx), getOpen(got), seq, cCond)
        case RuleConstruct(cond: Boolean, post) => if cond || context.contains(post)
          then Leaf(line, getIdx(got, idx), getOpen(got), true)
          else throw DerivationError(s"wrong derivation for ${goal._1}-${goal._2+1}")// check
      case None =>
        val got = nodes.get(goal._1)
        Leaf(line, getIdx(got, idx), getOpen(got), false)

  def extend(step: (Positive, ProofStep)): DerivationTree =
    step match
      case (stepLine, Use(rule, attach, subst)) => addNode(stepLine, rule, attach, subst)
      case (stepLine, Repeat(attach, from)) => repeat(stepLine, attach, from)

  private def repeat(stepLine: Positive, attach: (Positive, Int),
                     from: Positive): DerivationTree =
    if stepLine <= from then throw DerivationError(s"at line $stepLine, can't reference $from (proof " +
      "not yet specified)")
    val repeated: Condition = nodes.get(from) match
      case Some(value) => value._2
      case _ => throw DerivationError(s"no proof found at line $from")
    objectives.get(attach) match
      case Some(MAIN_GOAL) => mainGoals(attach._1) match
        case vect if vect(attach._2) == repeated =>
        case _ => throw DerivationError(s"the proof step at line $stepLine is invalid")
      case Some(SUBGOAL) => nodes(attach._1) match
        case RuleConstruct(vect: Vector[(Set[Condition], Condition)],
          _) if vect(attach._2)._2 == repeated =>
        case _ => throw DerivationError(s"the proof step at line $stepLine is invalid")
      case _ => throw DerivationError(s"${attach._1}-${attach._2} is not a goal at line $stepLine")
    val attachments2 = attachments + (attach -> from)
    DerivationTree(mainGoals, nodes, attachments2,
      objectives - attach, repeats + attach)

  private def addNode(stepLine: Positive, rule: Rule, attach: (Positive, Int),
                      subst: Map[Name, Construction | Condition]): DerivationTree =
    val ruleResult = RuleConstruct(rule, subst)
    objectives.get(attach) match
      case Some(MAIN_GOAL) => mainGoals(attach._1) match
        case vect if vect(attach._2) == ruleResult.post =>
        case _ => throw DerivationError(s"the proof step at line $stepLine is invalid")
      case Some(SUBGOAL) => nodes(attach._1) match
        case RuleConstruct(vect: Vector[(Set[Condition], Condition)],
          _) if vect(attach._2)._2 == ruleResult.post =>
        case _ => throw DerivationError(s"the proof step at line $stepLine is invalid")
      case _ => throw DerivationError(s"${attach._1}-${attach._2} is not a goal at line $stepLine")
    val nodes2 = nodes + (stepLine -> ruleResult)
    val attachments2 = attachments + (attach -> stepLine)
    val objectives2 = (objectives - attach) ++ (0 until (ruleResult.pre match
      case vect: Vector[(Set[Condition], Condition)] => vect.size
      case _ => 0)).map {
      (stepLine, _) -> SUBGOAL
    }.toMap
    DerivationTree(mainGoals, nodes2, attachments2, objectives2, repeats)

object DerivationTree:

  enum GoalType:
    case MAIN_GOAL
    case SUBGOAL

  class UnificationError
    extends IllegalArgumentException("Unification failed")
  
  private def apply(mainGoals: Map[Positive, Vector[Condition]],
                    nodes: Map[Positive, RuleConstruct],
                    attachments: Map[(Positive, Int), Positive],
                    objectives: Map[(Positive, Int), GoalType],
                    repeats: Set[(Positive, Int)]): DerivationTree =
    new DerivationTree(mainGoals, nodes, attachments, objectives, repeats)

  def apply(goalsMap: Map[Positive, Vector[Condition]]): DerivationTree =
    DerivationTree(goalsMap, Map(), Map(), goalsMap.keys.flatMap {
      line => goalsMap(line).indices.map{ (line, _) -> MAIN_GOAL } }.toMap, Set())