package logic.derivation.semantics

import logic.derivation.Diagram
import logic.derivation.procedure.*
import logic.derivation.procedure.Condition.TypeJudgement
import logic.parsing.{Concatenation, Construction, Expression, Formula, NameBound}
import logic.parsing.Construction.*
import logic.parsing.NameBound.*
import logic.parsing.Formula.*
import logic.parsing.Type.*
import logic.derivation.semantics.*
import TranslateCapsule.*
import logic.parsing.Concatenation.*
import utils.{Name, Positive}

import scala.annotation.tailrec

class SemanticError(message: String)
  extends IllegalArgumentException(s"Semantic error: $message")


def translateAssList(map: Map[Name, Diagram],
                     types: Map[Name, Type],
                     s: List[Formula]): (List[Condition], Map[Name, Type]) =
  s match
    case h :: tail =>
      val (conds, type_res) = translateFormula(map, types, h)
      val (condsL, type_resL) = translateAssList(map, type_res, tail)
      (condsL ++ conds, type_resL)
    case Nil => (Nil, types)


def translateGoalList(map: Map[Name, Diagram],
                      types: Map[Name, Type],
                      s: List[(Positive, Formula)],
                      process: Set[Condition] = Set()): (Map[Positive, Vector[Condition]], Map[Name, Type]) =
  s match
    case (index, formula) :: tail =>
      val (conds, type_res) = translateFormula(map, types, formula)
      val (condsL, type_resL) = translateGoalList(map, type_res, tail, process ++ conds)
      (conds.diff(process) match
        case remain if remain.nonEmpty => condsL + (index -> remain.toVector)
        case _ => condsL, type_resL)
    case Nil => (Map(), types)

private def translateFormula(map: Map[Name, Diagram],
                     types: Map[Name, Type],
                     f: Formula): (Set[Condition], Map[Name, Type]) =
  f match
    case Include(diagram) =>
      val (conds, newTypes) = translateDiagram(types, map.get(diagram) match
        case Some(d) => d
        case None => throw SemanticError(s"the $diagram diagram cannot be found")
      )
      (conds, newTypes)
    case Expr(exp) =>
      val (cond, newTypes) = translateExpression(types, exp)
      (Set(cond), newTypes)

private def translateDiagram(types: Map[Name, Type],
                     diag: Diagram): (Set[Condition], Map[Name, Type]) =

  def createTypes(morphisms: List[(Object, Object, Morphism)]): Map[Name, Type] =
    morphisms match
      case (dom @ Object.Base(name1), cod @ Object.Base(name2), Morphism.Base(name3)) :: tail =>
        val res = createTypes(tail)
        val r1 = extendTypes(res, name1, ObjectType.Cat(diag.cat))
        val r2 = extendTypes(r1, name2, ObjectType.Cat(diag.cat))
        extendTypes(r2, name3, MorphismType.HomSet(diag.cat, dom, cod))
      case Nil => Map()
      case _ => throw IllegalArgumentException("Diagrams with constructions not yet implemented")

  def diagramDFS(): (Set[Object], Map[Object, Map[Object, Set[Morphism]]]) =

    @tailrec
    def linearVisit(front: List[Object], visited: Set[Object],
                    eqs: Map[Object, Map[Object, Set[Morphism]]]): (Set[Object],
      Map[Object, Map[Object, Set[Morphism]]]) =
        front match
          case head :: tail =>
            val (nVisited, nEqs) = diagramDFS_r(head, visited, eqs)
            linearVisit(tail, nVisited, nEqs)
          case Nil => (visited, eqs)

    def diagramDFS_r(node: Object, visited: Set[Object],
                   eqs: Map[Object, Map[Object, Set[Morphism]]]): (Set[Object],
      Map[Object, Map[Object, Set[Morphism]]]) =

        if visited.contains(node) then (visited, eqs)
        else
          val (nVisited, nEqs) = linearVisit(diag.adjacency(node)
            .toList
            .map(_._2),
            visited + node, eqs)

          val morphs: Map[Object, Set[Morphism]] = diag.adjacency(node).groupBy { _._2 }
            .map { (obj, set) => obj -> set.map { _._1 } }.withDefaultValue(Set())

          val morphs2: Map[Object, Set[Morphism]] =
            (for {
              (cod1, morphSet) <- morphs
              morph1 <- morphSet
              (cod2, morphSet2) <- nEqs(cod1)
              morph2 <- morphSet2
            } yield (cod2, Morphism.Concatenation(morph1, morph2)))
              .groupBy { _._1 }
              .map { (obj, map) => obj ->
                map.map { (obj, morph) => morph }.toSet }.withDefaultValue(Set())

          val morphsMerge: Map[Object, Set[Morphism]] =
            (morphs.keys.toSet ++ morphs2.keys).map {
              num => num -> (morphs(num) ++ morphs2(num))
            }.toMap

          (nVisited, eqs + (node -> morphsMerge))

    linearVisit(diag.adjacency.toList.map(_._1), Set(),
      Map().withDefaultValue(Map().withDefaultValue(Set())))

  // type all the edges and nodes
  val typesAdd: Map[Name, Type] =
    createTypes(
      diag.adjacency.toList.flatMap{ (dom: Object, morphs: Set[(Morphism, Object)])
      => morphs.toList.map { (morph: Morphism, cod: Object) => (dom, cod, morph) }
      }
    )

  //add all missing typing
  val conditionAdd: Set[Condition] =
    typesAdd.keys.toSet.map {
      name => if types.contains(name) then
                if types(name) != typesAdd(name)
                    then throw SemanticError(s"more than one type assigned to $name")
                else None
              else Some( Condition.TypeJudgement(typesAdd(name) match
                case _: ObjectType => Object.Base(name)
                case _: MorphismType => Morphism.Base(name)
                case casted => throw SemanticError(s"unexpected error at Interpreter.scala, line 138"),
                typesAdd(name)) )
    }. collect {
      case Some(x) => x
    }

  val (_, equalityConstraints) = diagramDFS()

  val eqConditions: Set[Condition] = (
    for {
      (_, point) <- equalityConstraints
      (_, set) <- point
      first: Morphism <- set.headOption
      morph: Morphism <- set.tail
    } yield Condition.Equation(Check(first, morph))
  ).toSet

  val newTypes = diag.cat match
    case Category.Base(name) => extendTypes(types ++ typesAdd, name, CategoryType.-)
    case _ => throw SemanticError("diagram definition can't rely on a parametric category")

  (conditionAdd ++ eqConditions, newTypes)

private def createTypeJudge(types: Map[Name, Type], subj: Object | Morphism,
                            typ: logic.parsing.Type): (TypeJudgement, Map[Name, Type]) =
  val (translatedType, newTypes) = translateType(types, typ)
  (subj, translatedType) match
    case (_: Object, _: ObjectType) | (_: Morphism, _: MorphismType)
      => (Condition.TypeJudgement(subj, translatedType), newTypes)
    case _ => throw SemanticError(s"$subj can't be of type $translatedType")

private def createTypeJudge(types: Map[Name, Type], subj: Name,
                    typ: logic.parsing.Type): (TypeJudgement, Map[Name, Type]) =
  val (translatedType, newTypes) = translateType(types, typ)
  val construct: Object | Morphism = translatedType match
    case _: ObjectType => Object.Base(subj)
    case _: MorphismType => Morphism.Base(subj)
    case casted => throw SemanticError(s"a type can't be assigned to $casted")
  (Condition.TypeJudgement(construct, translatedType), extendTypes(newTypes, subj, translatedType))

private def translateExpression(types: Map[Name, Type],
                        e: Expression): (Condition, Map[Name, Type]) =

  e match
    case logic.parsing.Expression.Equation(left, right)
      => (Condition.Equation(Check(
        translateConcatenation(types, left),
        translateConcatenation(types, right))), types)
    case logic.parsing.Expression.TypeJudgement(subj, typ)
      =>
      subj match
        case Leaf(Atomic(Base(name))) =>
          createTypeJudge(types, name, typ)
        case _ =>
          translateConcatenation(types, subj) match
            case x: (Object | Morphism) =>
              createTypeJudge(types, x, typ)
            case _ => throw SemanticError("a category cannot have an assigned type")

private def extendTypes(types: Map[Name, Type],
                        name: Name, ttype: Type): Map[Name, Type] =
  types.get(name) match
    case Some(other) => if other != ttype
      then throw SemanticError(s"more than one type assigned to $name")
      else types
    case _ => types + (name -> ttype)

private def translateConcatenation(types: Map[Name, Type],
                           c: Concatenation): logic.derivation.semantics.Construction =
  c match
    case Binary(lhs, rhs) => Morphism.Concatenation(
      translateConcatenation(types, lhs) match
        case casted: Morphism => casted
        case els => throw SemanticError(s"$els is expected to be a morphism but isn't"), 
      translateConcatenation(types, rhs) match
        case casted: Morphism => casted
        case els => throw SemanticError(s"$els is expected to be a morphism but isn't"))
    case Leaf(const) => translateConstruction(types, const)

private def translateConstruction(types: Map[Name, Type],
                          c: logic.parsing.Construction): logic.derivation.semantics.Construction =
  c match
    case Atomic(name) => translateNameBound(name) match
      case CategoryCapsule(casted) => casted
      case NameCapsule(casted) => types.get(casted) match
        case Some(value) => value match
          case _ : MorphismType => Morphism.Base(casted)
          case _ : ObjectType => Object.Base(casted)
          case _ : CategoryType => Category.Base(casted)
        case _ => throw SemanticError(s"the type of $name is not defined before use")
    case Dom(morph) => translateConcatenation(types, morph) match
      case casted: Morphism => Object.Domain(casted)
      case _ => throw SemanticError(s"the domain of $morph is undefined as it's not a morphism")
    case Cod(morph) => translateConcatenation(types, morph) match
      case casted: Morphism => Object.Codomain(casted)
      case _ => throw SemanticError(s"the codomain of $morph is undefined as it's not a morphism")
    case Id(obj) => translateConstruction(types, obj) match
      case casted: Object => Morphism.Identity(casted)
      case _ => throw SemanticError(s"the identity of $obj is undefined as it's not an object")

private def translateType(types: Map[Name, Type], t: logic.parsing.Type):
    (logic.derivation.semantics.Type, Map[Name, Type]) =
  t match
    case Cat(cat) => translateNameBound(cat) match
      case NameCapsule(casted) => (ObjectType.Cat(Category.Base(casted)),
        extendTypes(types, casted, CategoryType.-))
      case CategoryCapsule(casted) => (ObjectType.Cat(casted), types)
    case HomSet(cat, dom, cod) => translateNameBound(cat) match
      case NameCapsule(casted) => (MorphismType.HomSet(Category.Base(casted),
        translateConcatenation(types, dom) match
          case casted: Object => casted
          case _ => throw SemanticError("a homset is defines only between two objects"),
        translateConcatenation(types, cod) match
          case casted: Object => casted
          case _ => throw SemanticError("a homset is defines only between two objects")),
        extendTypes(types, casted, CategoryType.-))
      case CategoryCapsule(casted) => (ObjectType.Cat(casted), types)

private enum TranslateCapsule:
  case NameCapsule(name: Name)
  case CategoryCapsule(cat: Category)

private def translateNameBound(n: NameBound): TranslateCapsule =
  n match
    case NameBound.Base(name) => NameCapsule(name)
    // TODO: extend

def translateProofStep(types: Map[Name, Type],
                       p: (Positive, logic.parsing.ProofStep)): (Positive, ProofStep) =
  val (pos, logic.parsing.ProofStep(rule, post, map)) = p
  (pos, ProofStep(rule, post, translateMap(types, map)))
  
private def translateMap(types: Map[Name, Type],
                         map: List[(Name, Concatenation)]): Map[Name, logic.derivation.semantics.Construction] =
  map match
    case Nil => Map.empty
    case (name, conc) :: tail => 
      val tMap = translateMap(types, tail)
      val cons = translateConcatenation(types, conc)
      tMap.get(name) match
        case Some(value) if value != cons => throw SemanticError("invalid substitution specified")
        case _ => tMap + (name -> cons)

//def normalizeContext