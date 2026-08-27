package logic.derivation.semantics

import logic.derivation.Diagram
import logic.derivation.derivationTree.*
import logic.derivation.derivationTree.Condition.TypeJudgement
import logic.parsing.{Concatenation, Construction, Expression, Formula, NameBound}
import logic.parsing.Construction.*
import logic.parsing.NameBound.*
import logic.parsing.Formula.*
import logic.parsing.Type.*
import logic.derivation.semantics.*
import TranslateCapsule.*
import utils.{Name, Positive}

import scala.annotation.tailrec

class SemanticError(message: String)
  extends IllegalArgumentException(s"Semantic error: $message")


def translateFormulaList(map: Map[Name, Diagram],
                         types: Map[Name, Type],
                         s: Seq[(Positive, Formula)]): Map[Positive, Set[Condition]] =
  s match
    case h :: tail =>
      val (conds, type_res) = translateFormula(map, types, h)
      translateFormulaList(map, type_res, tail) + conds
    case Nil => Map()

def translateFormula(map: Map[Name, Diagram],
                     types: Map[Name, Type],
                     tup: (Positive, Formula)): ((Positive, Set[Condition]), Map[Name, Type]) =
  val (p, f) = tup
  f match
    case Include(diagram) =>
      val (conds, newTypes) = translateDiagram(types, map.get(diagram) match
        case Some(d) => d
        case None => throw SemanticError(s"the $diagram diagram cannot be found")
      )
      (p -> conds, newTypes)
    case Expr(exp) =>
      val (cond, newTypes) = translateExpression(types, exp)
      (p -> Set(cond), newTypes)

def translateDiagram(types: Map[Name, Type],
                     diag: Diagram): (Set[Condition], Map[Name, Type]) =

  def diagramDFS(): (Set[Object], Map[Object, Map[Object, Set[Morphism]]]) =

      @tailrec
      def linearVisit(front: Seq[Object], visited: Set[Object],
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
              .toSeq
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
              } yield (cod2, Morphism.Concatenation(morph2 match
                case Morphism.Concatenation(seq) => morph1 +: seq
                case _ => Seq(morph1, morph2))))
                .groupBy { _._1 }
                .map { (obj, map) => obj ->
                  map.map { (obj, morph) => morph }.toSet }.withDefaultValue(Set())

            val morphsMerge: Map[Object, Set[Morphism]] =
              (morphs.keys.toSet ++ morphs2.keys.toSet).map {
                num => num -> (morphs(num) ++ morphs2(num))
              }.toMap

            (nVisited, eqs + (node -> morphsMerge))

      linearVisit(diag.adjacency.toSeq.map(_._1), Set(),
        Map().withDefaultValue(Map().withDefaultValue(Set())))

  // type all the edges and nodes
  val typesAdd: Map[Name, Type] = diag.adjacency.flatMap { (dom: Object, morphs: Set[(Morphism, Object)])
    =>
    morphs.map { (morph: Morphism, cod: Object) =>
      morph match
        case Morphism.Base(name) =>
          name -> MorphismType.HomSet(diag.cat, dom, cod)
        case _ => throw NotImplementedError("Diagrams with constructions not yet implemented")
    }
      + ( dom match
        case Object.Base(name) => name -> ObjectType.Cat(diag.cat)
        case _ => throw NotImplementedError("Diagrams with constructions not yet implemented")
      )
  } // da rivedere: i codomini non vengono tipati e bisognerebbe controllare le inconsistenze
  // ad es. oggetti o morfismi con più tipi

  //add all missing typing
  val conditionAdd: Set[Condition] =
    typesAdd.keys.toSet.map {
      name => if types.contains(name) then
                if types(name) != typesAdd(name)
                    then
                      println(types(name))
                      println(typesAdd(name))
                      throw SemanticError(s"more than one type assigned to $name")
                else ()
              Condition.TypeJudgement(name, typesAdd(name))
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

  (conditionAdd ++ eqConditions, types ++ typesAdd)

def translateExpression(types: Map[Name, Type],
                        e: Expression): (Condition, Map[Name, Type]) =
  e match
    case logic.parsing.Expression.Equation(left, right)
      => (Condition.Equation(Check(
        translateConcatenation(types, left),
        translateConcatenation(types, right))), types)
    case logic.parsing.Expression.TypeJudgement(subj, typ)
      =>
      val (translatedType, newTypes) = translateType(types, typ)
      val judge: TypeJudgement = Condition.TypeJudgement(subj, translatedType)
      (judge, extendTypes(newTypes, judge))

def extendTypes(types: Map[Name, Type], judge: TypeJudgement): Map[Name, Type] =
  val TypeJudgement(name, ttype) = judge
  extendTypes(types, name, ttype)

def extendTypes(types: Map[Name, Type], name: Name, ttype: Type): Map[Name, Type] =
  types.get(name) match
    case Some(other) => if other != ttype
      then throw SemanticError(s"more than one type assigned to $name")
      else types
    case _ => types + (name -> ttype)

def translateConcatenation(types: Map[Name, Type],
                           c: Concatenation): logic.derivation.semantics.Construction =
  c.constructions match
    case Nil => throw IllegalArgumentException("Parser error: a concatenation of zero elements was parsed")
    case Seq(c) => translateConstruction(types, c)
    case seq => Morphism.Concatenation(seq.map {
      translateConstruction(types, _) match
        case casted: Morphism => casted
        case b => throw SemanticError(s"$b is expected to be a morphism but isn't")
    })

def translateConstruction(types: Map[Name, Type],
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
      case _ => throw NotImplementedError("Non-basic categories are not yet implemented")
    case Dom(morph) => translateConcatenation(types, morph) match
      case casted: Morphism => Object.Domain(casted)
      case _ => throw SemanticError(s"the domain of $morph is undefined as it's not a morphism")
    case Cod(morph) => translateConcatenation(types, morph) match
      case casted: Morphism => Object.Codomain(casted)
      case _ => throw SemanticError(s"the codomain of $morph is undefined as it's not a morphism")
    case Id(obj) => translateConstruction(types, obj) match
      case casted: Object => Morphism.Identity(casted)
      case _ => throw SemanticError(s"the identity of $obj is undefined as it's not an object")

def translateType(types: Map[Name, Type], t: logic.parsing.Type):
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

enum TranslateCapsule:
  case NameCapsule(name: Name)
  case CategoryCapsule(cat: Category)

def translateNameBound(n: NameBound): TranslateCapsule =
  n match
    case NameBound.Base(name) => NameCapsule(name)
    // TODO: extend