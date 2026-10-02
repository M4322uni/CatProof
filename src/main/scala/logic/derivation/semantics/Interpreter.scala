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
import logic.derivation.Diagram.Node
import logic.derivation.semantics.Construction.{Morph, Obj}
import logic.derivation.semantics.ProofStep.*
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
      case Nil => diag.adjacency.keys.map{ _.content }.map {
        case Object.Base(name) => name -> ObjectType.Cat(diag.cat)
        case _ => throw IllegalArgumentException("Diagrams with constructions not yet implemented")
      }.toMap
      case _ => throw IllegalArgumentException("Diagrams with constructions not yet implemented")

  def catBellman(step: Int): (Boolean, Map[Node, Map[Node,
    Set[(Morphism, Set[(Morphism, Node, Node)])]]]) =
    step match
      case 0 | 1 =>
        (step == 0, diag.adjacency.map { (node1, set) => node1 -> set.groupBy { _._2 }
          .map { (node2, set) => node2 -> set.map { (morph, _) => (morph, Set((morph, node1, node2))) } } })
      case i if i > 1 =>
        val (cut, rec) = catBellman(step-1)
        if cut then (true, rec)
        else
          val nTab: Iterable[(Node, Node, Morphism, Set[(Morphism, Node, Node)])] =
            (for {
              key1 <- rec.keys
              (morph1, neighbor) <- diag.adjacency(key1)
              key2 <- rec(neighbor).keys
              (morph2, checkSet) <- rec(neighbor)(key2)
            }
            yield {
              if !checkSet.contains((morph1, key1, neighbor)) then
                Some(key1, key2,
                  Morphism.Concatenation(morph1, morph2), checkSet + ((morph1, key1, neighbor)))
              else None
            }).collect {
              case Some(value: (Node, Node, Morphism, Set[(Morphism, Node, Node)])) => value
            }
          val uTab: Map[Node, Map[Node, Set[(Morphism, Set[(Morphism, Node, Node)])]]] =
            nTab.foldLeft(rec) { case (table, (from, to, morph, checkSet)) =>
              val destinations = table(from)
              val paths = destinations.getOrElse(to, Set.empty) + (morph -> checkSet)
              table.updated(from, destinations.updated(to, paths))
            }
          ((for {
            key1 <- uTab.keys
            key2 <- uTab(key1).keys
          } yield {
            rec(key1).get(key2) match
              case Some(set) =>
                set.size == uTab(key1)(key2).size
              case _ => false
          }).forall { identity }, uTab)
      case _ => throw DerivationError("error in diagram translation")

  // type all the edges and nodes
  val typesAdd: Map[Name, Type] =
    createTypes(
      diag.adjacency.toList.flatMap{ (dom: Node, morphs: Set[(Morphism, Node)])
      => morphs.toList.map { (morph: Morphism, cod: Node) => (dom.content, cod.content, morph) }
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
                case casted: ObjectType => TCheck(Object.Base(name), casted)
                case casted: MorphismType => TCheck(Morphism.Base(name), casted)
                case casted => throw SemanticError(s"unexpected error at Interpreter.scala, line 138")))
    }. collect {
      case Some(x) => x
    }

  val (_, equalityConstraints) = catBellman(diag.adjacency.map { _._2.size }.sum)

  val eqConditions: Set[Condition] = (
    for {
      (_, point) <- equalityConstraints
      (_, disc) <- point
      set = disc.map { _._1 }
      first: Morphism <- set.headOption
      morph: Morphism <- set.tail
    } yield Condition.Equation(ECheck(Morph(first), Morph(morph)))
  ).toSet

  val newTypes = diag.cat match
    case Category.Base(name) => extendTypes(types ++ typesAdd, name, CategoryType.-)
    case _ => throw SemanticError("diagram definition can't rely on a parametric category")

  (conditionAdd ++ eqConditions, newTypes)

private def createTypeJudge(types: Map[Name, Type], subj: Object | Morphism,
                            typ: logic.parsing.Type): (TypeJudgement, Map[Name, Type]) =
  val (translatedType, newTypes) = translateType(types, typ)
  ((subj, translatedType) match
    case (obj: Object, typ: ObjectType) 
      => Condition.TypeJudgement(TCheck(obj, typ))
    case (morph: Morphism, typ: MorphismType)
      => Condition.TypeJudgement(TCheck(morph, typ))
    case _ => throw SemanticError(s"$subj can't be of type $translatedType"), newTypes)

private def createTypeJudge(types: Map[Name, Type], subj: Name,
                    typ: logic.parsing.Type): (TypeJudgement, Map[Name, Type]) =
  val (translatedType, newTypes) = translateType(types, typ)
  val construct: TypeJudgement = translatedType match
    case casted: ObjectType => TypeJudgement(TCheck(Object.Base(subj), casted))
    case casted: MorphismType => TypeJudgement(TCheck(Morphism.Base(subj), casted))
    case casted => throw SemanticError(s"a type can't be assigned to $casted")
  (construct, extendTypes(newTypes, subj, translatedType))

private def translateExpression(types: Map[Name, Type],
                        e: Expression): (Condition, Map[Name, Type]) =

  e match
    case logic.parsing.Expression.Equation(left, right)
      => (Condition.Equation(ECheck(
        translateConcatenation(types, left),
        translateConcatenation(types, right))), types)
    case logic.parsing.Expression.TypeJudgement(subj, typ)
      =>
      subj match
        case Leaf(Atomic(Base(name))) =>
          createTypeJudge(types, name, typ)
        case _ =>
          translateConcatenation(types, subj) match
            case Obj(x) => createTypeJudge(types, x, typ)
            case Morph(x) => createTypeJudge(types, x, typ)
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
    case Binary(lhs, rhs) => Morph(Morphism.Concatenation(
      translateConcatenation(types, lhs) match
        case Morph(casted) => casted
        case els => throw SemanticError(s"$els is expected to be a morphism but isn't"), 
      translateConcatenation(types, rhs) match
        case Morph(casted) => casted
        case els => throw SemanticError(s"$els is expected to be a morphism but isn't")))
    case Leaf(const) => translateConstruction(types, const)

private def translateConstruction(types: Map[Name, Type],
                          c: logic.parsing.Construction): logic.derivation.semantics.Construction =
  c match
    case Atomic(name) => translateNameBound(name) match
      case CategoryCapsule(casted) => logic.derivation.semantics.Construction.Cat(casted)
      case NameCapsule(casted) => types.get(casted) match
        case Some(value) => value match
          case _ : MorphismType => Morph(Morphism.Base(casted))
          case _ : ObjectType => Obj(Object.Base(casted))
          case _ : CategoryType => logic.derivation.semantics.Construction.Cat(Category.Base(casted))
        case _ => throw SemanticError(s"the type of $name is not defined before use")
    case Dom(morph) => translateConcatenation(types, morph) match
      case Morph(casted) => Obj(Object.Domain(casted))
      case _ => throw SemanticError(s"the domain of $morph is undefined as it's not a morphism")
    case Cod(morph) => translateConcatenation(types, morph) match
      case Morph(casted) => Obj(Object.Codomain(casted))
      case _ => throw SemanticError(s"the codomain of $morph is undefined as it's not a morphism")
    case Id(obj) => translateConstruction(types, obj) match
      case Obj(casted) => Morph(Morphism.Identity(casted))
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
          case Obj(casted) => casted
          case _ => throw SemanticError("a homset is defines only between two objects"),
        translateConcatenation(types, cod) match
          case Obj(casted) => casted
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
  p match
    case (pos, logic.parsing.ProofStep.Use(rule, post, map)) 
      => (pos, Use(rule, post, translateMap(types, map)))
    case (pos, logic.parsing.ProofStep.Repeat(post, where))
      => (pos, Repeat(post, where))
  
private def translateMap(types: Map[Name, Type],
                         map: List[(Name, Expression | Concatenation)]): Map[Name, 
  Condition | logic.derivation.semantics.Construction] =
  map match
    case Nil => Map.empty
    case (name, conc) :: tail => 
      val tMap = translateMap(types, tail)
      val mapped = conc match
        case casted: Expression => translateExpression(types, casted)._1
        case casted: Concatenation => translateConcatenation(types, casted)
      tMap.get(name) match
        case Some(value) if value != mapped => throw SemanticError("invalid substitution specified")
        case _ => tMap + (name -> mapped)

//def normalizeContext
