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
import TranslateEncapsulate.*
import utils.Name

class SemanticError(message: String)
  extends IllegalArgumentException(s"Semantic error: $message")

def translateFormulaList(map: Map[Name, Diagram],
                         types: Map[Name, Type],
                         s: Seq[Formula]): Set[Condition] =
  s match
    case h :: tail =>
      val (conds, type_res) = translateFormula(map, types, h)
      conds ++ translateFormulaList(map, type_res, tail)
    case Nil => Set()

def translateFormula(map: Map[Name, Diagram],
                     types: Map[Name, Type],
                     f: Formula): (Set[Condition], Map[Name, Type]) =
  f match
    case Include(diagram) => translateDiagram(types, map.get(diagram) match
      case Some(d) => d
      case _ => throw SemanticError(s"the $diagram diagram cannot be found")
    )
    case Expr(exp) =>
      val (cond, type_res) = translateExpression(types, exp)
      (Set(cond), type_res)

def translateDiagram(types: Map[Name, Type],
                     diag: Diagram): (Set[Condition], Map[Name, Type]) =
  def diagramDFS(node: Name, visited: Set[Name]) = ???
  // type all the edges
  // create a map : node x node -> concatenations, by dfs
  // those are the equality classes, add equality conditions
  ???

def translateExpression(types: Map[Name, Type],
                        e: Expression): (Condition, Map[Name, Type]) =
  e match
    case logic.parsing.Expression.Equation(left, right)
      => (Condition.Equation(EquationPair(
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
      case NameCapsule(casted) => (ObjectType.Cat(Category.Base(casted)),
        extendTypes(types, casted, CategoryType.-))
      case CategoryCapsule(casted) => (ObjectType.Cat(casted), types)

enum TranslateEncapsulate:
  case NameCapsule(name: Name)
  case CategoryCapsule(cat: Category)

def translateNameBound(n: NameBound): TranslateEncapsulate =
  n match
    case Base(name) => NameCapsule(name)
    // TODO: extend