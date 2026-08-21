package logic.derivation.semantics

import logic.derivation.Diagram
import logic.derivation.derivationTree.*
import logic.parsing.*
import logic.parsing.Formula.*
//import logic.parsing.Expression.*
import logic.derivation.derivationTree.Condition.*
import utils.Name

def translateFormulaList(map: Map[Name, Diagram],
                         types: Set[TypeJudgement],
                         s: Seq[Formula]): Set[Condition] =
  s match
    case h :: tail =>
      val (conds, type_res) = translateFormula(map, types, h)
      conds ++ translateFormulaList(map, type_res, tail)
    case Nil => Set()

def translateFormula(map: Map[Name, Diagram],
                     types: Set[TypeJudgement],
                     f: Formula): (Set[Condition], Set[TypeJudgement]) =
  f match
    case Include(diagram) => translateDiagram(types, map.get(diagram) match
      case Some(d) => d
      case _ => throw IllegalArgumentException(s"Semantic error: the $diagram diagram cannot be found")
    )
    case Expr(exp) =>
      val (cond, type_res) = translateExpression(types, exp)
      (Set(cond), type_res)

def translateDiagram(types: Set[TypeJudgement],
                     diag: Diagram): (Set[Condition], Set[TypeJudgement]) =
  ???

def translateExpression(types: Set[TypeJudgement],
                        e: Expression): (Condition, Set[TypeJudgement]) =
  e match
    case logic.parsing.Expression.Equation(left, right)
      => (Condition.Equation(EquationPair(
        translateConcatenation(types, left),
        translateConcatenation(types, right))), types)
    case logic.parsing.Expression.TypeJudgement(subj, typ)
      =>
      val judge: TypeJudgement = Condition.TypeJudgement(subj, translateType(typ))
      (judge, extendTypes(types, judge))

def extendTypes(types: Set[TypeJudgement], newType: TypeJudgement): Set[TypeJudgement]
  = ???

def translateConcatenation(types: Set[TypeJudgement],
                           c: Concatenation): logic.derivation.semantics.Construction =
  c.constructions.size match
    case 0 => throw IllegalArgumentException("Parser error: a concatenation of zero elements was parsed")
    case 1 => ???
    case _ => ???

def translateConstruction(c: logic.parsing.Construction): logic.derivation.semantics.Construction =
  ???

def translateType(t: Type): logic.derivation.semantics.Type =
  ???

def translateNameBound(n: NameBound): Name | Category =
  ???