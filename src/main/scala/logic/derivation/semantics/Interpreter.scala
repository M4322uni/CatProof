package logic.derivation.semantics

import logic.derivation.Diagram
import logic.derivation.derivationTree.*
import logic.parsing.*
import logic.parsing.Formula.*
import logic.parsing.Expression.*
import logic.derivation.derivationTree.Condition.*
import utils.Name

def translateFormula(f: Formula, map: Map[Name, Diagram]): Set[Condition] =
  f match
    case Include(diagram) => translateDiagram(map(diagram))
    case Expr(exp) => Set(translateExpression(exp))

def translateDiagram(diag: Diagram): Set[Condition] =
  ???

def translateExpression(e: Expression): Condition =
  e match
    case logic.parsing.Expression.Equation(left, right)
      => Condition.Equation(EquationPair(
        translateConcatenation(left),
        translateConcatenation(right)))
    case logic.parsing.Expression.TypeJudgement(subj, typ)
      => Condition.TypeJudgement(subj, translateType(typ))

def translateConcatenation(c: Concatenation): logic.derivation.semantics.Construction =
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