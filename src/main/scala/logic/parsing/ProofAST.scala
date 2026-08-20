package logic.parsing

import utils.*

private[parsing] case class FakeTree(assumptions: Seq[(Int, Formula)],
                                     goals: Seq[(Int, Formula)],
                                     proof: Seq[(Int, ProofStep)])

case class Tree(assumptions: Seq[(Positive, Formula)],
                goals: Seq[(Positive, Formula)],
                proof: Seq[(Positive, ProofStep)])

enum Formula:
  case Include(diagram: Name)
  case Expr(exp: Expression)

enum Expression:
  case Equation(left: Concatenation, right: Concatenation)
  case TypeJudgement(subj: Name, typ: Type)

case class Concatenation(constructions: Seq[Construction]):
  require(constructions.nonEmpty)

enum Construction:
  case Atomic(name: NameBound)
  case Dom(morph: Concatenation)
  case Cod(morph: Concatenation)
  case Id(obj: Construction)

enum Type:
  case Cat(cat: NameBound)
  case HomSet(cat: NameBound, dom: Concatenation, cod: Concatenation)
  
enum NameBound:
  case Base(name: Name)

case class ProofStep(rule: Rule, lines: Seq[Positive])

enum Rule:
  case TRANSITIVITY
  case COMPOSITION
  // ...