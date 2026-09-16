package logic.parsing

import utils.*

private[parsing] case class FakeTree(assumptions: List[Formula],
                                     goals: List[(Int, Formula)],
                                     proof: List[(Int, ProofStep)])

case class Tree(assumptions: List[Formula],
                goals: List[(Positive, Formula)],
                proof: List[(Positive, ProofStep)])

enum Formula:
  case Include(diagram: Name)
  case Expr(exp: Expression)

enum Expression:
  case Equation(left: Concatenation, right: Concatenation)
  case TypeJudgement(subj: Concatenation, typ: Type)

enum Concatenation:
  case Binary(lhs: Concatenation, rhs: Concatenation)
  case Leaf(con: Construction)

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

case class ProofStep(rule: Rule, post: (Positive, Int), 
                     subst: List[(Name, Expression | Concatenation)])

case class Rule(name: Name, args: Option[String])