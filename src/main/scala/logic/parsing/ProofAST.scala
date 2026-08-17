package logic.parsing

import utils.*

case class Text(assumption: Seq[Formula], goals: Seq[Formula], proof: Seq[ProofStep])

enum Formula:
  case Include(diagram: Name)
  case Expr(exp: Expression)

enum Expression:
  case Equation(left: Concatenation, right: Concatenation)
  case Judgement(obj: Concatenation, typ: Type)

case class Concatenation(constructions: Seq[Construction]):
  require(constructions.nonEmpty)

enum Construction:
  case Atomic(name: Name)
  case Dom(morph: Concatenation)
  case Cod(morph: Concatenation)
  case Id(obj: Construction)

enum Type:
  case Cat(cat: Category)
  case HomSet(cat: Category, dom: Concatenation, cod: Concatenation)
  
enum Category:
  case Base(name: Name)

case class ProofStep(rule: Rule, lines: Seq[Positive])

enum Rule:
  case TRANSITIVITY
  case COMPOSITION
  // ...