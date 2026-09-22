package logic.derivation.semantics

import logic.derivation.procedure.Condition
import logic.derivation.semantics.Morphism.capsule
import logic.parsing.Rule
import utils.{Name, Positive}

enum Construction:
  case Cat(cat: Category)
  case Morph(morph: Morphism)
  case Obj(obj: Object)
  case Parameter(name: Name)

  override def toString: String =
    this match
      case Cat(cat) => s"$cat"
      case Morph(morph) => s"$morph"
      case Obj(obj) => s"$obj"
      case Parameter(name) => name

enum Category:
  case Base(name: Name)
  case Parameter(name: Name)

  override def toString: String =
    this match
      case Base(name) => name
      case Parameter(name) => name

enum Morphism:
  case Base(name: Name)
  case Concatenation(lhs: Morphism, rhs: Morphism)
  case Identity(obj: Object)
  case Parameter(name: Name)

  override def toString: String =
    this match
      case Base(name) => name
      case Concatenation(lhs, rhs) => s"${capsule(lhs)}; ${capsule(rhs)}"
      case Identity(obj) => s"Id($obj)"
      case Parameter(name) => name

object Morphism:

  private def capsule(morph: Morphism): String =
    morph match
      case Concatenation(_, _) => s"($morph)"
      case _ => s"$morph"

enum Object:
  case Base(name: Name)
  case Domain(morph: Morphism)
  case Codomain(morph: Morphism)
  case Parameter(name: Name)

  override def toString: String =
    this match
      case Base(name) => name
      case Domain(morph) => s"Dom($morph)"
      case Codomain(morph) => s"Cod($morph)"
      case Parameter(name) => name

enum ProofStep:
  case Use(rule: Rule, post: (Positive, Int), subst: Map[Name, Condition | Construction])
  case Repeat(post: (Positive, Int), where: Positive)