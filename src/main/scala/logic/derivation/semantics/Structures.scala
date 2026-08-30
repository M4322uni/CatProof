package logic.derivation.semantics

import logic.parsing.Rule
import utils.{Name, Positive}

type Construction = Category | Morphism | Object

case class Parameter(name: Name)

enum Category:
  case Base(name: Name)
  case X(parameter: Parameter)

enum Morphism:
  case Base(name: Name)
  case Concatenation(seq: Seq[Morphism])
  case Identity(obj: Object)
  case X(parameter: Parameter)

enum Object:
  case Base(name: Name)
  case Domain(morph: Morphism)
  case Codomain(morph: Morphism)
  case X(parameter: Parameter)
  
case class ProofStep(rule: Rule, post: (Positive, Int), map: Map[Name, Construction])