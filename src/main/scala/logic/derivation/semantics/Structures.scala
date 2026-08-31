package logic.derivation.semantics

import logic.parsing.Rule
import utils.{Name, Positive}

type Construction = Category | Morphism | Object

enum Category:
  case Base(name: Name)
  case Parameter(name: Name)

enum Morphism:
  case Base(name: Name)
  case Concatenation(seq: Seq[Morphism])
  case Identity(obj: Object)
  case Parameter(name: Name)

enum Object:
  case Base(name: Name)
  case Domain(morph: Morphism)
  case Codomain(morph: Morphism)
  case Parameter(name: Name)
  
case class ProofStep(rule: Rule, post: (Positive, Int), map: Map[Name, Construction])