package logic.derivation.semantics

import utils.Name

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