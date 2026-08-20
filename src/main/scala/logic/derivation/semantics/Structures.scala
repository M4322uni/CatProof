package logic.derivation.semantics

import utils.Name

type Construction = Category | Morphism | Object

enum Category:
  case Base(name: Name)

enum Morphism:
  case Base(name: Name)
  case Concatenation(seq: Seq[Morphism])
  case Identity(obj: Object)

enum Object:
  case Base(name: Name)
  case Domain(morph: Morphism)
  case Codomain(morph: Morphism)