package logic.derivation.structures

import utils.Name

enum Morphism:
  case Base(name: Name)
  case Concatenation(seq: Seq[Morphism])
  case Identity(obj: Object)