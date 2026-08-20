package logic.derivation.structures

import utils.Name

enum Object:
  case Base(name: Name)
  case Domain(morph: Morphism)
  case Codomain(morph: Morphism)