package logic.derivation.structures

type Type = MorphismType | ObjectType

enum MorphismType:
  case HomSet(cat: Category, dom: Object, cod: Object)

enum ObjectType:
  case Cat(cat: Category)