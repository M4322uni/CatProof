package logic.derivation.semantics

type Type = MorphismType | ObjectType | CategoryType

enum MorphismType:
  case HomSet(cat: Category, dom: Object, cod: Object)

enum ObjectType:
  case Cat(cat: Category)

enum CategoryType:
  case -