package logic.parsing

import utils.*

case class Text(assumption: Seq[Expression], goals: Seq[Expression], proof: Seq[ProofStep])

enum Expression:
  case EqualityObj(left: ConstructedObj, right: ConstructedObj)
  case EqualityMorph(left: ConstructedMorph, right: ConstructedMorph)
  case JudgementObj(obj: ConstructedObj, typ: TypeObj)
  case JudgementMorph(morph: ConstructedMorph, typ: TypeMorph)

enum ConstructedObj:
  case BaseObj(base: logic.parsing.Object)
  case Domain(morph: ConstructedMorph)
  case Codomain(morph: ConstructedMorph)

enum ConstructedMorph:
  case BaseMorph(base: Morphism)
  case Identity(obj: ConstructedObj)
  case Composition(left: ConstructedMorph, right: ConstructedMorph)

enum TypeObj:
  case TypeCat(cat: Category)

enum TypeMorph:
  case HomSet(cat: Category, dom: ConstructedObj, cod: ConstructedObj)

case class Category(name: NonBlankString)

case class Morphism(name: MorphismName)

case class Object(name: ObjectName)
