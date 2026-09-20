package logic.derivation.procedure.graph

import logic.derivation.procedure.Condition.{Equation, TypeJudgement}
import logic.derivation.procedure.{Condition, DerivationError, ECheck, TCheck, TypeSubj}
import logic.derivation.semantics.Category.*
import logic.derivation.semantics.Morphism.{Concatenation, Identity}
import logic.derivation.semantics.{Category, CategoryType, Construction, Morphism, MorphismType, Object, ObjectType, RestrictType, Type}
import logic.derivation.semantics.ObjectType.*
import logic.derivation.semantics.MorphismType.*
import logic.derivation.semantics.Object.{Codomain, Domain}
import logic.derivation.semantics.Construction.{Morph, Obj}
import logic.parsing.Rule
import utils.Name

case class RuleResult(pre: Vector[(Set[Condition], Condition)] | Boolean, post: Condition)

object RuleResult:

  private class SubstitutionError(msg: String)
    extends IllegalArgumentException(s"Substitution error: $msg")

  private val ruleTranslation1: Map[String, RuleResult] = Map(
    "identity" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Object.Parameter("A"),
          Cat(Parameter("Cat")))))
    ),
      TypeJudgement(TCheck(
        Morphism.Identity(Object.Parameter("A")),
        MorphismType.HomSet(Parameter("Cat"),
          Object.Parameter("A"), Object.Parameter("A"))))
    ),
    "given" -> RuleResult(false,
      Condition.Parameter("A")
    ),
    "composition_typing" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Object.Parameter("A"),
          Cat(Parameter("Cat"))))
      ),
      (Set(),
        TypeJudgement(TCheck(
          Object.Parameter("B"),
          Cat(Parameter("Cat"))))
      ),
      (Set(),
        TypeJudgement(TCheck(
          Object.Parameter("C"),
          Cat(Parameter("Cat"))))
      ),
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("f"),
          HomSet(Parameter("Cat"),
            Object.Parameter("A"),
            Object.Parameter("B"))))
      ),
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("g"),
          HomSet(Parameter("Cat"),
            Object.Parameter("B"),
            Object.Parameter("C"))))
      ),
    ),
      TypeJudgement(TCheck(
        Morphism.Concatenation(
        Morphism.Parameter("f"),
        Morphism.Parameter("g")
      ),
        HomSet(Parameter("Cat"),
          Object.Parameter("A"),
          Object.Parameter("C")))
      )
    ),
    "left_identity_law" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Object.Parameter("A"),
          Cat(Parameter("Cat"))))
      ),
      (Set(),
        Equation(ECheck(
          Obj(Object.Domain(Morphism.Parameter("f"))),
          Obj(Object.Parameter("A"))))
      )
    ),
      Equation(ECheck(
        Morph(Morphism.Concatenation(
          Identity(Object.Parameter("A")),
          Morphism.Parameter("f"))
        ),
        Morph(Morphism.Parameter("f"))
      ))
    ),
    "right_identity_law" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Object.Parameter("A"),
          Cat(Parameter("Cat"))))
      ),
      (Set(),
        Equation(ECheck(
          Obj(Object.Codomain(Morphism.Parameter("f"))),
          Obj(Object.Parameter("A"))))
      )
    ),
      Equation(ECheck(
        Morph(Morphism.Concatenation(
          Morphism.Parameter("f"),
          Identity(Object.Parameter("A")))
        ),
        Morph(Morphism.Parameter("f"))
      ))
    ),
    "composition_equality" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("f"),
          MorphismType.HomSet(
            Base("Cat"),
            Object.Parameter("A"),
            Object.Parameter("B")
          )
        ))
      ),
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("h"),
          MorphismType.HomSet(
            Base("Cat"),
            Object.Parameter("A"),
            Object.Parameter("B")
          )
        ))
      ),
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("g"),
          MorphismType.HomSet(
            Base("Cat"),
            Object.Parameter("B"),
            Object.Parameter("C")
          )
        ))
      ),
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("i"),
          MorphismType.HomSet(
            Base("Cat"),
            Object.Parameter("B"),
            Object.Parameter("C")
          )
        ))
      ),
      (Set(),
        Equation(ECheck(
          Morph(Morphism.Parameter("f")),
          Morph(Morphism.Parameter("h"))
        ))
      ),
      (Set(),
        Equation(ECheck(
          Morph(Morphism.Parameter("g")),
          Morph(Morphism.Parameter("i"))
        ))
      )
    ),
      Equation(ECheck(
        Morph(Morphism.Concatenation(
          Morphism.Parameter("f"),
          Morphism.Parameter("g")
        )),
        Morph(Morphism.Concatenation(
          Morphism.Parameter("h"),
          Morphism.Parameter("i")
        ))
      ))
    ),
    "domain_definition" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("f"),
          HomSet(Parameter("Cat"),
            Object.Parameter("A"),
            Object.Parameter("B"))
        ))
      )
    ),
      Equation(ECheck(
        Obj(Object.Domain(Morphism.Parameter("f"))),
        Obj(Object.Parameter("A"))
      ))
    ),
    "codomain_definition" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("f"),
          HomSet(Parameter("Cat"),
            Object.Parameter("A"),
            Object.Parameter("B"))
        ))
      )
    ),
      Equation(ECheck(
        Obj(Object.Codomain(Morphism.Parameter("f"))),
        Obj(Object.Parameter("B"))
      ))
    ),
    "associativity" -> RuleResult(Vector(
      (Set(),
        Equation(ECheck(
          Obj(Codomain(Morphism.Parameter("f"))),
          Obj(Domain(Morphism.Parameter("g")))
        ))
      ),
      (Set(),
        Equation(ECheck(
          Obj(Codomain(Morphism.Parameter("g"))),
          Obj(Domain(Morphism.Parameter("h")))
        ))
      )
    ),
      Equation(ECheck(
        Morph(Concatenation(
          Concatenation(
            Morphism.Parameter("f"),
            Morphism.Parameter("g")
          ),
          Morphism.Parameter("h")
        )),
        Morph(Concatenation(
          Morphism.Parameter("f"),
          Concatenation(
            Morphism.Parameter("g"),
            Morphism.Parameter("h")
          )
        ))
      ))
    ),
    "equality_reflexivity" -> RuleResult(true,
      Equation(ECheck(
        Construction.Parameter("A"),
        Construction.Parameter("A")
      ))
    ),
    "equality_symmetry" -> RuleResult(Vector(
      (Set(),
        Equation(ECheck(
          Construction.Parameter("A"),
          Construction.Parameter("B")
        ))
      )),
      Equation(ECheck(
        Construction.Parameter("B"),
        Construction.Parameter("A")
      ))
    ),
    "equality_transitivity" -> RuleResult(Vector(
      (Set(),
        Equation(ECheck(
          Construction.Parameter("A"),
          Construction.Parameter("B")
        ))
      ),
      (Set(),
        Equation(ECheck(
          Construction.Parameter("B"),
          Construction.Parameter("C")
        ))
      )),
      Equation(ECheck(
        Construction.Parameter("A"),
        Construction.Parameter("C")
      ))
    )
  )

  private val ruleTranslation2: Map[String, String => RuleResult] = Map(

  )

  def apply(rule: Rule, map: Map[Name, Construction | Condition]): RuleResult =
    subst(translate(rule), map)

  private def translate(rule: Rule): RuleResult =
    rule match
      case Rule(name, Some(parameter)) => ruleTranslation2.get(name.toLowerCase) match
        case Some(func) => func(parameter)
        case _ => throw DerivationError(s"a \"$name\" rule (with parameters) is not yet implemented")
      case Rule(name, _) => ruleTranslation1.get(name.toLowerCase) match
        case Some(ruleResult) => ruleResult
        case _ => throw DerivationError(s"a \"$name\" rule is not yet implemented")

  private def subst(ruleResult: RuleResult, subst: Map[Name, Construction | Condition]): RuleResult =
    val RuleResult(pre, post) = ruleResult
    val pre2: Vector[(Set[Condition], Condition)] | Boolean = pre match
      case vect: Vector[(Set[Condition], Condition)] => vect.map {
        (s: Set[Condition], c: Condition) => (s.map { substCondition(_, subst) }, substCondition(c, subst))
      }
      case els: Boolean => els
    RuleResult(pre2, substCondition(post, subst))

  private def substCondition(condition: Condition, subst: Map[Name, Construction | Condition]): Condition =
    condition match
      case Equation(ECheck(lhs, rhs)) =>
        Equation(ECheck(substConstruction(lhs, subst), substConstruction(rhs, subst)))
      case TypeJudgement(TCheck(subj, ttype)) =>
        TypeJudgement(TCheck(substTypeSubj(subj, subst), substRestrictType(ttype, subst)))
      case Condition.Parameter(name) => subst.get(name) match
        case Some(cons: Condition) => cons
        case None => throw SubstitutionError(s"no mapping found for parameter $name")
        case _ => throw SubstitutionError(s"the value mapped to $name is not of the correct type")

  private def substConstruction(construction: Construction, subst: Map[Name, Construction | Condition]): Construction =
    construction match
      case Obj(casted) => Obj(substObject(casted, subst))
      case Morph(casted) => Morph(substMorphism(casted, subst))
      case Construction.Cat(casted) => Construction.Cat(substCategory(casted, subst))
      case Construction.Parameter(name) => subst.get(name) match
        case Some(cons: Construction) => cons
        case None => throw SubstitutionError(s"no mapping found for parameter $name")
        case _ => throw SubstitutionError(s"the value mapped to $name is not of the correct type")

  private def substTypeSubj(subj: TypeSubj, subst: Map[Name, Construction | Condition]): TypeSubj =
    subj match
      case casted: Object => substObject(casted, subst)
      case casted: Morphism => substMorphism(casted, subst)

  private def substRestrictType(ttype: RestrictType, subst: Map[Name, Construction | Condition]): RestrictType =
    ttype match
      case casted: ObjectType => substObjectType(casted, subst)
      case casted: MorphismType => substMorphismType(casted, subst)

  private def substObjectType(ot: ObjectType, subst: Map[Name, Construction | Condition]): ObjectType =
    ot match
      case Cat(cat) => Cat(substCategory(cat, subst))

  private def substMorphismType(mt: MorphismType, subst: Map[Name, Construction | Condition]): MorphismType =
    mt match
      case HomSet(cat, dom, cod) => HomSet(substCategory(cat, subst),
        substObject(dom, subst), substObject(cod, subst))

  private def substCategory(category: Category, subst: Map[Name, Construction | Condition]): Category =
    category match
      case Base(name) => Base(name)
      case Parameter(name) => subst.get(name) match
        case Some(Construction.Cat(value)) => value
        case None => throw SubstitutionError(s"no mapping found for parameter $name")
        case _ => throw SubstitutionError(s"the value mapped to $name is not of the correct type")

  private def substMorphism(morphism: Morphism, subst: Map[Name, Construction | Condition]): Morphism =
    morphism match
      case Morphism.Base(name) => Morphism.Base(name)
      case Concatenation(lhs, rhs) => Concatenation(substMorphism(lhs, subst),
        substMorphism(rhs, subst))
      case Identity(obj) => Identity(substObject(obj, subst))
      case Morphism.Parameter(name) => subst.get(name) match
        case Some(Morph(value)) => value
        case None => throw SubstitutionError(s"no mapping found for parameter $name")
        case _ => throw SubstitutionError(s"the value mapped to $name is not of the correct type")

  private def substObject(obj: Object, subst: Map[Name, Construction | Condition]): Object =
    obj match
      case Object.Base(name) => Object.Base(name)
      case Domain(morph) => Domain(substMorphism(morph, subst))
      case Codomain(morph) => Codomain(substMorphism(morph, subst))
      case Object.Parameter(name) => subst.get(name) match
        case Some(Obj(value)) => value
        case None => throw SubstitutionError(s"no mapping found for parameter $name")
        case _ => throw SubstitutionError(s"the value mapped to $name is not of the correct type")