package logic.derivation.procedure.graph

import logic.derivation.procedure.Condition.{Equation, TypeJudgement}
import logic.derivation.procedure.{Condition, DerivationError, ECheck, TCheck, TypeSubj}
import logic.derivation.semantics.Category.*
import logic.derivation.semantics.Morphism.{Concatenation, Identity}
import logic.derivation.semantics.{Category, CategoryType, Construction, Morphism, MorphismType, Object, ObjectType, RestrictType, Type}
import logic.derivation.semantics.ObjectType.*
import logic.derivation.semantics.MorphismType.*
import logic.derivation.semantics.Object.{Codomain, Domain}
import logic.parsing.Rule
import utils.Name

case class RuleResult(pre: Vector[(Set[Condition], Condition)] | Boolean, post: Condition)

object RuleResult:

  class SubstitutionError(msg: String)
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
    "object_typing" -> RuleResult(false,
      TypeJudgement(TCheck(
        Object.Parameter("A"),
        Cat(Parameter("Cat"))))
    ),
    "morphism_typing" -> RuleResult(false,
      TypeJudgement(TCheck(
        Morphism.Parameter("f"),
        HomSet(Parameter("Cat"),
          Object.Parameter("A"),
          Object.Parameter("B"))))
    ),
    "concatenation_typing" -> RuleResult(Vector(
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
        TypeJudgement(TCheck(
          Morphism.Parameter("f"),
          HomSet(Parameter("Cat"),
            Object.Parameter("A"),
            Object.Parameter("B"))))
      )
    ),
      Equation(ECheck(
        Morphism.Concatenation(
          Identity(Object.Parameter("A")),
          Morphism.Parameter("f")
        ),
        Morphism.Parameter("f")
      ))
    ),
    "right_identity_law" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Object.Parameter("A"),
          Cat(Parameter("Cat"))))
      ),
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("f"),
          HomSet(Parameter("Cat"),
            Object.Parameter("B"),
            Object.Parameter("A"))))
      )
    ),
      Equation(ECheck(
        Morphism.Concatenation(
          Morphism.Parameter("f"),
          Identity(Object.Parameter("A"))
        ),
        Morphism.Parameter("f")
      ))
    ),
    "object_identity" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Object.Parameter("A"),
          Cat(Parameter("Cat")))
        )
      )
    ),
      Equation(ECheck(
        Object.Parameter("A"),
        Object.Parameter("A")
      ))
    ),
    "morphism_identity" -> RuleResult(Vector(
      (Set(),
        TypeJudgement(TCheck(
          Morphism.Parameter("f"),
          HomSet(Parameter("Cat"),
            Object.Parameter("A"),
            Object.Parameter("B")))
        )
      )
    ),
      Equation(ECheck(
        Morphism.Parameter("f"),
        Morphism.Parameter("f")
      ))
    ),
    "concatenation_equality" -> RuleResult(Vector(
      (Set(),
        Equation(ECheck(
          Morphism.Parameter("f"),
          Morphism.Parameter("h")
        ))
      ),
      (Set(),
        Equation(ECheck(
          Morphism.Parameter("g"),
          Morphism.Parameter("i")
        ))
      )
    ),
      Equation(ECheck(
        Morphism.Concatenation(
          Morphism.Parameter("f"),
          Morphism.Parameter("g")
        ),
        Morphism.Concatenation(
          Morphism.Parameter("h"),
          Morphism.Parameter("i")
        )
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
        Object.Domain(Morphism.Parameter("f")),
        Object.Parameter("A")
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
        Object.Codomain(Morphism.Parameter("f")),
        Object.Parameter("B")
      ))
    ),
    "associativity" -> RuleResult(true,
      Equation(ECheck(
        Concatenation(
          Concatenation(
            Morphism.Parameter("f"),
            Morphism.Parameter("g")
          ),
          Morphism.Parameter("h")
        ),
        Concatenation(
          Morphism.Parameter("f"),
          Concatenation(
            Morphism.Parameter("g"),
            Morphism.Parameter("h")
          )
        )
      ))
    )
  )

  private val ruleTranslation2: Map[String, String => RuleResult] = Map(

  )

  def apply(rule: Rule, map: Map[Name, Construction]): RuleResult =
    subst(translate(rule), map)

  private def translate(rule: Rule): RuleResult =
    rule match
      case Rule(name, Some(parameter)) => ruleTranslation2.get(name.toLowerCase) match
        case Some(func) => func(parameter)
        case _ => throw DerivationError(s"a \"$name\" rule (with parameters) is not yet implemented")
      case Rule(name, _) => ruleTranslation1.get(name.toLowerCase) match
        case Some(ruleResult) => ruleResult
        case _ => throw DerivationError(s"a \"$name\" rule is not yet implemented")

  private def subst(ruleResult: RuleResult, subst: Map[Name, Construction]): RuleResult =
    val RuleResult(pre, post) = ruleResult
    val pre2: Vector[(Set[Condition], Condition)] | Boolean = pre match
      case vect: Vector[(Set[Condition], Condition)] => vect.map {
        (s: Set[Condition], c: Condition) => (s.map { substCondition(_, subst) }, substCondition(c, subst))
      }
      case els: Boolean => els
    RuleResult(pre2, substCondition(post, subst))

  private def substCondition(condition: Condition, subst: Map[Name, Construction]): Condition =
    condition match
      case Equation(ECheck(lhs, rhs)) =>
        Equation(ECheck(substConstruction(lhs, subst), substConstruction(rhs, subst)))
      case TypeJudgement(TCheck(subj, ttype)) =>
        TypeJudgement(TCheck(substTypeSubj(subj, subst), substRestrictType(ttype, subst)))

  private def substConstruction(construction: Construction, subst: Map[Name, Construction]): Construction =
    construction match
      case casted: Object => substObject(casted, subst)
      case casted: Morphism => substMorphism(casted, subst)
      case casted: Category => substCategory(casted, subst)

  private def substTypeSubj(subj: TypeSubj, subst: Map[Name, Construction]): TypeSubj =
    subj match
      case casted: Object => substObject(casted, subst)
      case casted: Morphism => substMorphism(casted, subst)

  private def substRestrictType(ttype: RestrictType, subst: Map[Name, Construction]): RestrictType =
    ttype match
      case casted: ObjectType => substObjectType(casted, subst)
      case casted: MorphismType => substMorphismType(casted, subst)

  private def substObjectType(ot: ObjectType, subst: Map[Name, Construction]): ObjectType =
    ot match
      case Cat(cat) => Cat(substCategory(cat, subst))

  private def substMorphismType(mt: MorphismType, subst: Map[Name, Construction]): MorphismType =
    mt match
      case HomSet(cat, dom, cod) => HomSet(substCategory(cat, subst),
        substObject(dom, subst), substObject(cod, subst))

  private def substCategory(category: Category, subst: Map[Name, Construction]): Category =
    category match
      case Base(name) => Base(name)
      case Parameter(name) => subst.get(name) match
        case Some(value: Category) => value
        case None => throw SubstitutionError(s"no mapping found for parameter $name")
        case _ => throw SubstitutionError(s"the value mapped to $name is not of the correct type")

  private def substMorphism(morphism: Morphism, subst: Map[Name, Construction]): Morphism =
    morphism match
      case Morphism.Base(name) => Morphism.Base(name)
      case Concatenation(lhs, rhs) => Concatenation(substMorphism(lhs, subst),
        substMorphism(rhs, subst))
      case Identity(obj) => Identity(substObject(obj, subst))
      case Morphism.Parameter(name) => subst.get(name) match
        case Some(value: Morphism) => value
        case None => throw SubstitutionError(s"no mapping found for parameter $name")
        case _ => throw SubstitutionError(s"the value mapped to $name is not of the correct type")

  private def substObject(obj: Object, subst: Map[Name, Construction]): Object =
    obj match
      case Object.Base(name) => Object.Base(name)
      case Domain(morph) => Domain(substMorphism(morph, subst))
      case Codomain(morph) => Codomain(substMorphism(morph, subst))
      case Object.Parameter(name) => subst.get(name) match
        case Some(value: Object) => value
        case None => throw SubstitutionError(s"no mapping found for parameter $name")
        case _ => throw SubstitutionError(s"the value mapped to $name is not of the correct type")