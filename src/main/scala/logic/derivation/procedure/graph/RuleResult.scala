package logic.derivation.procedure.graph

import logic.derivation.procedure.{Condition, DerivationError}
import logic.derivation.semantics.{Category, Construction, Morphism, Object}
import logic.parsing.Rule
import utils.Name

case class RuleResult(pre: Option[Vector[(Set[Condition], Condition)]], post: Condition)

object RuleResult:

  private val ruleTranslation1: Map[String, RuleResult] = Map(

  )

  private val ruleTranslation2: Map[String, String => RuleResult] = Map(

  )

  def apply(rule: Rule, map: Map[Name, Construction]): RuleResult =
    subst(translate(rule), map)

  def translate(rule: Rule): RuleResult =
    rule match
      case Rule(name, Some(parameter)) => ruleTranslation2.get(name.toLowerCase) match
        case Some(func) => func(parameter)
        case _ => throw DerivationError(s"a \"$name\" rule (with parameters) is not yet implemented")
      case Rule(name, _) => ruleTranslation1.get(name.toLowerCase) match
        case Some(ruleResult) => ruleResult
        case _ => throw DerivationError(s"a \"$name\" rule is not yet implemented")

  def subst(ruleResult: RuleResult, subst: Map[Name, Construction]): RuleResult =
    val RuleResult(pre, post) = ruleResult
    val pre2: Option[Vector[(Set[Condition], Condition)]] = pre match
      case Some(vect) => Some( vect.map {
        (s: Set[Condition], c: Condition) => (s.map { substCondition(_, subst) }, substCondition(c, subst))
      } )
      case None => None
    ???

  def substCondition(condition: Condition, subst: Map[Name, Construction]): Condition =
    ???

  def substCategory(category: Category, subst: Map[Name, Construction]): Category =
    ???

  def substMorphism(morphism: Morphism, subst: Map[Name, Construction]): Morphism =
    ???

  def substObject(obj: Object, subst: Map[Name, Construction]): Object =
    ???