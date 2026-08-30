package logic.derivation.procedure.graph

import logic.derivation.procedure.{Condition, DerivationError}
import logic.parsing.Rule

case class RuleResult(pre: Option[Vector[(Set[Condition], Condition)]], post: Condition)

val ruleTranslation1: Map[String, RuleResult] = Map(

)

val ruleTranslation2: Map[String, String => RuleResult] = Map(

)

def translate(rule: Rule): RuleResult =
  rule match
    case Rule(name, Some(parameter)) => ruleTranslation2.get(name.toLowerCase) match
      case Some(func) => func(parameter)
      case _ => throw DerivationError(s"a \"$name\" rule (with parameters) is not yet implemented")
    case Rule(name, _) => ruleTranslation1.get(name.toLowerCase) match
      case Some(ruleResult) => ruleResult
      case _ => throw DerivationError(s"a \"$name\" rule is not yet implemented")
