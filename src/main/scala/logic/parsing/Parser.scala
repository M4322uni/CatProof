package logic.parsing

import fastparse.*
import NoWhitespace.*
import logic.parsing.NameBound.*
import logic.parsing.Construction.*
import logic.parsing.Expression.*
import logic.parsing.Formula.*
import logic.parsing.Type.*
import utils.*

class Parser(text: String):

  private def getLine(until: Int): Positive =
    text.take(until+1).count(_ == '\n') + 1

  private def start[$ : P]: P[FakeTree] =
    P(newBlank ~ IgnoreCase("assumptions:") ~ formula ~ newBlank ~
        IgnoreCase("goals:") ~ formula ~ newBlank ~
        IgnoreCase("proof:") ~ proof ~ blank ~ End)
      .map { (ass: Seq[(Int, Formula)],
              goal: Seq[(Int, Formula)],
              proof: Seq[(Int, ProofStep)])
        => FakeTree(ass, goal, proof) }

  private def newBlank[$ : P]: P[Unit] =
    P( CharsWhile(_ == '\n') | (CharsWhileIn("\t ") ~ "\n") ).repX

  private def blank[$ : P]: P[Unit] =
    P( CharsWhileIn("\t\n ", 0) )

  private def formula[$ : P]: P[Seq[(Int, Formula)]] =
    P( hard_indent ~ Index ~ (include | expression.map{ Expr.apply }) )
      .repX

  private def expression[$ : P]: P[Expression] =
    P( (concatenation ~ indent_blank ~ "=" ~ indent_blank ~ concatenation)
      .map { (c1: Concatenation, c2: Concatenation) => Equation(c1, c2) }
      | (name ~ indent_blank ~ ":" ~ indent_blank ~ ttype)
      .map { (c: Name, t: Type) => TypeJudgement(c, t) }
    )

  private def ttype[$ : P]: P[Type] =
    P( name_bound ~ (indent_blank ~ "(" ~ indent_blank ~ concatenation ~ indent_blank
      ~ "," ~ indent_blank ~ concatenation ~ indent_blank ~ ")").? )
      .map { (cat: NameBound, opt: Option[(Concatenation, Concatenation)])
      => opt match
        case Some((c1: Concatenation, c2: Concatenation)) => HomSet(cat, c1, c2)
        case _ => Cat(cat)}

  private def concatenation[$ : P]: P[Concatenation] =
    P( construction ~ (indent_blank ~ "+" ~ indent_blank ~ construction).repX )
      .map { (c: Construction, s: Seq[Construction]) => Concatenation(c +: s) }

  private def construction[$ : P]: P[Construction] =
    P( "dom(" ~ indent_blank ~ concatenation.map { Dom.apply } ~ indent_blank ~ ")"
      | "cod(" ~ indent_blank ~ concatenation.map { Cod.apply } ~ indent_blank ~ ")"
      | "id(" ~ indent_blank ~ construction.map { Id.apply } ~ indent_blank ~ ")"
      | name_bound.map { Atomic.apply } )

  private def name_bound[$ : P]: P[NameBound] =
    P( name ).map { Base.apply }

  private def name[$ : P]: P[Name] =
    P( CharIn("A-Za-z_").! ~ CharsWhileIn("A-Za-z0-9_", 0).! )
      .map { (s1: String, s2: String) => s1 + s2 }

  private def indent_bound[$ : P]: P[Unit] =
    P( (CharsWhile(_ == ' ', 4)
      | CharsWhile(_ == ' ', 0) ~ ("\t" | "\n" ~ indent_bound)) ~ indent_blank )

  private def indent_blank[$ : P]: P[Unit] =
      P( CharIn("\t ") ~ indent_blank | "\n" ~ indent_bound | Pass )

  private def hard_indent[$ : P]: P[Unit] =
    P( CharIn("\t ") ~ hard_indent | "\n" ~ indent_bound )

  private def space_indent[$ : P]: P[Unit] =
    P( CharIn("\t ") ~ indent_blank | "\n" ~ indent_bound )

  private def include[$ : P]: P[Formula.Include] =
    P( "<include " ~ name ~ ">" )
      .map { Include.apply }

  private def proof[$ : P]: P[Seq[(Int, ProofStep)]] =
    P( (hard_indent ~ Index ~ IgnoreCase("use") ~ space_indent ~ rule ~ space_indent ~ reference).repX )
      .map { (str: Seq[(Int, Rule, Seq[Positive])]) =>
        str.map { (arg: Int, rule: Rule, refs: Seq[Positive]) => (arg, ProofStep(rule, refs)) } }

  private def rule[$ : P]: P[Rule] =
    P( IgnoreCase("composition").map {_ => Rule.COMPOSITION}
      | IgnoreCase("transitivity").map {_ => Rule.TRANSITIVITY} )

  private def reference[$ : P]: P[Seq[Positive]] =
    P( IgnoreCase("with") ~ space_indent ~ line ~ ("," ~ space_indent ~ line).repX )
      .map { (p: Positive, s: Seq[Positive]) => p +: s }

  private def line[$ : P]: P[Positive] =
    P( CharIn("1-9").! ~ CharsWhileIn("0-9", 0).! )
      .map { (s1: String, s2: String) => (s1 + s2).toInt }

  private def space_trail[$ : P]: P[Unit] =
    P( CharsWhileIn("\t ", 0) )

  private def fix(tree: FakeTree): Tree =
    val FakeTree(assumption: Seq[(Int, Formula)],
      goals: Seq[(Int, Formula)],
      proof: Seq[(Int, ProofStep)]) = tree
    val (assumption2: Seq[(Positive, Formula)],
      lastIndex: Int,
      lastLine: Positive) = fix(assumption)
    val (goals2: Seq[(Positive, Formula)],
      lastIndex2: Int,
      lastLine2: Positive) = fix(goals, lastIndex, lastLine)
    val (proof2: Seq[(Positive, ProofStep)], _, _) = fix(proof, lastIndex2, lastLine2)
    Tree(assumption2, goals2, proof2)

  private def fix[T](s: Seq[(Int, T)], lastIndex: Int = 0, lastLine: Positive = 1): (Seq[(Positive, T)], Int, Positive) =
    val strip: Seq[Int] = s.map { _._1 }
    val seq: Seq[(Int, T)] =
      (lastIndex +: strip ).zip(s).map {
      case (p1, (p2, f)) => (count(p2, p1), f)
    }
    val sums: Seq[Positive] = seq.scanLeft(lastLine) {
      case (p1: Positive, (p2: Int, _)) => p1+p2
    }.tail
    (seq.zip(sums).map {
      case ((_, f), p) => (p, f)
    }, strip.lastOption.getOrElse(lastIndex),
      sums.lastOption.getOrElse(lastLine))

  private def count(until: Int, from: Int = 0): Int =
    text.slice(from, until).count(_ == '\n')

  private def errorPretty(fail: Parsed.Failure): String =
    //TODO
    fail.msg

  def apply(): Tree =
    fastparse.parse(text, start(using _)) match
      case Parsed.Success(result, _) =>
        fix(result)

      case f: Parsed.Failure =>
        throw IllegalArgumentException(errorPretty(f))

object Parser:
  def apply(text: String) = new Parser(text)
