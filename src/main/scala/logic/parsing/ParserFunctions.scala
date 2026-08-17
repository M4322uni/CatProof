package logic.parsing

import fastparse.*
import NoWhitespace.*
import logic.parsing.Category.*
import logic.parsing.Construction.*
import logic.parsing.Expression.*
import logic.parsing.Formula.*
import logic.parsing.Type.*
import utils.*

def start[$ : P]: P[Text] =
  P(newBlank ~/ "assumptions:" ~ formula ~ newBlank ~
      "goals:" ~ formula ~ newBlank ~
      "proof:" ~ proof ~ blank)
    .map { (ass: Seq[Formula], goal: Seq[Formula], proof: Seq[ProofStep])
      => Text(ass, goal, proof) }

def newBlank[$ : P]: P[Unit] =
  P( (CharsWhile(_ == '\n') | (CharsWhileIn("\t ") ~ "\n")).repX )

def blank[$ : P]: P[Unit] =
  P( CharsWhileIn("\n\t ", 0) )

def formula[$ : P]: P[Seq[Formula]] =
  P( ("\t" ~ (include | expression.map{ Formula.Expr(_) }) ~ space_trail ~ "\n").repX )

def expression[$ : P]: P[Expression] =
  P( (concatenation ~ indent_blank ~ "=" ~ indent_blank ~ concatenation)
    .map { (c1: Concatenation, c2: Concatenation) => Equation(c1, c2) }
    | (concatenation ~ indent_blank ~ ":" ~ indent_blank ~ ttype)
    .map { (c: Concatenation, t: Type) => Judgement(c, t) }
  )
  
def ttype[$ : P]: P[Type] =
  P( category ~ (indent_blank ~ "(" ~ indent_blank ~ concatenation ~ indent_blank
    ~ "," ~ indent_blank ~ concatenation ~ indent_blank ~ ")").? )
    .map { (cat: Category, opt: Option[(Concatenation, Concatenation)])
    => opt match
      case Some((c1: Concatenation, c2: Concatenation)) => HomSet(cat, c1, c2)
      case _ => Cat(cat)}
  
def concatenation[$ : P]: P[Concatenation] =
  P( construction ~ (indent_blank ~ "+" ~ indent_blank ~ construction).repX )
    .map { (c: Construction, s: Seq[Construction]) => Concatenation(c +: s) }

def construction[$ : P]: P[Construction] =
  P( name.map{ Atomic.apply } 
    | "dom(" ~ indent_blank ~ concatenation.map { Dom.apply } ~ indent_blank ~ ")" 
    | "cod(" ~ indent_blank ~ concatenation.map { Cod.apply } ~ indent_blank ~ ")"
    | "id(" ~ indent_blank ~ construction.map { Id.apply } ~ indent_blank ~ ")")

def category[$ : P]: P[Category] =
  P( name ).map { Base.apply }

def name[$ : P]: P[Name] =
  P( CharIn("A-Za-z_").! ~ CharsWhileIn("A-Za-z0-9_", 0).! )
    .map { (s1: String, s2: String) => s1 + s2 }

def indent_blank[$ : P]: P[Unit] =
  P( ("\t" | " " | "\n" ~ (" " | "\n").repX ~ "\t").repX )

def include[$ : P]: P[Formula.Include] =
  P( "<include " ~ name ~ ">" )
    .map { Include.apply }

def proof[$ : P]: P[Seq[ProofStep]] =
  P( ("\tuse " ~ rule ~ " " ~ reference ~ space_trail ~ "\n").repX )
    .map { (str: Seq[(Rule, Seq[Positive])]) =>
      str.map { (rule: Rule, refs: Seq[Positive]) => ProofStep(rule, refs) } }
  
def rule[$ : P]: P[Rule] =
  P( "composition".map {_ => Rule.COMPOSITION} |
    "transitivity".map {_ => Rule.TRANSITIVITY} )

def reference[$ : P]: P[Seq[Positive]] =
  P( "with " ~ line ~ (", " ~ line).repX )
    .map { (p: Positive, s: Seq[Positive]) => p +: s }
  
def line[$ : P]: P[Positive] =
  P( CharIn("1-9").! ~ CharsWhileIn("0-9").! )
    .map { (s1: String, s2: String) => (s1 + s2).toInt }

def space_trail[$ : P]: P[Unit] = ???