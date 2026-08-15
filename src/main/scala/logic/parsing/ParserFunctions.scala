package logic.parsing

import fastparse.*
import NoWhitespace.*
import logic.parsing.Expression.*
import logic.parsing.Formula.*
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
  
def ttype[$ : P]: P[Type] = ???
  
def concatenation[$ : P]: P[Concatenation] =
  P( construction ~ (indent_blank ~ "+" ~ indent_blank ~ construction).repX )
    .map { (c: Construction, s: Seq[Construction]) => Concatenation(c +: s) }

def construction[$ : P]: P[Construction] =
  ???

def indent_blank[$ : P]: P[Unit] = ???

def include[$ : P]: P[Formula.Include] =
  P( "<include " ~ CharPred(c => c != '>' && (!c.isWhitespace || c == ' ')).repX(1).! ~ ">" )
    .map { Include(_) }

def proof[$ : P]: P[Seq[ProofStep]] =
  P( ("\tuse " ~ rule ~ " " ~ reference ~ space_trail ~ "\n").repX )
    .map { (str: Seq[(Rule, Seq[Positive])]) =>
      str.map { (rule: Rule, refs: Seq[Positive]) => ProofStep(rule, refs) } }
  
def rule[$ : P]: P[Rule] = ???

def reference[$ : P]: P[Seq[Positive]] = ???

def space_trail[$ : P]: P[Unit] = ???