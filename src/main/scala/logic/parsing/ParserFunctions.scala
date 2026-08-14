package logic.parsing

import fastparse.*, NoWhitespace.*
import utils.*

def start[$ : P]: P[Text] =
  P(newBlank ~/ "assumptions:" ~ formula ~ newBlank ~
      "goals:" ~ formula ~ newBlank ~
      "proof:" ~ proof ~ blank)
    .map((ass: Seq[Formula], goal: Seq[Formula], proof: Seq[ProofStep])
      => Text(ass, goal, proof))

def newBlank[$ : P]: P[Unit] =
  P( (CharsWhile(_ == '\n') | (CharsWhileIn("\t ") ~ "\n")).repX )

def blank[$ : P]: P[Unit] =
  P( CharsWhileIn("\n\t ", 0) )

def formula[$ : P]: P[Seq[Formula]] = ???

def proof[$ : P]: P[Seq[ProofStep]] =
  P( ("\tuse " ~ rule ~ " " ~ reference ~ space_trail ~ "\n").repX )
    .map((str: Seq[(Rule, Seq[Positive])]) => 
      str.map((rule: Rule, refs: Seq[Positive]) => ProofStep(rule, refs)))
  
def rule[$ : P]: P[Rule] = ???

def reference[$ : P]: P[Seq[Positive]] = ???

def space_trail[$ : P]: P[Unit] = ???