package logic.parsing

import fastparse._, NoWhitespace._

def start[$ : P]: P[Text] =
  P(newBlank ~/ "assumptions:" ~ formula ~ newBlank ~
      "goals:" ~ formula ~ newBlank ~
      "proof:" ~ proof ~ blank)
    .map((ass: Seq[Formula], goal: Seq[Formula], proof: Seq[ProofStep])
      => Text(ass, goal, proof))

def newBlank[$ : P]: P[Unit] = ???

def blank[$ : P]: P[Unit] = ???

def formula[$ : P]: P[Seq[Formula]] = ???

def proof[$ : P]: P[Seq[ProofStep]] = ???