package logic.derivation

import logic.parsing.*

type ProofResult = Tree | String //TODO

class Proof(body: String, diagrams: Seq[Diagram]):
  
  def apply(): ProofResult =
    Parser(body)()
    
object Proof:
  
  def apply(body: String, diagrams: Seq[Diagram]) =
    new Proof(body, diagrams)