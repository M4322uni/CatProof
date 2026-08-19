package logic.derivation

import logic.parsing.*

type ProofResult = Tree | String //TODO

class Proof(body: String):
  
  def apply(): ProofResult =
    Parser(body)()
    
object Proof:
  
  def apply(body: String) = new Proof(body)