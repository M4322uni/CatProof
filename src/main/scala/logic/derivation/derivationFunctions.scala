package logic.derivation

import logic.derivation.derivationTree.Condition
import logic.parsing.ProofStep
import utils.Positive

def createProof(context: Map[Positive, Set[Condition]], 
                goals: Set[Condition],
                proof: Seq[(utils.Positive, logic.parsing.ProofStep)]): ProofResult =
  ???