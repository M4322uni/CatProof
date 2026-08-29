package logic.derivation

import logic.derivation.derivationTree.Condition
import utils.Positive

def createProof(context: Map[Positive, Seq[Condition]],
                goals: Set[Condition],
                proof: Seq[(utils.Positive, logic.parsing.ProofStep)]): ProofResult =
  ???