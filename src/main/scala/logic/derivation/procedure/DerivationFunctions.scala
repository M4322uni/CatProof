package logic.derivation.procedure

import logic.derivation.ProofResult
import logic.derivation.procedure.Condition
import utils.Positive

class DerivationError(message: String)
  extends IllegalArgumentException(s"Derivation error: $message")

def createProof(context: Map[Positive, Seq[Condition]],
                goals: Map[Positive, Seq[Condition]],
                proof: Seq[(utils.Positive, logic.parsing.ProofStep)]): ProofResult =
  ???