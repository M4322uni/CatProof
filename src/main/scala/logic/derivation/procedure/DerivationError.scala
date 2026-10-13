// SPDX-License-Identifier: AGPL-3.0-only
// Copyright (C) 2026 Matteo Piscitello

package logic.derivation.procedure
//
//import logic.derivation.procedure.Condition
//import utils.Positive
//
class DerivationError(message: String)
  extends IllegalArgumentException(s"Derivation error: $message")
//
//def createProof(context: Seq[Condition],
//                goals: Map[Positive, Seq[Condition]],
//                proof: Seq[(utils.Positive, logic.parsing.ProofStep)]): String =
//  ???