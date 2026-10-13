// SPDX-License-Identifier: AGPL-3.0-only
// Copyright (C) 2026 Matteo Piscitello

package logic.derivation.semantics

type Type = RestrictType | CategoryType

type RestrictType = MorphismType | ObjectType

enum MorphismType:
  case HomSet(cat: Category, dom: Object, cod: Object)

  override def toString: String =
    this match
      case HomSet(cat, dom, cod) => s"$cat($dom, $cod)"

enum ObjectType:
  case Cat(cat: Category)

  override def toString: String =
    this match
      case Cat(cat) => s"$cat"

enum CategoryType:
  case -