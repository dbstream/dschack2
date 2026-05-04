// SPDX-License-Identifier: GPL-3.0-or-later
/* really basic types

   Copyright (C) 2026 David Bergström  */

#pragma once

namespace DSchack {
  enum Color {
    WHITE,
    BLACK,
    BOTH
  };

  enum PieceType {
    PAWN,
    KNIGHT,
    BISHOP,
    ROOK,
    QUEEN,
    KING,
    ALL
  };
}
