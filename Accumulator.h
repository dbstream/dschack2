// SPDX-License-Identifier: GPL-3.0-or-later
/* Accumulator data types.

   Copyright (C) 2026 David Bergström  */

#pragma once

#include "Types.h"

namespace DSchack {
  class Position;

  struct Accumulator {
    int m_score_mg;
    int m_score_eg;
    int m_phase;

    void refresh(const Position &pos);

    void addPiece(Color color, PieceType piece, int square);

    void removePiece(Color color, PieceType piece, int square);
  };
} // namespace DSchack
