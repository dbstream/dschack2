// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine

   Copyright (C) 2026 David Bergström  */

#include "Engine.h"

namespace DSchack {
  bool Engine::searchInProgress()
  {
    return false;
  }

  void Engine::setPosition(const Position &pos, const std::vector<Move> &pastMoves)
  {
    m_position = pos;
    m_repetitionMoves.clear();

    for (Move move : pastMoves) {
      m_repetitionMoves.push_back(move);
      if (move.resetsRule50())
	m_repetitionMoves.clear();
    }
  }
}
