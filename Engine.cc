// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine

   Copyright (C) 2026 David Bergström  */

#include <iostream>

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

  void Engine::go(int depth, int wtime, int btime, int winc, int binc,
		  int movetime, int movestogo, bool infinite)
  {
    std::cout << "internal error: 'go' is unimplemented\n";
  }
}
