// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine

   Copyright (C) 2026 David Bergström  */

#include "Engine.h"
#include "Search.h"
#include "Transposition.h"

namespace DSchack {
  static TranspositionTable transpositionTable;

  Engine::~Engine()
  {}

  Engine::Engine()
  {}

  void Engine::newGame()
  {
    transpositionTable.clear();
  }

  void Engine::setPosition(const Position &pos, std::span<const Move> pastMoves)
  {
    m_position = pos;
    m_numRepetitionMoves = 0;

    for (Move move : pastMoves) {
      if (move.resetsRule50()) {
	m_numRepetitionMoves = 0;
	continue;
      }

      if (m_numRepetitionMoves == 100) {
	for (int i = 0; i < 99; i++)
	  m_repetitionMoves[i] = m_repetitionMoves[i + 1];
	m_repetitionMoves[99] = move;
      } else
	m_repetitionMoves[m_numRepetitionMoves++] = move;
    }
  }

  Move Engine::go()
  {
    return Search(this, &transpositionTable);
  }
}
