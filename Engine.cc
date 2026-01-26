// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine

   Copyright (C) 2026 David Bergström  */

#include <iostream>

#include "Engine.h"
#include "Search.h"
#include "Transposition.h"

namespace DSchack {
  struct EngineInternal {
    TranspositionTable transpositionTable;
  };

  Engine::~Engine()
  {}

  Engine::Engine()
  {
    m_internal = std::make_unique<EngineInternal>();
  }

  void Engine::newGame()
  {
    m_internal->transpositionTable.clear();
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

  Move Engine::go()
  {
    return Search(this, &m_internal->transpositionTable);
  }
}
