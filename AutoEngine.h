// SPDX-License-Identifier: GPL-3.0-or-later
/* AutoEngine: wrapper around Engine used for data-generation selfplay.

   Copyright (C) 2026 David Bergström */

#pragma once

#include "Engine.h"
#include "Position.h"

namespace DSchack {
  class AutoEngine {
    Move m_bestMove;
    std::optional<int> m_score;

    class Callbacks : public EngineCallbacks {
      AutoEngine *m_engine;
    public:
      Callbacks(AutoEngine *engine)
	: m_engine(engine)
      {}

      ~Callbacks() override {}

      void score(int score, BoundType boundType,
		 int depth, int seldepth, uint64_t nodes,
		 int search_ms, std::span<const Move> pv) override
      {
	if (boundType == EXACT) {
	  m_engine->m_score = score;
	} else if (m_engine->m_score) {
	  if (boundType == LOWERBOUND) {
	    if (m_engine->m_score.value() < score)
	      m_engine->m_score = score;
	  } else {
	    if (m_engine->m_score.value() > score)
	      m_engine->m_score = score;
	  }
	}
      }

      void nps(uint64_t count, int hashfull) override {}

      void bestmove(Move move, std::optional<Move> ponderMove) override
      {
	m_engine->m_bestMove = move;
      }

      void info(std::string_view s) override {}
    };

    Callbacks m_callbacks;
    Engine m_engine;

  public:
    AutoEngine()
      : m_callbacks(this), m_engine(m_callbacks)
    {}

    void setHash(int hash)
    {
      m_engine.setHashSize(hash);
    }

    void newGame()
    {
      m_engine.newGame();
    }

    void setPosition(const Position &pos, const std::vector<Move> &pastMoves)
    {
      m_engine.setPosition(pos, pastMoves);
    }

    std::tuple<Move, std::optional<int>> search(int movetime, int nodes)
    {
      m_score = std::nullopt;
      m_engine.goSynchronous(movetime, nodes);
      return {m_bestMove, m_score};
    }

    bool isRepetitionDraw(int count)
    {
      return m_engine.isRepetitionDraw(count);
    }

    bool isMaterialDraw()
    {
      return m_engine.isMaterialDraw();
    }
  };
} // namespace DSchack
