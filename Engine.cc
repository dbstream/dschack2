// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine

   Copyright (C) 2026 David Bergström  */

#include <iostream>

#include "Engine.h"
#include "Search.h"
#include "Threads.h"
#include "Transposition.h"

namespace DSchack {
  struct EngineInternal {
    EngineThread searcherThread;
    TranspositionTable transpositionTable;
    SearchGlobalState searchGlobalState;
  };

  Engine::~Engine()
  {
    stop();
  }

  Engine::Engine(EngineCallbacks &callbacks)
    : m_callbacks(callbacks)
  {
    m_internal = std::make_unique<EngineInternal>();
  }

  bool Engine::searchInProgress()
  {
    return m_internal->searchGlobalState.inProgress.load(std::memory_order_acquire);
  }

  bool Engine::ponderInProgress()
  {
    return m_internal->searchGlobalState.ponder.load(std::memory_order_acquire);
  }

  void Engine::newGame()
  {
    m_internal->transpositionTable.clear();
  }

  void Engine::setHashSize(int megabytes)
  {
    m_internal->transpositionTable.resize(megabytes);
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

  void Engine::stop()
  {
    m_internal->searchGlobalState.ponder.store(false, std::memory_order_release);
    m_internal->searchGlobalState.stopRequested.store(true, std::memory_order_release);
    m_internal->searcherThread.waitForIdle();
  }

  static std::function<void()> callSearch(Engine *engine, SearchGlobalState *state,
					  TranspositionTable *tt)
  {
    return [engine, state, tt](){
      Search(engine, state, tt);
    };
  }

  void Engine::go(int depth, int wtime, int btime, int winc, int binc,
		  int movetime, int movestogo, int nodes,
		  bool infinite, bool ponder)
  {
    SearchGlobalState &s = m_internal->searchGlobalState;

    m_internal->searcherThread.waitForIdle();
    s.goTime = CurrentTime();
    s.inProgress.store(true, std::memory_order_relaxed);
    s.stopRequested.store(false, std::memory_order_relaxed);
    s.ponder.store(ponder, std::memory_order_relaxed);
    s.infinite = infinite;
    s.fixed_movetime = false;
    if (m_position.sideToMove() == WHITE) {
      s.ctime = wtime;
      s.cinc = winc;
    } else {
      s.ctime = btime;
      s.cinc = binc;
    }
    s.movestogo = movestogo;
    s.movetime = movetime;
    s.nodes = nodes;
    s.softTimeLimit = 0;
    s.hardTimeLimit.store(0, std::memory_order_relaxed);
    s.depthLimit = depth;

    if (s.movetime)
      s.fixed_movetime = true;

    /* If no movetime is provided and we have timing information,
       calculate a "virtual" movetime ourselves.  */
    if (s.movetime == 0 && s.ctime != 0) {
      if (s.movestogo > 0)
	s.movetime = (s.ctime + s.cinc / 2) / s.movestogo;
      else
	s.movetime = s.ctime / 20 + s.cinc / 2;
    }

    if (s.movetime != 0 && !s.ponder) {
      int hard = s.movetime - 10;
      if (hard <= 10)
	hard = s.movetime / 2;
      int soft = hard / 2;
      if (s.fixed_movetime) {
	// If this really was a 'go movetime', use the hard limit directly.
	soft = hard;
      }
      s.softTimeLimit = s.goTime + soft;
      s.hardTimeLimit.store(s.goTime + hard, std::memory_order_relaxed);
      {
	std::stringstream ss;
	ss << "timeman soft " << soft << " hard " << hard;
	getCallbacks().info(ss.view());
      }
    }

    m_internal->searcherThread.start(callSearch(this, &s,
						&m_internal->transpositionTable));
  }

  void Engine::ponderhit()
  {
    SearchGlobalState &s = m_internal->searchGlobalState;

    uint64_t ponderhitTime = CurrentTime();
    int elapsed = ponderhitTime - s.goTime;

    if (s.movetime != 0) {
      int hard = s.movetime - 10;
      if (hard <= 10)
	hard = s.movetime / 2;
      int soft = (hard - elapsed) / 2;
      if (s.fixed_movetime)
	soft = hard;

      s.softTimeLimit = ponderhitTime + soft;
      s.hardTimeLimit.store(ponderhitTime + hard, std::memory_order_relaxed);
    }

    m_internal->searchGlobalState.ponder.store(false, std::memory_order_release);
  }
}
