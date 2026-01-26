// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine global state.

   Copyright (C) 2026 David Bergström

The engine global state (position, history arrays, transpositions) is
encapsulated into a single object of the type Engine.  */

#pragma once

#include <stdint.h>
#include <span>

#include "Position.h"
#include "Score.h"

namespace DSchack {
  class Engine {
    // The current engine position.
    Position m_position;

    // The list of past moves which are relevant for repetition detection.
    Move m_repetitionMoves[100];
    int m_numRepetitionMoves = 0;

  public:
    ~Engine();

    /** Engine: construct a new Engine.  */
    Engine();

    /** getPosition: get the current engine position.  */
    const Position &getPosition() const
    {
      return m_position;
    }

    std::span<const Move> getRepetitionMoves() const
    {
      return std::span(m_repetitionMoves, m_repetitionMoves + m_numRepetitionMoves);
    }

    /** newGame: inform the engine that positions will
	be from a new game.  The engine might use this
	to reset history arrays or transposition tables.

    May not be called if there is a search in progress.  */
    void newGame();

    /** setPosition: update the engine position.
    @pos: current position.
    @pastMoves: the moves that led up to this position.

    May not be called if there is a search in progress.  */
    void setPosition(const Position &pos, std::span<const Move> pastMoves);

    /** go: search the current position and return the best move.  */
    Move go();
  };
} // namespace DSchack
