// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine global state.

   Copyright (C) 2026 David Bergström

The engine global state (position, history arrays, transpositions) is
encapsulated into a single object of the type Engine.  */

#pragma once

#include <vector>

#include "Position.h"

namespace DSchack {
  class Engine {
    // The current engine position.
    Position m_position;

    // The list of past moves which are relevant for repetition detection.
    std::vector<Move> m_repetitionMoves;

  public:
    /** searchInProgress: returns true if the engine is
	currently searching a position.  */
    bool searchInProgress();

    /** setPosition: update the engine position.
    @pos: current position.
    @pastMoves: the moves that led up to this position.

    May not be called if there is a search in progress.  */
    void setPosition(const Position &pos, const std::vector<Move> &pastMoves);

    /** getPosition: get the current engine position.  */
    const Position &getPosition() const
    {
      return m_position;
    }
  };
} // namespace DSchack
