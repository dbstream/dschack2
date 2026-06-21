// SPDX-License-Identifier: GPL-3.0-or-later
/* Data generation utilities

   Copyright (C) 2026 David Bergström  */

#pragma once

#include <vector>

#include "Position.h"

namespace DSchack {
  /* EncodeGame: encode a game in viri format.
     @startpos: starting position
     @moves: move list
     @scores: score list, relative to side to move
     @result: game result, 0=win for black 1=draw 2=win for white  */
  std::vector<uint8_t> EncodeGame(const Position &startpos,
				  std::vector<Move> moves,
				  std::vector<int> scores,
				  int result);
}
