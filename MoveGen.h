// SPDX-License-Identifier: GPL-3.0-or-later
/* Move generator.

   Copyright (C) 2026 David Bergström  */

#pragma once

#include "Position.h"

namespace DSchack {
  /** GenerateCaptures: generate all legal captures, incl. en passant.
  @pos: the position.
  @buffer: pointer to the start of a buffer to fill with moves.
  Returns a pointer one past the last generated move.  */
  Move *GenerateCaptures(const Position &pos, Move *buffer);

  /** GenerateQuiets: generate all legal quiet moves.
  @pos: the position.
  @buffer: pointer to the start of a buffer to fill with moves.
  Returns a pointer one past the last generated move.  */
  Move *GenerateQuiets(const Position &pos, Move *buffer);

  /** GenerateLegalMoves: generate all legal moves.
  @pos: the position.
  @buffer: pointer to the start of a buffer to fill with moves.
  Returns a pointer one past the last generated move.  */
  Move *GenerateLegalMoves(const Position &pos, Move *buffer);
}
