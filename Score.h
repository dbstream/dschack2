// SPDX-License-Identifier: GPL-3.0-or-later
/* Score

   Copyright (C) 2026 David Bergström

This file defines a data type 'Score', representing the
score of a chess position.

We have three distinct categories of scores:
  - Mate-in-N scores from the search tree.
  - Known won scores from position cache.
  - Draw scores and evaluation scores.  */

#pragma once

#include <stdint.h>

namespace DSchack {
  enum BoundType {
    EXACT,
    UPPERBOUND,
    LOWERBOUND
  };

  struct Score {
    int16_t m_value;

    constexpr Score()
      : m_value(0)
    {}

    constexpr Score(int16_t value)
      : m_value(value)
    {}

    constexpr int centipawns() const
    {
      return m_value;
    }

    constexpr bool operator==(Score other) const
    {
      return m_value == other.m_value;
    }

    constexpr bool operator>(Score other) const
    {
      return m_value > other.m_value;
    }

    constexpr bool operator<(Score other) const
    {
      return m_value < other.m_value;
    }

    constexpr bool operator<=(Score other) const
    {
      return m_value <= other.m_value;
    }

    constexpr bool operator>=(Score other) const
    {
      return m_value >= other.m_value;
    }

    constexpr bool isWinningCheckmate() const
    {
      return m_value > 32000;
    }

    constexpr bool isLosingCheckmate() const
    {
      return m_value < -32000;
    }

    constexpr bool isCheckmate() const
    {
      return isWinningCheckmate() || isLosingCheckmate();
    }

    // Negative if lost, positive if won.
    constexpr int depthToMate() const
    {
      if (m_value >= 0)
	return 32767 - m_value;
      else
	return -32767 - m_value;
    }

    // Negate all scores, and decay checkmate scores.
    constexpr Score negated() const
    {
      if (isLosingCheckmate())
	return Score(-m_value - 1); // decay the mate-in-N score one move
      else
	return Score(-m_value);
    }

    // Negate the score but don't decay checkmate.
    constexpr Score operator-() const
    {
      return Score(-m_value);
    }

    constexpr Score boundedAdd(int count) const
    {
      int x = m_value + count;
      if (x > 32767)
	x = 32767;
      else if (x < -32767)
	x = -32767;

      return Score(x);
    }

    constexpr Score asTTScore() const
    {
      if (isWinningCheckmate())
	return Score(32000);
      if (isLosingCheckmate())
	return Score(-32000);
      return *this;
    }

  };

  static constexpr Score CHECKMATE_SCORE(-32767);
  static constexpr Score SCORE_MIN(-32767);
  static constexpr Score SCORE_MAX(32767);
} // namespace DSchack
