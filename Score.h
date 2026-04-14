// SPDX-License-Identifier: GPL-3.0-or-later
/* Score

   Copyright (C) 2026 David Bergström  */

#pragma once

#include <stdint.h>

namespace DSchack {
  enum BoundType {
    EXACT,
    UPPERBOUND,
    LOWERBOUND
  };

  static constexpr int SCORE_MIN = -32767;
  static constexpr int SCORE_MAX = 32767;

  static constexpr int SCORE_MIN_NON_DECISIVE = -32000;
  static constexpr int SCORE_MAX_NON_DECISIVE = 32000;

  static constexpr bool IsDecisive(int score)
  {
    return score < SCORE_MIN_NON_DECISIVE || score > SCORE_MAX_NON_DECISIVE;
  }

  static constexpr int DepthToMate(int score)
  {
    // Negative if lost, positive if won.

    if (score < 0)
      return -((32767 + score) / 2);
    else
      return (32768 - score) / 2;
  }

  static constexpr int MatedIn(int plies)
  {
    return -32767 + plies;
  }

  static constexpr int MateIn(int plies)
  {
    return 32767 - plies;
  }
} // namespace DSchack
