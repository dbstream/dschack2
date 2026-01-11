// SPDX-License-Identifier: GPL-3.0-or-later
/* Search state.

   Copyright (C) 2026 David Bergström  */

#pragma once

#include <atomic>
#include <chrono>

#include "Engine.h"

namespace DSchack {
  class TranspositionTable;

  /** CurrentTime: get the time in milliseconds since some epoch.  */
  static uint64_t CurrentTime()
  {
    using namespace std::chrono;

    return duration_cast<milliseconds>(steady_clock::now().time_since_epoch()).count();
  }

  struct SearchGlobalState {
    /* inProgress: whether there is an in-progress search. Cleared
       by the search thread when it is about to print bestmove.  */
    std::atomic<bool> inProgress;

    /* stopRequested: the search should be stopped immediately, for
       example because the UCI 'stop' command was received.  */
    std::atomic<bool> stopRequested;

    /* ponder: the search is in ponder mode. Keep searching unless this
       is cleared or stopRequested is set.  */
    std::atomic<bool> ponder;

    /* infinite: do not stop searching until stopRequested is
       set to true.  */
    bool infinite;

    // Note: durations are in milliseconds.

    bool fixed_movetime;

    /* ctime: our time in milliseconds.  */
    int ctime;

    /* cinc: our increment per move in milliseconds.  */
    int cinc;

    /* movestogo: number of moves to go in tournament TC.  */
    int movestogo;

    /* movetime: time for this move specifically, or 0.  */
    int movetime;

    // Note: time points are milliseconds since some epoch.

    /* goTime: the time when the 'go' command was received.  */
    uint64_t goTime;

    /* softTimeLimit: deadline for stopping before next iterative
       deepening iteration. If 0, there is no deadline.  */
    uint64_t softTimeLimit;

    /* hardTimeLimit: deadline for stopping in the middle of a
       search. If 0, there is no deadline.  */
    std::atomic<uint64_t> hardTimeLimit;

    /* depthLimit: stop searching when this depth is reached.  If
       0, there is no depth limit.  */
    int depthLimit;
  };

  void Search(Engine *engine, SearchGlobalState *state,
	      TranspositionTable *tt);
} // namespace DSchack
