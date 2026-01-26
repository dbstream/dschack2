// SPDX-License-Identifier: GPL-3.0-or-later
/* Transposition Tables.

   Copyright (C) 2026 David Bergström

A transposition table entry stores the following information:
  - Full Zobrist key.
  - The best move searched in a position.
  - The score that was assigned to that move.
  - Whether that score is a LOWERBOUND, an UPPERBOUND, or exact.
  - The depth to which a position was searched, expressed in
  fractional plies.  */

#pragma once

#include <stdint.h>
#include <vector>

#include "Position.h"
#include "Score.h"

namespace DSchack {

  struct TTEntryEncoded {
    uint64_t key;

    /* move[0..5]: fromSq
       move[6..11]: toSq
       move[12:15]: indicates promotion  */
    uint16_t move;

    int16_t score;

    /* flags[0]: UPPERBOUND
       flags[1]: LOWERBOUND  */
    uint16_t flags;

    uint16_t depth;
  };

  static_assert(sizeof(TTEntryEncoded) == 16 * sizeof(uint8_t));

  struct TTEntry {
    Move move;
    Score score = 0;
    BoundType boundType;
    int depth;
  };

  class TranspositionTable {
    std::vector<TTEntryEncoded> m_entries;
    uint64_t m_mask;
    uint64_t m_numEntries;
    uint64_t m_numFull;

  public:
    TranspositionTable(uint64_t numEntries = 4096);

    ~TranspositionTable();

    void resize(uint64_t numEntries);

    void clear();

    int hashfull();

    std::optional<TTEntry> probe(const Position &pos);

    void insert(const Position &pos, Move move, Score score,
		BoundType boundType, int depth);
  };
} // namespace DSchack
