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
    /* upper 16 bits of the 64-bit position hash key */
    uint16_t key;

    /* move[0..5]: fromSq
       move[6..11]: toSq
       move[12:15]: indicates promotion  */
    uint16_t move;

    int16_t score;

    /* flags[0]: UPPERBOUND
       flags[1]: LOWERBOUND
       flags[2:7]: generation  */
    uint8_t flags;

    uint8_t depth;
  };

  static_assert(sizeof(TTEntryEncoded) == 8 * sizeof(uint8_t));

  struct TTCluster {
    TTEntryEncoded entries[4];
  };

  static_assert(sizeof(TTCluster) == 32 * sizeof(uint8_t));

  struct TTEntry {
    Move move;
    int score = 0;
    BoundType boundType;
    int depth;
  };

  class TranspositionTable {
    std::vector<TTCluster> m_entries;
    uint64_t m_mask = 0;
    uint64_t m_numClusters = 0;
    uint64_t m_numFull = 0;
    uint8_t m_generation = 0;

  public:
    TranspositionTable(int megabytes = 16);

    ~TranspositionTable();

    void resize(int megabytes);

    void clear();

    int hashfull();

    std::optional<TTEntry> probe(const Position &pos, int ply);

    void insert(const Position &pos, Move move, int score,
		BoundType boundType, int depth, int ply);

    void stepGeneration();
  };
} // namespace DSchack
