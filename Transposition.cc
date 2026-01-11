// SPDX-License-Identifier: GPL-3.0-or-later
/* Transposition table.

   Copyright (C) 2026 David Bergström

The transposition table is probably the single most important
datastructure for the performance of a chess engine. Every node
probes the transposition table as the very first thing it does,
so it is really important that it is fast.  */

#include "Transposition.h"

namespace DSchack {
  TranspositionTable::TranspositionTable(int megabytes)
  {
    m_numEntries = 0;
    resize(megabytes); // Default to a 16 MiB transposition table.
  }

  TranspositionTable::~TranspositionTable()
  {}

  void TranspositionTable::resize(int megabytes)
  {
    if (megabytes < 1)
      megabytes = 1;
    else if (megabytes > 1024)
      megabytes = 1024;

    uint64_t numEntries = (uint64_t) megabytes * 1048576 / sizeof(TTEntryEncoded);

    /* Make sure numEntries is a power of two.  */
    while (numEntries & ~(numEntries & -numEntries))
      numEntries &= ~(numEntries & -numEntries);

    m_entries.resize(numEntries);
    m_mask = numEntries - 1;
    m_numEntries = numEntries;
    clear();
  }

  void TranspositionTable::clear()
  {
    m_numFull = 0;
    for (uint64_t i = 0; i < m_numEntries; i++)
      m_entries[i].key = 0;
  }

  int TranspositionTable::hashfull()
  {
    return (100 * m_numFull) / m_numEntries;
  }

  std::optional<TTEntry> TranspositionTable::probe(const Position &pos)
  {
    uint64_t key = pos.hash();
    if (!key)
      return std::nullopt;

    TTEntryEncoded enc = m_entries[key & m_mask];
    if (enc.key != key)
      return std::nullopt;

    int fromSq = enc.move & 63;
    int toSq = (enc.move >> 6) & 63;
    PromoteType promotion = static_cast<PromoteType>(enc.move >> 12);
    std::optional<Move> move = ParseMove(pos, fromSq, toSq, promotion);
    if (!move)
      return std::nullopt;

    TTEntry e;
    e.move = move.value();
    e.score = Score(enc.score);
    e.boundType = static_cast<BoundType>(enc.flags & 3);
    e.depth = enc.depth;
    return e;
  }

  void TranspositionTable::insert(const Position &pos, Move move, Score score,
				  BoundType boundType, int depth)
  {
    TTEntryEncoded enc;
    enc.key = pos.hash();
    if (!enc.key)
      return;

    enc.move = move.fromSquare() | (move.toSquare() << 6);
    if (move.isPromotion()) {
      switch (move.promotion()) {
      case QUEEN: enc.move |= static_cast<int>(PROMOTE_QUEEN) << 12; break;
      case KNIGHT: enc.move |= static_cast<int>(PROMOTE_KNIGHT) << 12; break;
      case BISHOP: enc.move |= static_cast<int>(PROMOTE_BISHOP) << 12; break;
      case ROOK: enc.move |= static_cast<int>(PROMOTE_ROOK) << 12; break;
      default: __builtin_unreachable();
      }
    }
    enc.score = score.asTTScore().m_value;
    enc.flags = static_cast<uint16_t>(boundType);
    enc.depth = depth;

    if (!m_entries[enc.key & m_mask].key)
      m_numFull++;
    m_entries[enc.key & m_mask] = enc;
  }
} // namespace DSchack
