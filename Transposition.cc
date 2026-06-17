// SPDX-License-Identifier: GPL-3.0-or-later
/* Transposition table.

   Copyright (C) 2026 David Bergström

The transposition table is probably the single most important
datastructure for the performance of a chess engine. Every node
probes the transposition table as the very first thing it does,
so it is really important that it is fast.

It is also really important that the transposition table makes
informed decisions about when to evict hash entries. This is
called the "replacement policy".

We implement a simple replacement policy where each hash index
into the transposition table maps to two entries: one which is
depth-preferred and one always-replace entry.  */

#include "Transposition.h"

namespace DSchack {
  TranspositionTable::TranspositionTable(int megabytes)
  {
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

    uint64_t numClusters = ((uint64_t) megabytes * 1048576 / sizeof(TTCluster));

    /* Make sure numEntries is a power of two.  */
    while (numClusters & ~(numClusters & -numClusters))
      numClusters &= ~(numClusters & -numClusters);

    m_entries.resize(numClusters);
    m_mask = numClusters - 1;
    m_numClusters = numClusters;
    clear();
  }

  void TranspositionTable::clear()
  {
    m_numFull = 0;
    for (uint64_t i = 0; i < m_numClusters; i++) {
      for (int j = 0; j < 4; j++) {
	m_entries[i].entries[j].key = 0;
	m_entries[i].entries[j].move = 0;
	m_entries[i].entries[j].score = 0;
	m_entries[i].entries[j].flags = 0;
      }
    }
  }

  int TranspositionTable::hashfull()
  {
    return (250 * m_numFull) / m_numClusters;
  }

  std::optional<TTEntry> TranspositionTable::probe(const Position &pos, int ply)
  {
    uint64_t key = pos.hash();
    uint64_t index = key & m_mask;
    TTCluster cluster = m_entries[index];
    TTEntryEncoded enc;
    for (int i = 0; i < 4; i++) {
      enc = cluster.entries[i];
      if (enc.key == (key >> 48))
	goto found;
    }

    return std::nullopt;

  found:
    int fromSq = enc.move & 63;
    int toSq = (enc.move >> 6) & 63;
    PromoteType promotion = static_cast<PromoteType>(enc.move >> 12);
    std::optional<Move> move = ParseMove(pos, fromSq, toSq, promotion);
    if (!move)
      return std::nullopt;

    TTEntry e;
    e.move = move.value();
    e.score = enc.score;
    e.boundType = static_cast<BoundType>(enc.flags & 3);
    e.depth = (int) enc.depth - 1;

    if (e.depth < 0)
      return std::nullopt;

    /* Adjust mate scores from depth-to-mate from root to
       depth-to-mate from transposition node. Additionally,
       set the score to the lowest or highest nondecisive
       score if depth-to-mate is greater than 100 - rule50.  */
    if (IsDecisive(e.score)) {
      if (e.score < 0 && e.boundType != LOWERBOUND) {
	if (-DepthToMate(e.score) >= (100 - pos.rule50())) {
	  e.score = SCORE_MIN_NON_DECISIVE;
	  e.boundType = UPPERBOUND;
	} else
	  e.score += ply;
      } else if (e.score > 0 && e.boundType != UPPERBOUND) {
	if (DepthToMate(e.score) >= (100 - pos.rule50())) {
	  e.score = SCORE_MAX_NON_DECISIVE;
	  e.boundType = LOWERBOUND;
	} else
	  e.score -= ply;
      }
    }

    return e;
  }

  void TranspositionTable::insert(const Position &pos, Move move, int score,
				  BoundType boundType, int depth, int ply)
  {
    TTEntryEncoded enc;
    enc.key = pos.hash() >> 48;

    /* Adjust mate scores from depth-to-mate from root to
       depth-to-mate from transposition node.  */
    if (IsDecisive(score)) {
      if (score < 0)
	score -= ply;
      else
	score += ply;
    }

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
    enc.score = score;
    enc.flags = static_cast<uint16_t>(boundType) | m_generation;
    enc.depth = depth + 1;

    uint64_t index = pos.hash() & m_mask;

    int minDepthIndex = 0;
    int minDepth = 1024;

    TTCluster cluster = m_entries[index];
    for (int i = 0; i < 4; i++) {
      if (enc.key == cluster.entries[i].key) {
	if (!(cluster.entries[i].depth))
	  m_numFull++;
	cluster.entries[i] = enc;
	m_entries[index] = cluster;
	return;
      }

      int d = cluster.entries[i].depth;
      if (d && m_generation == (cluster.entries[i].flags & 0xfc))
	d += 512;
      if (d < minDepth) {
	minDepth = d;
	minDepthIndex = i;
      }
    }

    if (!minDepth)
      m_numFull++;

    cluster.entries[minDepthIndex] = enc;
    m_entries[index] = cluster;
  }

  void TranspositionTable::stepGeneration()
  {
    m_generation = m_generation + 0x4;
  }
} // namespace DSchack
