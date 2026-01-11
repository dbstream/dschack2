// SPDX-License-Identifier: GPL-3.0-or-later
/* Search routine.

   Copyright (C) 2026 David Bergström  */

#include <array>
#include <algorithm>
#include <memory>
#include <optional>
#include <tuple>

#include "Engine.h"
#include "MoveGen.h"
#include "Search.h"
#include "Transposition.h"

namespace DSchack {
  static constexpr int MAX_HISTORY = 10000;

  static constexpr int ONEPLY = 16;

  static constexpr void updateHistory(int &location, int bonus)
  {
    if (bonus < -MAX_HISTORY)
      bonus = -MAX_HISTORY;
    if (bonus > MAX_HISTORY)
      bonus = MAX_HISTORY;

    int abs_bonus = (bonus > 0) ? bonus : -bonus;

    location += bonus - (location * abs_bonus) / MAX_HISTORY;
  }

  /** SEE: Static Exchange Evaluation - cheaply evaluate a capture.
  @pos: the current position
  @move: the move to statically evaluate
  Returns the expected change in material.  */
  static int SEE(const Position &pos, Move move)
  {
    static constexpr int pieceValues[6] = { 90, 300, 330, 495, 980, 10000 };

    Color us = pos.sideToMove();
    Color them = (us == WHITE) ? BLACK : WHITE;

    int gain[36];
    int depth = 0;
    int toSq = move.toSquare();
    int attacker = move.piece();
    Bitboard occ = pos.pieces(BOTH, ALL) ^ BB(move.fromSquare());
    Bitboard att = pos.attackers(BOTH, toSq);
    Bitboard from = BB(move.fromSquare());
    Bitboard bishopXray = BishopAttacks(0, toSq);
    Bitboard rookXray = RookAttacks(0, toSq);
    if (move.isCapture())
      gain[0] = pieceValues[move.capture()];
    else
      gain[0] = 0;
    do {
      depth++;
      gain[depth] = pieceValues[attacker] - gain[depth - 1];
      occ ^= from;
      att ^= from;
      if (from & bishopXray)
	att |= BishopAttacks(occ, toSq) & occ & (pos.pieces(BOTH, BISHOP)
					       | pos.pieces(BOTH, QUEEN));
      from = 0;
      for (int i = 0; i < 6; i++) {
	Bitboard pcs = att & pos.pieces((depth & 1) ? them : us, static_cast<PieceType>(i));
	if (pcs) {
	  attacker = i;
	  from = LS1B(pcs);
	  break;
	}
      }
    } while(from);
    while (--depth) {
      int x = -gain[depth];
      if (x < gain[depth - 1])
	gain[depth - 1] = x;
    }
    return gain[0];
  }

  static thread_local Move mp_moves_buffer[10000];
  static thread_local Move *mp_moves_end = mp_moves_buffer;

  class MovePicker {
    Move *m_prevEnd = mp_moves_end;
    int m_phase = 0;
    int m_endPhase;

    Move *m_current = nullptr;
    Move *m_end = nullptr;

    const Position &m_pos;

    int (&m_historyArray)[64][64];

  public:
    MovePicker(const Position &pos, int (&historyArray)[64][64], bool quiescence = false)
      : m_pos(pos), m_historyArray(historyArray)
    {
      if (quiescence)
	m_endPhase = 2;
      else
	m_endPhase = 3;
    }

    ~MovePicker()
    {
      mp_moves_end = m_prevEnd;
    }

    std::optional<Move> nextMove()
    {
      while (m_current == m_end) {
	m_phase++;
	if (m_phase == m_endPhase)
	  return std::nullopt;

	m_current = m_prevEnd;
	if (m_phase == 1) {
	  m_end = GenerateCaptures(m_pos, m_current);
	  std::sort(m_current, m_end, [](Move lhs, Move rhs){
	    if (lhs.capture() > rhs.capture())
	      return true;
	    if (lhs.capture() < rhs.capture())
	      return false;
	    return lhs.piece() < rhs.piece();
	  });
	} else {
	  m_end = GenerateQuiets(m_pos, m_current);
	  std::sort(m_current, m_end, [this](Move lhs, Move rhs){
	    return m_historyArray[lhs.fromSquare()][lhs.toSquare()]
	         > m_historyArray[rhs.fromSquare()][rhs.toSquare()];
	  });
	}
	mp_moves_end = m_end;
      }

      return *(m_current++);
    }

    void betaCutoff(int depth)
    {
      if (m_phase != 2 || depth < ONEPLY)
	return;

      int historyBonus = (300 * depth) / ONEPLY - 250;

      Move *pMove = m_current - 1;
      updateHistory(m_historyArray[pMove->fromSquare()][pMove->toSquare()],
		    historyBonus);

      while (pMove != m_prevEnd) {
	pMove--;
	updateHistory(m_historyArray[pMove->fromSquare()][pMove->toSquare()],
		      -historyBonus);
      }
    }
  };

  static constexpr int MAX_PLY = 100;

  class Searcher {
    Engine *m_pEngine;
    SearchGlobalState *m_pGlobal;
    TranspositionTable *m_tt;
    uint64_t m_numNodes = 0;
    uint64_t m_numNodesPerS = 0;
    uint64_t m_lastPeriodicInfo;
    int m_numRootMovesExamined;
    int m_moveOffset;
    Move m_movelist[MAX_PLY + 100]; // +100 to leave space for past moves (capped by rule50).
    int m_historyArray[2][64][64];

    bool pondering()
    {
      return m_pGlobal->ponder.load(std::memory_order_acquire);
    }

    bool shouldHardStop()
    {
      return m_pGlobal->stopRequested.load(std::memory_order_relaxed);
    }

    bool shouldSoftStop()
    {
      if (!pondering() && !m_pGlobal->infinite) {
	uint64_t limit = m_pGlobal->softTimeLimit;
	if (limit && CurrentTime() >= limit) {
	  return true;
	}
      }

      return false;
    }

    /** detectRepetition: detect repetition of a position.
    @ply0: the ply after the last rule50-breaking move was made.
    @ply: the current ply.  */
    bool detectRepetition(int ply0, int ply)
    {
      if ((ply0 & 1) != (ply & 1))
	ply0++;

      Bitboard colorBB = 0;
      Bitboard pieceBB[6];
      for (int i = 0; i < 6; i++)
	pieceBB[i] = 0;
      int count = 0;

      for (int i = ply; i > ply0; i -= 2) {
	Move move1 = m_movelist[m_moveOffset + i - 1];
	Move move2 = m_movelist[m_moveOffset + i - 2];
	if (!colorBB)
	  count++;
	colorBB ^= BB(move1.fromSquare()) ^ BB(move1.toSquare());
	if (!colorBB)
	  count--;
	if (!pieceBB[move1.piece()])
	  count++;
	pieceBB[move1.piece()] ^= BB(move1.fromSquare()) ^ BB(move1.toSquare());
	if (!pieceBB[move1.piece()])
	  count--;
	if (!pieceBB[move2.piece()])
	  count++;
	pieceBB[move2.piece()] ^= BB(move2.fromSquare()) ^ BB(move2.toSquare());
	if (!pieceBB[move2.piece()])
	  count--;
	if (!count)
	  return true;
      }

      return false;
    }

    // Increment numNodes. Return false if we should hard-stop.
    bool incrementNodes()
    {
      if (shouldHardStop())
	return false;

      m_numNodes++;
      m_numNodesPerS++;

      // Every 1000 or so nodes, check the current time.

      if (m_numNodesPerS & 1023)
	return true;

      uint64_t t = CurrentTime();

      // Try to print nps once per second.
      int elapsed = t - m_lastPeriodicInfo;
      if (elapsed >= 1000) {
	m_pEngine->getCallbacks().nps(1000 * m_numNodesPerS / elapsed,
				      m_tt->hashfull());
	m_numNodesPerS = 0;
	m_lastPeriodicInfo = t;
      }

      uint64_t limit = m_pGlobal->hardTimeLimit.load(std::memory_order_relaxed);
      if (limit && t >= limit) {
	m_pGlobal->stopRequested.store(true, std::memory_order_relaxed);
	return false;
      }

      return true;
    }

    static constexpr int pieceValueTable_mg[6] = { 82, 337, 365, 477, 1025, 0 };
    static constexpr int pieceValueTable_eg[6] = { 94, 281, 297, 512, 936, 0 };

    // These are the so called PeSTO tables.

    static constexpr short psqt_mg[6][64] = {
	{	// PAWN
      0,   0,   0,   0,   0,   0,  0,   0,
     98, 134,  61,  95,  68, 126, 34, -11,
     -6,   7,  26,  31,  65,  56, 25, -20,
    -14,  13,   6,  21,  23,  12, 17, -23,
    -27,  -2,  -5,  12,  17,   6, 10, -25,
    -26,  -4,  -4, -10,   3,   3, 33, -12,
    -35,  -1, -20, -23, -15,  24, 38, -22,
      0,   0,   0,   0,   0,   0,  0,   0,
	}, {	// KNIGHT
    -167, -89, -34, -49,  61, -97, -15, -107,
     -73, -41,  72,  36,  23,  62,   7,  -17,
     -47,  60,  37,  65,  84, 129,  73,   44,
      -9,  17,  19,  53,  37,  69,  18,   22,
     -13,   4,  16,  13,  28,  19,  21,   -8,
     -23,  -9,  12,  10,  19,  17,  25,  -16,
     -29, -53, -12,  -3,  -1,  18, -14,  -19,
    -105, -21, -58, -33, -17, -28, -19,  -23,
	}, {	// BISHOP
    -29,   4, -82, -37, -25, -42,   7,  -8,
    -26,  16, -18, -13,  30,  59,  18, -47,
    -16,  37,  43,  40,  35,  50,  37,  -2,
     -4,   5,  19,  50,  37,  37,   7,  -2,
     -6,  13,  13,  26,  34,  12,  10,   4,
      0,  15,  15,  15,  14,  27,  18,  10,
      4,  15,  16,   0,   7,  21,  33,   1,
    -33,  -3, -14, -21, -13, -12, -39, -21,
	}, {	// ROOK
     32,  42,  32,  51, 63,  9,  31,  43,
     27,  32,  58,  62, 80, 67,  26,  44,
     -5,  19,  26,  36, 17, 45,  61,  16,
    -24, -11,   7,  26, 24, 35,  -8, -20,
    -36, -26, -12,  -1,  9, -7,   6, -23,
    -45, -25, -16, -17,  3,  0,  -5, -33,
    -44, -16, -20,  -9, -1, 11,  -6, -71,
    -19, -13,   1,  17, 16,  7, -37, -26,
	}, {	// QUEEN
    -28,   0,  29,  12,  59,  44,  43,  45,
    -24, -39,  -5,   1, -16,  57,  28,  54,
    -13, -17,   7,   8,  29,  56,  47,  57,
    -27, -27, -16, -16,  -1,  17,  -2,   1,
     -9, -26,  -9, -10,  -2,  -4,   3,  -3,
    -14,   2, -11,  -2,  -5,   2,  14,   5,
    -35,  -8,  11,   2,   8,  15,  -3,   1,
     -1, -18,  -9,  10, -15, -25, -31, -50,
	}, {	// KING
    -65,  23,  16, -15, -56, -34,   2,  13,
     29,  -1, -20,  -7,  -8,  -4, -38, -29,
     -9,  24,   2, -16, -20,   6,  22, -22,
    -17, -20, -12, -27, -30, -25, -14, -36,
    -49,  -1, -27, -39, -46, -44, -33, -51,
    -14, -14, -22, -46, -44, -30, -15, -27,
      1,   7,  -8, -64, -43, -16,   9,   8,
    -15,  36,  12, -54,   8, -28,  24,  14,
	}
    };

    static constexpr short psqt_eg[6][64] = {
	{	// PAWN
      0,   0,   0,   0,   0,   0,   0,   0,
    178, 173, 158, 134, 147, 132, 165, 187,
     94, 100,  85,  67,  56,  53,  82,  84,
     32,  24,  13,   5,  -2,   4,  17,  17,
     13,   9,  -3,  -7,  -7,  -8,   3,  -1,
      4,   7,  -6,   1,   0,  -5,  -1,  -8,
     13,   8,   8,  10,  13,   0,   2,  -7,
      0,   0,   0,   0,   0,   0,   0,   0,
	}, {	// KNIGHT
    -58, -38, -13, -28, -31, -27, -63, -99,
    -25,  -8, -25,  -2,  -9, -25, -24, -52,
    -24, -20,  10,   9,  -1,  -9, -19, -41,
    -17,   3,  22,  22,  22,  11,   8, -18,
    -18,  -6,  16,  25,  16,  17,   4, -18,
    -23,  -3,  -1,  15,  10,  -3, -20, -22,
    -42, -20, -10,  -5,  -2, -20, -23, -44,
    -29, -51, -23, -15, -22, -18, -50, -64,
	}, {	// BISHOP
    -14, -21, -11,  -8, -7,  -9, -17, -24,
     -8,  -4,   7, -12, -3, -13,  -4, -14,
      2,  -8,   0,  -1, -2,   6,   0,   4,
     -3,   9,  12,   9, 14,  10,   3,   2,
     -6,   3,  13,  19,  7,  10,  -3,  -9,
    -12,  -3,   8,  10, 13,   3,  -7, -15,
    -14, -18,  -7,  -1,  4,  -9, -15, -27,
    -23,  -9, -23,  -5, -9, -16,  -5, -17,
	}, {	// ROOK
    13, 10, 18, 15, 12,  12,   8,   5,
    11, 13, 13, 11, -3,   3,   8,   3,
     7,  7,  7,  5,  4,  -3,  -5,  -3,
     4,  3, 13,  1,  2,   1,  -1,   2,
     3,  5,  8,  4, -5,  -6,  -8, -11,
    -4,  0, -5, -1, -7, -12,  -8, -16,
    -6, -6,  0,  2, -9,  -9, -11,  -3,
    -9,  2,  3, -1, -5, -13,   4, -20,
	}, {	// QUEEN
     -9,  22,  22,  27,  27,  19,  10,  20,
    -17,  20,  32,  41,  58,  25,  30,   0,
    -20,   6,   9,  49,  47,  35,  19,   9,
      3,  22,  24,  45,  57,  40,  57,  36,
    -18,  28,  19,  47,  31,  34,  39,  23,
    -16, -27,  15,   6,   9,  17,  10,   5,
    -22, -23, -30, -16, -16, -23, -36, -32,
    -33, -28, -22, -43,  -5, -32, -20, -41,
	}, {	// KING
    -74, -35, -18, -18, -11,  15,   4, -17,
    -12,  17,  14,  17,  17,  38,  23,  11,
     10,  17,  23,  15,  20,  45,  44,  13,
     -8,  22,  24,  27,  26,  33,  26,   3,
    -18,  -4,  21,  24,  27,  23,   9, -11,
    -19,  -3,  11,  21,  23,  16,   7,  -9,
    -27, -11,   4,  13,  14,   4,  -5, -17,
    -53, -34, -21, -11, -28, -14, -24, -43
	}
    };

    static constexpr int phaseInc[6] = {0, 1, 1, 2, 4, 0};

    static constexpr std::array<PieceType, 6> allPieceTypes = {
      PAWN,
      KNIGHT,
      BISHOP,
      ROOK,
      QUEEN,
      KING
    };

    Score Evaluate(const Position &pos)
    {
      int phase = 0;
      int score_mg = 0;
      int score_eg = 0;

      for (PieceType piece : allPieceTypes) {
	Bitboard white_occ = pos.pieces(WHITE, piece);
	Bitboard black_occ = pos.pieces(BLACK, piece);

	score_mg += pieceValueTable_mg[piece] * (Popcount(white_occ) - Popcount(black_occ));
	score_eg += pieceValueTable_eg[piece] * (Popcount(white_occ) - Popcount(black_occ));
	phase += phaseInc[piece] * (Popcount(white_occ) + Popcount(black_occ));

	while (Bitboard sqm = PopLS1B(white_occ)) {
	  score_mg += psqt_mg[piece][Sq(sqm) ^ 56];
	  score_eg += psqt_eg[piece][Sq(sqm) ^ 56];
	}
	while (Bitboard sqm = PopLS1B(black_occ)) {
	  score_mg -= psqt_mg[piece][Sq(sqm)];
	  score_eg -= psqt_eg[piece][Sq(sqm)];
	}
      }

      if (phase > 24) phase = 24;

      int materialScore = (score_mg * phase + score_eg * (24 - phase)) / 24;

      int score = materialScore;
      if (pos.sideToMove() == BLACK)
	return -score;
      else
	return score;
    }

    Score Quiesce(const Position &pos, int ply,
		  int repPly, Score alpha, Score beta)
    {
      if (ply >= MAX_PLY)
	return Evaluate(pos);

      if (!incrementNodes())
	return alpha;

      std::optional<TTEntry> ttEntry = m_tt->probe(pos);
      if (ttEntry) {
	if (ttEntry->boundType != UPPERBOUND && ttEntry->score >= beta)
	  return ttEntry->score;
      }

      Score standing_eval = Evaluate(pos);
      if (standing_eval >= alpha) {
	alpha = standing_eval;
	if (standing_eval >= beta)
	  return standing_eval;
      }

      Score bestScore = standing_eval;

      MovePicker movePicker(pos, m_historyArray[pos.sideToMove() != WHITE], true);
      while (std::optional<Move> optMove = movePicker.nextMove()) {
	Move move = optMove.value();

	Position nextPos = pos;
	nextPos.makeMove(move);
	m_movelist[m_moveOffset + ply] = move;

	Score score = Quiesce(nextPos, ply + 1,
			      ply + 1, -beta, -alpha).negated();
	if (score > bestScore) {
	  bestScore = score;
	  if (score > alpha) {
	    alpha = score;
	    if (score >= beta) {
	      m_tt->insert(pos, move, beta, LOWERBOUND, 0);
	      return score;
	    }
	  }
	}
      }

      return bestScore;
    }

    template<bool IsPV>
    Score SearchNode(const Position &pos, int ply, int depth,
		     int repPly, Score alpha, Score beta)
    {
      // Detect draw by 50-moves rule.
      if (pos.rule50() >= 100)
	return Score(0);

      // Detect draw by repetition.
      if (detectRepetition(repPly, ply))
	return Score(0);

      if (ply >= MAX_PLY)
	return Evaluate(pos);

      // Check extension.
      if (pos.inCheck())
	depth += ONEPLY;

      if (depth <= 0)
	return Quiesce(pos, ply, repPly, alpha, beta);

      if (!incrementNodes())
	return alpha;

      Score initialAlpha(alpha);

      Move bestMove;
      Score bestScore(CHECKMATE_SCORE);

      /* Always search the TT move first, if there is one. */

      Move ttMove;
      std::optional<TTEntry> ttEntry = m_tt->probe(pos);
      if (ttEntry) {
	ttMove = ttEntry->move;

	/* Allow transposition table cutoffs if the TT entry was
	   searched with at least our depth and we are not the
	   principal variation.  */

	if (!IsPV && ttEntry->depth >= depth) {
	  if (ttEntry->boundType != UPPERBOUND && ttEntry->score >= beta)
	    return ttEntry->score;
	}

	Position nextPos = pos;
	nextPos.makeMove(ttMove);
	m_movelist[m_moveOffset + ply] = ttMove;

	int nextRepPly = ttMove.resetsRule50() ? ply + 1 : repPly;
	Score score = SearchNode<IsPV>(nextPos, ply + 1, depth - ONEPLY,
				       nextRepPly, -beta, -alpha).negated();

	if (shouldHardStop())
	  return score;

	bestMove = ttMove;
	bestScore = score;
	if (score > alpha) {
	  alpha = score;
	  if (score >= beta) {
	    m_tt->insert(pos, ttMove, beta, LOWERBOUND, depth);
	    return score;
	  }
	}
      }

      /* Now search all other moves. */

      MovePicker movePicker(pos, m_historyArray[pos.sideToMove() != WHITE]);
      while (std::optional<Move> optMove = movePicker.nextMove()) {
	Move move = optMove.value();

	/* Skip the TT move, as we already searched that one before.  */

	if (move.sameAs(ttMove))
	  continue;

	Position nextPos = pos;
	nextPos.makeMove(move);
	m_movelist[m_moveOffset + ply] = move;

	int nextRepPly = move.resetsRule50() ? ply + 1 : repPly;
	Score score = SearchNode<false>(nextPos, ply + 1, depth - ONEPLY,
					nextRepPly, -beta, -alpha).negated();

	if (shouldHardStop())
	  return score;

	if (score > bestScore) {
	  bestMove = move;
	  bestScore = score;
	  if (score > alpha) {
	    alpha = score;
	    if (score >= beta) {
	      movePicker.betaCutoff(depth);
	      m_tt->insert(pos, move, beta, LOWERBOUND, depth);
	      return score;
	    }
	  }
	}
      }

      if (bestScore == CHECKMATE_SCORE && !pos.inCheck())
	bestScore = Score(0);

      if (bestScore <= initialAlpha)
	m_tt->insert(pos, bestMove, alpha, UPPERBOUND, depth);
      else
	m_tt->insert(pos, bestMove, bestScore, EXACT, depth);
      return bestScore;
    }

    std::tuple<Move, Score, BoundType> SearchRoot(int depth, Move prevBestMove,
						  Score alpha, Score beta)
    {
      m_numNodes = 0;
      m_numRootMovesExamined = 0;

      Move bestMove;
      Score bestScore(CHECKMATE_SCORE);

      Score initialAlpha = alpha;

      /* Always search prevBestMove first.  */

      {
	Position nextPos = m_pEngine->getPosition();
	nextPos.makeMove(prevBestMove);
	m_movelist[m_moveOffset] = prevBestMove;

	int repPly = prevBestMove.resetsRule50() ? 1 : -m_moveOffset;
	Score score = SearchNode<true>(nextPos, 1, depth - ONEPLY,
				       repPly, -beta, -alpha).negated();

	if (!shouldHardStop())
	  m_numRootMovesExamined++;

	bestMove = prevBestMove;
	bestScore = score;
	if (score > alpha) {
	  alpha = score;
	  if (score >= beta)
	    return std::make_tuple(prevBestMove, score, LOWERBOUND);
	}
      }

      MovePicker movePicker(m_pEngine->getPosition(),
			    m_historyArray[m_pEngine->getPosition().sideToMove() != WHITE]);

      bool searchedAllMoves = false;
      for(;;) {
	std::optional<Move> optMove = movePicker.nextMove();
	if (!optMove) {
	  searchedAllMoves = true;
	  break;
	}

	Move move = optMove.value();

	/* Search all moves in this position besides the TT
	   move, since we already searched that one earlier.  */
	if (move.sameAs(prevBestMove))
	  continue;

	Position nextPos = m_pEngine->getPosition();
	nextPos.makeMove(move);
	m_movelist[m_moveOffset] = move;

	int repPly = move.resetsRule50() ? 1 : -m_moveOffset;
	Score score = SearchNode<false>(nextPos, 1, depth - ONEPLY,
					repPly, -beta, -alpha).negated();

	if (shouldHardStop())
	  break;

	m_numRootMovesExamined++;
	if (score > bestScore) {
	  bestMove = move;
	  bestScore = score;
	  if (score > alpha) {
	    alpha = score;
	    if (score >= beta) {
	      movePicker.betaCutoff(depth);
	      return std::make_tuple(move, score, LOWERBOUND);
	    }
	  }
	}

	/* Now is a good time to check for soft-stop. We have
	   searched at least one move so we have a LOWERBOUND
	   that we can fall back on.  */
	if (shouldSoftStop())
	  break;
      }

      /* If we fail low, return UPPERBOUND. If we fail high,
	 or we stopped early, return LOWERBOUND. Otherwise,
	 return EXACT.

	 Note that we can incorrectly return UPPERBOUND in
	 some cases where we fail low before having searched
	 any move that raises alpha.  This can happen even
	 when there is a move which raises alpha, for example
	 if a time limit is reached before we have finished
	 searching.  */
      if (bestScore <= initialAlpha)
	return std::make_tuple(bestMove, alpha, UPPERBOUND);
      else if (bestScore < beta && searchedAllMoves)
	return std::make_tuple(bestMove, bestScore, EXACT);
      else
	return std::make_tuple(bestMove, bestScore, LOWERBOUND);
    }

  public:
    Searcher(Engine *engine, SearchGlobalState *global,
	     TranspositionTable *tt)
    {
      m_pEngine = engine;
      m_pGlobal = global;
      m_tt = tt;
      m_lastPeriodicInfo = global->goTime;

      const std::vector<Move> repMoves = engine->getRepetitionMoves();
      m_moveOffset = repMoves.size();
      for (int i = 0; i < m_moveOffset; i++)
	m_movelist[i] = repMoves[i];
      for (int i = 0; i < 2; i++) {
	for (int j = 0; j < 64; j++) {
	  for (int k = 0; k < 64; k++)
	    m_historyArray[i][j][k] = 0;
	}
      }
    }

    void sendScoreAndPV(Score score, BoundType boundType, int depth,
			uint64_t numNodes, int elapsedMs,
			std::optional<Move> bestMove)
    {
      int numPvMoves = 0;
      if (bestMove) {
	numPvMoves = 1;
	m_movelist[m_moveOffset] = bestMove.value();
	Position pos = m_pEngine->getPosition();
	pos.makeMove(bestMove.value());
	int rep_ply = -m_moveOffset;
	if (bestMove.value().resetsRule50())
	  rep_ply = 1;

	while (numPvMoves < MAX_PLY) {
	  if (detectRepetition(rep_ply, numPvMoves))
	    break;
	  std::optional<TTEntry> ttEntry = m_tt->probe(pos);
	  if (!ttEntry)
	    break;
	  m_movelist[m_moveOffset + numPvMoves++] = ttEntry->move;
	  pos.makeMove(ttEntry->move);
	  if (ttEntry->move.resetsRule50())
	    rep_ply = numPvMoves;
	}
      }

      std::span<Move> pv = std::span<Move>(m_movelist + m_moveOffset,
					   m_movelist + m_moveOffset + numPvMoves);
      m_pEngine->getCallbacks().score(score, boundType, depth,
				      numNodes, elapsedMs, pv);
    }

    std::optional<Move> getPonderMove(Move prevBestMove)
    {
      Position pos = m_pEngine->getPosition();
      pos.makeMove(prevBestMove);

      std::optional<TTEntry> ttEntry = m_tt->probe(pos);
      if (ttEntry)
	return ttEntry->move;
      else
	return std::nullopt;
    }

    void Search()
    {
      /* We always need some move and score to return in case we are
	 stopped before we even have a chance to search anything.  This
	 is a quick and dirty way to do that.  */
      Move prevBestMove = MovePicker(m_pEngine->getPosition(), m_historyArray[0]).nextMove().value();
      Score prevScore(-20000);

      /* Run a first iteration of iterative deepening with an open window
	 to get an initial approximation of score.  */
      {
	auto [move, score, boundType] = SearchRoot(ONEPLY, prevBestMove,
						   SCORE_MIN, SCORE_MAX);

	/* If we are stopped here, we have nothing to go on, so we
	   return the move we arbitrarily picked above  (the other
	   option would be crashing).  */
	if (shouldHardStop()) {
	  m_pGlobal->inProgress.store(false);
	  m_pEngine->getCallbacks().bestmove(prevBestMove, std::nullopt);
	  return;
	}

	prevBestMove = move;
	prevScore = score;
	sendScoreAndPV(score, boundType, 1, m_numNodes,
		       CurrentTime() - m_pGlobal->goTime,
		       std::nullopt);
      }

      /* Iterative deepening loop. We begin at depth 2, because we
         already searched depth 1 earlier. */
      for (int depth = 2 * ONEPLY;; depth += ONEPLY) {

	if (shouldHardStop())
	  break;

	/* If we are not pondering, right now is a very good time
	   to check for soft time constraints.  */

	if (!pondering() && !m_pGlobal->infinite) {
	  if (m_pGlobal->depthLimit && (depth / ONEPLY) > m_pGlobal->depthLimit)
	    break;
	  uint64_t limit = m_pGlobal->softTimeLimit;
	  if (limit && CurrentTime() >= limit) {
	    break;
	  }

	  /* If we know that we have a checkmate, we can break
	     early here to not waste our time.  Additionally,
	     when we see that we are being mated, we assume that
	     our opponent also sees this, so just move immediately.  */
	  if (prevScore.isCheckmate())
	    break;
	}

	/* Aspiration windows: use the search result from the previous
	   iteration of iterative deepening as a decent guess for the
	   score in this iteration. Set alpha and beta bounds based on
	   a fixed window around prevScore. If the search fails low or
	   high, increase the bound that failed but keep the other
	   bound as it is.  */
	static constexpr int maxWindowIndex = 2;
	static constexpr int gradualWidening[maxWindowIndex + 1] = { 25, 100, 65535 };

	int leftWindowIndex = 0;
	int rightWindowIndex = 0;
	Score alpha = prevScore.boundedAdd(-gradualWidening[leftWindowIndex]);
	Score beta = prevScore.boundedAdd(gradualWidening[rightWindowIndex]);

	auto [move, score, boundType] = SearchRoot(depth, prevBestMove, alpha, beta);
	while ((leftWindowIndex < maxWindowIndex && score <= alpha) || (rightWindowIndex < maxWindowIndex && beta <= score)) {
	  if (shouldSoftStop() || shouldHardStop() || !m_numRootMovesExamined)
	    break;

	  /* If the search failed low, the move returned can be bogus,
	     so don't update the PV in that case.  */

	  if (score <= alpha && leftWindowIndex < maxWindowIndex) {

	    alpha = prevScore.boundedAdd(-gradualWidening[++leftWindowIndex]);
	    sendScoreAndPV(score, boundType, depth / ONEPLY, m_numNodes,
			   CurrentTime() - m_pGlobal->goTime, move);

	  } else {
	    beta = prevScore.boundedAdd(gradualWidening[++rightWindowIndex]);

	    // Note: don't update prevScore as we use it for window centre.
	    prevBestMove = move;
	    sendScoreAndPV(score, boundType, depth / ONEPLY, m_numNodes,
			   CurrentTime() - m_pGlobal->goTime, move);
	  }

	  std::tie(move, score, boundType) = SearchRoot(depth, prevBestMove, alpha, beta);
	}

	/* If we did not have a chance to fully explore any root
	   moves, stop here.  Even if we did not examine all root
	   moves, as long as we examined at least one move we can
	   be sure that we examined the previous PV move, because
	   we always do that first.  */
	if (!m_numRootMovesExamined)
	  break;

	/* SearchRoot guarantees that if m_numRootMovesExamined is
	   greater than or equal to one, the move that is returned
	   was fully examined.  */
	prevBestMove = move;
	prevScore = score;
	sendScoreAndPV(score, boundType, depth / ONEPLY, m_numNodes,
		       CurrentTime() - m_pGlobal->goTime, move);
      }

      /* Search is done. Print a ponder move if prevBestMove is
         the same as the PV move.  */
      std::optional<Move> ponderMove = getPonderMove(prevBestMove);

      m_pGlobal->inProgress.store(false);
      m_pEngine->getCallbacks().bestmove(prevBestMove, ponderMove);
    }
  };

  void Search(Engine *engine, SearchGlobalState *state,
	      TranspositionTable *tt)
  {
    std::unique_ptr<Searcher> searcher = std::make_unique<Searcher>(engine, state, tt);
    searcher->Search();
  }
} // namespace DSchack
