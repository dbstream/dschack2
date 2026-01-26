// SPDX-License-Identifier: GPL-3.0-or-later
/* Search routine.

   Copyright (C) 2026 David Bergström  */

#include <algorithm>
#include <tuple>

#include "Engine.h"
#include "Evaluate.h"
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
    TranspositionTable *m_tt;
    uint64_t m_numNodes = 0;
    int m_moveOffset;
    Move m_movelist[MAX_PLY + 100]; // +100 to leave space for past moves (capped by rule50).
    int m_historyArray[2][64][64];
    bool m_shouldStop = false;

    bool shouldHardStop()
    {
      return m_shouldStop;
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

    // Increment numNodes and potentially change shouldHardStop.
    bool incrementNodes()
    {
      m_numNodes++;

      if (m_numNodes >= 1000000)
	m_shouldStop = true;

      return shouldHardStop();
    }

    template<bool IsPV>
    Score Negamax(const Position &pos,
		  int depth,
		  int ply, int repPly,
		  Score alpha, Score beta)
    {
      if (pos.rule50() >= 100)
	return Score(0);

      if (detectRepetition(repPly, ply))
	return Score(0);

      if (incrementNodes())
	return 0;

      if (ply >= MAX_PLY)
	return Evaluate(pos);

      if (depth < 0)
	depth = 0;

      /* Check extension: search deeper if we are in check.  */

      if (pos.inCheck())
	depth += ONEPLY;

      Score bestScore(SCORE_MIN);
      Move bestMove;

      bool raisedAlpha = false;

      /* Probe the transposition table.  */

      Move ttMove;
      std::optional<TTEntry> tt = m_tt->probe(pos);

      if (tt) {
	ttMove = tt->move;

	/* Transposition table cutoffs: if the TT depth is
	   greater than or equal than our depth, cutoff if
	   the score is a lowerbound and above beta or an
	   upperbound and below alpha.

	   If the bound if exact and we are in a PV node,
	   only cutoff if we are quiescing.  */
	if (tt->depth >= depth) {
	  switch(tt->boundType) {
	  case LOWERBOUND:
	    if (tt->score >= beta)
	      return tt->score;
	    break;
	  case UPPERBOUND:
	    if (tt->score <= alpha)
	      return tt->score;
	    break;
	  default:
	    if (!IsPV || depth <= 0)
	      return tt->score;
	  }
	}

	/* In quiescence search, don't search the hash
	   move if it is not a capture.  */
	if (depth <= 0 && !ttMove.isCapture())
	  goto skipTTMove;

	/* Search the hash move first.  */

	Position nextPos = pos;
	nextPos.makeMove(ttMove);
	m_movelist[m_moveOffset + ply] = ttMove;

	int nextRepPly = ttMove.resetsRule50() ? ply + 1 : repPly;

	Score score = Negamax<IsPV>(nextPos, depth - ONEPLY,
				    ply + 1, nextRepPly,
				    -beta, -alpha).negated();
	if (shouldHardStop())
	  return 0;

	bestScore = score;
	bestMove = ttMove;

	if (score > alpha) {
	  raisedAlpha = true;
	  alpha = score;
	  if (score >= beta) {
	    m_tt->insert(pos, ttMove, beta, LOWERBOUND, depth);
	    return score;
	  }
	}
      }

skipTTMove:
      Score standingPatScore(SCORE_MIN);

      /* If we are in quiescence search, ensure we have a
	 standing-pat score.  */
      if (depth <= 0) {
	standingPatScore = Evaluate(pos);

	if (standingPatScore > bestScore)
	  bestScore = standingPatScore;

	if (standingPatScore > alpha) {
	  raisedAlpha = true;
	  alpha = standingPatScore;
	}

	if (standingPatScore >= beta)
	  return standingPatScore;
      }

      /* Now search all other moves.  */

      MovePicker mp(pos, m_historyArray[ply & 1], depth <= 0);
      while (std::optional<Move> optMove = mp.nextMove()) {
	Move move = optMove.value();

	/* Skip the TT move, since we already searched
	   that one.  */
	if (move.sameAs(ttMove))
	  continue;

	Position nextPos = pos;
	nextPos.makeMove(move);
	m_movelist[m_moveOffset + ply] = move;

	int nextRepPly = move.resetsRule50() ? ply + 1 : repPly;

	Score score;

	if (!raisedAlpha || depth <= 0)
	  goto searchAsPV;

	/* Null window search: if we have raised alpha, we have
	   a best score in this position already. This means we
	   can do a scout search to prove that a move is worse
	   than our known best move.

	   We avoid NWS in quiescence search.  */

	score = Negamax<false>(nextPos, depth - ONEPLY,
			       ply + 1, nextRepPly,
			       (-alpha).boundedAdd(-1),
			       -alpha).negated();
	if (shouldHardStop())
	  return 0;

	if (IsPV && alpha < score) {
searchAsPV:
	  score = Negamax<IsPV>(nextPos, depth - ONEPLY,
				ply + 1, nextRepPly,
				-beta, -alpha).negated();
	  if (shouldHardStop())
	    return 0;
	}

	if (score > bestScore) {
	  bestScore = score;
	  bestMove = move;
	}

	if (score > alpha) {
	  raisedAlpha = true;
	  alpha = score;
	  if (score >= beta) {
	    /* Beta cutoff.  */
	    mp.betaCutoff(depth);
	    m_tt->insert(pos, move, beta, LOWERBOUND, depth);
	    return score;
	  }
	}
      }

      if (bestScore == SCORE_MIN) {
	if (pos.inCheck())
	  return CHECKMATE_SCORE;
	else
	  return Score(0);
      }

      /* We cannot insert into the transposition table if we
	 lack a bestMove (e.g. if quiescence search ends up
	 with the standing-pat score).  */
      if (bestScore != standingPatScore)
	m_tt->insert(pos, bestMove, bestScore,
		     raisedAlpha ? EXACT : UPPERBOUND,
		     depth);

      return bestScore;
    }

    std::tuple<Move, Score, BoundType> SearchRoot(int depth, Move pvMove,
						  Score alpha, Score beta)
    {
      const Position &pos = m_pEngine->getPosition();

      /* Search the pvMove first.

         This ensures we don't lose the best move when we
         exit in the middle of a search.  */

      {
	Position nextPos = pos;
	nextPos.makeMove(pvMove);
	m_movelist[m_moveOffset] = pvMove;

	int repPly = pvMove.resetsRule50() ? 1 : -m_moveOffset;

	Score score = Negamax<true>(nextPos, depth - ONEPLY,
				    1, repPly, -beta, -alpha).negated();
	if (shouldHardStop())
	  return std::make_tuple(pvMove, alpha, LOWERBOUND);

	/* If we fail low on the first move, we are aspiring and
	   we should immediately return the fail low so that the
	   window is widened.  */

	if (score <= alpha)
	  return std::make_tuple(pvMove, alpha, UPPERBOUND);

	alpha = score;
      }

      if (alpha >= beta)
	/* The principal variation caused a beta cutoff. This
	   also happens because of aspiration windows.  */
	return std::make_tuple(pvMove, alpha, LOWERBOUND);

      Move bestMove = pvMove;
      MovePicker mp(pos, m_historyArray[0]);

      while (std::optional<Move> optMove = mp.nextMove()) {
	Move move = optMove.value();

	/* Skip the principal variation move.  */
	if (move.sameAs(pvMove))
	  continue;

	Position nextPos = pos;
	nextPos.makeMove(move);
	m_movelist[m_moveOffset] = move;

	int repPly = move.resetsRule50() ? 1 : -m_moveOffset;

	/* Null window search. */
	Score score = Negamax<false>(nextPos, depth - ONEPLY,
				     1, repPly,
				     (-alpha).boundedAdd(-1),
				     -alpha).negated();

	/* If we run out of time in the midst of searching, we
	   can fall back to the best move we have searched thus
	   far as a lower-bound. This is safe because we search
	   the principal variation search.  */

	if (shouldHardStop())
	  return std::make_tuple(bestMove, alpha, LOWERBOUND);

	/* If the null window search fails high, we must retry
	   the search with an open window (or our (a; b) window).  */
	if (score > alpha) {
	  Score oldScore = score;
	  score = Negamax<true>(nextPos, depth - ONEPLY,
				1, repPly, -beta, -alpha).negated();

	  /* We failed high in the null window search. This
	     means that our move was deemed better than the
	     previous known best move.  */
	  if (shouldHardStop())
	    return std::make_tuple(move, oldScore, LOWERBOUND);

	  /* Because of search instability, the score might have
	     dropped below alpha when we re-search.  Test if the
	     score is still above alpha before updating bestMove.  */
	  if (score > alpha) {
	    bestMove = move;
	    alpha = score;
	    if (score >= beta) {
	      /* Beta cutoff.  */
	      mp.betaCutoff(depth);
	      return std::make_tuple(move, score, LOWERBOUND);
	    }
	  }
	}
      }

      return std::make_tuple(bestMove, alpha, EXACT);
    }

  public:
    void reset(Engine *engine, TranspositionTable *tt)
    {
      m_numNodes = 0;
      m_shouldStop = false;
      m_pEngine = engine;
      m_tt = tt;

      m_moveOffset = 0;
      for (Move move : engine->getRepetitionMoves())
	m_movelist[m_moveOffset++] = move;
      for (int i = 0; i < 2; i++) {
	for (int j = 0; j < 64; j++) {
	  for (int k = 0; k < 64; k++)
	    m_historyArray[i][j][k] = 0;
	}
      }
    }

    Move Search()
    {
      std::vector<Move> legalMoves;

      {
	MovePicker mp(m_pEngine->getPosition(),
		      m_historyArray[0]);
	while (std::optional<Move> optMove = mp.nextMove()) {
	  legalMoves.push_back(optMove.value());
	}
      }

      /* Start out with any random move as the PV.  */
      Move bestMove = legalMoves[0];

      Score score;
      BoundType boundType;

      /* Perform an initial fixed-depth search to get a
	 first approximation of the score.  */

      std::tie(bestMove, score, boundType) = SearchRoot(ONEPLY,
							bestMove,
							SCORE_MIN,
							SCORE_MAX);
      if (shouldHardStop())
	return bestMove;

      /* Iterative deepening.  */

      int searchDepth = 2 * ONEPLY;
      while (!shouldHardStop()) {
	if (searchDepth > 30 * ONEPLY)
	  break;

	/* If we have found a checkmate (for us or for the
	   opponent), exit the search immediately.  */

	if (score.isCheckmate())
	  break;

	/* Perform an aspirated search with a fixed window of
	   20 centipawns, on the assumption that the score
	   doesn't change much with each iteration.  */

	Score alpha = score.boundedAdd(-20);
	Score beta = score.boundedAdd(20);

	std::tie(bestMove, score, boundType) = SearchRoot(searchDepth,
							  bestMove,
							  alpha, beta);

	if (score <= alpha || beta <= score) {
	  if (shouldHardStop())
	    break;

	  /* The aspirated search failed and we should research the root
	     move with an open window.  */

	  std::tie(bestMove, score, boundType) = SearchRoot(searchDepth,
							    bestMove,
							    SCORE_MIN,
							    SCORE_MAX);
	}

	searchDepth += ONEPLY;

      }

      /* Search is done.  */

      return bestMove;
    }
  };

  Move Search(Engine *engine, TranspositionTable *tt)
  {
    static Searcher searcher;
    searcher.reset(engine, tt);
    return searcher.Search();;
  }
} // namespace DSchack
