// SPDX-License-Identifier: GPL-3.0-or-later
/* Search routine.

   Copyright (C) 2026 David Bergström  */

#include <array>
#include <algorithm>
#include <memory>
#include <optional>
#include <sstream>
#include <tuple>

#include "Engine.h"
#include "Evaluate.h"
#include "MoveGen.h"
#include "Search.h"
#include "Transposition.h"

namespace DSchack {
  struct SearchStatistics {
    int numAllNodes = 0;
    int ttMisses = 0;
    int ttHits = 0;
    int ttDeep = 0;
    int ttCutoffs = 0;
    int ttSearchCutoffs = 0;
    int ttFailLow = 0;
    int numPVSResearch = 0;
    int betaCutoffDistribution[300] = { 0 };

    void show(EngineCallbacks &cb)
    {
      std::stringstream ss;

      ss << "Search statistics:\n";
      ss << "  numAllNodes=" << numAllNodes << "\n";
      ss << "  ttHits=" << ttHits << "\n";
      ss << "  ttMisses=" << ttMisses << "\n";
      ss << "  ttDeep=" << ttDeep << "\n";
      ss << "  ttCutoffs=" << ttCutoffs << "\n";
      ss << "  ttSearchCutoffs=" << ttSearchCutoffs << "\n";
      ss << "  ttFailLow=" << ttFailLow << "\n";
      ss << "  numPVSResearch=" << numPVSResearch << "\n";
      ss << "  betaCutoffDistribution=";
      int m = 0;
      for (int i = 0; i < 300; i++) {
	if (betaCutoffDistribution[i])
	  m = i + 1;
      }
      for (int i = 0; i < m; i++)
	ss << " " << betaCutoffDistribution[i];
      ss << "\n";

      cb.info(ss.view());
    }
  };

  static constexpr int MAX_HISTORY = 10000;

  static constexpr int ONEPLY = 16;

  struct TimeOverToken {};

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
    SearchGlobalState *m_pGlobal;
    TranspositionTable *m_tt;
    uint64_t m_numNodes = 0;
    uint64_t m_numNodesPerS = 0;
    uint64_t m_lastPeriodicInfo;
    int m_maxPlySearched;
    int m_numRootMovesExamined;
    int m_moveOffset;
    Move m_movelist[MAX_PLY + 100]; // +100 to leave space for past moves (capped by rule50).
    int m_historyArray[2][64][64];

    SearchStatistics m_stat;

    bool pondering()
    {
      return m_pGlobal->ponder.load(std::memory_order_acquire);
    }

    bool shouldHardStop()
    {
      return m_pGlobal->stopRequested.load(std::memory_order_relaxed);
    }

    void waitForNonPonder()
    {
      /* This is really awful.

	 Busy wait until we receive ponderhit or stop. This is
	 required because the UCI protocol doesn't allow us to
	 send bestmove while in infinite search or pondering
	 search.  */

      while (pondering()) {
#ifdef __AMD64__
	__builtin_ia32_pause();
#endif
      }

      if (m_pGlobal->infinite) {
	while (!shouldHardStop()) {
#ifdef __AMD64__
	  __builtin_ia32_pause();
#endif
	}
      }
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

    // Increment numNodes and potentially change shouldHardStop.
    bool incrementNodes()
    {
      m_numNodes++;
      m_numNodesPerS++;

      // Every 1000 or so nodes, check the current time.

      if (m_numNodesPerS & 1023)
	return false;

      if (shouldHardStop())
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
	return true;
      }

      return false;
    }

    void sendScoreAndPV(Score score, BoundType boundType,
			int depth, std::optional<Move> bestMove)
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
	  if (pos.rule50() >= 100)
	    break;
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
      m_pEngine->getCallbacks().score(score, boundType,
				      depth / ONEPLY,
				      m_maxPlySearched,
				      m_numNodes,
				      CurrentTime() - m_pGlobal->goTime,
				      pv);
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
	throw TimeOverToken();

      if (ply > m_maxPlySearched)
	m_maxPlySearched = ply;

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
      int numMovesSearched = 0;

      /* Probe the transposition table.  */

      Move ttMove;
      std::optional<TTEntry> tt = m_tt->probe(pos);

      if (tt) {
	m_stat.ttHits++;

	ttMove = tt->move;

	/* Transposition table cutoffs: if the TT depth is
	   greater than or equal than our depth, cutoff if
	   the score is a lowerbound and above beta or an
	   upperbound and below alpha.

	   If the bound if exact and we are in a PV node,
	   only cutoff if we are quiescing.  */
	if (tt->depth >= depth) {
	  m_stat.ttDeep++;
	  switch(tt->boundType) {
	  case LOWERBOUND:
	    if (tt->score >= beta) {
	      m_stat.ttCutoffs++;
	      return tt->score;
	    }
	    break;
	  case UPPERBOUND:
	    if (tt->score <= alpha) {
	      m_stat.numAllNodes++;
	      m_stat.ttCutoffs++;
	      return tt->score;
	    }
	    break;
	  default:
	    if (!IsPV || depth <= 0) {
	      m_stat.ttCutoffs++;
	      return tt->score;
	    }
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

	numMovesSearched++;

	bestScore = score;
	bestMove = ttMove;

	if (score > alpha) {
	  raisedAlpha = true;
	  alpha = score;
	  if (score >= beta) {
	    m_stat.betaCutoffDistribution[0]++;
	    m_stat.ttSearchCutoffs++;
	    m_tt->insert(pos, ttMove, beta, LOWERBOUND, depth);
	    return score;
	  }
	} else
	  m_stat.ttFailLow++;
      } else
	m_stat.ttMisses++;

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

	if (IsPV && alpha < score) {
	  m_stat.numPVSResearch++;
searchAsPV:
	  score = Negamax<IsPV>(nextPos, depth - ONEPLY,
				ply + 1, nextRepPly,
				-beta, -alpha).negated();
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
	    m_stat.betaCutoffDistribution[numMovesSearched]++;
	    mp.betaCutoff(depth);
	    m_tt->insert(pos, move, beta, LOWERBOUND, depth);
	    return score;
	  }
	}

	numMovesSearched++;
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

      if (!raisedAlpha)
	m_stat.numAllNodes++;

      return bestScore;
    }

    std::tuple<Move, Score, BoundType> SearchRoot(int depth, Move pvMove,
						  Score alpha, Score beta)
    {
      const Position &pos = m_pEngine->getPosition();
      m_maxPlySearched = 0;

      /* Search the pvMove first.

         This ensures we don't lose the best move when we
         exit in the middle of a search.  */

      try {
	Position nextPos = pos;
	nextPos.makeMove(pvMove);
	m_movelist[m_moveOffset] = pvMove;

	int repPly = pvMove.resetsRule50() ? 1 : -m_moveOffset;

	Score score = Negamax<true>(nextPos, depth - ONEPLY,
				    1, repPly, -beta, -alpha).negated();

	/* If we fail low on the first move, we are aspiring and
	   we should immediately return the fail low so that the
	   window is widened.  */

	if (score <= alpha)
	  return std::make_tuple(pvMove, alpha, UPPERBOUND);

	alpha = score;
      } catch (TimeOverToken token) {
	/* If we did not even have a chance to search the
	   principal variation, just return alpha. The search
	   is over anyways and this score won't land in the TT.  */

	return std::make_tuple(pvMove, alpha, LOWERBOUND);
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

	Score score;
	try {
	  /* Null window search. */
	  score = Negamax<false>(nextPos, depth - ONEPLY,
				 1, repPly,
				 (-alpha).boundedAdd(-1),
				 -alpha).negated();
	} catch (TimeOverToken token) {

	  /* If we run out of time in the midst of searching, we
	     can fall back to the best move we have searched thus
	     far as a lower-bound. This is safe because we search
	     the principal variation search.  */

	  return std::make_tuple(bestMove, alpha, LOWERBOUND);
	}

	/* If the null window search fails high, we must retry
	   the search with an open window (or our (a; b) window).  */
	if (score > alpha) {
	  try {
	    score = Negamax<true>(nextPos, depth - ONEPLY,
				  1, repPly, -beta, -alpha).negated();
	  } catch (TimeOverToken token) {
	    /* We failed high in the null window search. This
	       means that our move was deemed better than the
	       previous known best move.  */

	    return std::make_tuple(move, score, LOWERBOUND);
	  }

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

	/* Now is a really good time to test for soft-stopping
	   conditions.  */
	if (shouldSoftStop()) {
	  if (mp.nextMove())
	    return std::make_tuple(bestMove, alpha, LOWERBOUND);
	  else
	    return std::make_tuple(bestMove, alpha, EXACT);
	}
      }

      return std::make_tuple(bestMove, alpha, EXACT);
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

    void Search()
    {
      std::vector<Move> legalMoves;
      std::vector<std::optional<Move>> ponderMoves;

      {
	MovePicker mp(m_pEngine->getPosition(),
		      m_historyArray[0]);
	while (std::optional<Move> optMove = mp.nextMove()) {
	  legalMoves.push_back(optMove.value());
	  ponderMoves.push_back(std::nullopt);
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

      sendScoreAndPV(score, boundType, ONEPLY, bestMove);

      /* Iterative deepening.  */

      int searchDepth = 2 * ONEPLY;
      while (!shouldHardStop() && !shouldSoftStop()) {
	if (m_pGlobal->depthLimit &&
	    searchDepth / ONEPLY > m_pGlobal->depthLimit)
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

	if (boundType != LOWERBOUND || !score.isCheckmate())
	  sendScoreAndPV(score, boundType, searchDepth, bestMove);

	if (score <= alpha || beta <= score) {
	  if (shouldHardStop() || shouldSoftStop())
	    break;

	  /* The aspirated search failed and we should research the root
	     move with an open window.  */

	  std::tie(bestMove, score, boundType) = SearchRoot(searchDepth,
							    bestMove,
							    SCORE_MIN,
							    SCORE_MAX);

	  if (boundType != LOWERBOUND || !score.isCheckmate())
	    sendScoreAndPV(score, boundType, searchDepth, bestMove);
	}

	searchDepth += ONEPLY;

	/* Update ponder moves.  */
	for (size_t i = 0; i < legalMoves.size(); i++) {
	  std::optional<Move> ponderMove = getPonderMove(legalMoves[i]);
	  if (ponderMove)
	    ponderMoves[i] = ponderMove;
	}
      }

      /* Search is done. Try really hard to get a ponder move.  */

      std::optional<Move> ponderMove = std::nullopt;
      for (size_t i = 0; i < legalMoves.size(); i++) {
	if (legalMoves[i].sameAs(bestMove)) {
	  ponderMove = ponderMoves[i];
	  break;
	}
      }

      m_pEngine->getCallbacks().info("search exited");
      m_stat.show(m_pEngine->getCallbacks());
      waitForNonPonder();

      m_pGlobal->inProgress.store(false);
      m_pEngine->getCallbacks().bestmove(bestMove, ponderMove);
    }
  };

  void Search(Engine *engine, SearchGlobalState *state,
	      TranspositionTable *tt)
  {
    std::unique_ptr<Searcher> searcher = std::make_unique<Searcher>(engine, state, tt);
    searcher->Search();
  }
} // namespace DSchack
