// SPDX-License-Identifier: GPL-3.0-or-later
/* Search routine.

   Copyright (C) 2026 David Bergström

   2026-04-27: Rewritten to use an explicit search stack; the old search
               routine was becoming difficult to maintain.  */

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
  static constexpr int MAX_PLY = 100; // This is the deepest the engine can search

  struct TimeOverToken {}; // A dummy struct which is thrown when the search timeouts

  struct Node {
    int ply;
    int staticEval;
    bool inCheck;
    Move currentMove;
    std::optional<Move> ttMove;
    Node *repNode;
    Position pos;
  };

  static constexpr int MAX_HISTORY = 10000;

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

  struct MoveHistory {
    int quietHistory[64][64] {};

    int quiet(Move move)
    {
      return quietHistory[move.fromSquare()][move.toSquare()];
    }

    void pushQuiet(Move move, int bonus)
    {
      updateHistory(quietHistory[move.fromSquare()][move.toSquare()], bonus);
    }
  };

  class MovePicker {
    Move *m_prevEnd = mp_moves_end;
    Move *m_current = mp_moves_end;
    Move *m_end = mp_moves_end;
    Move *m_firstQuiet = nullptr;
    int m_phase = 0;

    const Position &m_pos;

    bool m_skipQuiets;

    MoveHistory &m_history;

  public:
    MovePicker(const Position &pos, bool skipQuiets, MoveHistory &history)
      : m_pos(pos), m_skipQuiets(skipQuiets), m_history(history)
    {}

    ~MovePicker()
    {
      mp_moves_end = m_prevEnd;
    }

    std::optional<Move> nextMove()
    {
      while (m_current == m_end) {
	switch(m_phase++) {
	case 0:
	  mp_moves_end = m_end = GenerateCaptures(m_pos, m_end);
	  std::sort(m_current, m_end, [](Move lhs, Move rhs){
	    if (lhs.capture() > rhs.capture())
	      return true;
	    if (lhs.capture() < rhs.capture())
	      return false;
	    return lhs.piece() < rhs.piece();
	  });
	  break;
	case 1:
	  if (!m_skipQuiets) {
	    m_firstQuiet = m_end;
	    mp_moves_end = m_end = GenerateQuiets(m_pos, m_end);
	    std::sort(m_current, m_end, [this](Move lhs, Move rhs){
	      return m_history.quiet(lhs) > m_history.quiet(rhs);
	    });
	  }
	  break;
	default:
	  return std::nullopt;
	}
      }

      return *(m_current++);
    }

    void betaCutoff(int bonus, int malus)
    {
      Move *p = m_firstQuiet;
      if (p) {
	while (p != m_current - 1) {
	  m_history.pushQuiet(*p, -malus);
	  p++;
	}

	m_history.pushQuiet(*p, bonus);
      }
    }
  };

  class Searcher {
    Engine *m_pEngine;
    SearchGlobalState *m_pGlobal;
    TranspositionTable *m_tt;
    uint64_t m_numNodes = 0;
    uint64_t m_numNodesPerS = 0;
    uint64_t m_lastPeriodicInfo;
    int m_maxPlySearched;

    MoveHistory m_moveHistory[2];

    Node m_nodes[101 + MAX_PLY]; // +100 plies for 50-moves rule.
    Node *m_rootRepNode;

    Move m_bestRootMove;
    bool m_rootIsLowerBound = false;

public:
    Searcher(Engine *engine, SearchGlobalState *global,
	     TranspositionTable *tt)
    {
      m_pEngine = engine;
      m_pGlobal = global;
      m_tt = tt;
      m_lastPeriodicInfo = global->goTime;

      const std::vector<Move> repMoves = engine->getRepetitionMoves();
      m_rootRepNode = m_nodes + 100 - repMoves.size();
      for (int i = 0; i < (int) repMoves.size(); i++)
	m_rootRepNode[i].currentMove = repMoves[i];

      for (int i = 0; i < MAX_PLY + 1; i++)
	m_nodes[100 + i].ply = i;

      m_nodes[100].repNode = m_rootRepNode;
      m_nodes[100].pos = engine->getPosition();

      int staticEval = Evaluate(m_nodes[100].pos);
      for (int i = 0; i < 100; i += 2) {
	m_nodes[i].staticEval = staticEval;
	m_nodes[i + 1].staticEval = -staticEval;
      }
    }

private:
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
	if (limit && CurrentTime() >= limit)
	  return true;
      }

      return false;
    }

    bool detectRepetition(Node *now, Node *begin)
    {
      if ((now - begin) & 1)
	begin++;

      Bitboard colorBB = 0;
      Bitboard pieceBB[6] {};
      int count = 0;

      while (now != begin) {
	Move move1 = (now - 1)->currentMove;
	Move move2 = (now - 2)->currentMove;
	now -= 2;
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

    bool incrementNodes()
    {
      m_numNodes++;
      m_numNodesPerS++;

      if (m_pGlobal->nodes && m_numNodes >= m_pGlobal->nodes) {
	m_pGlobal->stopRequested.store(true, std::memory_order_relaxed);
	return true;
      }

      // Every 1000 or so nodes, check the current time.

      if (m_numNodesPerS & 1023)
	return false;

      if (shouldHardStop())
	return true;

      uint64_t t = CurrentTime();

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

    void sendScoreAndPV(int score, BoundType boundType,
			int depth, std::optional<Move> bestMove)
    {
      Move moves[MAX_PLY];
      int numPvMoves = 0;
      if (bestMove) {
	moves[0] = bestMove.value();
	numPvMoves = 1;
	Node *n = makeMove(m_nodes + 100, bestMove.value());
	while (numPvMoves < MAX_PLY) {
	  const Position &pos = n->pos;
	  if (pos.rule50() >= 100)
	    break;
	  if (detectRepetition(n, n->repNode))
	    break;
	  std::optional<TTEntry> ttEntry = m_tt->probe(pos, numPvMoves);
	  if (!ttEntry)
	    break;
	  moves[numPvMoves++] = ttEntry->move;
	  n = makeMove(n, ttEntry->move);
	}

	for (int i = 0; i < numPvMoves; i++)
	  moves[i] = m_nodes[100 + i].currentMove;
      }

      std::span<Move> pv = std::span<Move>(moves, moves + numPvMoves);
      m_pEngine->getCallbacks().score(score, boundType,
				      depth, m_maxPlySearched,
				      m_numNodes,
				      CurrentTime() - m_pGlobal->goTime,
				      pv);
    }

    std::optional<Move> getPonderMove(Move prevBestMove)
    {
      Position pos = m_pEngine->getPosition();
      pos.makeMove(prevBestMove);

      std::optional<TTEntry> ttEntry = m_tt->probe(pos, 1);
      if (ttEntry)
	return ttEntry->move;
      else
	return std::nullopt;
    }

    Node *makeMove(Node *current, Move move)
    {
      Node *next = current + 1;

      current->currentMove = move;
      next->pos = current->pos;
      next->pos.makeMove(move);
      next->ttMove = std::nullopt;

      if (move.resetsRule50())
	next->repNode = next;
      else
	next->repNode = current->repNode;

      return next;
    }

    Node *makeNullMove(Node *current)
    {
      Node *next = current + 1;

      current->currentMove = Move::makeNull();
      next->pos = current->pos;
      next->pos.makeNullMove();
      next->ttMove = std::nullopt;
      next->repNode = current->repNode;

      return next;
    }

    /* Quiescence search routine. */
    template<bool IsPV>
    int qsearch(Node *n, int alpha, int beta)
    {
      const Position &pos = n->pos;
      int ply = n->ply;

      if (pos.rule50() >= 100)
	return 0;
      if (detectRepetition(n, n->repNode))
	return 0;

      /* Mate Distance Pruning: we cannot possibly achieve a
	 better checkmate score than alpha, therefore prune
         this branch immediately. (and the same for beta)  */
      if (MateIn(ply + 1) <= alpha)
	return alpha;

      if (MatedIn(ply) >= beta)
	return beta;

      if (incrementNodes())
	throw TimeOverToken();

      if (ply > m_maxPlySearched)
	m_maxPlySearched = ply;

      int staticEval = Evaluate(pos);

      if (ply >= MAX_PLY)
	return staticEval;

      bool inCheck = pos.inCheck();
      n->staticEval = staticEval;
      n->inCheck = inCheck;

      if (staticEval >= beta && !inCheck)
	return staticEval;

      int bestScore = inCheck ? MatedIn(ply) : staticEval;
      Move bestMove;
      bool hasBestMove = false;
      bool raisedAlpha = false;

      if (!inCheck && staticEval > alpha)
	alpha = staticEval;

      Move ttMove;
      std::optional<TTEntry> tt = m_tt->probe(pos, ply);
      if (tt) {
	n->ttMove = ttMove = tt->move;

	/* In Quiescence search, the score from the transposition
	   table will always be searched at a depth greater than or
	   equal to quiescence.  */

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
	  return tt->score;
	}

	/* Only search the TT move if it is a capture or we are in check.  */
	if (ttMove.isCapture() || inCheck) {
	  /* Search the TT move first.  */
	  Node *next = makeMove(n, ttMove);
	  int score = -qsearch<IsPV>(next, -beta, -alpha);
	  if (score > bestScore) {
	    bestScore = score;
	    bestMove = ttMove;
	    hasBestMove = true;

	    if (score >= beta) {
	      m_tt->insert(pos, ttMove, beta, LOWERBOUND, 0, ply);
	      return score;
	    }

	    if (score > alpha) {
	      raisedAlpha = true;
	      alpha = score;
	    }
	  }
	}
      }

      MovePicker mp(pos, !inCheck, m_moveHistory[ply & 1]);
      while (std::optional<Move> optMove = mp.nextMove()) {
	Move move = optMove.value();

	if (move.sameAs(ttMove))
	  continue;

	Node *next = makeMove(n, move);
	int score;

	if (IsPV && inCheck && hasBestMove && raisedAlpha) {
	  score = -qsearch<false>(next, -alpha - 1, -alpha);
	  if (score > alpha)
	    score = -qsearch<true>(next, -beta, -alpha);
	} else
	  score = -qsearch<IsPV>(next, -beta, -alpha);

	if (score > bestScore) {
	  bestScore = score;
	  bestMove = move;
	  hasBestMove = true;

	  if (score >= beta) {
	    m_tt->insert(pos, move, beta, LOWERBOUND, 0, ply);
	    return score;
	  }

	  if (score > alpha) {
	    raisedAlpha = true;
	    alpha = score;
	  }
	}
      }

      if (hasBestMove)
	m_tt->insert(pos, bestMove, bestScore, raisedAlpha ? EXACT : UPPERBOUND, 0, ply);

      return bestScore;
    }

    /* Main search routine. */
    template<bool IsPV>
    int search(Node *n, int alpha, int beta, int depth)
    {
      /* If the depth becomes zero or negative, enter quiescence search here.  */
      if (depth <= 0)
	return qsearch<IsPV>(n, alpha, beta);

      const Position &pos = n->pos;
      int ply = n->ply;

      bool isRootNode = ply == 0;

      if (pos.rule50() >= 100)
	return 0;
      if (detectRepetition(n, n->repNode))
	return 0;

      /* Mate Distance Pruning: we cannot possibly achieve a
	 better checkmate score than alpha, therefore prune
         this branch immediately. (and the same for beta)  */
      if (MateIn(ply + 1) <= alpha)
	return alpha;

      if (MatedIn(ply) >= beta)
	return beta;

      if (incrementNodes())
	throw TimeOverToken();

      if (ply > m_maxPlySearched)
	m_maxPlySearched = ply;

      int staticEval = Evaluate(pos);

      if (ply >= MAX_PLY)
	return staticEval;

      bool inCheck = pos.inCheck();
      n->staticEval = staticEval;
      n->inCheck = inCheck;

      /* Check extension: search deeper if we are in check.  */
      if (inCheck)
	depth++;

      if (depth > MAX_PLY)
	depth = MAX_PLY;

      /* Reverse Futility Pruning: in non-PV nodes, if static evaluation
	 beats beta by a wide margin, prune this branch.  */
      if (!IsPV && !inCheck) {
	int margin = 150 * depth;

	if (staticEval >= beta + margin)
	  return staticEval;
      }

      Color stm = pos.sideToMove();

      int bestScore = MatedIn(ply);
      Move bestMove;
      bool raisedAlpha = false;

      /* Now probe the transposition table.  */
      Move ttMove;
      std::optional<TTEntry> tt = m_tt->probe(pos, ply);

      bool allowNMP = false;

      if (isRootNode) {
	/* If this is the root node, use the best move from the
	   previous iteration instead of from the TT. This move
	   is guaranteed to exist (at the beginning, it is any
	   legal move).  */
	n->ttMove = ttMove = m_bestRootMove;
	goto searchAsHashMove;
      }

      if (tt) {
	n->ttMove = ttMove = tt->move;

	/* Allow TT cutoffs if the TT depth is greater than or
	   equal to our depth. When the TT score is decisive
	   (winning or losing checkmate), depth doesn't matter
	   since this position was searched deep enough to find
	   a forced checkmate.  */
	if (tt->depth >= depth || IsDecisive(tt->score)) {
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
	    if (!IsPV)
	      return tt->score;
	  }
	}
      }

      /* Null Move Pruning: if we are not in check and we are not in the
	 PV, try a null move search unless we have only our king and pawns.

         We disallow two consecutive null moves.  */
      allowNMP = !IsPV && !inCheck
	&& !(n - 1)->currentMove.isNull()
	&& (pos.pieces(stm, ALL)
	    & ~pos.pieces(stm, KING) & ~pos.pieces(stm, PAWN));

      if (depth >= 4 && allowNMP) {
	Node *next = makeNullMove(n);

	int score = -search<false>(next, -beta, 1 - beta, depth - 4);
	if (score >= beta) {
	  if (depth < 8)
	    return score;

	  /* To mitigate zugzwang blindness, we perform a
	     verification search at high depths.  */
	  score = search<false>(n, beta - 1, beta, depth - 8);
	  if (score >= beta)
	    return score;
	}
      }

      if (tt) {
searchAsHashMove: /* At the root, the best move from the previous iteration is
		     searched as if it were the hash move.  */

	/* Search the hash move first.  */
	int score = -search<IsPV>(makeMove(n, ttMove), -beta, -alpha, depth - 1);

	if (score >= beta) {
	  m_tt->insert(pos, ttMove, beta, LOWERBOUND, depth, ply);
	  m_moveHistory[ply & 1].pushQuiet(ttMove, 300 * depth - 250);
	  return score;
	}

	bestScore = score;
	bestMove = ttMove;

	if (score > alpha) {
	  alpha = score;
	  raisedAlpha = true;
	} else if (isRootNode) {
	  /* In the root node, return early if the first move fails low.
	     This happens only because of aspiration windows.  */
	  return score;
	}
      }

      /* Search all moves now, except the ttMove which we already searched.  */
      MovePicker mp(pos, false, m_moveHistory[ply & 1]);
      while (std::optional<Move> optMove = mp.nextMove()) {
	Move move = optMove.value();
	if (move.sameAs(ttMove))
	  continue;

	Node *next = makeMove(n, move);
	int score;

	try {
	  /* Null window search: If we have raised alpha, we already have
	     an acceptable move and a best score in this position. This
	     lets us do a scout search to prove that an other move is
	     worse.  */
	  if (IsPV && raisedAlpha) {
	    score = -search<false>(next, -alpha - 1, -alpha, depth - 1);
	    /* If the scout search fails high, re-search with our window.  */
	    if (score > alpha)
	      score = -search<true>(next, -beta, -alpha, depth - 1);
	  } else {
	    score = -search<IsPV>(next, -beta, -alpha, depth - 1);
	  }
	} catch (TimeOverToken token) {
	  if (!isRootNode)
	    throw;

	  /* In the root node, handle search timeouts specially. We can
	     yield a value from an incomplete search but we have to mark
	     it as a lowerbound.  */
	  m_rootIsLowerBound = true;
	  return bestScore;
	}

	if (score >= beta) {
	  m_tt->insert(pos, move, beta, LOWERBOUND, depth, ply);
	  mp.betaCutoff(300 * depth - 250, 300 * depth - 250);
	  if (isRootNode)
	    m_bestRootMove = move;
	  return score;
	}

	if (score > bestScore) {
	  bestScore = score;
	  bestMove = move;
	  if (score > alpha) {
	    alpha = score;
	    raisedAlpha = true;
	  }
	  if (isRootNode)
	    m_bestRootMove = move;
	}

	/* In the root node, now is a very good time to test for
	   soft-stopping conditions.  */
	if (isRootNode) {
	  if (shouldSoftStop()) {
	    if (mp.nextMove())
	      m_rootIsLowerBound = true;
	    return bestScore;
	  }
	}
      }

      if (bestScore == MatedIn(ply))
	return inCheck ? bestScore : 0;

      m_tt->insert(pos, bestMove, bestScore,
		   raisedAlpha ? EXACT : UPPERBOUND,
		   depth, ply);

      return bestScore;
    }

public:
    void doSearch()
    {
      std::vector<Move> legalMoves;
      std::vector<std::optional<Move>> ponderMoves;

      {
	MovePicker mp(m_pEngine->getPosition(), false, m_moveHistory[0]);
	while (std::optional<Move> optMove = mp.nextMove()) {
	  legalMoves.push_back(optMove.value());
	  ponderMoves.push_back(std::nullopt);
	}
      }

      if (std::optional<TTEntry> tt = m_tt->probe(m_pEngine->getPosition(), 0))
	m_bestRootMove = tt->move;
      else
	m_bestRootMove = legalMoves[0];

      Node *root = m_nodes + 100;

      /* Perform an initial shallow search of the root node to obtain
	 an initial guess for the score of a position.  */
      int score;
      try {
	score = search<true>(root, SCORE_MIN, SCORE_MAX, 1);
      } catch (TimeOverToken token) {
	// If this timeouts, there is nothing we can do.
	m_pEngine->getCallbacks().bestmove(m_bestRootMove, std::nullopt);
	return;
      }

      sendScoreAndPV(score, m_rootIsLowerBound ? LOWERBOUND : EXACT, 1, m_bestRootMove);
      if (m_rootIsLowerBound) {
	m_pGlobal->inProgress.store(false);
	m_pEngine->getCallbacks().bestmove(m_bestRootMove, std::nullopt);
	return;
      }

      /* Iterative deepening loop.  */
      int depth = 2;
      while (!shouldHardStop() && !shouldSoftStop()) {
	if (m_pGlobal->depthLimit && depth > m_pGlobal->depthLimit)
	  break;
	if (depth > MAX_PLY)
	  break;

	/* If we have found a checkmate, quit searching instantly.  */
	if (IsDecisive(score))
	  break;

	/* Perform an aspirated search on a window centered around the
	   score from the previous iteration of search.  */
	int alpha = score - 20;
	int beta = score + 20;

	if (SCORE_MIN > alpha)
	  alpha = SCORE_MIN;
	if (SCORE_MAX < beta)
	  beta = SCORE_MAX;

	try {
	  score = search<true>(root, alpha, beta, depth);
	} catch (TimeOverToken token) {
	  break;
	}

	if (score <= alpha || beta <= score) {
	  if (shouldSoftStop())
	    break;
	  /* The aspirated search failed and we should research with an
	     open window.  */
	  sendScoreAndPV(score, (score <= alpha) ? UPPERBOUND :
			        (score >= beta) ? LOWERBOUND : EXACT,
			 depth, m_bestRootMove);

	  if (shouldHardStop() || shouldSoftStop())
	    break;

	  try {
	    score = search<true>(root, SCORE_MIN, SCORE_MAX, depth);
	  } catch (TimeOverToken token) {
	    goto researchTimedOut;
	  }
	}

	sendScoreAndPV(score, m_rootIsLowerBound ? LOWERBOUND : EXACT,
		       depth, m_bestRootMove);

researchTimedOut:
	for (int i = 0; i < (int) legalMoves.size(); i++) {
	  std::optional<Move> ponderMove = getPonderMove(legalMoves[i]);
	  if (ponderMove)
	    ponderMoves[i] = ponderMove;
	}

	if (m_rootIsLowerBound)
	  break;

	depth++;
      }

      /* Search is done. Get the ponder move.  */
      std::optional<Move> ponderMove = std::nullopt;
      for (int i = 0; i < (int) legalMoves.size(); i++) {
	if (m_bestRootMove.sameAs(legalMoves[i])) {
	  ponderMove = ponderMoves[i];
	  break;
	}
      }

      waitForNonPonder();
      m_pGlobal->inProgress.store(false);
      m_pEngine->getCallbacks().bestmove(m_bestRootMove, ponderMove);
    }
  };

  void Search(Engine *engine, SearchGlobalState *state,
	      TranspositionTable *tt)
  {
    std::unique_ptr<Searcher> searcher = std::make_unique<Searcher>(engine, state, tt);
    searcher->doSearch();
  }
} // namespace DSchack
