// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine global state.

   Copyright (C) 2026 David Bergström

The engine global state (position, history arrays, transpositions) is
encapsulated into a single object of the type Engine.  */

#pragma once

#include <memory>
#include <optional>
#include <stdint.h>
#include <span>
#include <vector>

#include "Position.h"
#include "Score.h"

namespace DSchack {
  struct EngineInternal;

  /* EngineCallbacks: callbacks for the engine to send information like
     search depth, score, nps, bestmove.  */
  class EngineCallbacks {
  public:
    virtual ~EngineCallbacks() {}

    /** score: send information about the current evaluation.
    @score: Score.
    @boundType: EXACT, LOWERBOUND or UPPERBOUND
    @depth: current search depth.
    @nodes: current nodecount searched.
    @search_ms: number of milliseconds since 'go' command.
    @pv: principal variation.

    This function should send two lines like the following if the
    protocol is UCI:
      info depth 5
      info score cp -32 depth 5 nodes 12345 time 1023 pv f2f3 e7e5 g2g4 ... */
    virtual void score(Score score, BoundType boundType,
		       int depth, int seldepth, uint64_t nodes,
		       int search_ms, std::span<const Move> pv) = 0;

    /** nps: send information about current nodes/s being searched.
    @count: nodes per second.
    @hashfull: transposition table usage (1/1000th of hashtable size).

    This function should send a line like the following if the
    protocol is UCI:
      info nps 11234 hashfull 220  */
    virtual void nps(uint64_t count, int hashfull) = 0;

    /** bestmove: send information about what the engine deems to
	be the best move.
    @move: the best move in the position.
    @ponderMove: the ponder move, if the engine wishes to ponder on
                 an opponent move.

    This function should send a line like the following if the
    protocol is UCI:
      bestmove e4d5 ponder d8d5  */
    virtual void bestmove(Move move, std::optional<Move> ponderMove) = 0;

    /** info: send an arbitrary string to the user.
    @s: information string.

    This function should send a line like the following if the
    protocol is UCI:
      info string (contents of @s...)  */
    virtual void info(std::string_view s) = 0;
  };

  class Engine {
    // The callbacks that will be used to send information to the user.
    EngineCallbacks &m_callbacks;

    // The current engine position.
    Position m_position;

    // The list of past moves which are relevant for repetition detection.
    std::vector<Move> m_repetitionMoves;

    std::unique_ptr<EngineInternal> m_internal;

  public:
    ~Engine();

    /** Engine: construct a new Engine.
    @callbacks: the callbacks that will be used to send
                information to the user.

    Note: callbacks may be invoked on a different thread than
    the one that 'owns' the Engine object.  */
    Engine(EngineCallbacks &callbacks);

    EngineCallbacks &getCallbacks()
    {
      return m_callbacks;
    }

    /** getPosition: get the current engine position.  */
    const Position &getPosition() const
    {
      return m_position;
    }

    const std::vector<Move> &getRepetitionMoves() const
    {
      return m_repetitionMoves;
    }

    /** searchInProgress: returns true if the engine is
	currently searching a position.  */
    bool searchInProgress();

    /** ponderInProgress: returns true if the engine is
	currently searching a position in ponder mode.  */
    bool ponderInProgress();

    /** stop: stop searching as soon as possible.  */
    void stop();

    /** newGame: inform the engine that positions will
	be from a new game.  The engine might use this
	to reset history arrays or transposition tables.

    May not be called if there is a search in progress.  */
    void newGame();

    /** setHashSize: set the hash table size.
    @megabytes: hash table size in megabytes.  */
    void setHashSize(int megabytes);

    /** setPosition: update the engine position.
    @pos: current position.
    @pastMoves: the moves that led up to this position.

    May not be called if there is a search in progress.  */
    void setPosition(const Position &pos, const std::vector<Move> &pastMoves);

    /** go: begin searching the current position.
    @depth: target search depth, or zero.
    @wtime: White's time in milliseconds, or zero if left unspecified.
    @btime: Black's time in milliseconds, or zero if left unspecified.
    @winc: White's increment in milliseconds.
    @binc: Black's increment in milliseconds.
    @movetime: UCI movetime in milliseconds.
    @movestogo: Number of moves to go in tournament-style TC, or zero.
    @infinite: Whether to search until stopped.
    @ponder: Whether to search in ponder mode.

    May not be called if there is already a search in progress.  */
    void go(int depth, int wtime, int btime, int winc, int binc,
	    int movetime, int movestogo, bool infinite, bool ponder);

    /** ponderhit: transition the pondering search into a
	normal search.

    May only be called if there is an ongoing search in ponder mode.  */
    void ponderhit();
  };
} // namespace DSchack
