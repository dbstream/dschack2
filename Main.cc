// SPDX-License-Identifier: GPL-3.0-or-later
/* DSchack.

   Copyright (C) 2026 David Bergström

This is the engine entry point.  It implements the receiver end of
the UCI protocol, which is used for user<->engine communication.  */

#include <iostream>
#include <span>
#include <stdio.h>
#include <string.h>
#include <string>
#include <vector>
#include <mutex>

#include "Bitboard.h"
#include "Engine.h"
#include "MoveGen.h"
#include "Position.h"

namespace DSchack {
  static const std::string_view STARTPOS_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

  static std::mutex gCoutMutex;

  static constexpr int parseInt(std::string_view s)
  {
    int value = 0;
    for (char c : s) {
      if (c < '0' || c > '9')
	return -1;
      if (value > 100000000)
	return -1;
      value *= 10;
      value += c - '0';
    }
    return value;
  }

  static constexpr std::vector<std::string_view> splitOnWhitespace(std::string_view s)
  {
      std::vector<std::string_view> parts;

      while (s.size()) {
	size_t index = s.find_first_of(" \t\n\f\v\r");
	if (index == std::string_view::npos)
	  index = s.size();

	if (index)
	  parts.push_back(s.substr(0, index));
	if (index == s.size())
	  break;

	s = s.substr(index + 1);
      }

      return parts;
  }

  static const std::vector<std::string_view> startpos_fen = splitOnWhitespace(STARTPOS_FEN);

  static int numberOfMoves(const Position &pos)
  {
    Move buffer[300];
    Move *pEnd = GenerateLegalMoves(pos, buffer);

    return pEnd - &buffer[0];
  }

  template<bool Print>
  static uint64_t perft(const Position &pos, int depth)
  {
    if (depth < 0)
      return 0;
    if (depth == 0)
      return 1;

    Move buffer[300];
    Move *pEnd = GenerateLegalMoves(pos, buffer);

    if (depth == 1 && !Print)
      return pEnd - &buffer[0];

    uint64_t nodeCount = 0;
    for (Move *it = buffer; it != pEnd; it++) {
      Position newpos = pos;
      newpos.makeMove(*it);
      uint64_t count = perft<false>(newpos, depth - 1);
      if (Print)
	std::cout << it->toUCI() << ": " << count << "\n";
      nodeCount += count;
    }
    return nodeCount;
  }

  static bool gShouldExit = false;

  class UCIEngineCallbacks : public EngineCallbacks {
  public:
    ~UCIEngineCallbacks() {}

    void score(Score score, BoundType boundType,
	       int depth, uint64_t nodes,
	       int search_ms, std::span<const Move> pv) override
    {
      gCoutMutex.lock();
      std::cout << "info depth " << depth << " score ";
      if (score.isCheckmate())
	std::cout << "mate " << score.depthToMate();
      else
	std::cout << "cp " << score.centipawns();
      if (boundType != EXACT)
	std::cout << ((boundType == LOWERBOUND) ? " lowerbound" : " upperbound");
      std::cout << " nodes " << nodes << " time " << search_ms;
      if (!pv.empty()) {
	std::cout << " pv";
	for (Move move : pv)
	  std::cout << " " << move.toUCI();
      }
      std::cout << "\n";
      gCoutMutex.unlock();
    }

    void nps(uint64_t count) override
    {
      gCoutMutex.lock();
      std::cout << "info nps " << count << "\n";
      gCoutMutex.unlock();
    }

    void bestmove(Move move, std::optional<Move> ponderMove) override
    {
      gCoutMutex.lock();
      std::cout << "bestmove " << move.toUCI();
      if (ponderMove) {
	std::cout << " ponder " << ponderMove.value().toUCI();
      }
      std::cout << "\n";
      gCoutMutex.unlock();
    }
  };

  static void executeUCI(Engine &engine, std::span<std::string_view> parts)
  {
    std::string_view command = parts[0];
    parts = parts.subspan(1);

    if (command == "uci") {
      gCoutMutex.lock();
      std::cout << "id name DSchack\n";
      std::cout << "id author David Bergström\n";
      std::cout << "option name Ponder type check\n";
      std::cout << "uciok\n";
      gCoutMutex.unlock();
      return;
    }

    if (command == "isready") {
      gCoutMutex.lock();
      std::cout << "readyok\n";
      gCoutMutex.unlock();
      return;
    }

    if (command == "quit") {
      gShouldExit = true;
      return;
    }

    if (command == "ucinewgame") {
      if (engine.searchInProgress()) {
	gCoutMutex.lock();
	std::cout << "protocol error: a search is in progress\n";
	gCoutMutex.unlock();
	return;
      }
      engine.newGame();
      return;
    }

    if (command == "stop") {
      // Note: no error to send 'stop' even if not searching.
      engine.stop();
      return;
    }

    if (command == "ponderhit") {
      if (!engine.ponderInProgress()) {
	gCoutMutex.lock();
	std::cout << "protocol error: the engine is not pondering\n";
	gCoutMutex.unlock();
	return;
      }
      engine.ponderhit();
      return;
    }

    if (command == "position") {
      if (parts.empty()) {
	gCoutMutex.lock();
	std::cout << "protocol error: received 'position' without a FEN or startpos\n";
	gCoutMutex.unlock();
	return;
      }

      Position pos;
      if (parts[0] == "fen") {
	parts = parts.subspan(1);
	if (parts.size() < 6) {
	  gCoutMutex.lock();
	  std::cout << "protocol error: incomplete FEN string\n";
	  gCoutMutex.unlock();
	  return;
	}
	std::optional<Position> optPos = ParseFEN(parts.subspan(0, 6));
	if (!optPos) {
	  gCoutMutex.lock();
	  std::cout << "protocol error: invalid FEN string\n";
	  gCoutMutex.unlock();
	  return;
	}
	pos = optPos.value();
	parts = parts.subspan(6);
      } else if (parts[0] == "startpos") {
	parts = parts.subspan(1);
	std::optional<Position> optPos = ParseFEN(startpos_fen);
	if (!optPos) {
	  gCoutMutex.lock();
	  std::cout << "internal error: failed to parse startpos_fen\n";
	  gCoutMutex.unlock();
	  return;
	}
	pos = optPos.value();
      } else {
	gCoutMutex.lock();
	std::cout << "protocol error: '" << parts[0] << "' is not implemented\n";
	gCoutMutex.unlock();
	return;
      }

      std::vector<Move> moves;
      if (!parts.empty()) {
	if (parts[0] != "moves") {
	  gCoutMutex.lock();
	  std::cout << "protocol error: expected 'moves' but found '" << parts[0] << "'\n";
	  gCoutMutex.unlock();
	  return;
	}

	parts = parts.subspan(1);
	for (std::string_view s : parts) {
	  if (s.size() != 4 && s.size() != 5) {
	    gCoutMutex.lock();
	    std::cout << "protocol error: invalid move '" << s << "'\n";
	    gCoutMutex.unlock();
	    return;
	  }
	  if (s[0] < 'a' || s[0] > 'h' || s[1] < '1' || s[1] > '8'
	   || s[2] < 'a' || s[2] > 'h' || s[3] < '1' || s[3] > '8') {
	    gCoutMutex.lock();
	    std::cout << "protocol error: invalid move '" << s << "'\n";
	    gCoutMutex.unlock();
	    return;
	  }

	  int fromSq = SqFR(s[0] - 'a', s[1] - '1');
	  int toSq = SqFR(s[2] - 'a', s[3] - '1');
	  PromoteType promote = PROMOTE_NONE;
	  if (s.size() == 5) {
	    switch (s[4]) {
	    case 'q': case 'Q': promote = PROMOTE_QUEEN; break;
	    case 'n': case 'N': promote = PROMOTE_KNIGHT; break;
	    case 'b': case 'B': promote = PROMOTE_BISHOP; break;
	    case 'r': case 'R': promote = PROMOTE_ROOK; break;
	    default:
	      gCoutMutex.lock();
	      std::cout << "protocol error: invalid move '" << s << "'\n";
	      gCoutMutex.unlock();
	      return;
	    }
	  }

	  std::optional<Move> optMove = ParseMove(pos, fromSq, toSq, promote);
	  if (!optMove) {
	    gCoutMutex.lock();
	    std::cout << "protocol error: illegal move '" << s << "'\n";
	    gCoutMutex.unlock();
	    return;
	  }

	  pos.makeMove(optMove.value());
	  moves.push_back(optMove.value());

	  if (numberOfMoves(pos) == 0) {
	    gCoutMutex.lock();
	    std::cout << "protocol error: the game is over after '" << s << "'\n";
	    gCoutMutex.unlock();
	    return;
	  }
	}
      }

      if (engine.searchInProgress()) {
	gCoutMutex.lock();
	std::cout << "protocol error: search is already in progress\n";
	gCoutMutex.unlock();
	return;
      }

      engine.setPosition(pos, moves);
      return;
    }

    if (command == "go") {
      enum {
	NONE,
	PERFT,
	DEPTH,
	WTIME,
	BTIME,
	WINC,
	BINC,
	MOVETIME,
	MOVESTOGO,
      };

      int perft_ = -1;
      int depth = 0;
      int wtime = 0;
      int btime = 0;
      int winc = 0;
      int binc = 0;
      int movetime = 0;
      int movestogo = 0;
      bool infinite = false;
      bool ponder = false;

      // Note: we ignore any unknown fields in UCI 'go'.
      int field = NONE;
      for (std::string_view s : parts) {
	if (field) {
	  int value = parseInt(s);
	  if (value < 0)
	    continue;
	  switch (field) {
	  case PERFT: perft_ = value; break;
	  case DEPTH: depth = value; break;
	  case WTIME: wtime = value; break;
	  case BTIME: btime = value; break;
	  case WINC: winc = value; break;
	  case BINC: binc = value; break;
	  case MOVETIME: movetime = value; break;
	  case MOVESTOGO: movestogo = value; break;
	  }
	}

	if (s == "perft") field = PERFT;
	else if (s == "depth") field = DEPTH;
	else if (s == "wtime") field = WTIME;
	else if (s == "btime") field = BTIME;
	else if (s == "winc") field = WINC;
	else if (s == "binc") field = BINC;
	else if (s == "movetime") field = MOVETIME;
	else if (s == "movestogo") field = MOVESTOGO;
	else {
	  field = NONE;
	  if (s == "infinite") infinite = true;
	  else if (s == "ponder") ponder = true;
	}
      }

      if (engine.searchInProgress()) {
	gCoutMutex.lock();
	std::cout << "protocol error: search is already in progress\n";
	gCoutMutex.unlock();
	return;
      }

      if (perft_ >= 0) {
	gCoutMutex.lock();
	uint64_t nodeCount = perft<true>(engine.getPosition(), perft_);
	std::cout << "\n";
	std::cout << "Total: " << nodeCount << "\n";
	gCoutMutex.unlock();
      } else
	engine.go(depth, wtime, btime, winc, binc,
		  movetime, movestogo, infinite, ponder);
      return;
    }

    gCoutMutex.lock();
    std::cout << "protocol error: unimplemented command '" << command << "'\n";
    gCoutMutex.unlock();
  }

  void start()
  {
    setvbuf(stdin, nullptr, _IOLBF, 0);
    setvbuf(stdout, nullptr, _IOLBF, 0);

    std::cout << "DSchack by David Bergström\n";
    InitBitboards();
    TestBitboards();
    InitZobristTables();

    UCIEngineCallbacks uciCallbacks;
    Engine engine(uciCallbacks);

    engine.setPosition(ParseFEN(startpos_fen).value(), {});

    // Process input commands, line by line.

    char *lineptr = nullptr;
    size_t n = 0;
    for (;;) {
      ssize_t nread = getline(&lineptr, &n, stdin);
      if (nread < 0)
	break;

      // Split the line into whitespace-separated parts.
      std::vector<std::string_view> parts = splitOnWhitespace(lineptr);
      if (parts.empty())
	continue;

      executeUCI(engine, parts);

      if (gShouldExit)
	break;
    }
  }
}

int main()
{
  DSchack::start();
}
