// SPDX-License-Identifier: GPL-3.0-or-later
/* DSchack.

   Copyright (C) 2026 David Bergström  */

#include <iostream>

#include "Bitboard.h"
#include "MoveGen.h"
#include "Position.h"

namespace DSchack {
  static const std::vector<std::string> startpos_fen = {
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR",
    "w",
    "KQkq",
    "-",
    "0",
    "1"
  };

  static const std::vector<std::string> kiwipete_fen = {
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R",
    "w",
    "KQkq",
    "-",
    "0",
    "1"
  };

  static const std::vector<std::string> position_3 = {
    "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8",
    "w",
    "-",
    "-",
    "0",
    "1"
  };

  static const std::vector<std::string> position_4 = {
    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1",
    "w",
    "kq",
    "-",
    "0",
    "1"
  };

  static const std::vector<std::string> position_4_mirrored = {
    "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R",
    "b",
    "KQ",
    "-",
    "0",
    "1"
  };

  static const std::vector<std::string> position_5 = {
    "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R",
    "w",
    "KQ",
    "-",
    "1",
    "8"
  };

  static const std::vector<std::string> position_6 = {
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1",
    "w",
    "-",
    "-",
    "0",
    "10"
  };

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

  void start()
  {
    std::cout << "DSchack by David Bergström\n";
    InitBitboards();
    TestBitboards();
    InitZobristTables();

    std::optional<Position> opt_pos = ParseFEN(kiwipete_fen);

    if (!opt_pos) {
      std::cout << "Failed to parse startpos_fen??\n";
      return;
    }

    Position pos = opt_pos.value();
    std::cout << "Startpos hash=" << pos.hash() << "\n";
    for (int i = 1; i <= 8; i++) {
      std::cout << "Perft(" << i << ")\n";
      uint64_t nodeCount = perft<true>(pos, i);
      std::cout << "\nTotal: " << nodeCount << "\n";
    }
  }
}

int main()
{
  DSchack::start();
}
