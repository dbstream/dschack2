// SPDX-License-Identifier: GPL-3.0-or-later
/* FEN and move parsing.

   Copyright (C) 2026 David Bergström  */

#include <string_view>

#include "MoveGen.h"
#include "Position.h"

namespace DSchack {
  uint64_t pieceZobristTable[6][64];
  uint64_t whiteZobristTable[64];
  uint64_t enpassantZobristTable[8];
  uint64_t castlingRightsZobristTable[2][2];
  uint64_t sideToMoveZobristValue;

  void InitZobristTables()
  {
    auto gen = [](){
      static uint64_t state[2] = {
	UINT64_C(0x1234567890abcdef),
	UINT64_C(0xdeafbeef76543210)
      };

      uint64_t t = state[0];
      uint64_t s = state[1];
      state[0] = s;
      t ^= t << 23;
      t ^= t >> 17;
      t ^= s;
      state[1] = t + s;
      return t;
    };

    for (int piece = 0; piece < 6; piece++) {
      for (int sq = 0; sq < 64; sq++)
	pieceZobristTable[piece][sq] = gen();
    }
    for (int sq = 0; sq < 64; sq++)
      whiteZobristTable[sq] = gen();
    for (int i = 0; i < 8; i++)
      enpassantZobristTable[i] = gen();
    for (int i = 0; i < 2; i++)
      for (int j = 0; j < 2; j++)
	castlingRightsZobristTable[i][j] = gen();
    sideToMoveZobristValue = gen();
  }

  static int parseInt(std::string_view sv)
  {
    // Bah, the C++ standard library is shite in this regard.
    int res = 0;
    for (char c : sv) {
      if (c < '0' || c > '9')
	return -1;
      if (res >= 1000)
	return -1;
      res *= 10;
      res += c - '0';
    }
    return res;
  }

  static std::optional<PieceType> charToPieceType(char c)
  {
    switch(c) {
    case 'P': case 'p': return PAWN;
    case 'N': case 'n': return KNIGHT;
    case 'B': case 'b': return BISHOP;
    case 'R': case 'r': return ROOK;
    case 'Q': case 'q': return QUEEN;
    case 'K': case 'k': return KING;
    default: return std::nullopt;
    }
  }

  static std::optional<Color> charToColor(char c)
  {
    switch(c) {
    case 'P': case 'N': case 'B': case 'R': case 'Q': case 'K': return WHITE;
    case 'p': case 'n': case 'b': case 'r': case 'q': case 'k': return BLACK;
    default: return std::nullopt;
    }
  }

  static bool parsePiecePlacement(Position &pos, const std::string &pieces)
  {
    int rank = 7, file = 0;
    for (char c : pieces) {
      if (c == '/') {
	if (file != 8)
	  return false;
	file = 0;
	rank--;
	continue;
      }
      if ('1' <= c && c <= '8') {
	file += c - '0';
	continue;
      }
      if (rank < 0 || file >= 8)
	return false;
      std::optional<Color> optColor = charToColor(c);
      if (!optColor)
	return false;
      Color color = optColor.value();
      PieceType piece = charToPieceType(c).value();
      pos.xorPiece(color, piece, SqFR(file, rank));
      file++;
    }
    return rank == 0 && file == 8;
  }

  std::optional<Position> ParseFEN(const std::vector<std::string> &parts)
  {
    if (parts.size() != 6)
      return std::nullopt;

    Position pos;
    if (!parsePiecePlacement(pos, parts[0]))
      return std::nullopt;

    if (parts[1] == "w")
      pos.setSideToMove(WHITE);
    else if (parts[1] == "b")
      pos.setSideToMove(BLACK);
    else
      return std::nullopt;

    if (parts[2] != "-") {
      for (char c : parts[2]) {
	switch(c) {
	case 'Q': pos.setCastlingRight(WHITE, QUEENSIDE); break;
	case 'K': pos.setCastlingRight(WHITE, KINGSIDE); break;
	case 'q': pos.setCastlingRight(BLACK, QUEENSIDE); break;
	case 'k': pos.setCastlingRight(BLACK, KINGSIDE); break;
	default: return std::nullopt;
	}
      }
    }

    Color us = pos.sideToMove();
    Color them = (us == WHITE) ? BLACK : WHITE;

    if (parts[3] != "-") {
      if (parts[3].size() != 2)
	return std::nullopt;
      char fileC = parts[3][0];
      char rankC = parts[3][1];

      if (fileC < 'a' || fileC > 'h') return std::nullopt;
      if (rankC < '1' || rankC > '8') return std::nullopt;

      int file = fileC - 'a';
      int rank = rankC - '1';
      if (rank != ((us == WHITE) ? 5 : 2))
	return std::nullopt;
      int epSq = SqFR(file, rank);
      int forward = (them == WHITE) ? BBNorth : BBSouth;
      if (pos.pieces(BOTH, ALL) & BB(epSq))
	return std::nullopt;
      if (!(pos.pieces(them, PAWN) & BB(epSq + forward)))
	return std::nullopt;

      pos.setEnpassantFile(file);
    }

    int rule50 = parseInt(parts[4]);
    if (rule50 < 0 || rule50 >= 100)
      return std::nullopt;

    pos.setRule50(rule50);
    if (parseInt(parts[5]) < 0)
      return std::nullopt;

    // Position validations.

    // Both players must have exactly one king.
    if (Popcount(pos.pieces(WHITE, KING)) != 1) return std::nullopt;
    if (Popcount(pos.pieces(BLACK, KING)) != 1) return std::nullopt;

    // There are no pawns on the first or eight ranks.
    if (pos.pieces(BOTH, PAWN) & (BB_RANK_1 | BB_RANK_8))
      return std::nullopt;

    // If there is a castling right, there is also a king and rook.
    if (pos.castlingRights(WHITE) & (KINGSIDE | QUEENSIDE)) {
      if (!(pos.pieces(WHITE, KING) & BB(E1)))
	return std::nullopt;
    }
    if (pos.castlingRights(BLACK) & (KINGSIDE | QUEENSIDE)) {
      if (!(pos.pieces(BLACK, KING) & BB(E8)))
	return std::nullopt;
    }
    if (pos.castlingRights(WHITE) & QUEENSIDE) {
      if (!(pos.pieces(WHITE, ROOK) & BB(A1)))
	return std::nullopt;
    }
    if (pos.castlingRights(WHITE) & KINGSIDE) {
      if (!(pos.pieces(WHITE, ROOK) & BB(H1)))
	return std::nullopt;
    }
    if (pos.castlingRights(BLACK) & QUEENSIDE) {
      if (!(pos.pieces(BLACK, ROOK) & BB(A8)))
	return std::nullopt;
    }
    if (pos.castlingRights(BLACK) & KINGSIDE) {
      if (!(pos.pieces(BLACK, ROOK) & BB(H8)))
	return std::nullopt;
    }

    // We cannot capture the opponent's king.
    if (pos.attackers(us, Sq(pos.pieces(them, KING))))
      return std::nullopt;

    // We have at least one legal move.
    {
      Move buffer[1000];
      Move *pEnd = GenerateLegalMoves(pos, &buffer[0]);
      if (pEnd == &buffer[0])
	return std::nullopt;
    }

    return pos;
  }
} // namespace DSchack
