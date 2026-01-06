// SPDX-License-Identifier: GPL-3.0-or-later
/* FEN and move parsing.

   Copyright (C) 2026 David Bergström  */

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

  static bool parsePiecePlacement(Position &pos, std::string_view pieces)
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

  static bool isPositionLegal(const Position &pos)
  {
    Color us = pos.sideToMove();
    Color them = (us == WHITE) ? BLACK : WHITE;

    // Both players must have exactly one king.
    if (Popcount(pos.pieces(WHITE, KING)) != 1) return false;
    if (Popcount(pos.pieces(BLACK, KING)) != 1) return false;

    // There are no pawns on the first or eight ranks.
    if (pos.pieces(BOTH, PAWN) & (BB_RANK_1 | BB_RANK_8))
      return false;

    // If there is a castling right, there is also a king and rook.
    if (pos.castlingRights(WHITE) & (KINGSIDE | QUEENSIDE)) {
      if (!(pos.pieces(WHITE, KING) & BB(E1)))
	return false;
    }
    if (pos.castlingRights(BLACK) & (KINGSIDE | QUEENSIDE)) {
      if (!(pos.pieces(BLACK, KING) & BB(E8)))
	return false;
    }
    if (pos.castlingRights(WHITE) & QUEENSIDE) {
      if (!(pos.pieces(WHITE, ROOK) & BB(A1)))
	return false;
    }
    if (pos.castlingRights(WHITE) & KINGSIDE) {
      if (!(pos.pieces(WHITE, ROOK) & BB(H1)))
	return false;
    }
    if (pos.castlingRights(BLACK) & QUEENSIDE) {
      if (!(pos.pieces(BLACK, ROOK) & BB(A8)))
	return false;
    }
    if (pos.castlingRights(BLACK) & KINGSIDE) {
      if (!(pos.pieces(BLACK, ROOK) & BB(H8)))
	return false;
    }

    // We cannot capture the opponent's king.
    if (pos.attackers(us, Sq(pos.pieces(them, KING))))
      return false;

    return true;
  }

  std::optional<Position> ParseFEN(std::span<const std::string_view> parts)
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

    if (!isPositionLegal(pos))
      return std::nullopt;

    return pos;
  }

  std::optional<Move> ParseMove(const Position &pos, int fromSq, int toSq,
				PromoteType promotion)
  {
    Color us = pos.sideToMove();
    Color them = (us == WHITE) ? BLACK : WHITE;

    Bitboard occ = pos.pieces(BOTH, ALL);

    // Make sure there is actually a piece on that square.
    if (!(pos.pieces(us, ALL) & BB(fromSq)))
      return std::nullopt;

    // Prevent cannibalism.
    if (pos.pieces(us, ALL) & BB(toSq))
      return std::nullopt;

    bool isCapture = false;
    bool isCastle = false;
    bool isEnpassant = false;
    bool isPromotion = false;
    bool isDoublePush = false;

    if (pos.pieces(them, ALL) & BB(toSq))
      isCapture = true;

    // Make sure that the from and to squares are legal.
    PieceType piece = pos.pieceOnSquare(fromSq);
    if (piece == BISHOP) {
      if (!(BishopAttacks(occ, fromSq) & BB(toSq)))
	return std::nullopt;
    } else if (piece == ROOK) {
      if (!(RookAttacks(occ, fromSq) & BB(toSq)))
	return std::nullopt;
    } else if (piece == QUEEN) {
      Bitboard atts = BishopAttacks(occ, fromSq);
      atts |= RookAttacks(occ, fromSq);
      if (!(atts & BB(toSq)))
	return std::nullopt;
    } else if (piece == KNIGHT) {
      if (!(KnightAttacks(fromSq) & BB(toSq)))
	return std::nullopt;
    } else if (piece == KING) {
      if (!(KingAttacks(fromSq) & BB(toSq))) {
	isCastle = true;
      }
    } else if (us == WHITE) { // white pawn
      Bitboard epMask = 0;
      if (pos.enpassantFile() != -1)
	epMask = BB(SqFR(pos.enpassantFile(), 5));
      if (!(PawnAttacksU(occ | epMask, fromSq) & BB(toSq)))
	return std::nullopt;

      if (toSq - fromSq == 2 * BBNorth)
	isDoublePush = true;
      else if (BB(toSq) & epMask)
	isEnpassant = true;
      else if (BB(toSq) & BB_RANK_8)
	isPromotion = true;
    } else { // black pawn
      Bitboard epMask = 0;
      if (pos.enpassantFile() != -1)
	epMask = BB(SqFR(pos.enpassantFile(), 2));
      if (!(PawnAttacksD(occ | epMask, fromSq) & BB(toSq)))
	return std::nullopt;

      if (toSq - fromSq == 2 * BBSouth)
	isDoublePush = true;
      else if (BB(toSq) & epMask)
	isEnpassant = true;
      else if (BB(toSq) & BB_RANK_1)
	isPromotion = true;
    }

    if (isPromotion) {
      // If this move promotes a pawn, there must be a promotion specified.
      if (promotion == PROMOTE_NONE)
	return std::nullopt;
    } else {
      // If this move doesn't promote a pawn, no promotion may be specified.
      if (promotion != PROMOTE_NONE)
	return std::nullopt;
    }

    if (isCastle) {
      int kingSq, checkL, targetL, checkR, targetR;
      Bitboard emptyL, emptyR;
      if (us == WHITE) {
	kingSq = E1;
	checkL = D1;
	targetL = C1;
	emptyL = BB(B1) | BB(C1) | BB(D1);
	checkR = F1;
	targetR = G1;
	emptyR = BB(F1) | BB(G1);
      } else {
	kingSq = E8;
	checkL = D8;
	targetL = C8;
	emptyL = BB(B8) | BB(C8) | BB(D8);
	checkR = F8;
	targetR = G8;
	emptyR = BB(F8) | BB(G8);
      }

      if (fromSq != kingSq)
	return std::nullopt;

      // Cannot castle when in check.
      if (pos.attackedByOcc(occ, kingSq, them))
	return std::nullopt;

      if (toSq == targetL) { // queenside castling
	if (!(pos.castlingRights(us) & QUEENSIDE))
	  return std::nullopt;
	if (occ & emptyL)
	  return std::nullopt;
	if (pos.attackedByOcc(occ, checkL, them))
	  return std::nullopt;
	if (pos.attackedByOcc(occ, targetL, them))
	  return std::nullopt;
	return Move::makeCastles(fromSq, toSq);
      } else if (toSq == targetR) { // kingside castling
	if (!(pos.castlingRights(us) & KINGSIDE))
	  return std::nullopt;
	if (occ & emptyR)
	  return std::nullopt;
	if (pos.attackedByOcc(occ, checkR, them))
	  return std::nullopt;
	if (pos.attackedByOcc(occ, targetR, them))
	  return std::nullopt;
	return Move::makeCastles(fromSq, toSq);
      } else
	return std::nullopt;
    }

    if (piece == KING) {
      // The to-square of the king cannot be under attack.
      if (pos.attackedByOcc(occ & ~BB(fromSq), toSq, them))
	return std::nullopt;
      if (isCapture)
	return Move::makeCapture(fromSq, toSq, KING,
				 pos.pieceOnSquare(toSq));
      else
	return Move::makeRegular(fromSq, toSq, KING);
    }

    Bitboard removeMask = BB(toSq);
    if (isEnpassant)
      removeMask = BB(toSq + ((us == WHITE) ? BBSouth : BBNorth));

    Bitboard newOcc = occ & ~removeMask;
    newOcc ^= BB(fromSq) ^ BB(toSq);

    int kingSq = Sq(pos.pieces(us, KING));
    if (pos.attackersByOcc(newOcc, kingSq, them) & ~removeMask) {
      // The move would leave us in check.
      return std::nullopt;
    }

    if (isEnpassant)
      return Move::makeEnpassant(fromSq, toSq);

    if (isPromotion) {
      PieceType promotionPiece;
      switch (promotion) {
      case PROMOTE_QUEEN: promotionPiece = QUEEN; break;
      case PROMOTE_KNIGHT: promotionPiece = KNIGHT; break;
      case PROMOTE_BISHOP: promotionPiece = BISHOP; break;
      case PROMOTE_ROOK: promotionPiece = ROOK; break;
      default: __builtin_unreachable();
      }

      if (isCapture)
	return Move::makeCapturingPromotion(fromSq, toSq,
					    pos.pieceOnSquare(toSq),
					    promotionPiece);
      else
	return Move::makePromotion(fromSq, toSq, promotionPiece);
    }

    if (isCapture)
      return Move::makeCapture(fromSq, toSq, piece, pos.pieceOnSquare(toSq));

    if (isDoublePush)
      return Move::makeDoublePush(fromSq, toSq);

    return Move::makeRegular(fromSq, toSq, piece);
  }
} // namespace DSchack
