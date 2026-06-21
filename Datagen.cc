// SPDX-License-Identifier: GPL-3.0-or-later
/* Data generation utilities

   Copyright (C) 2026 David Bergström

This file doesn't actually implement the self-play used for data
generation. Utilities for writing a binary format of games played
are instead implemented here.  */

#include "Position.h"

namespace DSchack {
  static constexpr void put_le16(uint8_t *dst, uint16_t value)
  {
    dst[0] = value;
    dst[1] = value >> 8;
  }

  static constexpr void put_le64(uint8_t *dst, uint64_t value)
  {
    dst[0] = value;
    dst[1] = value >> 8;
    dst[2] = value >> 16;
    dst[3] = value >> 24;
    dst[4] = value >> 32;
    dst[5] = value >> 40;
    dst[6] = value >> 48;
    dst[7] = value >> 56;
  }

  /* We use viriformat for storage.

     All integers in viriformat are little-endian.

     In viriformat, a game consists of a header followed by a list
     of (move, score) pairs. The header has the following format:
       offset  size  purpose
            0     8  occupancy bitboard (LERF)
	    8    16  32 4-bit entries representing piece
	    24    1  en-passant and STM
	    25    1  rule50 count
	    26    2  full move count (unused by us)
	    28    2  score
	    30    1  game result (0=black, 1=draw, 2=white)
	    31    1  unused
     This is followed by a list of (move, score) pairs, which is
     terminated by four null bytes.

     The i'th piece of the piece array corresponds to the i'th
     square of the occupancy bitboard. A piece is encoded as such:
        bits[0..2]: piece type.
	  0 - pawn
	  1 - knight
	  2 - bishop
	  3 - rook
	  4 - queen
	  5 - king
	  6 - unmoved rook (this encodes castling rights)
	bits[3]: color. (0=white, 1=black)

      The highest bit of the en-passant and STM field represents the
      side to move (0=white, 1=black). The lowest 7 bits represent
      en-passant target square, or 64 if there is no en passant.

      A move is a 16-bit integer of the following format:
        bits[0..5]: from square
        bits[6..11]: to square
        bits[12..13]: promotion type (knight=0, bishop=1, rook=2, queen=3)
        bits[14..15]: move type (0=regular, 1=en passant, 2=castling, 3=promotion)
      For castling moves, the from square is the square of the king
      and the to square is the square of the rook.  */

  static constexpr size_t VIRIFORMAT_GAME_HEADER_LEN = 32;

  static constexpr uint8_t viri_piece(Color color, PieceType pieceType)
  {
    uint8_t result = 0;
    if (color == BLACK)
      result |= 8;
    switch(pieceType) {
    case PAWN:   result |= 0; break;
    case KNIGHT: result |= 1; break;
    case BISHOP: result |= 2; break;
    case ROOK:   result |= 3; break;
    case QUEEN:  result |= 4; break;
    case KING:   result |= 5; break;
    }
    return result;
  }

  static constexpr int get_en_passant(const Position &pos)
  {
    Color stm = pos.sideToMove();
    Color sntm = (stm == WHITE) ? BLACK : WHITE;

    int file = pos.enpassantFile();
    if (file < 0)
      return 64;

    int epSq = SqFR(file, (stm == WHITE) ? 5 : 2);
    int pawnSq = epSq ^ 8;

    Bitboard takers = pos.pieces(stm, PAWN) & ((stm == WHITE)
					       ? PawnAttackersU(epSq)
					       : PawnAttackersD(epSq));
    if (!takers)
      return 64;

    int kingSq = Sq(pos.pieces(stm, KING));
    Bitboard checkers = pos.attackers(sntm, kingSq);

    if (checkers & ~BB(pawnSq))
      return 64;

    while (Bitboard taker = PopLS1B(takers)) {
      Bitboard occ = pos.pieces(BOTH, ALL) ^ BB(epSq) ^ BB(pawnSq) ^ taker;
      Bitboard attackers = pos.attackersByOcc(occ, kingSq, sntm);
      if (!(attackers & ~pawnSq))
	return epSq;
    }

    return 64;
  }

  static void encode_position(uint8_t *dst, const Position &pos, int score, int result)
  {
    for (int i = 0; i < VIRIFORMAT_GAME_HEADER_LEN; i++)
      dst[i] = 0;

    Bitboard occ = pos.pieces(BOTH, ALL);
    put_le64(dst + 0, occ);
    for (int i = 0; i < 32; i++) {
      if (!occ)
	break;

      int sq = Sq(PopLS1B(occ));
      Color c = (pos.pieces(WHITE, ALL) & BB(sq)) ? WHITE : BLACK;
      PieceType pt = pos.pieceOnSquare(sq);

      uint8_t viri_piece_type = viri_piece(c, pt);
      if (sq == A1 && (pos.castlingRights(WHITE) & QUEENSIDE))
	viri_piece_type = 6;
      else if (sq == H1 && (pos.castlingRights(WHITE) & KINGSIDE))
	viri_piece_type = 6;
      else if (sq == A8 && (pos.castlingRights(BLACK) & QUEENSIDE))
	viri_piece_type = 14;
      else if (sq == H8 && (pos.castlingRights(BLACK) & KINGSIDE))
	viri_piece_type = 14;

      if (i & 1)
	viri_piece_type <<= 4;

      dst[8 + i / 2] |= viri_piece_type;
    }

    if (pos.sideToMove() == BLACK)
      score = -score;

    dst[24] = (pos.sideToMove() == BLACK ? 128 : 0) | get_en_passant(pos);
    dst[25] = pos.rule50();
    put_le16(dst + 28, (uint16_t) (int16_t) score);
    dst[30] = result;
  }

  static void encode_move(uint8_t *dst, Move move)
  {
    uint16_t fromSq = move.fromSquare();
    uint16_t toSq = move.toSquare();
    uint16_t result = 0;

    if (move.isCastles()) {
      result = 0x8000;
      if (toSq == C1)
	toSq = A1;
      else if (toSq == G1)
	toSq = H1;
      else if (toSq == C8)
	toSq = A8;
      else if (toSq == G8)
	toSq = H8;
    } else if (move.isPromotion()) {
      switch(move.promotion()) {
      case KNIGHT: result = 0xc000; break;
      case BISHOP: result = 0xd000; break;
      case ROOK:   result = 0xe000; break;
      case QUEEN:  result = 0xf000; break;
      }
    } else if (move.isEnPassant())
      result = 0x4000;

    result |= fromSq | (toSq << 6);
    put_le16(dst, result);
  }

  static constexpr size_t game_size(int num_moves)
  {
    return VIRIFORMAT_GAME_HEADER_LEN + 4 * num_moves + 4;
  }

  std::vector<uint8_t> EncodeGame(const Position &startpos,
				  std::vector<Move> moves,
				  std::vector<int> scores,
				  int gameResult)
  {
    int numMoves = moves.size();
    if ((int) scores.size() < numMoves)
      numMoves = scores.size();

    std::vector<uint8_t> result(game_size(numMoves));

    int root_score = 0;
    if (numMoves > 0)
      root_score = scores[0];

    encode_position(result.data(), startpos, root_score, gameResult);

    for (int i = 0; i <= numMoves; i++) {
      int offset = VIRIFORMAT_GAME_HEADER_LEN + 4 * i;
      if (i == numMoves) {
	put_le16(result.data() + offset, 0);
	put_le16(result.data() + offset + 2, 0);
      } else {
	encode_move(result.data() + offset, moves[i]);
	if ((i + ((startpos.sideToMove() == BLACK) ? 1 : 0)) & 1)
	  put_le16(result.data() + offset + 2, -scores[i]);
	else
	  put_le16(result.data() + offset + 2, scores[i]);
      }
    }

    return result;
  }
} // namespace DSchack
