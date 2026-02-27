// SPDX-License-Identifier: GPL-3.0-or-later
/* Board representation.

   Copyright (C) 2026 David Bergström  */

#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <stdint.h>
#include <vector>

#include "Bitboard.h"

namespace DSchack {
  enum Color {
    WHITE,
    BLACK,
    BOTH
  };

  enum PieceType {
    PAWN,
    KNIGHT,
    BISHOP,
    ROOK,
    QUEEN,
    KING,
    ALL
  };

  typedef int CastlingRights;
  enum {
    QUEENSIDE = 1,
    KINGSIDE = 2
  };

  extern uint64_t pieceZobristTable[6][64];
  extern uint64_t whiteZobristTable[64];
  extern uint64_t enpassantZobristTable[8];
  extern uint64_t castlingRightsZobristTable[2][2];
  extern uint64_t sideToMoveZobristValue;

  void InitZobristTables();

  enum MoveFlags {
    MOVE_CAPTURE = 1,
    MOVE_ENPASSANT = 2,
    MOVE_DOUBLE_PUSH = 4,
    MOVE_CASTLES = 8,
    MOVE_PROMOTION = 16
  };

  class Move {
    uint8_t m_fromSquare = 0;
    uint8_t m_toSquare = 0;
    uint8_t m_moveFlags = 0;
    PieceType m_movedPiece = PAWN;
    PieceType m_capturedPiece = PAWN;
    PieceType m_promotionPiece = PAWN;

    constexpr Move(uint8_t fromSq, uint8_t toSq, uint8_t flags,
		   PieceType movedPiece, PieceType capturedPiece,
		   PieceType promotionPiece)
      : m_fromSquare(fromSq), m_toSquare(toSq), m_moveFlags(flags),
	m_movedPiece(movedPiece), m_capturedPiece(capturedPiece),
	m_promotionPiece(promotionPiece)
    {}

  public:
    constexpr Move() = default;

    constexpr bool sameAs(const Move &other) const
    {
      return m_fromSquare == other.m_fromSquare
	&& m_toSquare == other.m_toSquare
	&& m_promotionPiece == other.m_promotionPiece;
    }

    constexpr std::string toUCI() const
    {
      char res[6];
      res[0] = 'a' + FileOf(m_fromSquare);
      res[1] = '1' + RankOf(m_fromSquare);
      res[2] = 'a' + FileOf(m_toSquare);
      res[3] = '1' + RankOf(m_toSquare);
      if (m_moveFlags & MOVE_PROMOTION) {
	res[4] = [&](){
	  switch (m_promotionPiece) {
	  case QUEEN: return 'q';
	  case KNIGHT: return 'n';
	  case BISHOP: return 'b';
	  case ROOK: return 'r';
	  default: return '\\';
	  }
	}();
	res[5] = 0;
      } else
	res[4] = 0;

      return res;
    }

    constexpr static Move makeNull()
    {
      return Move(0, 0, 0, PAWN, PAWN, PAWN);
    }

    constexpr static Move makeRegular(int fromSq, int toSq, PieceType movedPiece)
    {
      return Move(fromSq, toSq, 0, movedPiece, PAWN, PAWN);
    }

    constexpr static Move makeCapture(int fromSq, int toSq, PieceType movedPiece,
			       PieceType capturedPiece)
    {
      return Move(fromSq, toSq, MOVE_CAPTURE, movedPiece, capturedPiece, PAWN);
    }

    constexpr static Move makeEnpassant(int fromSq, int toSq)
    {
      return Move(fromSq, toSq, MOVE_ENPASSANT, PAWN, PAWN, PAWN);
    }

    constexpr static Move makeDoublePush(int fromSq, int toSq)
    {
      return Move(fromSq, toSq, MOVE_DOUBLE_PUSH, PAWN, PAWN, PAWN);
    }

    constexpr static Move makeCastles(int fromSq, int toSq)
    {
      return Move(fromSq, toSq, MOVE_CASTLES, KING, PAWN, PAWN);
    }

    constexpr static Move makePromotion(int fromSq, int toSq, PieceType promotedPiece)
    {
      return Move(fromSq, toSq, MOVE_PROMOTION, PAWN, PAWN, promotedPiece);
    }

    constexpr static Move makeCapturingPromotion(int fromSq, int toSq,
					  PieceType capturedPiece,
					  PieceType promotedPiece)
    {
      return Move(fromSq, toSq, MOVE_CAPTURE | MOVE_PROMOTION, PAWN,
		  capturedPiece, promotedPiece);
    }

    /** fromSquare: get the origin square of a move. */
    constexpr int fromSquare() const
    {
      return m_fromSquare;
    }

    /** toSquare: get the destination square of a move. */
    constexpr int toSquare() const
    {
      return m_toSquare;
    }

    /** piece: get the piece which is moved. */
    constexpr PieceType piece() const
    {
      return m_movedPiece;
    }

    /** isNull: returns true if the move is a null move. */
    constexpr bool isNull() const
    {
      return m_fromSquare == 0 && m_toSquare == 0;
    }

    /** isCapture: returns true if the move is a capture. */
    constexpr bool isCapture() const
    {
      return (m_moveFlags & MOVE_CAPTURE) ? true : false;
    }

    /** isEnPassant: returns true if the move is en-passant. */
    constexpr bool isEnPassant() const
    {
      return (m_moveFlags & MOVE_ENPASSANT) ? true : false;
    }

    /** isDoublePush: returns true if the move is a double-push. */
    constexpr bool isDoublePush() const
    {
      return (m_moveFlags & MOVE_DOUBLE_PUSH) ? true : false;
    }

    /** isCastles: returns true if this is castles. */
    constexpr bool isCastles() const
    {
      return (m_moveFlags & MOVE_CASTLES) ? true : false;
    }

    /** isPromotion: returns true if this move is a promotion. */
    constexpr bool isPromotion() const
    {
      return (m_moveFlags & MOVE_PROMOTION) ? true : false;
    }

    /** capture: get the captured piece. */
    constexpr PieceType capture() const
    {
      return m_capturedPiece;
    }

    /** promotion: get the promoted-to piece in case
	of a pawn move to the last rank.  */
    constexpr PieceType promotion() const
    {
      return m_promotionPiece;
    }

    /** resetsRule50: return true if this move resets the
	50-move rule halfmove clock.  */
    constexpr bool resetsRule50() const
    {
      return isCapture() || (piece() == PAWN);
    }
  };

  class Position {
    Bitboard m_colorBB[2] {};
    Bitboard m_pieceBB[6] {};

    uint64_t m_zobristHash = 0;

    int m_rule50 = 0;
    int m_enpassantFile = -1;

    CastlingRights m_castlingRights[2] {};

    Color m_sideToMove = WHITE;

  public:
    /** pieces: get the corresponding piece Bitboard.
    @color: a color, or BOTH.
    @pieceType: a piece type, or ALL.
    Returns a Bitboard with one bit set corresponding to each
    matching piece on the board.  */
    constexpr Bitboard pieces(Color color, PieceType pieceType) const
    {
      Bitboard pieceMask = 0;
      if (pieceType == ALL) {
	for (int i = 0; i < 6; i++)
	  pieceMask |= m_pieceBB[i];
      } else
	pieceMask |= m_pieceBB[pieceType];

      if (color != BOTH)
	pieceMask &= m_colorBB[color];

      return pieceMask;
    }

    /** pieceOnSquare: get the type of a piece.
    @sq: square that the piece stands on.
    Returns the PieceType of that piece.

    If there is no piece on that square, behavior is undefined.  */
    constexpr PieceType pieceOnSquare(int sq) const
    {
      for (int i = 0; i < 6; i++) {
	if (m_pieceBB[i] & BB(sq))
	  return static_cast<PieceType>(i);
      }

      __builtin_unreachable();
    }

    /** enpassantFile: get the en-passant file index (0-7).
    Returns -1 if there is no en-passant square.  */
    constexpr int enpassantFile() const
    {
      return m_enpassantFile;
    }

    /** rule50: get the 50-move rule halfmove clock.  */
    constexpr int rule50() const
    {
      return m_rule50;
    }

    /** castlingRights: get the castling rights for some player.
    @color: player to get the castling rights for.
    Returns the castling rights for that player.  */
    constexpr CastlingRights castlingRights(Color color) const
    {
      return m_castlingRights[color];
    }

    /** sideToMove: return the current side to move.  */
    constexpr Color sideToMove() const
    {
      return m_sideToMove;
    }

    /** hash: get the Zobrist hash for this position.  */
    constexpr uint64_t hash() const
    {
      return m_zobristHash;
    }

    /** setRule50: set the 50-move rule counter.
    @count: new 50-move rule counter value.  */
    constexpr void setRule50(int count)
    {
      m_rule50 = count;
    }

    /** setSideToMove: update the side to move.
    @side: new side to move.  */
    constexpr void setSideToMove(Color side)
    {
      if (m_sideToMove != side) {
	m_sideToMove = side;
	m_zobristHash ^= sideToMoveZobristValue;
      }
    }

    /** setCastlingRight: set a castling right.
    @side: player whose castling right to set.
    @right: the castling right to set.  */
    constexpr void setCastlingRight(Color side, CastlingRights right)
    {
      if (!(castlingRights(side) & right)) {
	m_castlingRights[side] |= right;
	m_zobristHash ^= castlingRightsZobristTable[side][right == KINGSIDE];
      }
    }

    /** clearCastlingRight: remove a castling right.
    @side: player whose castling right to remove.
    @right: the castling right to remove.  */
    constexpr void clearCastlingRight(Color side, CastlingRights right)
    {
      if (castlingRights(side) & right) {
	m_castlingRights[side] &= ~right;
	m_zobristHash ^= castlingRightsZobristTable[side][right == KINGSIDE];
      }
    }

    /** setEnpassantFile: update the en-passant file.
    @file: file index (0-7) or -1 for no en passant.  */
    constexpr void setEnpassantFile(int file)
    {
      if (enpassantFile() != -1)
	m_zobristHash ^= enpassantZobristTable[enpassantFile()];

      m_enpassantFile = file;
      if (file != -1)
	m_zobristHash ^= enpassantZobristTable[file];
    }

    /** xorPiece: update the color and piece BB's to reflect having
	added or removed a piece.
    @color: piece color
    @piece: piece type
    @sq: square  */
    constexpr void xorPiece(Color color, PieceType piece, int sq)
    {
      m_pieceBB[piece] ^= BB(sq);
      m_colorBB[color] ^= BB(sq);
      m_zobristHash ^= pieceZobristTable[piece][sq];
      if (color == WHITE)
	m_zobristHash ^= whiteZobristTable[sq];
    }

    /** xorPieceNoHash: update the color and piece BB's but not the
	hash.
    @color: piece color
    @piece: piece type
    @sq: square  */
    constexpr void xorPieceNoHash(Color color, PieceType piece, int sq)
    {
      m_pieceBB[piece] ^= BB(sq);
      m_colorBB[color] ^= BB(sq);
    }

    /** attackers: get the bitboard of attackers of a
	particular square.
    @color: player whose attacking pieces to get (e.g. if you
            are testing whether the WHITE king is under attack,
	    this should be BLACK).
    @sq: square which is being tested.
    Returns a Bitboard with one bit set for each piece which
    attacks the square.

    Note: en-passant attacks and regular pawn pushes are not
    included.  */
    constexpr Bitboard attackers(Color color, int sq) const
    {
      Bitboard occ = pieces(BOTH, ALL);
      Bitboard res = 0;

      if (color == WHITE || color == BOTH)
	res |= pieces(color, PAWN) & PawnAttackersU(sq);
      if (color == BLACK || color == BOTH)
	res |= pieces(color, PAWN) & PawnAttackersD(sq);

      res |= pieces(color, KNIGHT) & KnightAttacks(sq);
      res |= pieces(color, KING) & KingAttacks(sq);

      Bitboard bishopAttacks = BishopAttacks(occ, sq);
      Bitboard rookAttacks = RookAttacks(occ, sq);
      Bitboard queenAttacks = bishopAttacks | rookAttacks;

      res |= pieces(color, BISHOP) & bishopAttacks;
      res |= pieces(color, ROOK) & rookAttacks;
      res |= pieces(color, QUEEN) & queenAttacks;

      return res;
    }

    /** attackersByOcc: get the bitboard of attackers of a
	particular square.
    @occ: blockers to consider.
    @sq: square to test.
    @color: attacking player.  */
    constexpr Bitboard attackersByOcc(Bitboard occ, int sq, Color color) const
    {
      Bitboard res = 0;
      if (color == WHITE)
	res |= pieces(color, PAWN) & PawnAttackersU(sq);
      else
	res |= pieces(color, PAWN) & PawnAttackersD(sq);

      res |= pieces(color, KNIGHT) & KnightAttacks(sq);
      res |= pieces(color, KING) & KingAttacks(sq);

      Bitboard bishopAttacks = BishopAttacks(occ, sq);
      Bitboard rookAttacks = RookAttacks(occ, sq);
      Bitboard queenAttacks = bishopAttacks | rookAttacks;

      res |= pieces(color, BISHOP) & bishopAttacks;
      res |= pieces(color, ROOK) & rookAttacks;
      res |= pieces(color, QUEEN) & queenAttacks;

      return res;
    }

    /** attackedByOcc: test whether a square is attacked.
    @occ: blockers to consider.
    @sq: square to test.
    @color: attacking player.  */
    constexpr bool attackedByOcc(Bitboard occ, int sq, Color color) const
    {
      if (color == WHITE) {
	if (pieces(color, PAWN) & PawnAttackersU(sq))
	  return true;
      } else {
	if (pieces(color, PAWN) & PawnAttackersD(sq))
	  return true;
      }

      if (pieces(color, KNIGHT) & KnightAttacks(sq))
	return true;
      if (pieces(color, KING) & KingAttacks(sq))
	return true;

      Bitboard bishopAttacks = BishopAttacks(occ, sq);
      if (pieces(color, BISHOP) & bishopAttacks)
	return true;

      Bitboard rookAttacks = RookAttacks(occ, sq);
      if (pieces(color, ROOK) & rookAttacks)
	return true;

      Bitboard queenAttacks = bishopAttacks | rookAttacks;
      if (pieces(color, QUEEN) & queenAttacks)
	return true;

      return false;
    }

    /** inCheck: test if the side to move is in check.  */
    constexpr bool inCheck(void) const
    {
      Color us = sideToMove();
      Color them = (us == WHITE) ? BLACK : WHITE;
      int kingSq = Sq(pieces(us, KING));

      return attackedByOcc(pieces(BOTH, ALL), kingSq, them);
    }

    constexpr void makeNullMove()
    {
      Color us = sideToMove();
      Color them = (us == WHITE) ? BLACK : WHITE;

      setSideToMove(them);
      setEnpassantFile(-1);
      m_rule50++;
    }

    constexpr void makeMove(Move move)
    {
      Color us = sideToMove();
      Color them = (us == WHITE) ? BLACK : WHITE;

      setSideToMove(them);

      if (move.resetsRule50())
	m_rule50 = 0;
      else
	m_rule50++;

      Bitboard bbFromTo = BB(move.fromSquare()) ^ BB(move.toSquare());

      // Move the piece.
      xorPiece(us, move.piece(), move.fromSquare());
      xorPiece(us, move.piece(), move.toSquare());

      if (move.isEnPassant()) {
	// On enpassant, capture the pawn.
	int epsq = move.toSquare() + ((us == WHITE) ? BBSouth : BBNorth);
	xorPiece(them, PAWN, epsq);
	setEnpassantFile(-1);
      } else if(move.isDoublePush()) {
	// On a double push, set the new en-passant file.
	setEnpassantFile(FileOf(move.toSquare()));
      } else {
	setEnpassantFile(-1);

	// Did we capture a piece?
	if (move.isCapture()) {
	  // Remove it from the board.
	  xorPiece(them, move.capture(), move.toSquare());
	}

	if (move.isPromotion()) {
	  // Replace the pawn with the promoted to piece.
	  xorPiece(us, PAWN, move.toSquare());
	  xorPiece(us, move.promotion(), move.toSquare());
	} else if (move.isCastles()) {
	  // Move the castled rook.
	  if (bbFromTo & BB(C1)) {
	    xorPiece(WHITE, ROOK, A1);
	    xorPiece(WHITE, ROOK, D1);
	  } else if (bbFromTo & BB(G1)) {
	    xorPiece(WHITE, ROOK, H1);
	    xorPiece(WHITE, ROOK, F1);
	  } else if (bbFromTo & BB(C8)) {
	    xorPiece(BLACK, ROOK, A8);
	    xorPiece(BLACK, ROOK, D8);
	  } else {
	    xorPiece(BLACK, ROOK, H8);
	    xorPiece(BLACK, ROOK, F8);
	  }
	}

	// Do we disturb castling rights?
	if (bbFromTo & (BB(A1) | BB(E1)))
	  clearCastlingRight(WHITE, QUEENSIDE);
	if (bbFromTo & (BB(H1) | BB(E1)))
	  clearCastlingRight(WHITE, KINGSIDE);
	if (bbFromTo & (BB(A8) | BB(E8)))
	  clearCastlingRight(BLACK, QUEENSIDE);
	if (bbFromTo & (BB(H8) | BB(E8)))
	  clearCastlingRight(BLACK, KINGSIDE);
      }
    }
  };

  enum PromoteType {
    PROMOTE_NONE = 0,
    PROMOTE_QUEEN = 1,
    PROMOTE_KNIGHT = 2,
    PROMOTE_BISHOP = 3,
    PROMOTE_ROOK = 4
  };

  /** ParseFEN: parse a FEN string into a Position object.
  @parts: the parts of the FEN string.
  Returns nullopt if the FEN is invalid. */
  std::optional<Position> ParseFEN(std::span<const std::string_view> parts);

  /** ParseMove: parse a move.
  @pos: Position.
  @fromSq: From square index.
  @toSq: To square index.
  @promotion: PROMOTE_NONE or the promoted-to piece if the
              move promotes a pawn.
  Returns nullopt if the move is invalid. */
  std::optional<Move> ParseMove(const Position &pos, int fromSq, int toSq,
				PromoteType promotion);
} // namespace DSchack
