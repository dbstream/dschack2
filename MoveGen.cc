// SPDX-License-Identifier: GPL-3.0-or-later
/* Move generator.

   Copyright (C) 2026 David Bergström  */

#include "MoveGen.h"

namespace DSchack {
  static Bitboard getRookPinMask(const Position &pos, int kingSq)
  {
    Color us = pos.sideToMove();
    Color them = (us == WHITE) ? BLACK : WHITE;

    Bitboard occ = pos.pieces(BOTH, ALL);
    Bitboard attacks = RookAttacks(occ, kingSq);
    Bitboard xray = RookAttacks(occ & ~attacks, kingSq) & ~attacks;
    Bitboard pinners = xray & (pos.pieces(them, ROOK) | pos.pieces(them, QUEEN));
    Bitboard result = 0;
    while (Bitboard pinner = PopLS1B(pinners))
      result |= InBetween(kingSq, Sq(pinner));

    return result;
  }

  static Bitboard getBishopPinMask(const Position &pos, int kingSq)
  {
    Color us = pos.sideToMove();
    Color them = (us == WHITE) ? BLACK : WHITE;

    Bitboard occ = pos.pieces(BOTH, ALL);
    Bitboard attacks = BishopAttacks(occ, kingSq);
    Bitboard xray = BishopAttacks(occ & ~attacks, kingSq) & ~attacks;
    Bitboard pinners = xray & (pos.pieces(them, BISHOP) | pos.pieces(them, QUEEN));
    Bitboard result = 0;
    while (Bitboard pinner = PopLS1B(pinners))
      result |= InBetween(kingSq, Sq(pinner));

    return result;
  }

  static Move *generatePawnCapturePromotions(int fromSq, int toSq, PieceType capturedPiece, Move *buffer)
  {
    *(buffer++) = Move::makeCapturingPromotion(fromSq, toSq, capturedPiece, QUEEN);
    *(buffer++) = Move::makeCapturingPromotion(fromSq, toSq, capturedPiece, KNIGHT);
    *(buffer++) = Move::makeCapturingPromotion(fromSq, toSq, capturedPiece, BISHOP);
    *(buffer++) = Move::makeCapturingPromotion(fromSq, toSq, capturedPiece, ROOK);
    return buffer;
  }

  static Move *generatePawnPromotions(int fromSq, int toSq, Move *buffer)
  {
    *(buffer++) = Move::makePromotion(fromSq, toSq, QUEEN);
    *(buffer++) = Move::makePromotion(fromSq, toSq, KNIGHT);
    *(buffer++) = Move::makePromotion(fromSq, toSq, BISHOP);
    *(buffer++) = Move::makePromotion(fromSq, toSq, ROOK);
    return buffer;
  }

  Move *GenerateCaptures(const Position &pos, Move *buffer)
  {
    Color us = pos.sideToMove();
    Color them = (us == WHITE) ? BLACK : WHITE;
    int kingSq = Sq(pos.pieces(us, KING));

    Bitboard occ = pos.pieces(BOTH, ALL);
    Bitboard occ_them = pos.pieces(them, ALL);

    Bitboard checkers = pos.attackers(them, kingSq);

    Bitboard kingMoves = KingAttacks(kingSq) & occ_them;
    while (Bitboard to = PopLS1B(kingMoves)) {
      int toSq = Sq(to);
      if (!pos.attackedByOcc(occ & ~BB(kingSq), toSq, them))
	*(buffer++) = Move::makeCapture(kingSq, toSq, KING,
					pos.pieceOnSquare(toSq));
    }

    // Test if we are in double-check.
    if (checkers & (checkers - 1)) {
      // If we are in double-check, we can only make king moves.
      return buffer;
    }

    /* checkMask is a bitmask of squares we can move to. Because this
       is captures-only movegen, we set this to occ_them.  */
    Bitboard checkMask = occ_them;

    // Test for regular check ("single-check").
    if (checkers) {
      // If we are in check, we have to capture or block.
      checkMask &= InBetween(kingSq, Sq(checkers));
    }

    /* When generating rook-like moves, if fromBB intersects with
       rookPinMask then toBB also has to intersect with rookPinMask.
       When generating bishop-like moves, if fromBB intersects with
       bishopPinMask then toBB also has to intersect with
       bishopPinMask.

       No rook-like moves are generated for a piece which intersects
       bishopPinMask, and no bishop-like moves are generated for a
       piece which intersects rookPinMask. No knight moves at all are
       generated for a knight which intersects with rookPinMask or
       bishopPinMask.

       Pawn pushes and double-pushes are treated as rook-like and
       pawn captures are treated as bishop-like.  */
    Bitboard rookPinMask = getRookPinMask(pos, kingSq);
    Bitboard bishopPinMask = getBishopPinMask(pos, kingSq);

    // As per the comment above, discard any knights that are pinned.
    Bitboard knights = pos.pieces(us, KNIGHT) & ~(rookPinMask | bishopPinMask);
    while (Bitboard knight = PopLS1B(knights)) {
      int fromSq = Sq(knight);
      Bitboard attacks = KnightAttacks(fromSq) & checkMask;
      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	*(buffer++) = Move::makeCapture(fromSq, toSq, KNIGHT,
					pos.pieceOnSquare(toSq));
      }
    }

    // If a rook intersects bishopPinMask, it cannot move.
    Bitboard rooks = pos.pieces(us, ROOK) & ~bishopPinMask;
    while (Bitboard rook = PopLS1B(rooks)) {
      int fromSq = Sq(rook);
      Bitboard attacks = RookAttacks(occ, fromSq) & checkMask;
      if (rook & rookPinMask)
	attacks &= rookPinMask;
      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	*(buffer++) = Move::makeCapture(fromSq, toSq, ROOK,
					pos.pieceOnSquare(toSq));
      }
    }

    // If a bishop intersects rookPinMask, then it cannot move.
    Bitboard bishops = pos.pieces(us, BISHOP) & ~rookPinMask;
    while (Bitboard bishop = PopLS1B(bishops)) {
      int fromSq = Sq(bishop);
      Bitboard attacks = BishopAttacks(occ, fromSq) & checkMask;
      if (bishop & bishopPinMask)
	attacks &= bishopPinMask;
      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	*(buffer++) = Move::makeCapture(fromSq, toSq, BISHOP,
					pos.pieceOnSquare(toSq));
      }
    }

    Bitboard queens = pos.pieces(us, QUEEN);
    while (Bitboard queen = PopLS1B(queens)) {
      int fromSq = Sq(queen);
      Bitboard attacks;
      if (queen & rookPinMask)
	attacks = RookAttacks(occ, fromSq) & rookPinMask & checkMask;
      else if (queen & bishopPinMask)
	attacks = BishopAttacks(occ, fromSq) & bishopPinMask & checkMask;
      else
	attacks = (RookAttacks(occ, fromSq) | BishopAttacks(occ, fromSq)) & checkMask;

      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	*(buffer++) = Move::makeCapture(fromSq, toSq, QUEEN,
					pos.pieceOnSquare(toSq));
      }
    }

    // We are only considering captures so ignore rook-pinned pawns.
    Bitboard pawns = pos.pieces(us, PAWN) & ~rookPinMask;
    while (Bitboard pawn = PopLS1B(pawns)) {
      int fromSq = Sq(pawn);
      Bitboard attacks;
      if (us == WHITE)
	attacks = PawnAttacksU(occ, fromSq) & checkMask;
      else
	attacks = PawnAttacksD(occ, fromSq) & checkMask;

      if (pawn & bishopPinMask)
	attacks &= bishopPinMask;

      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	if (RankOf(toSq) == 0 || RankOf(toSq) == 7)
	  buffer = generatePawnCapturePromotions(fromSq, toSq,
						 pos.pieceOnSquare(toSq),
						 buffer);
	else
	  *(buffer++) = Move::makeCapture(fromSq, toSq, PAWN,
					  pos.pieceOnSquare(toSq));
      }
    }

    // En passant (this is a real bread in the ass).
    int enpassantFile = pos.enpassantFile();
    if (enpassantFile == -1) {
      // No en passant, no pain.
      return buffer;
    }

    /* One thing which is nice about en passant is that we know
       that the last move was a double-push. This means we can
       only be in check by a slider piece (ROOK, BISHOP, QUEEN)
       or by the pawn which was pushed.  It therefore suffices
       to test whether a capture reveals an attack on our king
       by a slider piece.  */

    int epSq = SqFR(enpassantFile, (us == WHITE) ? 5 : 2);
    int epPawn = epSq + ((us == WHITE) ? BBSouth : BBNorth);

    if (checkers & ~BB(epPawn)) {
      /* If we are in check by a piece other than the en-passant
	 pawn, capturing en-passant cannot resolve it.  */
      return buffer;
    }

    Bitboard takers;
    if (us == WHITE)
      takers = PawnAttackersU(epSq);
    else
      takers = PawnAttackersD(epSq);

    // Vertically pinned pawns cannot capture the pawn.
    takers &= pos.pieces(us, PAWN) & ~rookPinMask;

    /* Pawns cannot capture if they are pinned to the wrong
       diagonal.  */
    if (!(BB(epSq) & bishopPinMask))
      takers &= ~bishopPinMask;

    if (!takers)
      return buffer;

    /* Make sure that the en-passant move would not reveal an attack
       from a rook which is hidden by both pawns.  */
    Bitboard tmpOcc = occ & ~(BB(epPawn) | LS1B(takers));
    // We still have to consider where the capturing pawn moves.
    tmpOcc |= BB(epSq);
    Bitboard atts = RookAttacks(tmpOcc, kingSq);
    if (atts & (pos.pieces(them, ROOK) | pos.pieces(them, QUEEN)))
      return buffer;

    while (Bitboard from = PopLS1B(takers)) {
      int fromSq = Sq(from);
      *(buffer++) = Move::makeEnpassant(fromSq, epSq);
    }

    return buffer;
  }

  Move *GenerateQuiets(const Position &pos, Move *buffer)
  {
    Color us = pos.sideToMove();
    Color them = (us == WHITE) ? BLACK : WHITE;
    int kingSq = Sq(pos.pieces(us, KING));

    Bitboard occ = pos.pieces(BOTH, ALL);

    Bitboard checkers = pos.attackers(them, kingSq);

    Bitboard kingMoves = KingAttacks(kingSq) & ~occ;
    while (Bitboard to = PopLS1B(kingMoves)) {
      int toSq = Sq(to);
      if (!pos.attackedByOcc(occ & ~BB(kingSq), toSq, them))
	*(buffer++) = Move::makeRegular(kingSq, toSq, KING);
    }

    // Test for double-check.
    if (checkers & (checkers - 1)) {
      // Double-check, we can only make king moves.
      return buffer;
    }

    /* checkMask it a bitmask of squares we can move to. Because this
       is quiet movegen, we cannot capture any piece. Therefore, we
       set this to ~occ.  */
    Bitboard checkMask = ~occ;

    // Test for regular check.
    if (checkers) {
      // If we are in check, we have to capture or block.
      checkMask &= InBetween(kingSq, Sq(checkers));
    }

    /* When generating rook-like moves, if fromBB intersects with
       rookPinMask then toBB also has to intersect with rookPinMask.
       When generating bishop-like moves, if fromBB intersects with
       bishopPinMask then toBB also has to intersect with
       bishopPinMask.

       No rook-like moves are generated for a piece which intersects
       bishopPinMask, and no bishop-like moves are generated for a
       piece which intersects rookPinMask. No knight moves at all are
       generated for a knight which intersects with rookPinMask or
       bishopPinMask.

       Pawn pushes and double-pushes are treated as rook-like and
       pawn captures are treated as bishop-like.  */
    Bitboard rookPinMask = getRookPinMask(pos, kingSq);
    Bitboard bishopPinMask = getBishopPinMask(pos, kingSq);

    // As per the comment above, discard any knights that are pinned.
    Bitboard knights = pos.pieces(us, KNIGHT) & ~(rookPinMask | bishopPinMask);
    while (Bitboard knight = PopLS1B(knights)) {
      int fromSq = Sq(knight);
      Bitboard attacks = KnightAttacks(fromSq) & checkMask;
      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	*(buffer++) = Move::makeRegular(fromSq, toSq, KNIGHT);
      }
    }

    // If a rook intersects bishopPinMask, it cannot move.
    Bitboard rooks = pos.pieces(us, ROOK) & ~bishopPinMask;
    while (Bitboard rook = PopLS1B(rooks)) {
      int fromSq = Sq(rook);
      Bitboard attacks = RookAttacks(occ, fromSq) & checkMask;
      if (rook & rookPinMask)
	attacks &= rookPinMask;
      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	*(buffer++) = Move::makeRegular(fromSq, toSq, ROOK);
      }
    }

    // If a bishop intersects rookPinMask, then it cannot move.
    Bitboard bishops = pos.pieces(us, BISHOP) & ~rookPinMask;
    while (Bitboard bishop = PopLS1B(bishops)) {
      int fromSq = Sq(bishop);
      Bitboard attacks = BishopAttacks(occ, fromSq) & checkMask;
      if (bishop & bishopPinMask)
	attacks &= bishopPinMask;
      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	*(buffer++) = Move::makeRegular(fromSq, toSq, BISHOP);
      }
    }

    Bitboard queens = pos.pieces(us, QUEEN);
    while (Bitboard queen = PopLS1B(queens)) {
      int fromSq = Sq(queen);
      Bitboard attacks;
      if (queen & rookPinMask)
	attacks = RookAttacks(occ, fromSq) & rookPinMask & checkMask;
      else if (queen & bishopPinMask)
	attacks = BishopAttacks(occ, fromSq) & bishopPinMask & checkMask;
      else
	attacks = (RookAttacks(occ, fromSq) | BishopAttacks(occ, fromSq)) & checkMask;

      while (Bitboard to = PopLS1B(attacks)) {
	int toSq = Sq(to);
	*(buffer++) = Move::makeRegular(fromSq, toSq, QUEEN);
      }
    }

    /* We generate non-capture non-e.p. pawn moves in two phases:
         - Single pushes.
         - Double pushes.
       We are only considering pushes so ignore bishop-pinned pawns.
       Additionally, ignore pawns which are horizontally pinned,
       since those cannot move upwards.  */
    Bitboard pawns = pos.pieces(us, PAWN) & ~bishopPinMask;
    pawns &= ~(LShift(BB_RANK_1, RankOf(kingSq) * BBNorth) & rookPinMask);

    int forwards = (us == WHITE) ? BBNorth : BBSouth;

    // Mask off pawns which are blocked by a piece in the way.
    pawns &= ~LShift(occ, -forwards);

    // Generate pawn promotions.
    Bitboard tmpPawns = pawns & ((us == WHITE) ? BB_RANK_7 : BB_RANK_2);
    if (checkers)
      tmpPawns &= LShift(checkMask, -forwards);
    while (Bitboard pawn = PopLS1B(tmpPawns)) {
      int fromSq = Sq(pawn);
      int toSq = fromSq + forwards;
      buffer = generatePawnPromotions(fromSq, toSq, buffer);
    }

    // Generate non-promoting single pushes.
    tmpPawns = pawns & ~((us == WHITE) ? BB_RANK_7 : BB_RANK_2);
    if (checkers)
      tmpPawns &= LShift(checkMask, -forwards);
    while (Bitboard pawn = PopLS1B(tmpPawns)) {
      int fromSq = Sq(pawn);
      int toSq = fromSq + forwards;
      *(buffer++) = Move::makeRegular(fromSq, toSq, PAWN);
    }

    // Generate double pushes.
    pawns &= ~LShift(occ, -2 * forwards);
    pawns &= (us == WHITE) ? BB_RANK_2 : BB_RANK_7;
    pawns &= LShift(checkMask, -2 * forwards);
    while (Bitboard pawn = PopLS1B(pawns)) {
      int fromSq = Sq(pawn);
      int toSq = fromSq + 2 * forwards;
      *(buffer++) = Move::makeDoublePush(fromSq, toSq);
    }

    if (checkers) {
      // If we are in check, we cannot castle.
      return buffer;
    }

    int caTargetL, caTargetR, caCheckL, caCheckR;
    Bitboard caEmptyL, caEmptyR;
    if (us == WHITE) {
      caTargetL = C1;
      caTargetR = G1;
      caCheckL = D1;
      caCheckR = F1;
      caEmptyL = BB(B1) | BB(C1) | BB(D1);
      caEmptyR = BB(F1) | BB(G1);
    } else {
      caTargetL = C8;
      caTargetR = G8;
      caCheckL = D8;
      caCheckR = F8;
      caEmptyL = BB(B8) | BB(C8) | BB(D8);
      caEmptyR = BB(F8) | BB(G8);
    }

    // Generate queenside castles.
    if ((pos.castlingRights(us) & QUEENSIDE) && !(occ & caEmptyL)) {
      if (!pos.attackedByOcc(occ, caCheckL, them)
       && !pos.attackedByOcc(occ, caTargetL, them))
	*(buffer++) = Move::makeCastles(kingSq, caTargetL);
    }

    // Generate kingside castles.
    if ((pos.castlingRights(us) & KINGSIDE) && !(occ & caEmptyR)) {
      if (!pos.attackedByOcc(occ, caCheckR, them)
       && !pos.attackedByOcc(occ, caTargetR, them))
	*(buffer++) = Move::makeCastles(kingSq, caTargetR);
    }

    return buffer;
  }

  Move *GenerateLegalMoves(const Position &pos, Move *buffer)
  {
    buffer = GenerateCaptures(pos, buffer);
    buffer = GenerateQuiets(pos, buffer);
    return buffer;
  }
}
