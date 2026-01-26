// SPDX-License-Identifier: GPL-3.0-or-later
/* Bitboards.

   Copyright (C) 2026  David Bergström  */

#include <array>
#include <iomanip>
#include <iostream>
#include <stdexcept>

#include "Bitboard.h"

namespace DSchack {
  static constexpr Bitboard EDGES = BB_A_FILE | BB_H_FILE | BB_RANK_1 | BB_RANK_8;
  static constexpr Bitboard CORNERS = BB(A1) | BB(A8) | BB(H1) | BB(H8);

  // Slow version of bishop attack generator.
  static constexpr Bitboard calcBishopAttacks(Bitboard occ, int sq)
  {
    Bitboard res = 0;
    Bitboard origin = BB(sq);

    // north east
    Bitboard tmp = origin;
    while (tmp & ~(BB_H_FILE | BB_RANK_8)) {
      tmp = LShift(tmp, BBNorthEast);
      res |= tmp;
      if (tmp & occ) break;
    }
    // north west
    tmp = origin;
    while (tmp & ~(BB_A_FILE | BB_RANK_8)) {
      tmp = LShift(tmp, BBNorthWest);
      res |= tmp;
      if (tmp & occ) break;
    }
    // south east
    tmp = origin;
    while (tmp & ~(BB_H_FILE | BB_RANK_1)) {
      tmp = LShift(tmp, BBSouthEast);
      res |= tmp;
      if (tmp & occ) break;
    }
    // south west
    tmp = origin;
    while (tmp & ~(BB_A_FILE | BB_RANK_1)) {
      tmp = LShift(tmp, BBSouthWest);
      res |= tmp;
      if (tmp & occ) break;
    }

    return res;
  }

  // Slow version of rook attack generator.
  static constexpr Bitboard calcRookAttacks(Bitboard occ, int sq)
  {
    Bitboard res = 0;
    Bitboard origin = BB(sq);

    // north
    Bitboard tmp = origin;
    while (tmp & ~BB_RANK_8) {
      tmp = LShift(tmp, BBNorth);
      res |= tmp;
      if (tmp & occ) break;
    }
    // east
    tmp = origin;
    while (tmp & ~BB_H_FILE) {
      tmp = LShift(tmp, BBEast);
      res |= tmp;
      if (tmp & occ) break;
    }
    // west
    tmp = origin;
    while (tmp & ~BB_A_FILE) {
      tmp = LShift(tmp, BBWest);
      res |= tmp;
      if (tmp & occ) break;
    }
    // south
    tmp = origin;
    while (tmp & ~BB_RANK_1) {
      tmp = LShift(tmp, BBSouth);
      res |= tmp;
      if (tmp & occ) break;
    }

    return res;
  }

  // Slow version of king attack generator.
  static constexpr Bitboard calcKingAttacks(int sq)
  {
    Bitboard res = 0;
    Bitboard origin = BB(sq);

    if (origin & ~BB_RANK_8) res |= LShift(origin, BBNorth);
    if (origin & ~BB_RANK_1) res |= LShift(origin, BBSouth);
    origin |= res;
    if (origin & ~BB_H_FILE) res |= LShift(origin, BBEast);
    if (origin & ~BB_A_FILE) res |= LShift(origin, BBWest);

    return res;
  }

  // Slow version of knight attack generator.
  static constexpr Bitboard calcKnightAttacks(int sq)
  {
    Bitboard res = 0;
    Bitboard origin = BB(sq);

    if (origin & ~(BB_A_FILE | BB_B_FILE)) {
      if (origin & ~BB_RANK_1) res |= LShift(origin, 2 * BBWest + BBSouth);
      if (origin & ~BB_RANK_8) res |= LShift(origin, 2 * BBWest + BBNorth);
    }
    if (origin & ~(BB_G_FILE | BB_H_FILE)) {
      if (origin & ~BB_RANK_1) res |= LShift(origin, 2 * BBEast + BBSouth);
      if (origin & ~BB_RANK_8) res |= LShift(origin, 2 * BBEast + BBNorth);
    }
    if (origin & ~(BB_RANK_1 | BB_RANK_2)) {
      if (origin & ~BB_A_FILE) res |= LShift(origin, 2 * BBSouth + BBWest);
      if (origin & ~BB_H_FILE) res |= LShift(origin, 2 * BBSouth + BBEast);
    }
    if (origin & ~(BB_RANK_7 | BB_RANK_8)) {
      if (origin & ~BB_A_FILE) res |= LShift(origin, 2 * BBNorth + BBWest);
      if (origin & ~BB_H_FILE) res |= LShift(origin, 2 * BBNorth + BBEast);
    }

    return res;
  }

  // Slow version of upward pawn attack generator.
  static constexpr Bitboard calcPawnAttacksU(Bitboard occ, int sq)
  {
    Bitboard origin = BB(sq);

    if (origin & BB_RANK_8)
      // Note: we return zero for pseudo-pawns on the eight rank.
      return 0;

    Bitboard captures = occ;
    if (origin & BB_A_FILE)
      captures &= LShift(origin, BBNorthEast);
    else if (origin & BB_H_FILE)
      captures &= LShift(origin, BBNorthWest);
    else
      captures &= LShift(origin, BBNorthEast) | LShift(origin, BBNorthWest);

    Bitboard pushes = LShift(origin, BBNorth) & ~occ;
    if (pushes && (origin & BB_RANK_2))
      pushes |= LShift(origin, 2 * BBNorth) & ~occ;

    return captures | pushes;
  }

  // Slow version of downward pawn attack generator.
  static constexpr Bitboard calcPawnAttacksD(Bitboard occ, int sq)
  {
    Bitboard origin = BB(sq);

    if (origin & BB_RANK_1)
      // Note: we return zero for pseudo-pawns on the first rank.
      return 0;

    Bitboard captures = occ;
    if (origin & BB_A_FILE)
      captures &= LShift(origin, BBSouthEast);
    else if (origin & BB_H_FILE)
      captures &= LShift(origin, BBSouthWest);
    else
      captures &= LShift(origin, BBSouthEast) | LShift(origin, BBSouthWest);

    Bitboard pushes = LShift(origin, BBSouth) & ~occ;
    if (pushes && (origin & BB_RANK_7))
      pushes |= LShift(origin, 2 * BBSouth) & ~occ;

    return captures | pushes;
  }

  // Slow version of InBetween.
  static constexpr Bitboard calcInBetween(int sq1, int sq2)
  {
    static constexpr Bitboard rank = UINT64_C(0x00000000000000ff); // a1a8
    static constexpr Bitboard file = UINT64_C(0x0101010101010101); // a1h1
    static constexpr Bitboard diagonal = UINT64_C(0x8040201008040201); // a1h8
    static constexpr Bitboard antidiagonal = UINT64_C(0x0102040810204080); // h1a8
    if (sq1 == sq2)
      return BB(sq2);

    int u = (sq1 < sq2) ? sq1 : sq2; // the smaller of (sq1, sq2)
    int v = (sq1 < sq2) ? sq2 : sq1; // the greater of (sq1, sq2)

    Bitboard allBetween = BB(v) - (BB(u) << 1);

    int fileDelta = FileOf(sq1) - FileOf(sq2);
    int rankDelta = RankOf(sq1) - RankOf(sq2);

    Bitboard result = BB(sq2);

    if (fileDelta == 0)
      result |= allBetween & (file << u);
    else if (rankDelta == 0)
      result |= allBetween & (rank << u);
    else if (fileDelta == rankDelta)
      result |= allBetween & (diagonal << u);
    else if (fileDelta == -rankDelta)
      result |= allBetween & (antidiagonal << u);

    return result;
  }

  static Bitboard kingTable[64];
  static Bitboard knightTable[64];

  static Bitboard pawnAttackersUTable[64];
  static Bitboard pawnAttackersDTable[64];

  void InitBitboards()
  {
    for (int sq = 0; sq < 64; sq++) {
      kingTable[sq] = calcKingAttacks(sq);
      knightTable[sq] = calcKnightAttacks(sq);

      pawnAttackersUTable[sq] = PawnAttacksD(~UINT64_C(0), sq);
      pawnAttackersDTable[sq] = PawnAttacksU(~UINT64_C(0), sq);
    }
  }

  Bitboard BishopAttacks(Bitboard occ, int sq)
  {
    return calcBishopAttacks(occ, sq);
  }

  Bitboard RookAttacks(Bitboard occ, int sq)
  {
    return calcRookAttacks(occ, sq);
  }

  Bitboard KingAttacks(int sq)
  {
    return kingTable[sq];
  }

  Bitboard KnightAttacks(int sq)
  {
    return knightTable[sq];
  }

  Bitboard PawnAttacksU(Bitboard occ, int sq)
  {
    return calcPawnAttacksU(occ, sq);
  }

  Bitboard PawnAttacksD(Bitboard occ, int sq)
  {
    return calcPawnAttacksD(occ, sq);
  }

  Bitboard PawnAttackersU(int sq)
  {
    return pawnAttackersUTable[sq];
  }

  Bitboard PawnAttackersD(int sq)
  {
    return pawnAttackersDTable[sq];
  }

  Bitboard InBetween(int sq1, int sq2)
  {
    return calcInBetween(sq1, sq2);
  }

  void TestBitboards()
  {}
} // namespace DSchack
