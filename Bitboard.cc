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

  // Slow version of bishop attack generator, used for verification.
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

  // Slow version of rook attack generator, used for verification.
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

  // Slow version of king attack generator, used for verification.
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

  // Slow version of knight attack generator, used for verification.
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

  // Slow version of upward pawn attack generator, used for verification.
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

  // Slow version of downward pawn attack generator, used for verification.
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

  static constexpr Bitboard sideFiles[8] = {
    BB_A_FILE | BB_B_FILE,
    BB_A_FILE | BB_B_FILE | BB_C_FILE,
    BB_B_FILE | BB_C_FILE | BB_D_FILE,
    BB_C_FILE | BB_D_FILE | BB_E_FILE,
    BB_D_FILE | BB_E_FILE | BB_F_FILE,
    BB_E_FILE | BB_F_FILE | BB_G_FILE,
    BB_F_FILE | BB_G_FILE | BB_H_FILE,
    BB_G_FILE | BB_H_FILE
  };

  static constexpr Bitboard calcPawnFrontSpanU(int sq)
  {
    Bitboard front = LShift(~Bitboard(0), (RankOf(sq) + 1) * BBNorth);
    Bitboard sides = sideFiles[FileOf(sq)];

    return front & sides;
  }

  static constexpr Bitboard calcPawnFrontSpanD(int sq)
  {
    Bitboard front = LShift(~Bitboard(0), (8 - RankOf(sq)) * BBSouth);
    Bitboard sides = sideFiles[FileOf(sq)];

    return front & sides;
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

  // Compute n.o. 16-bit entries per square for bishop attacks.
  static constexpr int calcBishopAttacksSize(int sq)
  {
    return 1 << Popcount(calcBishopAttacks(0, sq) & ~EDGES);
  }

  // Compute n.o. 16-bit entries per square for rook attacks.
  static constexpr int calcRookAttacksSize(int sq)
  {
    return 1 << Popcount(calcRookAttacks(0, sq) &
			 ((BB(sq) & EDGES) ? ~CORNERS : ~EDGES));
  }

  static constexpr int bishopTableSize = [](){
    int sum = 0;
    for (int sq = 0; sq < 64; sq++)
      sum += calcBishopAttacksSize(sq);
    return sum;
  }();

  static constexpr int rookTableSize = [](){
    int sum = 0;
    for (int sq = 0; sq < 64; sq++)
      sum += calcRookAttacksSize(sq);
    return sum;
  }();

  static uint16_t bishopTable[bishopTableSize];
  static uint16_t rookTable[rookTableSize];

  // PEXT/PDEP bitboards.
  struct Magic {
    /* occMask forms the relevant occupance bitmask, which
       is passed to PEXT.  */
    Bitboard occMask;
    /* mask itself is the mask to be used with PDEP of the
       looked-up 16-bit word.  */
    Bitboard mask;
    // Pointer to the lookup table.
    const uint16_t *table;
  };

  static Magic bishopMagics[64];
  static Magic rookMagics[64];

  static Bitboard kingTable[64];
  static Bitboard knightTable[64];

  static Bitboard pawnAttackersUTable[64];
  static Bitboard pawnAttackersDTable[64];

  static Bitboard pawnFrontSpanUTable[64];
  static Bitboard pawnFrontSpanDTable[64];

  static Bitboard inBetweenTable[64][64];

  void InitBitboards()
  {
    int offset = 0;
    for (int sq = 0; sq < 64; sq++) {
      bishopMagics[sq].mask = calcBishopAttacks(0, sq);
      bishopMagics[sq].table = bishopTable + offset;
      Bitboard relevant_occ = bishopMagics[sq].mask & ~EDGES;
      bishopMagics[sq].occMask = relevant_occ;
      uint16_t count = 1U << Popcount(relevant_occ);
      for (uint16_t i = 0; i < count; i++) {
	Bitboard occ = PDEP(i, relevant_occ);
	Bitboard attacks = calcBishopAttacks(occ, sq);
	bishopTable[offset + i] = PEXT(attacks, bishopMagics[sq].mask);
      }
      offset += count;
    }

    offset = 0;
    for (int sq = 0; sq < 64; sq++) {
      rookMagics[sq].mask = calcRookAttacks(0, sq);
      rookMagics[sq].table = rookTable + offset;
      Bitboard relevant_occ = rookMagics[sq].mask;
      if (BB(sq) & EDGES)
	relevant_occ &= ~CORNERS;
      else
	relevant_occ &= ~EDGES;
      rookMagics[sq].occMask = relevant_occ;
      uint16_t count = 1U << Popcount(relevant_occ);
      for (uint16_t i = 0; i < count; i++) {
	Bitboard occ = PDEP(i, relevant_occ);
	Bitboard attacks = calcRookAttacks(occ, sq);
	rookTable[offset + i] = PEXT(attacks, rookMagics[sq].mask);
      }
      offset += count;
    }

    for (int sq = 0; sq < 64; sq++) {
      kingTable[sq] = calcKingAttacks(sq);
      knightTable[sq] = calcKnightAttacks(sq);

      pawnAttackersUTable[sq] = PawnAttacksD(~UINT64_C(0), sq);
      pawnAttackersDTable[sq] = PawnAttacksU(~UINT64_C(0), sq);

      pawnFrontSpanUTable[sq] = calcPawnFrontSpanU(sq);
      pawnFrontSpanDTable[sq] = calcPawnFrontSpanD(sq);

      for (int sq2 = 0; sq2 < 64; sq2++)
	inBetweenTable[sq][sq2] = calcInBetween(sq, sq2);
    }
  }

  Bitboard BishopAttacks(Bitboard occ, int sq)
  {
    Magic magic = bishopMagics[sq];
    int index = PEXT(occ, magic.occMask);
    return PDEP(magic.table[index], magic.mask);
  }

  Bitboard RookAttacks(Bitboard occ, int sq)
  {
    Magic magic = rookMagics[sq];
    int index = PEXT(occ, magic.occMask);
    return PDEP(magic.table[index], magic.mask);
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

  Bitboard PawnFrontSpanU(int sq)
  {
    return pawnFrontSpanUTable[sq];
  }

  Bitboard PawnFrontSpanD(int sq)
  {
    return pawnFrontSpanDTable[sq];
  }

  Bitboard InBetween(int sq1, int sq2)
  {
    return inBetweenTable[sq1][sq2];
  }

  void TestBitboards()
  {
    for (int sq = 0; sq < 64; sq++) {
      Bitboard mask = rookMagics[sq].mask & ~EDGES;
      uint16_t count = Popcount(mask);
      for (uint16_t i = 0; i < count; i++) {
	Bitboard occ = PDEP(i, mask);
	Bitboard attacks = RookAttacks(occ, sq);
	Bitboard attacksSlow = calcRookAttacks(occ, sq);
	if (attacks != attacksSlow) {
	  std::cerr << "TestBitboards: incorrect RookAttacks. sq=" << sq << " i=" << i << " RookAttacks=" << std::fixed << std::setprecision(16) << std::hex << attacks << "\n";
	  throw std::runtime_error("TestBitboards: incorrect RookAttacks");
	}
      }
    }
    for (int sq = 0; sq < 64; sq++) {
      Bitboard mask = bishopMagics[sq].mask & ~EDGES;
      uint16_t count = Popcount(mask);
      for (uint16_t i = 0; i < count; i++) {
	Bitboard occ = PDEP(i, mask);
	Bitboard attacks = BishopAttacks(occ, sq);
	Bitboard attacksSlow = calcBishopAttacks(occ, sq);
	if (attacks != attacksSlow) {
	  std::cerr << "TestBitboards: incorrect BishopAttacks. sq=" << sq << " i=" << i << " BishopAttacks=" << std::fixed << std::setprecision(16) << std::hex << attacks << "\n";
	  throw std::runtime_error("TestBitboards: incorrect BishopAttacks");
	}
      }
    }
  }
} // namespace DSchack
