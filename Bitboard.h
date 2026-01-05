// SPDX-License-Identifier: GPL-3.0-or-later
/* Bitboards.

   Copyright (C) 2026 David Bergström

We use little-endian rank-file mapping, which means that
files A-H and ranks 1-8 are mapped to indices 0-7, and the
following relationship holds between square indices and
file/rank indices:

  square index = 8 * rank index + file index

This means, for example, that A1 < H1 < A8 < H8.  */

#pragma once

#include <bit>
#include <stdint.h>

#ifdef __BMI2__
#include <x86intrin.h>
#endif

namespace DSchack {
  typedef uint64_t Bitboard;

  enum BBSquare {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8
  };

  static constexpr Bitboard BB_A_FILE = UINT64_C(0x0101010101010101);
  static constexpr Bitboard BB_B_FILE = UINT64_C(0x0202020202020202);
  static constexpr Bitboard BB_C_FILE = UINT64_C(0x0404040404040404);
  static constexpr Bitboard BB_D_FILE = UINT64_C(0x0808080808080808);
  static constexpr Bitboard BB_E_FILE = UINT64_C(0x1010101010101010);
  static constexpr Bitboard BB_F_FILE = UINT64_C(0x2020202020202020);
  static constexpr Bitboard BB_G_FILE = UINT64_C(0x4040404040404040);
  static constexpr Bitboard BB_H_FILE = UINT64_C(0x8080808080808080);
  static constexpr Bitboard BB_RANK_1 = UINT64_C(0x00000000000000ff);
  static constexpr Bitboard BB_RANK_2 = UINT64_C(0x000000000000ff00);
  static constexpr Bitboard BB_RANK_3 = UINT64_C(0x0000000000ff0000);
  static constexpr Bitboard BB_RANK_4 = UINT64_C(0x00000000ff000000);
  static constexpr Bitboard BB_RANK_5 = UINT64_C(0x000000ff00000000);
  static constexpr Bitboard BB_RANK_6 = UINT64_C(0x0000ff0000000000);
  static constexpr Bitboard BB_RANK_7 = UINT64_C(0x00ff000000000000);
  static constexpr Bitboard BB_RANK_8 = UINT64_C(0xff00000000000000);
  static constexpr Bitboard BB_LIGHT_SQUARES = UINT64_C(0x55aa55aa55aa55aa);
  static constexpr Bitboard BB_DARK_SQUARES = UINT64_C(0xaa55aa55aa55aa55);

  enum BBDirection {
    BBSouthWest = -9,
    BBSouth = -8,
    BBSouthEast = -7,
    BBWest = -1,
    BBEast = 1,
    BBNorthWest = 7,
    BBNorth = 8,
    BBNorthEast = 9
  };

  /** LShift: leftwards logical bitshift.
  @bits: Bitboard value.
  @count: Number of bits to shift leftwards (may be negative!).  */
  static constexpr Bitboard LShift(Bitboard bits, int count)
  {
    if (count >= 0)
      return bits << count;
    else
      return bits >> -count;
  }

  /** LS1B: get the least significant one-bit of a Bitboard.
  @bits: Bitboard value.
  Returns a Bitboard with the least significant bit of @bits
  set and all other bits clear. If @bits is zero, returns zero.  */
  static constexpr Bitboard LS1B(Bitboard bits)
  {
    return bits & -bits;
  }

  /** LS1BIndex: get the index of the least significant one-bit
      of a bitboard.
  @bits: Bitboard value.
  Returns the index of the LS1B of @bits, or 64 is @bits was zero.  */
  static constexpr int LS1BIndex(Bitboard bits)
  {
    return std::countr_zero(bits);
  }

  /** PopLS1B: clear and return the least significant bit.
  @bits: Bitboard value. NOTE: the least significant bit is cleared.
  Returns the LS1B of @bits, or zero if @bits was zero.  */
  static constexpr Bitboard PopLS1B(Bitboard &bits)
  {
    Bitboard lsb = LS1B(bits);
    bits &= ~lsb;
    return lsb;
  }

  /** Popcount: get the number of one bits in a bitboard.
  @bits: Bitboard value.  */
  static constexpr int Popcount(Bitboard bits)
  {
    return std::popcount(bits);
  }

  /** PDEP: parallel bits deposit.
  @value: value
  @mask: mask  */
  static constexpr Bitboard PDEP(Bitboard value, Bitboard mask)
  {
#ifdef __BMI2__
    return _pdep_u64(value, mask);
#else
    Bitboard result = 0;
    for (Bitboard bit = 1; mask; bit += bit) {
      Bitboard x = PopLS1B(mask);
      if (value & bit)
	result |= x;
    }

    return result;
#endif
  }

  /** PEXT: parallel bits extract.
  @value: value
  @mask: mask  */
  static Bitboard PEXT(Bitboard value, Bitboard mask)
  {
#ifdef __BMI2__
    return _pext_u64(value, mask);
#else
    Bitboard result = 0;
    for (Bitboard bit = 1; mask; bit += bit) {
      Bitboard x = PopLS1B(mask);
      if (value & x)
	result |= bit;
    }

    return result;
#endif
  }

  /** BB: convert an int (BBSquare) to a Bitboard.
  @sq: square index
  Returns a Bitboard with the corresponding bit set.  */
  static constexpr Bitboard BB(int sq)
  {
    return UINT64_C(1) << sq;
  }

  /** Sq: convert a Bitboard to a square index.
  @bit: Bitboard with exactly one bit set.  */
  static constexpr int Sq(Bitboard bit)
  {
    return LS1BIndex(bit);
  }

  /** SqFR: convert a file and a rank to a square index.
  @file: file index (0-7).
  @rank: rank index (0-7).
  Returns the corresponding square index.  */
  static constexpr int SqFR(int file, int rank)
  {
    return 8 * rank + file;
  }

  /** FileOf: convert a square index to a file index.
  @sq: square index.
  Returns the corresponding file index (0-7).  */
  static constexpr int FileOf(int sq)
  {
    return sq & 7;
  }

  /** RankOf: convert a square index to a rank index.
  @sq: square index.
  Returns the corresponding rank index (0-7).  */
  static constexpr int RankOf(int sq)
  {
    return sq >> 3;
  }

  /** BishopAttacks: get the bitboard of bishop target squares.
  @occ: Square occupance bitboard (0=empty, 1=any piece).
  @sq: Bishop source square.
  Returns a bitboard where each set bit indicates a bishop
  could move there.  */
  Bitboard BishopAttacks(Bitboard occ, int sq);

  /** RookAttacks: get the bitboard of rook target squares.
  @occ: Square occupance bitboard (0=empty, 1=any piece).
  @sq: Rook source square.
  Returns a bitboard where each set bit indicates a rook could
  move there.  */
  Bitboard RookAttacks(Bitboard occ, int sq);

  /** KnightAttacks: get the bitboard of knight target squares.
  @sq: Knight source square.
  Returns a bitboard where each set bit indicates a knight
  could move there.  */
  Bitboard KnightAttacks(int sq);

  /** KingAttacks: get the bitboard of king target squares.
  @sq: King source square.
  Returns a bitboard where each set bit indicates a king could
  move there. Castling moves are not included.  */
  Bitboard KingAttacks(int sq);

  /** PawnAttacksU: get the bitboard of pawn target squares,
      where pawns move upwards.
  @occ: Square occupance bitboard (0=empty, 1=any piece).
  @sq: Pawn source square.
  Returns a bitboard where each set bit indicates a pawn
  could move there.

  Note that the en-passant square can be ORed into @occ, with
  the result that en-passant moves are also generated.  */
  Bitboard PawnAttacksU(Bitboard occ, int sq);

  /** PawnAttacksD: get the bitboard of pawn target squares,
      where pawns move downwards.
  @occ: Square occupance bitboard (0=empty, 1=any piece).
  @sq: Pawn source square.
  Returns a bitboard where each set bit indicates a pawn
  could move there.

  Note that the en-passant square can be ORed into @occ, with
  the result that en-passant moves are also generated.  */
  Bitboard PawnAttacksD(Bitboard occ, int sq);

  /** PawnAttackersU: get the bitboard of squares where
      upwards-moving pawns would threaten to capture the
      given square.  */
  Bitboard PawnAttackersU(int sq);

  /** PawnAttackersD: get the bitboard of squares where
      downwards-moving pawns would threaten to capture the
      given square.  */
  Bitboard PawnAttackersD(int sq);

  /** InBetween: get the bitboard representing the line
      between two squares. If this is not a horizontal,
      vertical, or diagonal, only the target square bit
      will be set.
  @sq1: source square. This square is not included in the
        returned bitmask.
  @sq2: target square. This square is included in the
        returned bitmask.  */
  Bitboard InBetween(int sq1, int sq2);

  /** InitBitboards: initialize bitboard tables.  */
  void InitBitboards();

  /** TestBitboards: run bitboard self-tests.  */
  void TestBitboards();
} // namespace DSchack
