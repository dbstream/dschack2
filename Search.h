// SPDX-License-Identifier: GPL-3.0-or-later
/* Search state.

   Copyright (C) 2026 David Bergström  */

#pragma once

#include <atomic>
#include <chrono>

#include "Engine.h"

namespace DSchack {
  class TranspositionTable;

  Move Search(Engine *engine, TranspositionTable *tt);
} // namespace DSchack
