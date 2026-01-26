# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright (C) 2026  David Bergström
cmake_minimum_required(VERSION 4.0.0)

project(DSchack LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(DSchack)

target_compile_options(DSchack PUBLIC -march=native)

target_sources(DSchack PRIVATE
  "Bitboard.cc"
  "Engine.cc"
  "Evaluate.cc"
  "Main.cc"
  "MoveGen.cc"
  "Position.cc"
  "Search.cc"
  "Transposition.cc"
)
