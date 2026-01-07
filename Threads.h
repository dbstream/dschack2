// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine search threads.

   Copyright (C) 2026 David Bergström

Currently there is only one search thread, but in the future we
might support more.  */

#pragma once

#include "Engine.h"

#include <atomic>
#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>
#include <thread>

namespace DSchack {
  class EngineThread {
    std::mutex m_mutex;
    std::condition_variable m_condvar;

    // state protected by m_mutex and m_condvar:
    std::function<void()> m_job;
    bool m_running = false;
    bool m_shouldExit = false;

    std::thread m_thread;

    void lock();

    void unlock();

    void notify();

    void waitOnCondvar();

    void run();
  public:
    ~EngineThread();

    EngineThread();

    /** start: begin working.
    @job: function to execute.

    The thread may not already be executing a job when this
    function is called.  */
    void start(std::function<void()> &&job);

    /** waitForIdle: wait for this thread to be idle.  */
    void waitForIdle();
  };
} // namespace DSchack
