// SPDX-License-Identifier: GPL-3.0-or-later
/* Engine threads.

   Copyright (C) 2026 David Bergström  */

#include <stdexcept>

#include "Threads.h"

namespace DSchack {
  EngineThread::EngineThread()
    : m_thread([this](){
      this->run();
    })
  {}

  EngineThread::~EngineThread()
  {
    waitForIdle();
    lock();
    m_shouldExit = true;
    unlock();
    notify();
    m_thread.join();
  }

  void EngineThread::lock()
  {
    m_mutex.lock();
  }

  void EngineThread::unlock()
  {
    m_mutex.unlock();
  }

  void EngineThread::notify()
  {
    m_condvar.notify_all();
  }

  void EngineThread::waitOnCondvar()
  {
    std::unique_lock<std::mutex> guard(m_mutex, std::adopt_lock);
    m_condvar.wait(guard);
    guard.release();
  }

  void EngineThread::run()
  {
    // Engine thread main loop.

    lock();
    while (!m_shouldExit) {
      // m_running signals someone submitted a job to us.

      if (m_running) {
	std::function<void()> job(std::move(m_job));
	m_job = nullptr;
	unlock();

	// run the job with the mutex released.
	job();
	job = nullptr;

	lock();

	// signal job completion and notify any waiters in waitForIdle.
	m_running = false;
	notify();
      } else
	// if m_running is not set, go into idle.
	waitOnCondvar();
    }
  }

  void EngineThread::start(std::function<void()> &&job)
  {
    lock();
    if (m_running) {
      unlock();
      throw std::runtime_error("This EngineThread is already running a job.");
    }

    m_job = std::move(job);
    m_running = true;
    unlock();
    notify();
  }

  void EngineThread::waitForIdle()
  {
    lock();
    while (m_running)
      waitOnCondvar();
    unlock();
  }
} // namespace DSchack
