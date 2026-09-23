#ifndef BT_PROFILE_H
#define BT_PROFILE_H
#include <chrono>
#include <cstdlib>
#include <cstring>

// Opt-in diagnostic timing. Call only on R's main thread; never from workers.
struct BtProfile {
  using Clock = std::chrono::steady_clock;
  const char* operation;
  bool enabled;
  Clock::time_point last;
  explicit BtProfile(const char* name) : operation(name) {
    const char* flag = std::getenv("BT_PROFILE");
    enabled = flag && std::strcmp(flag, "1") == 0;
    if (enabled) last = Clock::now();
  }
  void mark(const char* phase) {
    if (!enabled) return;
    auto now = Clock::now();
    double ms = std::chrono::duration<double, std::milli>(now - last).count();
    Rprintf("BT_PROFILE,%s,%s,%.6f\n", operation, phase, ms);
    last = Clock::now();
  }
};
#endif
