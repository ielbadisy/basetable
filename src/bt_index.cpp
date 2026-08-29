// Memory mapping + RFC 4180 row indexing + field splitting.

#include "bt_common.h"

#ifdef _WIN32
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#  include <io.h>       // _close
#else
#  include <sys/mman.h>
#  include <sys/stat.h>
#  include <fcntl.h>
#  include <unistd.h>
#endif

#include <cstdio>
#include <thread>
#include <algorithm>

namespace bt {

MappedFile::~MappedFile() {
#ifdef _WIN32
  if (data && !owns_buffer) UnmapViewOfFile((LPCVOID) data);
  if (handle) CloseHandle((HANDLE) handle);
  if (fd >= 0) _close(fd);
#else
  if (data && !owns_buffer && size) munmap((void*) data, size);
  if (fd >= 0) ::close(fd);
#endif
  if (owns_buffer && data) std::free((void*) data);
}

std::unique_ptr<MappedFile> MappedFile::open(const std::string& path, std::string& err) {
  auto mf = std::unique_ptr<MappedFile>(new MappedFile());

#ifdef _WIN32
  HANDLE h = CreateFileA(path.c_str(), GENERIC_READ, FILE_SHARE_READ, nullptr,
                         OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, nullptr);
  if (h == INVALID_HANDLE_VALUE) { err = "cannot open file"; return mf; }
  LARGE_INTEGER sz;
  if (!GetFileSizeEx(h, &sz)) { CloseHandle(h); err = "cannot stat file"; return mf; }
  mf->size = (size_t) sz.QuadPart;
  if (mf->size == 0) { CloseHandle(h); mf->data = ""; return mf; }
  HANDLE map = CreateFileMappingA(h, nullptr, PAGE_READONLY, 0, 0, nullptr);
  if (!map) { CloseHandle(h); err = "CreateFileMapping failed"; return mf; }
  const char* view = (const char*) MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
  if (!view) { CloseHandle(map); CloseHandle(h); err = "MapViewOfFile failed"; return mf; }
  mf->data = view;
  mf->handle = map;
  CloseHandle(h);
  return mf;
#else
  int fd = ::open(path.c_str(), O_RDONLY);
  if (fd < 0) { err = "cannot open file"; return mf; }
  struct stat st;
  if (fstat(fd, &st) != 0) { ::close(fd); err = "cannot stat file"; return mf; }
  mf->fd = fd;
  mf->size = (size_t) st.st_size;
  if (mf->size == 0) { mf->data = ""; return mf; }

  void* p = mmap(nullptr, mf->size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (p == MAP_FAILED) {
    // fall back to a plain read into a heap buffer
    char* buf = (char*) std::malloc(mf->size);
    if (!buf) { err = "out of memory"; return mf; }
    size_t got = 0;
    while (got < mf->size) {
      ssize_t r = ::read(fd, buf + got, mf->size - got);
      if (r <= 0) break;
      got += (size_t) r;
    }
    if (got != mf->size) { std::free(buf); err = "short read"; return mf; }
    mf->data = buf;
    mf->owns_buffer = true;
    return mf;
  }
#ifdef MADV_SEQUENTIAL
  madvise(p, mf->size, MADV_SEQUENTIAL);
#endif
  mf->data = (const char*) p;
  return mf;
#endif
}

// ---------------------------------------------------------------------------

static inline const char* skip_raw_lines(const char* p, const char* end, int64_t n) {
  while (n > 0 && p < end) {
    const char* nl = static_cast<const char*>(memchr(p, '\n', end - p));
    if (!nl) return end;
    p = nl + 1;
    --n;
  }
  return p;
}

RowIndex build_row_index(const char* data, size_t size, const Options& opt) {
  RowIndex idx;
  const char* p = data;
  const char* end = data + size;

  // UTF-8 BOM
  if (size >= 3 && (unsigned char) p[0] == 0xEF &&
      (unsigned char) p[1] == 0xBB && (unsigned char) p[2] == 0xBF) {
    p += 3;
  }

  if (opt.skip > 0) p = skip_raw_lines(p, end, opt.skip);

  auto is_comment_line = [&](const char* q) {
    return opt.comment != '\0' && q < end && *q == opt.comment;
  };

  // header line
  if (opt.has_header) {
    while (p < end && is_comment_line(p)) {
      const char* nl = static_cast<const char*>(memchr(p, '\n', end - p));
      p = nl ? nl + 1 : end;
    }
    idx.header_off = static_cast<size_t>(p - data);
    idx.has_header_line = true;
    // advance past the (possibly quoted) header line
    bool inq = false;
    while (p < end) {
      char c = *p++;
      if (c == opt.quote) {
        if (inq && p < end && *p == opt.quote) { ++p; }
        else inq = !inq;
      } else if (c == '\n' && !inq) {
        break;
      }
    }
  }

  idx.starts.reserve(1024);
  int64_t rows = 0;
  const int64_t limit = opt.n_max;

  while (p < end) {
    if (is_comment_line(p)) {
      const char* nl = static_cast<const char*>(memchr(p, '\n', end - p));
      p = nl ? nl + 1 : end;
      continue;
    }
    if (limit >= 0 && rows >= limit) break;

    idx.starts.push_back(static_cast<size_t>(p - data));
    ++rows;

    bool inq = false;
    while (p < end) {
      char c = *p++;
      if (c == opt.quote) {
        idx.any_quote = true;
        if (inq && p < end && *p == opt.quote) { ++p; }      // "" escape
        else inq = !inq;
      } else if (c == '\n' && !inq) {
        break;
      }
    }
  }

  // trailing sentinel: end of the final row (strip a final blank line)
  size_t last = static_cast<size_t>(p - data);
  idx.starts.push_back(last);

  // drop a trailing empty record produced by a final newline
  if (idx.starts.size() >= 2) {
    size_t n = idx.starts.size();
    size_t a = idx.starts[n - 2], b = idx.starts[n - 1];
    bool empty = (b - a == 0) ||
                 (b - a == 1 && data[a] == '\n') ||
                 (b - a == 2 && data[a] == '\r' && data[a + 1] == '\n');
    if (empty) { idx.starts[n - 2] = idx.starts[n - 1]; idx.starts.pop_back(); }
  }
  return idx;
}

// ---------------------------------------------------------------------------
// multi-threaded newline scan for the quote-free, comment-free common case
// ---------------------------------------------------------------------------

RowIndex build_row_index_mt(const char* data, size_t size, const Options& opt,
                            int n_threads) {
  if (opt.comment != '\0' || n_threads < 2 || size < (1u << 20))
    return build_row_index(data, size, opt);

  const char* base = data;
  const char* end = data + size;
  const char* p0 = base;
  if (size >= 3 && (unsigned char) p0[0] == 0xEF &&
      (unsigned char) p0[1] == 0xBB && (unsigned char) p0[2] == 0xBF)
    p0 += 3;
  if (opt.skip > 0) p0 = skip_raw_lines(p0, end, opt.skip);
  if (p0 >= end) return build_row_index(data, size, opt);

  const size_t span = static_cast<size_t>(end - p0);
  int nth = std::min<int>(n_threads, static_cast<int>(span / (1u << 18)));
  if (nth < 2) return build_row_index(data, size, opt);

  std::vector<std::vector<size_t>> parts(nth);
  std::vector<char> saw_quote(nth, 0);
  std::vector<std::thread> pool;
  const size_t chunk = (span + nth - 1) / nth;

  for (int t = 0; t < nth; ++t) {
    size_t a = t * chunk;
    size_t b = std::min(span, a + chunk);
    if (a >= b) break;
    pool.emplace_back([&, t, a, b] {
      const char* s = p0 + a;
      const char* e = p0 + b;
      if (std::memchr(s, opt.quote, e - s)) { saw_quote[t] = 1; return; }
      auto& out = parts[t];
      out.reserve((b - a) / 16 + 4);
      const char* q = s;
      while (q < e) {
        const char* nl = static_cast<const char*>(std::memchr(q, '\n', e - q));
        if (!nl) break;
        out.push_back(static_cast<size_t>(nl + 1 - base)); // start of next row
        q = nl + 1;
      }
    });
  }
  for (auto& th : pool) th.join();
  for (char q : saw_quote)
    if (q) return build_row_index(data, size, opt);

  // merge: candidate row starts = p0, then the byte after every newline
  std::vector<size_t> all;
  size_t total = 1;
  for (auto& v : parts) total += v.size();
  all.reserve(total + 1);
  all.push_back(static_cast<size_t>(p0 - base));
  for (auto& v : parts) all.insert(all.end(), v.begin(), v.end());

  // ensure a sentinel == size unless the file already ended on a newline
  if (all.back() < size) all.push_back(size);

  RowIndex idx;
  size_t first_data = 0;
  if (opt.has_header) {
    idx.header_off = all[0];
    idx.has_header_line = true;
    first_data = 1;
  }

  // number of data rows available (entries are starts + one sentinel)
  int64_t navail = static_cast<int64_t>(all.size()) - 1 - static_cast<int64_t>(first_data);
  if (navail < 0) navail = 0;
  int64_t nkeep = navail;
  if (opt.n_max >= 0 && opt.n_max < nkeep) nkeep = opt.n_max;

  idx.starts.assign(all.begin() + first_data,
                    all.begin() + first_data + nkeep + 1); // +1 sentinel

  // strip a trailing blank line (only relevant when we kept through EOF)
  size_t n = idx.starts.size();
  if (nkeep == navail && n >= 2) {
    size_t a = idx.starts[n - 2], b = idx.starts[n - 1];
    bool empty = (b - a == 0) ||
                 (b - a == 1 && data[a] == '\n') ||
                 (b - a == 2 && data[a] == '\r' && data[a + 1] == '\n');
    if (empty) { idx.starts[n - 2] = idx.starts[n - 1]; idx.starts.pop_back(); }
  }
  return idx;
}

// ---------------------------------------------------------------------------

static inline void trim(const char*& p, size_t& n) {
  while (n && (*p == ' ' || *p == '\t')) { ++p; --n; }
  while (n && (p[n - 1] == ' ' || p[n - 1] == '\t')) { --n; }
}

size_t split_row(const char* row, size_t len, const Options& opt,
                 std::vector<std::pair<const char*, size_t>>& fields,
                 std::string& scratch) {
  fields.clear();
  scratch.clear();
  // strip line terminator
  while (len && (row[len - 1] == '\n' || row[len - 1] == '\r')) --len;

  const char delim = opt.delim;
  const char quote = opt.quote;
  size_t i = 0;
  // We stage every field's [begin,end) either as a direct view into `row`
  // or, when unquoting was needed, as an offset range into `scratch`.
  struct Stage { bool in_scratch; size_t a, b; };
  static thread_local std::vector<Stage> stage;
  stage.clear();

  while (i <= len) {
    const char* fstart = row + i;
    if (i < len && row[i] == quote) {
      // quoted field
      size_t sbeg = scratch.size();
      ++i;
      while (i < len) {
        char c = row[i];
        if (c == quote) {
          if (i + 1 < len && row[i + 1] == quote) { scratch.push_back(quote); i += 2; }
          else { ++i; break; }
        } else {
          scratch.push_back(c);
          ++i;
        }
      }
      stage.push_back({ true, sbeg, scratch.size() });
      // consume up to next delimiter
      while (i < len && row[i] != delim) ++i;
      if (i < len && row[i] == delim) { ++i; if (i == len) stage.push_back({false, len, len}); }
      else break;
    } else {
      size_t j = i;
      while (j < len && row[j] != delim) ++j;
      stage.push_back({ false, i, j });
      if (j < len) { i = j + 1; if (i == len) { stage.push_back({false, len, len}); break; } }
      else { i = j; break; }
    }
  }

  fields.reserve(stage.size());
  for (auto& s : stage) {
    const char* p; size_t n;
    if (s.in_scratch) { p = scratch.data() + s.a; n = s.b - s.a; }
    else              { p = row + s.a; n = s.b - s.a; }
    if (opt.trim_ws && !s.in_scratch) trim(p, n);
    fields.push_back({ p, n });
  }
  return fields.size();
}

} // namespace bt
