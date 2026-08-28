#ifndef BT_COMMON_H
#define BT_COMMON_H

// Shared machinery for the basetable delimited reader/writer.
//
// Design (see bench/ and the package docs):
//   1. map the file with mmap (POSIX) / MapViewOfFile (Windows), no copy
//   2. one RFC 4180 aware scan records the byte offset of every row start
//   3. type guessing samples a bounded number of rows per column
//   4. materialisation converts fields to typed R vectors; numeric columns
//      are filled by a std::thread pool, character columns on the R thread
//      because mkCharLenCE touches R's global string table
//   5. lazy mode (btread lazy = TRUE) hands back ALTREP numeric columns that
//      run step 4 for a single column only when it is first touched

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <cstdlib>
#include <cmath>
#include <string>
#include <vector>
#include <memory>
#include <atomic>

#if defined(__has_include)
#  if __has_include(<charconv>)
#    include <charconv>
#    if defined(__cpp_lib_to_chars)
#      define BT_HAVE_FROM_CHARS_FP 1
#    endif
#  endif
#endif

namespace bt {

// ---------------------------------------------------------------------------
// memory mapped source
// ---------------------------------------------------------------------------

struct MappedFile {
  const char* data = nullptr;
  size_t size = 0;
  void* handle = nullptr;   // platform bookkeeping
  int fd = -1;
  bool owns_buffer = false; // true when we fell back to a heap copy

  MappedFile() = default;
  MappedFile(const MappedFile&) = delete;
  MappedFile& operator=(const MappedFile&) = delete;
  ~MappedFile();

  // returns empty .data on failure; err receives a human message
  static std::unique_ptr<MappedFile> open(const std::string& path, std::string& err);
};

// ---------------------------------------------------------------------------
// parse options
// ---------------------------------------------------------------------------

struct Options {
  char delim = ',';
  char quote = '"';
  char comment = '\0';        // '\0' == no comment char
  bool has_header = true;
  bool trim_ws = false;
  int64_t skip = 0;           // raw lines to drop before anything else
  int64_t n_max = -1;         // -1 == all rows
  int n_threads = 1;
  std::vector<std::string> na_strings{ "NA", "" };

  bool is_na(const char* p, size_t n) const {
    for (const auto& s : na_strings)
      if (s.size() == n && std::memcmp(s.data(), p, n) == 0) return true;
    return false;
  }
};

// ---------------------------------------------------------------------------
// column types (ordered by promotion: a column settles on the widest seen)
// ---------------------------------------------------------------------------

enum ColType : int {
  COL_LOGICAL = 0,
  COL_INTEGER = 1,
  COL_DOUBLE  = 2,
  COL_STRING  = 3,
  COL_SKIP    = 4   // requested via col_select
};

// ---------------------------------------------------------------------------
// low level field parsers. All operate on [p, p+n) with no trailing NUL.
// ---------------------------------------------------------------------------

inline bool parse_bool(const char* p, size_t n, int& out) {
  // "0"/"1" are deliberately NOT accepted here: they should guess as integer.
  if (n == 1) {
    if (*p == 'T' || *p == 't') { out = 1; return true; }
    if (*p == 'F' || *p == 'f') { out = 0; return true; }
    return false;
  }
  if ((n == 4 && (std::memcmp(p, "TRUE", 4) == 0 || std::memcmp(p, "true", 4) == 0)) ||
      (n == 4 &&  std::memcmp(p, "True", 4) == 0)) { out = 1; return true; }
  if ((n == 5 && (std::memcmp(p, "FALSE", 5) == 0 || std::memcmp(p, "false", 5) == 0)) ||
      (n == 5 &&  std::memcmp(p, "False", 5) == 0)) { out = 0; return true; }
  return false;
}

// strict 32-bit integer, no leading/trailing space (caller trims)
inline bool parse_int32(const char* p, size_t n, int& out) {
  if (n == 0) return false;
  bool neg = false;
  size_t i = 0;
  if (p[0] == '-') { neg = true; i = 1; }
  else if (p[0] == '+') { i = 1; }
  if (i == n) return false;
  int64_t v = 0;
  for (; i < n; ++i) {
    char c = p[i];
    if (c < '0' || c > '9') return false;
    v = v * 10 + (c - '0');
    if (v > 2147483648LL) return false;        // past INT_MIN magnitude
  }
  if (neg) v = -v;
  if (v > 2147483647LL || v < -2147483647LL) return false; // keep NA_integer_ free
  out = static_cast<int>(v);
  return true;
}

// exact powers of ten that are representable as double
inline double pow10_exact(int e) {
  static const double tab[] = {
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10,
    1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22
  };
  return tab[e];
}

// double parser: fast path for <= 19 significant digits and |exp| <= 22
// (Clinger's exact case), otherwise fall back to strtod on a small copy.
inline bool parse_double(const char* p, size_t n, double& out) {
  if (n == 0) return false;

#ifdef BT_HAVE_FROM_CHARS_FP
  {
    double v;
    auto res = std::from_chars(p, p + n, v);
    if (res.ec == std::errc() && res.ptr == p + n) { out = v; return true; }
    // fall through for things from_chars rejects but R accepts (Inf/NaN spellings)
  }
#endif

  const char* end = p + n;
  const char* q = p;
  bool neg = false;
  if (q < end && (*q == '-' || *q == '+')) { neg = (*q == '-'); ++q; }

  // Inf / NaN spellings R understands
  auto ci = [](char c){ return c >= 'A' && c <= 'Z' ? c + 32 : c; };
  if (q < end) {
    size_t rem = static_cast<size_t>(end - q);
    if (rem >= 3 && ci(q[0]) == 'i' && ci(q[1]) == 'n' && ci(q[2]) == 'f') {
      out = neg ? -HUGE_VAL : HUGE_VAL; return true;
    }
    if (rem >= 3 && ci(q[0]) == 'n' && ci(q[1]) == 'a' && ci(q[2]) == 'n') {
      out = std::nan(""); return true;
    }
  }

  uint64_t mant = 0;
  int digits = 0;
  int frac_exp = 0;
  bool any = false;
  bool overflow = false;

  for (; q < end && *q >= '0' && *q <= '9'; ++q) {
    any = true;
    if (digits < 19) { mant = mant * 10 + (*q - '0'); ++digits; }
    else { ++frac_exp; overflow = true; }
  }
  if (q < end && *q == '.') {
    ++q;
    for (; q < end && *q >= '0' && *q <= '9'; ++q) {
      any = true;
      if (digits < 19) { mant = mant * 10 + (*q - '0'); ++digits; --frac_exp; }
      else overflow = true;
    }
  }
  if (!any) return false;

  int exp10 = 0;
  if (q < end && (*q == 'e' || *q == 'E')) {
    ++q;
    bool eneg = false;
    if (q < end && (*q == '-' || *q == '+')) { eneg = (*q == '-'); ++q; }
    if (q == end || *q < '0' || *q > '9') return false;
    int e = 0;
    for (; q < end && *q >= '0' && *q <= '9'; ++q) {
      e = e * 10 + (*q - '0');
      if (e > 100000) { e = 100000; }
    }
    exp10 = eneg ? -e : e;
  }
  if (q != end) return false; // trailing junk

  int total_exp = exp10 + frac_exp;
  if (!overflow && total_exp >= -22 && total_exp <= 22 && digits <= 19) {
    double d = static_cast<double>(mant);
    if (total_exp >= 0) d *= pow10_exact(total_exp);
    else               d /= pow10_exact(-total_exp);
    out = neg ? -d : d;
    return true;
  }

  // slow, always-correct fallback
  char buf[512];
  if (n >= sizeof(buf)) return false;
  std::memcpy(buf, p, n);
  buf[n] = '\0';
  char* strend = nullptr;
  double d = std::strtod(buf, &strend);
  if (strend != buf + n) return false;
  out = d;
  return true;
}

// ---------------------------------------------------------------------------
// row index: byte offset of the first character of every data row
// ---------------------------------------------------------------------------

struct RowIndex {
  std::vector<size_t> starts;  // size == nrow + 1, last == end of last row
  size_t header_off = 0;       // byte offset of the header line (if any)
  bool has_header_line = false;
  bool any_quote = false;      // did the data region contain the quote char?
};

// One RFC 4180 aware pass. Honours skip, comment, n_max, quoting with ""
// escapes and embedded newlines. CR is tolerated and folded into the CRLF.
RowIndex build_row_index(const char* data, size_t size, const Options& opt);

// Multi-threaded newline scan. Valid only when the file contains no quote
// character and no comment lines; it verifies the first condition while
// scanning and transparently falls back to build_row_index() otherwise.
RowIndex build_row_index_mt(const char* data, size_t size, const Options& opt,
                            int n_threads);

// Split one row into fields, writing unquoted/untrimmed views into `fields`.
// `scratch` backs any field that needed unquoting. Returns field count.
// Used off the hot path (header parsing, type guessing fallback).
size_t split_row(const char* row, size_t len, const Options& opt,
                 std::vector<std::pair<const char*, size_t>>& fields,
                 std::string& scratch);

// ---------------------------------------------------------------------------
// RowReader: allocation-free field iteration over a single row.
//
// The fast path is a memchr walk to the next delimiter. A row that actually
// contains the quote character falls back to an RFC 4180 unquoting pass whose
// output is staged in `scratch` (kept alive by the caller for the row).
// ---------------------------------------------------------------------------

struct RowReader {
  const char* p;
  const char* end;
  char delim;
  char quote;
  bool trim;
  bool quoted_row;
  std::string* scratch;
  bool exhausted = false;

  RowReader(const char* row, size_t len, const Options& opt, std::string* scr,
            bool file_has_quotes = true)
      : delim(opt.delim), quote(opt.quote), trim(opt.trim_ws), scratch(scr) {
    while (len && (row[len - 1] == '\n' || row[len - 1] == '\r')) --len;
    p = row;
    end = row + len;
    quoted_row = file_has_quotes && std::memchr(row, quote, len) != nullptr;
    if (scratch) scratch->clear();
  }

  // fetch the next field; false once the row is fully consumed
  bool next(const char*& fb, size_t& fn) {
    if (exhausted) return false;

    if (!quoted_row) {
      const char* s = p;
      const char* d = static_cast<const char*>(std::memchr(p, delim, end - p));
      if (d) { fb = s; fn = static_cast<size_t>(d - s); p = d + 1; }
      else   { fb = s; fn = static_cast<size_t>(end - s); p = end; exhausted = true; }
    } else if (p < end && *p == quote) {
      size_t sbeg = scratch->size();
      ++p;
      while (p < end) {
        char c = *p;
        if (c == quote) {
          if (p + 1 < end && p[1] == quote) { scratch->push_back(quote); p += 2; }
          else { ++p; break; }
        } else { scratch->push_back(c); ++p; }
      }
      fb = scratch->data() + sbeg;
      fn = scratch->size() - sbeg;
      while (p < end && *p != delim) ++p;   // tolerate junk after closing quote
      if (p < end) ++p; else exhausted = true;
      return true; // never trim a quoted field
    } else {
      const char* s = p;
      while (p < end && *p != delim) ++p;
      fb = s; fn = static_cast<size_t>(p - s);
      if (p < end) ++p; else exhausted = true;
    }

    if (trim) {
      while (fn && (*fb == ' ' || *fb == '\t')) { ++fb; --fn; }
      while (fn && (fb[fn - 1] == ' ' || fb[fn - 1] == '\t')) --fn;
    }
    return true;
  }
};

// ---------------------------------------------------------------------------
// shared state for lazy (ALTREP) columns
// ---------------------------------------------------------------------------

struct LazySource {
  std::unique_ptr<MappedFile> file;
  RowIndex index;
  Options opt;
  int ncol = 0;
  std::vector<int> types;          // ColType per column
  std::vector<std::string> names;
  int64_t nrow = 0;
};

// Materialise a single column into a freshly allocated R vector (SEXP-less
// here: returns via out pointers). Defined in btread.cpp so it can share the
// conversion code with the eager path.
void materialise_column(const LazySource& src, int col, int type,
                        void* out_ptr, int* n_parse_fail);

// Fill a single character column (out_sexp is a STRSXP of length src.nrow).
// Runs on the R thread; used by the eager path and the string ALTREP.
void materialise_string_column(const LazySource& src, int col, void* out_sexp);

} // namespace bt

#endif // BT_COMMON_H
