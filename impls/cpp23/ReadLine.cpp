#include "ReadLine.h" // IWYU pragma: associated

#include <readline/readline.h>
#include <readline/history.h>
#include <readline/tilde.h>

#include <cstdlib>
#include <memory>
#include <optional>
#include <string>

namespace {
static auto guardedString(char *str)
{
  return std::unique_ptr<char, void (*)(void *)>(str, &std::free);
}
} // namespace

namespace mal {

namespace detail {

ReadLineHistoryInit::ReadLineHistoryInit(int size) noexcept {
  [[maybe_unused]]
  static const bool initialised = [&]() {
    using_history();
    stifle_history(size);
    return true;
  }();
}

} // namespace detail

static const int ReadLineHistorySize{100};

ReadLine::ReadLine()
    : detail::ReadLineHistoryInit{ReadLineHistorySize},
      lines_{ReadLineHistorySize}, historyFile_{nullptr} {}

ReadLine::ReadLine(const std::string &file)
    : detail::ReadLineHistoryInit{ReadLineHistorySize},
      lines_{ReadLineHistorySize}, historyFile_{std::shared_ptr<const char>{
                                      tilde_expand(file.c_str()), std::free}} {
  read_history(historyFile_.get());
  for (auto line = history_list(); line && *line;
       // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic) - C style API
       ++line) {
    lines_.push((*line)->line);
  }
}

std::optional<std::string> ReadLine::get(const std::string &prompt) {
  if (last_ != id) {
    clear_history();
    for (auto &&line : lines_) {
      add_history(line.c_str());
    }
    last_ = id;
  }

  auto &&line = guardedString(readline(prompt.c_str()));
  if (!line)
    return {};

  lines_.push(line.get());
  add_history(line.get());
  if (historyFile_) {
    append_history(1, line.get());
    write_history(historyFile_.get());
  }

  return line.get();
}

} // namespace mal
