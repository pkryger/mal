#include "ReadLine.h" // IWYU pragma: associated

#include <readline/readline.h>
#include <readline/history.h>
#include <readline/tilde.h>

#include <cstdlib>
#include <memory>
#include <string>

namespace {
static auto guardedString(char *str)
{
  return std::unique_ptr<char[], void (*)(void *)>(str, &std::free);
}
} // namespace

namespace mal {

namespace detail {

ReadLineHistoryInit::ReadLineHistoryInit(std::size_t size) noexcept {
  static bool initialised = [&]() {
    using_history();
    stifle_history(size);
    return true;
  }();
}

} // namespace detail

static const std::size_t ReadLineHistorySize{100};

ReadLine::ReadLine() noexcept
    : detail::ReadLineHistoryInit{ReadLineHistorySize},
      lines{ReadLineHistorySize}, historyFile{nullptr} {}

ReadLine::ReadLine(const std::string &file)
    : detail::ReadLineHistoryInit{ReadLineHistorySize},
      lines{ReadLineHistorySize}, historyFile{std::shared_ptr<const char>{
                                      tilde_expand(file.c_str()), std::free}} {
  read_history(historyFile.get());
  for (auto line = history_list(); line && *line; ++line) {
    lines.push((*line)->line);
  }
}

std::optional<std::string> ReadLine::get(const std::string &prompt) {
  if (last != id) {
    clear_history();
    for (auto &&line : lines) {
      add_history(line.c_str());
    }
    last = id;
  }

  auto &&line = guardedString(readline(prompt.c_str()));
  if (!line)
    return {};

  lines.push(line.get());
  add_history(line.get());
  if (historyFile) {
    append_history(1, line.get());
    write_history(historyFile.get());
  }

  return line.get();
}

} // namespace mal
