#include "ReadLine.h" // IWYU pragma: associated

#include <readline/readline.h>
#include <readline/history.h>
#include <readline/tilde.h>

#include <cstdlib>
#include <memory>
#include <string>

static auto guardedString(char *str)
{
  return std::unique_ptr<char[], void (*)(void *)>(str, &std::free);
}

ReadLine::ReadLine(const std::string &file) noexcept
    : historyFile{[&]() {
        auto &&expanded = guardedString(tilde_expand(file.c_str()));
        return std::string{expanded.get()};
      }()} {
  using_history();
  stifle_history(100);
  read_history(historyFile.c_str());
}

ReadLine::~ReadLine() {
  write_history(historyFile.c_str());
}

std::optional<std::string> ReadLine::get(const std::string &prompt) const
{
  auto &&line = guardedString(readline(prompt.c_str()));
  if (!line)
    return {};

  add_history(line.get());
  append_history(1, line.get());
  return line.get();
}
