#include "ReadLine.h"

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

ReadLine::ReadLine(const std::string &file)
  : historyFile{[&]() {
    const auto&& expanded = guardedString(tilde_expand(file.c_str()));
    return std::string{expanded.get()};
  }()}
{
  read_history(historyFile.c_str());
}

bool ReadLine::get(const std::string &prompt, std::string &out)
{
  const auto&& line = guardedString(readline(prompt.c_str()));
  if (!line)
    return false;

  add_history(line.get());
  append_history(1, line.get());
  out = line.get();
  return true;
}
