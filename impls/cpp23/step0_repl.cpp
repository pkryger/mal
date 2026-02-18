#include "ReadLine.h"

#include <optional>
#include <print>
#include <string>
#include <utility>

namespace mal {
static ReadLine rl("~/.mal_history");

std::string READ(std::string str) { return str; }


std::string EVAL(std::string str) { return str; }


std::string PRINT(std::string str) { return str; }


std::string rep(std::string str) { return PRINT(EVAL(READ(std::move(str)))); }

} // namespace mal

int main() {
  std::string line;
  while (auto line = mal::rl.get("user> "))
    std::print("{}\n", mal::rep(std::move(line.value())));
}
