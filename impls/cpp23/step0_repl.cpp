#include "ReadLine.h"

#include <optional>
#include <print>
#include <string>

namespace mal {
static ReadLine rl("~/.mal_history");

std::string READ(std::string str) { return str; }


std::string EVAL(std::string str) { return str; }


std::string PRINT(std::string str) { return str; }


std::string rep(const std::string& str) { return PRINT(EVAL(READ(str))); }

} // namespace mal

int main() {
  std::string line;
  while (auto line = mal::rl.get("user> "))
    std::print("{}\n", mal::rep(line.value()));
}
