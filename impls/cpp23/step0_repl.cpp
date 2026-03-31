#include "ReadLine.h"

#include <optional>
#include <print>
#include <string>

namespace mal {

std::string READ(std::string str) { return str; }


std::string EVAL(std::string str) { return str; }


std::string PRINT(std::string str) { return str; }


std::string rep(const std::string& str) { return PRINT(EVAL(READ(str))); }

} // namespace mal

int main() {
  static mal::ReadLine rl("~/.mal_history");
  std::string line;
  while (auto line = rl.get("user> "))
    std::print("{}\n", mal::rep(line.value()));
}
