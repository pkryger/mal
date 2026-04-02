#include "ReadLine.h"

#include <optional>
#include <print>
#include <string>

namespace {

std::string READ(std::string str) { return str; }


std::string EVAL(std::string str) { return str; }


std::string PRINT(std::string str) { return str; }


std::string rep(const std::string& str) { return PRINT(EVAL(READ(str))); }

} // namespace

int main() {
  static mal::ReadLine readLine("~/.mal_history");
  while (auto line = readLine.get("user> ")) {
    std::print("{}\n", rep(line.value()));
  }
}
