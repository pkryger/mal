#include "ReadLine.h"
#include <iostream>

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
    std::cout << mal::rep(std::move(line.value())) << "\n";
}
