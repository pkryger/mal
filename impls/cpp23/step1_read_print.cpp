#include "ReadLine.h"
#include "Reader.h"
#include "Types.h"

#include <iostream>

namespace mal {
static ReadLine rl("~/.mal_history");

ValuePtr READ(std::string str) { return readStr(std::move(str)); }

ValuePtr EVAL(ValuePtr val, EnvPtr) {
  assert(val);
  return val;
}

std::string PRINT(ValuePtr val) {
  assert(val);
  return val->print(true);
}

std::string rep(std::string str) {
  return PRINT(EVAL(READ(std::move(str)), nullptr));
}

} // namespace mal

int main() {
  std::string line;
  while (mal::rl.get("user> ", line)) {
    std::string out;
    try {
      out = mal::rep(std::move(line));
    } catch (mal::ReaderException ex) {
      out = std::string{"ReaderException: "} + ex.what();
    }
    std::cout << out << "\n";
  }
}
