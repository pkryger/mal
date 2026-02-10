#include "ReadLine.h"
#include "Reader.h"
#include "Types.h"

#include <iostream>

static ReadLine rl("~/.mal_history");

MalValuePtr READ(std::string str) { return readStr(std::move(str)); }

MalValuePtr EVAL(MalValuePtr val) {
  assert(val);
  return val;
}

std::string PRINT(MalValuePtr val) {
  assert(val);
  return val->print(true);
}

std::string rep(std::string str) { return PRINT(EVAL(READ(std::move(str)))); }

int main() {
  std::string line;
  while (rl.get("user> ", line)) {
    std::string out;
    try {
      out = rep(std::move(line));
    } catch (ReaderException ex) {
      out = std::string{"ReaderException: "} + ex.what();
    }
    std::cout << out << "\n";
  }
}
