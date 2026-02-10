#include "ReadLine.h"
#include <iostream>

static ReadLine rl("~/.mal_history");

std::string READ(std::string str) { return str; }


std::string EVAL(std::string str) { return str; }


std::string PRINT(std::string str) { return str; }


std::string rep(std::string str)
{
  return PRINT(EVAL(READ(std::move(str))));
}

int main() {
  std::string line;
  while (rl.get("user> ", line))
    std::cout <<  rep(std::move(line)) << "\n";
}
