#include "ReadLine.h"
#include <iostream>

static ReadLine rl("~/.mal_history");

std::string READ(const std::string &str)
{
  return str;
}

std::string EVAL(const std::string &str)
{
  return str;
}

std::string PRINT(const std::string &str)
{
  return str;
}

std::string rep(const std::string &str)
{
  return PRINT(EVAL(READ(str)));
}

int main() {
  std::string line;
  while (rl.get("user> ", line))
    std::cout <<  rep(line) << "\n";
}
