#ifndef INCLUDE_CORE_H
#define INCLUDE_CORE_H

#include "Types.h"
#include <stdexcept>
#include <string>

void checkArgsIs(std::string name, MalValues values, std::size_t expected);

void checkArgsAtLeast(std::string name, MalValues values, std::size_t expected);

void checkArgsBetween(std::string name, MalValues values,
                      std::size_t expectedMin, std::size_t expectedMax);


[[noreturn]]
void throwWrongArgument(std::string name, MalValuePtr val);

void installBuiltIns(MalEnv& env);

class CoreException : public std::runtime_error {
public:
  explicit CoreException(const std::string &str) : std::runtime_error{str} {}
};


#endif // INCLUDE_CORE_H
