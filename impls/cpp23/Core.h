#ifndef INCLUDE_CORE_H
#define INCLUDE_CORE_H

#include "Types.h"
#include <stdexcept>
#include <string>

void installBuiltIns(MalEnv& env);

class CoreException : public std::runtime_error {
public:
  explicit CoreException(const std::string &str) : std::runtime_error{str} {}
};


#endif // INCLUDE_CORE_H
