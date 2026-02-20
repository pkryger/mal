#ifndef INCLUDE_CORE_H
#define INCLUDE_CORE_H

#include "Types.h"

#include <cstddef>
#include <stdexcept>
#include <string>

namespace mal {

using EvalFn = ValuePtr (ValuePtr, EnvPtr);

void checkArgsIs(std::string name, ValuesSpan values, std::size_t expected);

void checkArgsAtLeast(std::string name, ValuesSpan values, std::size_t expected);

void checkArgsBetween(std::string name, ValuesSpan values,
                      std::size_t expectedMin, std::size_t expectedMax);


[[noreturn]]
void throwWrongArgument(std::string name, ValuePtr val);

void prepareEnv(EvalFn &evalFn, Env &env);

class CoreException : public std::runtime_error {
public:
  explicit CoreException(const std::string &str) : std::runtime_error{str} {}
};


} // namespace mal

#endif // INCLUDE_CORE_H
