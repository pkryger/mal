#ifndef INCLUDE_CORE_H
#define INCLUDE_CORE_H

#include "Types.h"

#include <cstddef>
#include <stdexcept>
#include <string>
#include <string_view>

namespace mal {

void checkArgsIs(std::string_view name, ValuesSpan values, std::size_t expected);

void checkArgsAtLeast(std::string_view name, ValuesSpan values,
                      std::size_t expected);

void checkArgsBetween(std::string_view name, ValuesSpan values,
                      std::size_t expectedMin, std::size_t expectedMax);


[[noreturn]]
void throwWrongArgument(std::string_view name, ValuePtr val);

void prepareEnv(EvalFn &evalFn, Env &env);

class CoreException : public std::runtime_error {
public:
  explicit CoreException(const std::string &str) : std::runtime_error{str} {}
};


} // namespace mal

#endif // INCLUDE_CORE_H
