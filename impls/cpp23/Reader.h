#ifndef INCLUDE_READER_H
#define INCLUDE_READER_H

#include "Types.h"

#include <stdexcept>
#include <string>

namespace mal {

ValuePtr readStr(std::string str);

class ReaderException : public std::runtime_error {
public:
  explicit ReaderException(const std::string &str) : std::runtime_error{str} {}
};

} // namespace mal
#endif //INCLUDE_READER_H
