#ifndef INCLUDE_READLINE_H
#define INCLUDE_READLINE_H

#include <string>

class ReadLine {
public:
  explicit ReadLine(const std::string &file) noexcept;
  ~ReadLine();

  bool get(const std::string& prompt, std::string& out);

private:
  std::string historyFile;
};

#endif // INCLUDE_READLINE_H
