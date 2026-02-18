#ifndef INCLUDE_READLINE_H
#define INCLUDE_READLINE_H

#include <optional>
#include <string>

class ReadLine {
public:
  explicit ReadLine(const std::string &file) noexcept;
  ~ReadLine();

  std::optional<std::string> get(const std::string &prompt) const;

private:
  std::string historyFile;
};


#endif // INCLUDE_READLINE_H
