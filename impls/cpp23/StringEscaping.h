#ifndef INCLUDE_STRINGESCAPING
#define INCLUDE_STRINGESCAPING

#ifndef MAL_HAVE_NEON
#if defined(__aarch64__) || defined(_M_ARM64)
#define MAL_HAVE_NEON
#endif // defined(__aarch64__) || defined(_M_ARM64)
#endif // MAL_HAVE_NEON

#include <string>
#include <string_view>

namespace mal {

std::string unescapeString(std::string_view input);
std::string escapeString(std::string_view input);

} // namespace mal

#endif // INCLUDE_STRINGESCAPING
