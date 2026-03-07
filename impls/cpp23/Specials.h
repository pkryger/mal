#ifndef INCLUDE_SPECIALS_H
#define INCLUDE_SPECIALS_H

#include "Mal.h"

#include <string_view>

namespace mal {

using SpecialForm = InvocableResult (&)(std::string_view, ValuesSpan, EnvPtr);

InvocableResult specialDefBang(std::string_view name, ValuesSpan values,
                               EnvPtr env);

InvocableResult specialLetStar(std::string_view name, ValuesSpan values,
                               EnvPtr env);

InvocableResult specialIf(std::string_view name, ValuesSpan values, EnvPtr env);

InvocableResult specialFnStar(std::string_view name, ValuesSpan values,
                              EnvPtr env);

InvocableResult specialDo(std::string_view name, ValuesSpan values, EnvPtr env);

InvocableResult specialQuote(std::string_view name, ValuesSpan values,
                             EnvPtr env);

InvocableResult specialQuasiquote(std::string_view name, ValuesSpan values,
                                  EnvPtr env);

InvocableResult specialDefmacroBang(std::string_view name, ValuesSpan values,
                                    EnvPtr env);

InvocableResult specialTryStar(std::string_view name, ValuesSpan values,
                               EnvPtr env);


} // namespace mal

#endif // INCLUDE_SPECIALS_H
