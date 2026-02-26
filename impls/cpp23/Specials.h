#ifndef INCLUDE_SPECIALS_H
#define INCLUDE_SPECIALS_H

#include "Types.h"

#include <string_view>

namespace mal {

using SpecialForm = InvocableResult (&)(std::string_view, ValuesSpan, EnvPtr,
                                        EvalFn);

InvocableResult specialDefBang(std::string_view name, ValuesSpan values,
                               EnvPtr env, EvalFn evalFn);

InvocableResult specialLetStar(std::string_view name, ValuesSpan values,
                               EnvPtr env, EvalFn evalFn);

InvocableResult specialIf(std::string_view name, ValuesSpan values, EnvPtr env,
                          EvalFn evalFn);

InvocableResult specialFnStar(std::string_view name, ValuesSpan values,
                              EnvPtr env, EvalFn evalFn);

InvocableResult specialDo(std::string_view name, ValuesSpan values, EnvPtr env,
                          EvalFn evalFn);

InvocableResult specialQuote(std::string_view name, ValuesSpan values,
                             EnvPtr env, EvalFn /* evalFn */);

InvocableResult specialQuasiquote(std::string_view name, ValuesSpan values,
                                  EnvPtr env, EvalFn /* evalFn */);


InvocableResult specialDefmacroBang(std::string_view name, ValuesSpan values,
                                    EnvPtr env, EvalFn evalFn);

} // namespace mal

#endif // INCLUDE_SPECIALS_H
