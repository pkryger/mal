#ifndef INCLUDE_SPECIALS_H
#define INCLUDE_SPECIALS_H

#include "Core.h"
#include "Types.h"

#include <string>

namespace mal {

using SpecialForm = InvocableResult(&)(std::string, ValuesSpan, EnvPtr, EvalFn);

InvocableResult specialDefBang(std::string name, ValuesSpan values, EnvPtr env,
                               EvalFn evalFn);

InvocableResult specialLetStar(std::string name, ValuesSpan values, EnvPtr env,
                               EvalFn evalFn);

InvocableResult specialIf(std::string name, ValuesSpan values, EnvPtr env,
                          EvalFn evalFn);

InvocableResult specialFnStar(std::string name, ValuesSpan values, EnvPtr env,
                              EvalFn evalFn);

InvocableResult specialDo(std::string name, ValuesSpan values, EnvPtr env,
                          EvalFn evalFn);

InvocableResult specialQuote(std::string name, ValuesSpan values, EnvPtr env,
                             EvalFn /* evalFn */);

InvocableResult specialQuasiquote(std::string name, ValuesSpan values,
                                  EnvPtr env, EvalFn /* evalFn */);


InvocableResult specialDefmacroBang(std::string name, ValuesSpan values,
                                    EnvPtr env, EvalFn evalFn);

} // namespace mal

#endif // INCLUDE_SPECIALS_H
