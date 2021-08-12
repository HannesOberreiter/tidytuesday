// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector foundC(CharacterVector x, CharacterVector lookup)
{
    std::unordered_set<String> seen(lookup.begin(), lookup.end());
    CharacterVector result;

    CharacterVector::iterator it;
    for (it = x.begin(); it != x.end(); ++it)
    {
        auto search = seen.find(*it);
        if (search != seen.end())
        {
            result.push_back(*it);
        }
    }
    return result;
}
