#ifndef MY_UTILS
#define MY_UTILS

#include <string>
#include <vector>

namespace utils {

std::vector<std::string> getInput(int argc, char *argv[]);
std::vector<std::string> splitString(const std::string &s, const char sep);

}; // namespace utils

#endif