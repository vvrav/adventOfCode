#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

namespace utils {

std::vector<std::string> getInput(int argc, char *argv[]) {
  std::string fileName;
  if (argc < 2) {
    fileName = "example";
    std::cout << "No argument found" << std::endl
              << "Using default input: '" << fileName << "'" << std::endl;
  } else {
    fileName = argv[1];
  }

  std::ifstream inputFile(fileName, std::ios::in);
  if (!inputFile.is_open()) {
    std::cerr << "Cannot open file '" << fileName << "'" << std::endl;
    exit(EXIT_FAILURE);
  }

  std::vector<std::string> input;
  std::string line;
  while (std::getline(inputFile, line)) {
    input.push_back(line);
  }

  return input;
}

std::vector<std::string> splitString(const std::string &inputString,
                                     const char sep) {
  std::vector<std::string> result;
  std::stringstream stream(inputString);
  std::string item;
  while (getline(stream, item, sep)) {
    result.push_back(item);
  }
  return result;
}

} // namespace utils
