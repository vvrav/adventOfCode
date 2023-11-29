#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <map>
#include <ostream>
#include <string>
#include <utility>
#include <vector>

#include "../include/utils.hpp"

std::map<std::pair<int, int>, int> readLine(const std::string &line) {
  const std::vector<std::string> instructions = utils::splitString(line, ',');
  std::map<std::pair<int, int>, int> result;
  int x = 0, y = 0, steps = 0;

  for (const std::string instruction : instructions) {
    char dir = instruction[0];
    int dist = std::stod(instruction.substr(1));
    switch (dir) {
    case 'U':
      for (int i = 0; i < dist; i++)
        result.insert({{x, ++y}, ++steps});
      break;
    case 'D':
      for (int i = 0; i < dist; i++)
        result.insert({{x, --y}, ++steps});
      break;
    case 'L':
      for (int i = 0; i < dist; i++)
        result.insert({{--x, y}, ++steps});
      break;
    case 'R':
      for (int i = 0; i < dist; i++)
        result.insert({{++x, y}, ++steps});
      break;
    default:
      std::cerr << "ERROR: wrong direction: " << dir << std::endl;
      exit(EXIT_FAILURE);
    }
  }
  return result;
}

int manhattanToOrig(const std::pair<int, int> &point) {
  return abs(point.first) + abs(point.second);
}

int main(int argc, char *argv[]) {
  std::vector<std::string> input = utils::getInput(argc, argv);

  std::map<std::pair<int, int>, int> map1 = readLine(input[0]);
  std::map<std::pair<int, int>, int> map2 = readLine(input[1]);

  std::vector<std::pair<std::pair<int, int>, int>> inter;
  std::set_intersection(
      map1.begin(), map1.end(), map2.begin(), map2.end(),
      std::back_inserter(inter),
      [](const auto &lhs, const auto &rhs) { return lhs.first < rhs.first; });

  std::cout << "Found " << inter.size() << " intersection(s)" << std::endl;

  if (inter.size() > 0) {
    int minDist = manhattanToOrig(inter[0].first);
    int minSteps = map1[inter[0].first] + map2[inter[0].first];
    for (const auto point : inter) {
      std::cout << "(" << point.first.first << "," << point.first.second
                << "), ";
      int d = manhattanToOrig(point.first);
      if (d < minDist)
        minDist = d;
      int steps = map1[point.first] + map2[point.first];
      if (steps < minSteps)
        minSteps = steps;
    }
    std::cout << std::endl
              << "Manhattan distance of intersection closest to origin: "
              << minDist << std::endl
              << "Minimum steps to reach intersection: " << minSteps
              << std::endl;

  } else {
    std::cout << "Found no intersection!" << std::endl;
  }

  return EXIT_SUCCESS;
}