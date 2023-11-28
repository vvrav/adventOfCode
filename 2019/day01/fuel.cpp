#include <iostream>
#include <ostream>
#include <vector>

#include "../include/utils.hpp"

int getFuel(int mass) { return mass / 3 - 2; }

int totalFuel1(std::vector<int> &masses) {
  int total = 0;
  for (int mass : masses) {
    total += getFuel(mass);
  }
  return total;
}

int totalFuel2(std::vector<int> &masses) {
  int total = 0;
  for (int mass : masses) {
    int last = getFuel(mass);
    while (last > 0) {
      total += last;
      last = getFuel(last);
    }
  }
  return total;
}

int main(int argc, char *argv[]) {
  auto input = utils::getInput(argc, argv);

  std::vector<int> masses;
  for (std::string line : input) {
    masses.push_back(std::stoi(line));
  }

  int total1 = totalFuel1(masses);
  std::cout << "STEP 1: " << total1 << std::endl;

  int total2 = totalFuel2(masses);
  std::cout << "STEP 2: " << total2 << std::endl;

  return EXIT_SUCCESS;
}