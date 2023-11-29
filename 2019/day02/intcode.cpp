#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "../include/utils.hpp"

std::vector<int> parse(const std::string &s) {
  std::vector<int> result;
  std::stringstream ss(s);
  std::string item;
  while (getline(ss, item, ',')) {
    result.push_back(std::stoi(item));
  }
  return result;
}

void printProg(const std::vector<int> &prog) {
  for (int value : prog) {
    std::cout << value << ',';
  }
  std::cout << std::endl;
}

int runProg(std::vector<int> program, int noun, int verb) {
  int current = 0;
  int size = program.size();

  program[1] = noun;
  program[2] = verb;

  while (program[current] != 99) {
    if (current > size - 4) {
      std::cerr << "ERROR: not enough ints in program." << std::endl
                << "  Reading pos: " << current << std::endl
                << "  Program size: " << size << std::endl;
      exit(EXIT_FAILURE);
    }
    switch (program[current]) {
    case 1:
      // should check if the indexes not outside program :O
      program[program[current + 3]] =
          program[program[current + 1]] + program[program[current + 2]];
      break;
    case 2:
      program[program[current + 3]] =
          program[program[current + 1]] * program[program[current + 2]];
      break;
    default:
      std::cerr << "ERROR: unrecognized instruction: " << program[current]
                << std::endl;
      exit(EXIT_FAILURE);
    }
    current += 4;
  }

  return program[0];
}

int main(int argc, char *argv[]) {
  std::vector<std::string> input = utils::getInput(argc, argv);

  std::vector<int> program = parse(input[0]);

  // STEP 1
  int res = runProg(program, 12, 2);
  std::cout << "Step1 Program completed" << std::endl
            << "Value left at position 0: " << res << std::endl;

  // STEP 2
  int noun = 0, verb = 0;
  // lets bruteforce that...
  while (runProg(program, noun, verb) != 19690720) {
    noun++;
    if (noun > 99) {
      noun = 0;
      verb++;
    }
  }

  std::cout << "Step2 completed" << std::endl
            << "Pair producing the expected result: " << noun << ", " << verb
            << std::endl
            << ">> answer is " << 100 * noun + verb << std::endl;

  return EXIT_SUCCESS;
}