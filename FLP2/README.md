# Documentation

Author: Jan Kalenda
Login: xkalen07
Academical year: 2023/2024
Turing Machine

## Introduction

The program simulates a Turing machine; the states of the machine are represented as dynamic predicates, and the tape is represented as a list of symbols. The entire input is validated before the simulation begins. When the simulation finishes, the output is printed to the console. The Turing Machine is intentionally designed to search in Breadth-first search (further as BFS) fashion, simulating the nondeterministic nature of the required form of the Turing Machine.
This is reached by retracting and asserting the rules when applied, changing the order in which Prolog evaluates the rules. This solution prevents some of the infinite loops that could occur in the case of Depth-first search, as seen in `input2` or `input3`.

The loop of the model of the Turing machine is as follows:

1. The model gets the current state and the current symbol from the tape.
2. The model searches for the rule that matches the current state and symbol.
3. The model applies the rule and changes the state and the symbol on the tape.
4. A new tape is saved, and the model continues in a new state and a new symbol.

## How to run

### Requirements

- SWI-Prolog
- make

### Running the program with examples

To run the program, use the following commands:

```bash
make run
```

This will run the program with the example input files. The outputs will be in `output` `output2` and `output3` files. The first lines represent the rules for the Turing Machine, and the last line represents the tape, which can also be empty, like in `input2`.

### Running the program with custom input

```bash
make
./flp23-log < <input>
```

where `<input>` is a file containing the input for the Turing machine. The examples can be seen in `input` `input2` and `input3` files.

### Cleaning the project

To clean the project, use the following command:

```bash
make clean
```

## Extra features

The program is extended with input validation, checking whether the tape contains only lowercase letters or empty symbols (spaces). The Turing Machine rules are also validated, checking whether the rules are in the correct format. When the validation fails, the program will print an error message and terminate. This extension is **not commented out**, as a wrong input should not be expected.

Additionally, the program is extended with a feature that detects whether any final state is at all present in the rules. The same logic applies to the start state. Both of these validations, when failed, will result in an error message, and the program will terminate. This extension is commented out in the `init/1` predicate.

The BFS search mentioned in the introduction could also be deemed as an extra feature, as whether it is required or not is not clear.

The final extra feature is an extra print statement that happens when no solution could be found. This is also commented out as a predicate in the code, namely `run_sim/0`.

### Exit code mapping

With the extra features, the exit codes are mapped as follows:

0 - The program ran successfully and found a solution
1 - No solution could be found
2 - The input tape is not valid
3 - No rules or tape were found
4 - The rules are not valid
10 - no start state was found
11 - no final state was found

## Examples

Times are measured on the Merlin server.

### Example 1

Command:

```bash
./flp23-log < input > output
```

Input:

```txt
S a B a
B a B b
B b B R
B c B a
B c F c
B c B a
aaacaa
```

Output:

```txt
Saaacaa
Baaacaa
Bbaacaa
bBaacaa
bBbacaa
bbBacaa
bbBbcaa
bbbBcaa
bbbFcaa
```

Time:

```txt
real    0m0.016s
user    0m0.008s
sys     0m0.005s
```

This example is taken from the assignment.

### Example 2

Command:

```bash
./flp23-log < input2 > output2
```

Input:

```txt
S   A R
B b F b
A   B b
A   A R

```

Output:

```txt
S
 A
  A
  Bb
  Fb
```

Time:

```txt
real    0m0.015s
user    0m0.011s
sys     0m0.002s
```

This example shows the case when the tape is empty, and we are also offered a rule that can endlessly shift to the right in a loop.
The empty symbols (spaces) are printed only when they are on the left side of a current state.

### Example 3

Command:

```bash
./flp23-log < input3 > output3
```

Input:

```txt
S a B a
A a A d
A d B R
B d B R
B a A R
B a B L
B b F R
A b B b
aaabaaa
```

Output:

```txt
Saaabaaa
Baaabaaa
aAaabaaa
aAdabaaa
adBabaaa
aBdabaaa
adBabaaa
adaAbaaa
adaBbaaa
adabFaaa
```

Time:

```txt
real    0m0.016s
user    0m0.011s
sys     0m0.003s
```

This example shows the case when the tape is not empty, and the Turing Machine has to search for the final state, avoiding loops.

### Example 4

Command:

```bash
./flp23-log < input4 > output4
```

Input:

```txt
S a B a
A a A d
A d B R
B d B R
B a A R
B a B L
B b F R
A b B b
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab
```

Output:

```txt
...
adadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadbF
```

Time:

```txt
real    0m0.055s
user    0m0.045s
sys     0m0.006s
```

The output here has been shortened for brevity, as the output is 246 lines long. The Turing Machine has to search for the final state, avoiding loops, just like in example 3. This example shows empirically tested time complexity scaling.

## Limitations

One of the limitations is that the program is unable to accept an infinite tape as an input. Another limitation is that there is no extra logic for detecting loops in the rules, which could be useful in some cases.

Another very specific limitation is the search for a final state. When applying a rule, the model always first checks whether a finite state can be reached within the next step. This greedy approach is slightly suboptimal as it adds deterministic behavior to the model. However, with this modification, the final state should always find a solution if there is one present.
