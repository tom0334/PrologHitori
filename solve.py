#!/usr/bin/env python3
import sys
import os
import subprocess

#This python script reads a puzzle file,
#and then tasks the PROLOG solver with finding a solution.

#It will do timing for us, using the PROLOG built in timer.
#Time only includes solving time!
#So the time spent starting up Python, PROLOG and reading the file is NOT counted.

SOLVER_FILE = "solver.pl"


if(len(sys.argv) < 2):
	print("ERROR: no puzzle passed. USAGE: ./solve.py PATH_TO_PUZZLE") 
	sys.exit(1)

path = sys.argv[1]
if not os.path.isfile(path):
    print("ERROR: puzzle file not found. Does it exist?")
    sys.exit(1)


with open(sys.argv[1], "r") as f:
    lines = [line.strip() for line in f if line.strip()]

n = int(lines[0])

matrix = []
for i in range(1, 1 + n):
    matrix.append([int(x) for x in lines[i].split()])

puzzleID = lines[1 + n]
print(f"Read {n}X{n} puzzle: {puzzleID}.")
print(f"Starting {SOLVER_FILE}")

goal = (
    f"Board = {matrix}, "
    f"writeln('PROLOG: Solving...'),"
    f"time(isSolution(Board, Solution)), "
    f"writeln(Solution)"
)

cmd = [
    "swipl",
    "-q",  #this hides the "Welcome to Swi prolog" text"
    "-f", SOLVER_FILE,
    "-g", goal, #find this goal
    "-t", "halt" #stop after finding the first solution
]

subprocess.run(cmd, check=True)