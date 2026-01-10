#!/usr/bin/env python3
import sys
import os
import subprocess

#This python script reads a puzzle file,
#and then tasks the PROLOG solver with finding a solution.

#IT WAS CREATED WITH SOME HELP BY CHATGPT.

#It will do timing for us, using the PROLOG built in timer.
#Time only includes solving time!
#So the time spent starting up Python, PROLOG and reading the file is NOT counted.

#the time and inferences will be printed in the solution file as a comment, in the format of #prologTime = X and #prologInferences = X


#Example usage of this solve script:
# python3 solve.py puzzles/unbiased/10x10/ff253783-eb3d-4026-8fae-777c87ef078a.singles

# Example Usage: TESTER: 
# python3 tester.py puzzles/unbiased/10x10/ solve.py testoutput.out -v "prologTime" -v "prologInferences"

#SETTINGS:
VERBOSE = 0 #Set this to zero to disable prints during testing!
SOLVER_FILE = "solver.pl"



#BEGIN SCRIPT:
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

seed = lines[1 + n][1:]
puzzleFileNameWithExtension =  os.path.basename(path)
puzzleID = os.path.splitext(puzzleFileNameWithExtension)[0]


if(VERBOSE):
    print(f"\n\nRead {n}X{n} puzzle: {puzzleID}")

outputDir = os.path.dirname(path)
outputFileName = puzzleID + ".singlessol"
outputFile = os.path.join(outputDir, outputFileName)

goal = (
    f"Board = {matrix}, "
    f"N = {n}, "
    f"Verbose = {VERBOSE},"
    f"Seed = \"{puzzleID}\", "
    f"OutputFile = \"{outputFile}\","
    f"isSolutionAndWrite(Board,N,Seed, OutputFile, Verbose, Solution)."
)

cmd = [
    "swipl",
    "-q",  #this hides the "Welcome to Swi prolog" text",
    "-O", #Enables optimization. Free performance!
    "-f", SOLVER_FILE,
    "-g", goal, #find this goal
    "-t", "halt" #stop after finding the first solution
]

subprocess.run(cmd, check=True)
