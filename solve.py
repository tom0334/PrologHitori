#!/usr/bin/env python3
import sys
import os
import subprocess

#This python script reads a puzzle file,
#and then tasks the PROLOG solver with finding a solution.

#It will do timing for us, using the PROLOG built in timer.
#Time only includes solving time!
#So the time spent starting up Python, PROLOG and reading the file is NOT counted.

#the time and inferences will be printed in the solution file as a comment, in the format of #prologTime = X and #prologInferences = X


#Example usage of this solve script:
# python3 solve.py puzzles/unbiased/10x10/ff253783-eb3d-4026-8fae-777c87ef078a.singles

# Example Usage: TESTER: 
# python3 tester.py puzzles/unbiased/10x10/ solve.py testoutput.out -v "prologTime" -v "prologInferences"

#SETTINGS:
VERBOSE = 1 #Set this to zero to disable prints during testing!
SOLVER_FILE = "solver.pl"


if(len(sys.argv) < 2):
	print("ERROR: no puzzle passed. USAGE: ./solve.py PATH_TO_PUZZLE") 
	sys.exit(1)

path = sys.argv[1]
if not os.path.isfile(path):
    print("ERROR: puzzle file not found. Does it exist?")
    sys.exit(1)

file = open(path, 'r')
allLines = file.readlines()

if( len (allLines) == 0 ):
    print("ERROR: puzzle file is empty!") 
    sys.exit(1)

n = int( allLines [0] )

#file starts with n = x, and then an empty line 
firstPuzzleLine = 2 
lastPuzzleLine = 2 + (n-1)

if(lastPuzzleLine >= len (allLines) ):
    print("ERROR: puzzle does not contain enough lines!") 
    sys.exit(1)

puzzleLines = allLines[firstPuzzleLine : (lastPuzzleLine +1)]
rows = map(lambda line: line.rstrip().split(" "), puzzleLines)
elements = map (lambda line: map(int, line), rows)

#Python list.map returns a map iterable, instead of just a list.
#Convert it back 
matrix = list( map (list, elements))

if ( len (matrix) != n ):
    print("ERROR: matrix does not contain enough lines!") 
    sys.exit(1)

for line in matrix:
    if ( len (line) != n ):
        print("ERROR: a line does not contain enough elements!") 
        sys.exit(1)

seedLine = lastPuzzleLine + 2
if( len (allLines) <= seedLine):
    print("ERROR: no seed line found!" + str( seedLine)) 
    sys.exit(1)

#remove the first @ sign
seed = allLines[seedLine][1:] 


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
    f"Seed = \"{seed}\", "
    f"PuzzleID = \"{puzzleID}\", "
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
