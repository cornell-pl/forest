Zach DeVito

The main parts of the code are found in these files:

main.sml -- Contains the function main' which takes an IR and uses it to analyize a given file. Also includes example runs in the function main.

interp.sml -- All the interpreting code
constraints.sml -- Runs constraint analysis including table generation and finding functional deps
functionaldep.sml -- TANE algorithm implementation
table.sml -- Table creation algorithms

options.sml -- List of options for debugging and report print out. Sometimes its difficult to find errors in an IR that prevent it from parsing data. Enable the interpreter debugging in this file to get a more detailed report of the process to see where it fails. 

runctest - bash script to test the system on random data
apachetest - bash script to test using apache file

Further information is in the comments in each file.  To compile the code sources-new.cm is used for newer versions of SML and sources.cm is used for older ones.