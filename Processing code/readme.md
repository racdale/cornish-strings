# Sequence Memory Constraints Give Rise to Linguistic Structure through Iterated Learning

## Experiment code

This directory includes the original [Processing](http://processing.org) code for running the experiment. This is no longer compatible with the current version of Processing, but it should be possible to update it without too much effort. It is given here for those wishing to understand the procedure more closely. 

When the experiment is started, it requests an input file which contains the language to be learned plus some parameters (specifically: the number of strings; the number of passes over the language during training; the pretest, test and posttest delays in milliseconds; a list of legal characters). An example input file is given as 'input.txt'. The software will produce an output file that can be used as input for the next generation. This also includes information on the training phase of the experiment. An example is given as 'output.txt'.

Also included is an example python script that will complete the shuffling of letters between generations as described in the paper.