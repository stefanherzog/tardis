# Background

This package contains the code used in the simulations of Analytis, Barkoczi and Herzog (2017).

# Usage

## Joke ratings and contextual information

- The empirical dataset of joke ratings can be found in: 'JesterDatasets/jesterFull.Rdata'. The original Jester dataset contained 14116 individuals who rated all the jokes. To simplify the analyses, we excluded at random 116 people and we rounded up the number once and for all to 14000. 

- Information about word length of the jokes used by the joke length strategy can be found in 'jesterDatasets/contextualinfo'


## Simulation files

1. To run the main simulation open the file 'simulationCode/jesterBaseline.R'

2. To run the main simulation with missing values open the file 'simulationCode/jesterMissing.R'

3. Bias-variance analyses can be performed using 'simulationCode/biasVarianceJester.R'

Additional functions used by the simulation code described above can be found in 'simulationCode/jesterAllFunctions.R'

## Result files

All simulation output can be found in the folder 'results'

## Code for creating plots

All plots in the manuscript can be reproduced using the code in: 'figures/allPlots.R'