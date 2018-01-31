# Simulations

1. `baseline.Rdata`: results for the baseline simulation assuming no missing values.

2. `missing.Rdata`: results for the baseline simulation assuming 50% missing values.

3. `baseline14000.Rdata`: results assuming a population of 14,000 agents.
 
4. `missing14000.Rdata`: results assuming a population of 14,000 agents and 50% missing values.


Structure for 1-4 is: 1:14000 x 1:18 x 1:8

1. 14,000 agents
2. 18 experience levels (5, 10, 15, ..., 85, 90)
3. 8 strategies
    1. Doppelgänger
    2. WholeCrowd
    3. Clique
    4. Random
    5. SimilarityWeightedCrowd
    6. SimilarCrowd
    7. Joke-length
    8. SimilarOptions


# Bias-variance simulations

`biasvariance.Rdata`: results for the bias-variance simulations.

list of two arrays:

- res[[1]]: 1:14000 x 1:8 x 1 - contains results for overallError^2
- res[[2]]: 1:14000 x 1:8 x 1 - contains results for variance^2


# Crossing point analyses

`crossingPointBaseline.Rdata` & `crossingPointMissing.Rdata` & `crossingPointBaseline14000.Rdata` & `crossingPointMissing14000.Rdata`: results for the crossing point analyses. 

Structure is: 1:14000 x 1:7

1. id
2. similarityWeightedBetter
3. similarityCrowdBetter
4. cliqueBetter
5. similarityWeightedNcross
6. similarityCrowdNcross
7. cliqueNcross

# Sensitivity analysis for size of Clique (k):

`varyK.Rdata`

1. rows corresponsd to different experience levels (5,10,15,...,85,90)

2. columns correspond to value of 'k': (1,5,10,15,20,25,30,35,40,45,50,60,70,80,90,100,130,160,190,220,250) 