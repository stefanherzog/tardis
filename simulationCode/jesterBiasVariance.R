# This script includes baseline simulation reported by Analytis, Barkoczi and Herzog in their paper "Social learning strategies for matters of taste" (2017)

#############################
#HOUSEKEEPING AND DATA INPUT#
#############################

rm(list=ls())

library(rprojroot)
library(reshape)
path <- function(x) rprojroot::find_root_file(x, criterion = has_file("social-recommendation.Rproj")) # create a path to the direction that contains social-recommendation.Rproj

load(path("jesterDatasets/jesterFull.Rdata")) # load the data used in the simulation
source(path("simulationCode/jesterAllFunctions.R")) #load all the relevant functions used in the simulation. 

jokeLength <- read.table(path("jesterDatasets/jokeLengthInfo"), header = TRUE) #retrieve information about the length of the jokes. 
jokeLength[,1] <- NULL 
jokeLength <-  jokeLength[,2]

##############################
#####SIMULATION VARIABLES ####
##############################

options(warn=1)
learningCurve <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90) # the examined positions on the learning curve
agents <- 14000 # the total number of agents.
numberOfStrategies <- 8 # the number of examined strategies. 
numberOfGroups <- 56 # the number of communities 
agentsPerGroup <- agents/numberOfGroups # (each has this number of individuals)
itemNeighbors <- 5 # the number of neigboring items used by the similar-options strategy. 
minusOne <- agentsPerGroup - 1 #the number of agents when we exclude the person under consideration
repetitions <- 1
resample <- 20
trainingSampleSize <- 25

# Data structures

 predictionsRandomOther <- array(dim = c(repetitions,resample,300,agents))
 predictionsRandomOther[is.na(predictionsRandomOther)] <- 0
 predictionsSimilarityWeightedCrowd<- array(dim = c(repetitions,resample,300,agents))
 predictionsSimilarityWeightedCrowd[is.na(predictionsSimilarityWeightedCrowd)] <- 0
 predictionsWholeCrowd <- array(dim = c(repetitions,resample,300,agents))
 predictionsWholeCrowd[is.na(predictionsWholeCrowd)] <- 0
 predictionsDoppelganger <- array(dim = c(repetitions,resample,300,agents))
 predictionsDoppelganger[is.na(predictionsDoppelganger)] <- 0
 predictionsSimilarCrowd <- array(dim = c(repetitions,resample,300,agents))
 predictionsSimilarCrowd[is.na(predictionsSimilarCrowd)] <- 0
 predictionsJokeLength <- array(dim = c(repetitions,resample,300,agents))
 predictionsJokeLength[is.na(predictionsJokeLength)] <- 0
 predictionsSimilarOptions <- array(dim = c(repetitions,resample,300,agents))
 predictionsSimilarOptions[is.na(predictionsSimilarOptions)] <- 0
 predictionsClique <- array(dim = c(repetitions,resample,300,agents))
 predictionsClique[is.na(predictionsClique)] <- 0
 testSets <- matrix(c(rep(0,25*repetitions)),nrow = repetitions)
 

##############################
#####FOR CLUSTER COMPUTING####
##############################
##############################
# Repetition id and seed number
 f <- as.integer( commandArgs(TRUE)[1])
 set.seed(f)
 q=1


###############################
#####FOR PARALLEL COMPUTING####
###############################
#library(foreach)
#library(doParallel)
#library(doSNOW)
# repetitions <- 100
# q=0
# n_cores <- 2
# cl<-makeCluster(n_cores)
# registerDoParallel(cl)
# ls<-foreach(q=1:repetitions) %dopar% {


    # Randomizing the process 

     shuffledIndex <- sample(agents,agents) # random index of agents 
     shuffledJester <- bigLaugh[shuffledIndex,] # change the order of 14000 agents in the data
     trainingOptions <- sample(100,75)
     testOptions <- setdiff(seq(1,100),trainingOptions)
     testSets[q,] <- testOptions
     # loop through the groups that are created by equally dividing the dataset.

     for (k in 1:numberOfGroups){

     community <- shuffledJester[(1 + (k-1)*agentsPerGroup):(k*agentsPerGroup),] # pick on community 
     testData <- community[,testOptions] # the test data for the community under consideration.
     lengthTest <- jokeLength[testOptions] # the test data for the joke length strategy
     lengthTest = data.frame(length=c(lengthTest))   


        # finally this loops in all the individual agents.

        for (o in 1:agentsPerGroup){


            predictMe <- testData[o,]  #Select the agent to be tested.
            communityMembersTest <- testData[-o,]  # Consider items that the agent has experienced.
            for (l in 1:resample){

                subSample <-  sample(trainingOptions,trainingSampleSize)
                trainingData <- community[,subSample] # this is potentially error prone.
                lengthTraining <- jokeLength[subSample]
                consideredAgent <- trainingData[o,]
                communityMembersTraining <- trainingData[-o,]
                onIndex <- shuffledIndex[(k-1)*agentsPerGroup + o] # the index of the agent.

            # this calculates the Pearson correlations, and the ordering of the agents accordingly. The results of this function are later used by many of the strategies.

                similarityOrder <- correlationsFullMatrix(trainingData,trainingSampleSize,o)[[2]] # order all the possible peers according to the similarity to the target 
                similarityValues <- correlationsFullMatrix(trainingData ,trainingSampleSize,o)[[1]] # a vector with the similarity values as captured by the pearson correlations
                similarityOptions <- cor(communityMembersTest,communityMembersTraining) # calculate the similarity between different options

                 # The doppelganger strategy.
                doppelganger <- communityMembersTest[similarityOrder[1],] # find the individual with the highest similarity.
                pairedDoppelganger <- predictPairComparisons(doppelganger)

                 # The whole crowd strategy .
                wholeCrowd <- colMeans(communityMembersTest) # aggregate all opinions unconditionally.
                pairedWholeCrowd <- predictPairComparisons(wholeCrowd)

                 #  The clique strategy. 
                clique <- findClique(communityMembersTest,similarityOrder,10) # find the 10 most individuals in the population.
                pairedClique <- predictPairComparisons(clique)


                 # Random agent strategy.
                randomAgent <- communityMembersTest[sample(minusOne,1),] # sample a random other individual.
                pairedRandomOther <- predictPairComparisons(randomAgent)

                 # The weighted average strategy. It uses the Pearson correlations as weights.
                 weightedIndex <- similarityValues # retrieve the weights for sim. weighted crow
                 similarityWeightedCrowdPredictions <- (weightedIndex%*%communityMembersTest) # multiply weights by the ratings of people in the community.
                pairedSimilarityWeightedCrowd <- predictPairComparisons(similarityWeightedCrowdPredictions) # evaluate the performance of the strategy in pair-comparisons.

            # similar crowd strategy. It excludes people with negative taste correlation and gives equal weights to everybody else. 
            similarCrowdIndex <- similarityValues #the correlations for the similar crowd strategy.
            similarCrowdIndex[similarCrowdIndex >= 0] <- 1 #turn positive correlations to equal weights
            similarCrowdIndex[similarCrowdIndex < 0] <- 0 #people with negative correlations are excluded
            similarCrowd <-  similarCrowdIndex%*%communityMembersTest/sum(similarCrowdIndex) # aggregate other peoples opinions 
            pairedSimilarCrowd <- predictPairComparisons(similarCrowd) # evaluate the strategy using pair-comparisons

            # A strategy that is based on the length of the jokes.

            lengthModel <- lm(consideredAgent ~  lengthTraining[1:trainingSampleSize])
            jokeLengthModel <- lengthModel[[1]][[1]] + lengthModel[[1]][[2]]*lengthTest
            pairedJokeLength <- predictPairComparisons(t(jokeLengthModel))

            # This strategy looks for the most similar items that the decision maker has evaluated so far and aggregates over them. This is similar to the logic of exemplar models in cognitive science.

            similarOptions <- findSimilarOptions(similarityOptions,itemNeighbors,trainingSampleSize,25)
            pairedSimilarOptions <- predictPairComparisons(similarOptions)

            # store the data.            
            
            predictionsRandomOther[q,l,,onIndex] <- pairedRandomOther
            predictionsSimilarityWeightedCrowd[q,l,,onIndex] <- pairedSimilarityWeightedCrowd
            predictionsWholeCrowd[q,l,,onIndex] <- pairedWholeCrowd
            predictionsSimilarCrowd[q,l,,onIndex] <- pairedSimilarCrowd
            predictionsClique[q,l,,onIndex] <- pairedClique
            predictionsJokeLength[q,l,,onIndex] <- pairedJokeLength
            predictionsSimilarOptions[q,l,,onIndex] <- pairedSimilarOptions
            predictionsDoppelganger[q,l,,onIndex] <- pairedDoppelganger

        }
    }
    }


    #UNCOMMENT FOR PARALLEL COMPUTING
    #list(performanceStrategies)
    #}
    #stopCluster(cl)
    #memoryStrategies <- ls
    #save(memoryStrategies, file="Results.Rdata")

###################################################
#DECOMPOSE BIAS AND VARIANCE#######################
###################################################
   # the following calculations are based on Kohavi and Wolpert's decomposision method reported in Bias plus Variance Decomposision for Zero-One Loss Functions (1996) ICML.

   smallLaugh <- bigLaugh[1:agents,] # reduce the dataset to the 14.000 used in this study.
   resample <- 20 
   k <- 1
   
   # build containerrs to save the different error components of the simulation. 
   
   totalError <- array(rep(0,repetitions*8*agents),dim = c(agents,8,repetitions)) 
   variance <- array(rep(0,repetitions*8*agents),dim = c(agents,8,repetitions))
   bias2 <- array(rep(0,repetitions*8*agents),dim = c(agents,8,repetitions))
   

   # loop in all the people in the dataset.
     for (i in 1:agents){
       # creating the large matrixes that will be used to calculate the error.  
       evaluations <- bigLaugh[i,testSets[k,]] 
       correctPairComparisons <- correctComparisons(evaluations) # the decomposition is done on the basis of 0-1 loss function. This transforms the evaluation to classifications.
       replicatedWorld <- t(replicate(resample,correctPairComparisons)) # replicate the correct results. This helps with the matrix operations coming up afterwards.
       
       # calculating the overall error. 
       totalError[i,1,k] <- mean(colMeans(abs(replicatedWorld - predictionsRandomOther[k,,,i])))
       totalError[i,2,k] <- mean(colMeans(abs(replicatedWorld - predictionsSimilarityWeightedCrowd[k,,,i])))
       totalError[i,3,k] <- mean(colMeans(abs(replicatedWorld - predictionsSimilarCrowd[k,,,i])))
       totalError[i,4,k] <- mean(colMeans(abs(replicatedWorld - predictionsClique[k,,,i])))
       totalError[i,5,k] <- mean(colMeans(abs(replicatedWorld - predictionsDoppelganger[k,,,i])))
       totalError[i,6,k] <- mean(colMeans(abs(replicatedWorld - predictionsSimilarOptions[k,,,i])))
       totalError[i,7,k] <- mean(colMeans(abs(replicatedWorld - predictionsJokeLength[k,,,i])))
       totalError[i,8,k] <- mean(colMeans(abs(replicatedWorld - predictionsWholeCrowd[k,,,i])))
       
       # calculating the variance component of the error. 
       variance[i,1,k] <- mean(1/2*(1 - (colMeans(predictionsRandomOther[k,,,i])^2 + (1 - colMeans(predictionsRandomOther[k,,,i]))^2)))
       variance[i,2,k] <- mean(1/2*(1 - (colMeans(predictionsSimilarityWeightedCrowd[k,,,i])^2 + (1 - colMeans(predictionsSimilarityWeightedCrowd[k,,,i]))^2)))
       variance[i,3,k] <- mean(1/2*(1 - (colMeans(predictionsSimilarCrowd[k,,,i])^2 + (1 - colMeans(predictionsSimilarCrowd[k,,,i]))^2)))
       variance[i,4,k] <- mean(1/2*(1 - (colMeans(predictionsClique[k,,,i])^2 + (1 - colMeans(predictionsClique[k,,,i]))^2)))
       variance[i,5,k] <- mean(1/2*(1 - (colMeans(predictionsDoppelganger[k,,,i])^2 + (1 - colMeans(predictionsDoppelganger[k,,,i]))^2)))
       variance[i,6,k] <- mean(1/2*(1 - (colMeans(predictionsSimilarOptions[k,,,i])^2 + (1 - colMeans(predictionsSimilarOptions[k,,,i]))^2)))
       variance[i,7,k] <- mean(1/2*(1 - (colMeans(predictionsJokeLength[k,,,i])^2 + (1 - colMeans(predictionsJokeLength[k,,,i]))^2)))
       variance[i,8,k] <- mean(1/2*(1 - (colMeans(predictionsWholeCrowd[k,,,i])^2 + (1 - colMeans(predictionsWholeCrowd[k,,,i]))^2)))
       
       # caluclating the bias component of the error 
       divergenceRandom <- abs(colMeans(replicatedWorld - predictionsRandomOther[k,,,i]))
       divergenceWeightedCrowd <- abs(colMeans(replicatedWorld - predictionsSimilarityWeightedCrowd[k,,,i]))
       divergenceSimilarCrowd <- abs(colMeans(replicatedWorld - predictionsSimilarCrowd[k,,,i]))
       divergenceClique <- abs(colMeans(replicatedWorld - predictionsClique[k,,,i]))
       divergenceDoppelganger <- abs(colMeans(replicatedWorld - predictionsDoppelganger[k,,,i]))
       divergenceSimilarOptions <- abs(colMeans(replicatedWorld - predictionsSimilarOptions[k,,,i]))
       divergenceJokeLength <- abs(colMeans(replicatedWorld - predictionsJokeLength[k,,,i]))
       divergenceWholeCrowd <- abs(colMeans(replicatedWorld - predictionsWholeCrowd[k,,,i]))
       bias2[i,1,k] <-  2*mean(1/2*(divergenceRandom^2))
       bias2[i,2,k] <-  2*mean(1/2*(divergenceWeightedCrowd^2))
       bias2[i,3,k] <-  2*mean(1/2*(divergenceSimilarCrowd^2))
       bias2[i,4,k] <-  2*mean(1/2*(divergenceClique^2))
       bias2[i,5,k] <-  2*mean(1/2*(divergenceDoppelganger^2))
       bias2[i,6,k] <-  2*mean(1/2*(divergenceSimilarOptions^2))
       bias2[i,7,k] <-  2*mean(1/2*(divergenceJokeLength^2))
       bias2[i,8,k] <-  2*mean(1/2*(divergenceWholeCrowd^2))
     }
  
   
   theError <- melt(totalError,c(2,3))
   theVariance <- melt(variance,c(2,3))
   biasVariance <- cbind(theVariance,theError[,4] - theVariance[,4])
   colnames(biasVariance) <- c("participant",
                               "strategy",
                               "repetition",
                               "variance",
                               "biasIrError")
   
 
   
###########################################
#SAVE RESULTS##############################
###########################################
name<-paste0(f,'.Rdata',sep="",collapse=NULL)
save(biasVariance,file=name)






