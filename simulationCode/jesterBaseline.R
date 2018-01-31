# This script includes baseline simulation reported by Analytis, Barkoczi and Herzog in their paper "Social learning strategies for matters of taste" (2017)

#############################
#HOUSEKEEPING AND DATA INPUT#
#############################

rm(list=ls())

library(rprojroot)
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
learningCurve <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75) # the examined positions on the learning curve
agents <- 14000 # the total number of agents.
numberOfStrategies <- 8 # the number of examined strategies.
numberOfGroups <- 56 # the number of communities
agentsPerGroup <- agents/numberOfGroups # (each has this number of individuals)
itemNeighbors <- 5 # the number of neigboring items used by the similar-options strategy.
testSetSize = 25
minusOne <- agentsPerGroup - 1 #the number of agents when we exclude the person under consideration
corrType <- "cardinal" #select "cardinal" or "rank"
errorFun <- "pc" #select "pc" for proportion correct, or "rmse" for root mean squared error
# Data structures
memoryStrategies <- array(dim = c(agents,15,numberOfStrategies)) # an array for saving the main results
memoryStrategies[is.na(memoryStrategies)] <- 0 # replace NA's with 0.

##############################
#####FOR CLUSTER COMPUTING####
##############################
##############################
# Repetition id and seed number
q <- as.integer( commandArgs(TRUE)[1])
set.seed(q)

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
     shuffledJester <- bigLaugh[shuffledIndex,] # change the order of 14000 agents in the dataset.

     # Data structures for each single iteration.

     performanceStrategies <- array(dim = c(agents,15,numberOfStrategies)) # An array that is stored in the memoryStrategies array
     performanceStrategies[is.na(performanceStrategies)] <- 0 # change NAs to 0.

     # loop through the groups that are created by equally dividing the dataset.

     for (k in 1:numberOfGroups){

     community <- shuffledJester[(1 + (k-1)*agentsPerGroup):(k*agentsPerGroup),] # pick on community
     trainingOptions<- sample(100,75) # sample the training items
     testOptions <- setdiff(seq(1,100),trainingOptions) # and the test items
     trainingData <- community[,trainingOptions] # the training data for the community under consideration.
     testData <- community[,testOptions] # the test data for the community under consideration.
     lengthTraining <- jokeLength[trainingOptions] # the training data for the joke length strategy
     lengthTest <- jokeLength[testOptions] # the test data for the joke length strategy

    # this loop passes throuugh all the levels of experience.

    for (p in learningCurve){

        # finally this loops in all the individual agents.

        for (o in 1:agentsPerGroup){


            consideredAgent <- trainingData[o,]  #Select the agent to be tested.
            consideredAgentExperience <- consideredAgent[1:p] # Consider items that the agent has experienced.
            predictMe <- testData[o,] # The items that will be used to evaluate teh strategies.
            communityMembersTest <- testData[-o,]   #Define the test values of the community members
            communityMembersTraining <- trainingData[-o,] #Define the training values of all the community members.
            onIndex <- shuffledIndex[(k-1)*agentsPerGroup + o] # the index of the agent.

            # this calculates the Pearson correlations, and the ordering of the agents accordingly. The results of this function are later used by many of the strategies.

            similarityOrder <- correlationsFullMatrix(trainingData,p,o,corrType)[[2]] # order all the possible peers according to the similarity to the target
            similarityValues <- correlationsFullMatrix(trainingData ,p,o,corrType)[[1]] # a vector with the similarity values as captured by the pearson correlations
            similarityOptions <- cor(communityMembersTest,communityMembersTraining) # calculate the similarity between different options

             # The doppelganger strategy.
            doppelganger <- communityMembersTest[similarityOrder[1],] # find the individual with the highest similarity.
            if(errorFun == "pc"){
            scoreDoppelganger <- correctPredictions(predictMe,doppelganger)
            } else {
            scoreDoppelganger <-  sqrt(mean(   (predictMe - doppelganger)^2 ,na.rm=TRUE))
            }

            # The whole crowd strategy .
            wholeCrowd <- colMeans(communityMembersTest) # aggregate all opinions unconditionally.
            if(errorFun == "pc"){
            scoreWholeCrowd <- correctPredictions(predictMe,wholeCrowd)
            } else {
            scoreWholeCrowd <-  sqrt(mean(   (predictMe - wholeCrowd)^2 ,na.rm=TRUE))
            }
            #  The clique strategy.
            clique <- findClique(communityMembersTest,similarityOrder,10) # find the 10 most individuals in the population.
            if(errorFun == "pc"){
            scoreClique <- correctPredictions(predictMe,clique)
            } else {
            scoreClique <-  sqrt(mean(   (predictMe - clique)^2 ,na.rm=TRUE))
            }

            # Random agent strategy.
            randomAgent <- communityMembersTest[sample(minusOne,1),] # sample a random other individual.
            if(errorFun == "pc"){
            scoreRandomOther <- correctPredictions(predictMe,randomAgent)
            } else {
            scoreRandomOther <-  sqrt(mean(   (predictMe - randomAgent)^2 ,na.rm=TRUE))
            }
            # The weighted average strategy. It uses the Pearson correlations as weights.
            weightedIndex <- similarityValues # retrieve the weights for sim. weighted crow
            similarityWeightedCrowdPredictions <- (weightedIndex%*%communityMembersTest) # multiply weights by the ratings of people in the community.



            if(errorFun == "pc"){
            scoreSimilarityWeightedCrowd <- correctPredictions(predictMe,similarityWeightedCrowdPredictions) # evaluate the performance of the strategy in pair-comparisons.
            } else {
                    similaritySums <- sum(abs(similarityValues))
                    weightedCrowdPredictions <- similarityWeightedCrowdPredictions/similaritySums
                    scoreSimilarityWeightedCrowd <-  sqrt(mean(   (predictMe - weightedCrowdPredictions)^2 ,na.rm=TRUE))
            }
            # similar crowd strategy. It excludes people with negative taste correlation and gives equal weights to everybody else.
            similarCrowdIndex <- similarityValues #the correlations for the similar crowd strategy.
            similarCrowdIndex[similarCrowdIndex >= 0] <- 1 #turn positive correlations to equal weights
            similarCrowdIndex[similarCrowdIndex < 0] <- 0 #people with negative correlations are excluded
            similarCrowd <-  similarCrowdIndex%*%communityMembersTest/sum(similarCrowdIndex) # aggregate other peoples opinions
            if(errorFun == "pc"){
            scoreSimilarCrowd <- correctPredictions(predictMe,similarCrowd) # evaluate the strategy using pair-comparisons
            } else {
            scoreSimilarCrowd <-  sqrt(mean(   (predictMe - similarCrowd)^2 ,na.rm=TRUE))
            }
            # A strategy that is based on the length of the jokes.

            lengthModel <- lm(consideredAgentExperience ~ lengthTraining[1:p])
            jokeLengthModel <- lengthModel[[1]][[1]] + lengthModel[[1]][[2]]*lengthTest
            if(errorFun == "pc"){
            scoreJokeLength <- correctPredictions(predictMe,jokeLengthModel)
            } else {
            scoreJokeLength <-  sqrt(mean(   (predictMe - jokeLengthModel)^2 ,na.rm=TRUE))
                }
            # This strategy looks for the most similar items that the decision maker has evaluated so far and aggregates over them. This is similar to the logic of exemplar models in cognitive science.

            similarOptions <- findSimilarOptions(similarityOptions,itemNeighbors,p)
            if(errorFun == "pc"){
            scoreSimilarOptions<- correctPredictions(predictMe,similarOptions)
            } else {
            scoreSimilarOptions <-  sqrt(mean(   (predictMe - similarOptions)^2 ,na.rm=TRUE))

}
            # store the data.

            performanceStrategies[onIndex,p/5,1] <- performanceStrategies[onIndex,p/5,1] + scoreDoppelganger
            performanceStrategies[onIndex,p/5,2] <- performanceStrategies[onIndex,p/5,2] + scoreWholeCrowd
            performanceStrategies[onIndex,p/5,3] <- performanceStrategies[onIndex,p/5,3] + scoreClique
            performanceStrategies[onIndex,p/5,4] <- performanceStrategies[onIndex,p/5,4] + scoreRandomOther
            performanceStrategies[onIndex,p/5,5] <- performanceStrategies[onIndex,p/5,5] + scoreSimilarityWeightedCrowd
            performanceStrategies[onIndex,p/5,6] <- performanceStrategies[onIndex,p/5,6] + scoreSimilarCrowd
            performanceStrategies[onIndex,p/5,7] <- performanceStrategies[onIndex,p/5,7] + scoreJokeLength
            performanceStrategies[onIndex,p/5,8] <- performanceStrategies[onIndex,p/5,8] + scoreSimilarOptions

        }
    }
    }


    #UNCOMMENT FOR PARALLEL COMPUTING
    #list(performanceStrategies)
    #}
    #stopCluster(cl)
    #memoryStrategies <- ls
    #save(memoryStrategies, file="Results.Rdata")

    # The data from the repetitions is added up to the general data structure.
    memoryStrategies <- memoryStrategies + performanceStrategies
    name<-paste0(q,'res','.Rdata',sep="",collapse=NULL)
    save(memoryStrategies, file=name)






