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

options(warn=-1)
learningCurve <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75) # the examined positions on the learning curve
agents <- 14000 # the total number of agents.
numberOfStrategies <- 8 # the number of examined strategies.
numberOfGroups <- 56 # the number of communities
agentsPerGroup <- agents/numberOfGroups # (each has this number of individuals)
itemNeighbors <- 5 # the number of neigboring items used by the similar-options strategy.
minusOne <- agentsPerGroup - 1 #the number of agents when we exclude the person under consideration
testSetSize <- 25 # the number of items in the test set that will be used to create pair comparisons.
trainingSetSize <- 75 # the maximum number of items in the training set (last value in the learning curve)

# Data structures
memoryStrategies <- array(dim = c(agents,15,numberOfStrategies)) # an array for saving the main results
memoryStrategies[is.na(memoryStrategies)] <- 0 # replace NA's with 0.

##############################
#####FOR CLUSTER COMPUTING####
##############################
##############################
# Repetition id and seed number
q <- as.integer(commandArgs(TRUE)[1])
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
                 # prepare the data of the target agent
                 consideredAgent <- trainingData[o,]  #Select the agent to be tested.
                 consideredAgentExperience <- consideredAgent[1:p] # Consider items that the agent has experienced.
                 predictMe <- testData[o,] # The items that will be used to evaluate teh strategies.

                 #prepare the test data from the rest of the community.
                 communityMembersTest <- testData[-o,]   #Define the test values of the community members
                 communityTestMissing <- communityMembersTest #insert missing values to the test data.
                 missingValuesTest <- sample(minusOne*testSetSize,minusOne*testSetSize/2)
                 communityTestMissing[missingValuesTest] <- NA #insert missing values to the test data.
                 communityTestCorrected <- communityTestMissing
                 communityTestCorrected[is.na(communityTestCorrected)] <- 0 # replace NA values with 0s.

                 #prepare the training data from the rest of the community
                 communityMembersTraining <- trainingData[-o,] #Define the training values of all the community members.
                 communityTrainingMissing <- communityMembersTraining
                 communityTrainingMissing[sample(minusOne*trainingSetSize,minusOne*trainingSetSize/2)] <- NA
                 onIndex <- shuffledIndex[(k-1)*agentsPerGroup + o] # the index of the agent.

                 # this calculates the Pearson correlations, and the ordering of the agents accordingly.
                 similarityOrder <- calculateCorrelationsNAs(consideredAgentExperience,communityTrainingMissing,p)[[2]] # order all the possible peers according to the similarity to the target
                 similarityValues <-  calculateCorrelationsNAs(consideredAgentExperience,communityTrainingMissing,p)[[1]]# a vector with the similarity values as captured by the pearson correlations
                 similarityOptions <- cor(communityTestMissing,communityTrainingMissing,use="pairwise.complete.obs") # calculate the similarity between different options
                  # The doppelganger strategy.
                 doppelganger <- findDoppelgangerSparse(communityTestMissing,similarityValues,testSetSize)
                 scoreDoppelganger <- correctPredictions(predictMe,doppelganger)

                 # The whole crowd strategy .

                 wholeCrowd <- colMeans(communityMembersTest) # aggregate all opinions unconditionally.
                 scoreWholeCrowd <- correctPredictions(predictMe,wholeCrowd)

                  #  The clique strategy.

                 clique <- findCliqueSparse(communityTestMissing,similarityValues,10,testSetSize)
                 scoreClique <- correctPredictions(predictMe,clique)

                  # Random agent strategy.
                 randomAgent <- communityMembersTest[sample(minusOne,1),] # sample a random other individual.
                 scoreRandomOther <- correctPredictions(predictMe,randomAgent)


                # The similarity weighted crowd strategy.
                 similarityWeightedCrowd <- weightedCrowdSparse(communityTestMissing,similarityValues)
                 scoreSimilarityWeightedCrowd <- correctPredictions(predictMe,similarityWeightedCrowd)

                  # similar crowd strategy.
                 similarCrowdIndex <- similarityValues # all the similarities between people in the community and the target
                 similarCrowdIndex[similarCrowdIndex >= 0] <- 1 #change all positive similarities to 1.
                 similarCrowdIndex[similarCrowdIndex < 0] <- 0  #change all negative similarities to 0.
                 similarCrowdIndex[similarCrowdIndex == 0] <- NA
                 similarCrowdMissing <-  communityTestMissing*as.vector(similarCrowdIndex) #multiply with the actual opinions in the community.
                 similarCrowd <- colMeans(similarCrowdMissing,na.rm = TRUE) # aggregate the columns.
                 scoreSimilarCrowd <- correctPredictions(predictMe,similarCrowd)

                 # A strategy that is based on the length of the jokes.

                 lengthModel <- lm(consideredAgentExperience ~ lengthTraining[1:p])
                 jokeLengthModel <- lengthModel[[1]][[1]] + lengthModel[[1]][[2]]*lengthTest
                 scoreJokeLength <- correctPredictions(predictMe,jokeLengthModel)

                 # This strategy looks for the most similar items that the decision maker has evaluated so far and aggregates over them. This is similar to the logic of exemplar models in cognitive science.
                 similarOptions <- findSimilarOptions(similarityOptions,itemNeighbors,p,testSetSize)
                 scoreSimilarOptions <- correctPredictions(predictMe,similarOptions)

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






