# This script includes functions that are used for the simulations performed by Analytis, Barkoczi and Herzog in their paper "Social learning strategies for matters of taste" (2017)


#  This function caclulates and order the Pearson correlations between the agent and everybody else in a given community and an amount of experience p.

correlationsFullMatrix <- function(trainingData,p,o,type){
        consideredAgent <- trainingData[o,]  #select the considered agent.
        agentSpace <- trainingData[-o,]  #the other agents used to predict one’s own taste
        if (type == "cardinal"){
        correlations <- array(cor(consideredAgent[1:p],t(agentSpace[,1:p]),use="pairwise.complete.obs"))} # calculate the pearson correlations between the considered agent and everybody else in the community given a number of experiences p.
        else {correlations <- array(cor(consideredAgent[1:p],t(agentSpace[,1:p]),use="pairwise.complete.obs",method = "kendall"))} # calculate the kendall correlations between the considered agent and everybody else in the community given a number of experiences p
        correlations[is.na(correlations)] <- 0 #if degenerate replace with 0.
        correlationsRanks <- order(correlations,decreasing = "TRUE") # rank the correlations
        correlationsAbsoluteDecreasing <- order(abs(correlations),decreasing = "TRUE") # rank the absolute values of the correlations.
        correlationList <- list(correlations,correlationsRanks,correlationsAbsoluteDecreasing)
        return(correlationList)
}



# The same as above but using the Kendall correlation instead.

calculateCorrelationsNAs <- function(consideredAgentExperience,communityTrainingMissing,p){
        communityTrainingMissingExperience <- communityTrainingMissing[,1:p] #reduce the experiences of the community.
        pearsonCorrelations <- array(cor(consideredAgentExperience,t(communityTrainingMissingExperience),use="pairwise.complete.obs")) #calculate the pearson correlations between different people;
        pearsonCorrelations[is.na(pearsonCorrelations)] <- 0
        pearsonCorrelationsRanks <- order(pearsonCorrelations,decreasing = "TRUE")
        pearsonAbsoluteDecreasing <- order(abs(pearsonCorrelations),decreasing = "TRUE")
        correlationList <- list(pearsonCorrelations,pearsonCorrelationsRanks,pearsonAbsoluteDecreasing)
        similarity <-correlationList
        return(similarity)
    }


# This function compares predictions in pair comparisons to the actual truth and calculates the % correct.

correctPredictions <- function(theData,predictions){
    allPairs <- combn(seq(1,length(theData),1),2) # Create all possible pair comparisons
    truth <- theData[allPairs[1,]] - theData[allPairs[2,]] # Check which of the two options has higher utility.
    truth[truth < 0] <- -1 # If the second option is larger than first replace by -1
    truth[truth > 0] <- 1 # If the first larger that the second replace by 1
    truth[which(truth == 0)] <- sample(c(-1,1),length(truth[which(truth == 0)]),replace = TRUE) # assign them at random to one of the above categories.
    forecasts <- as.vector(predictions[allPairs[1,]]) - as.vector(predictions[allPairs[2,]]) # check which of the options has higher forecasted utility
    forecasts[forecasts < 0] <- -1 # If second option is larger than first replace by -1
    forecasts[forecasts > 0] <- 1 # If the first larger that the second replace by 1
    forecasts[which(forecasts == 0)] <- sample(c(-1,1),length(forecasts[which(forecasts == 0)]),replace = TRUE) # resolve possible ties at random.
    correspondenceScore <- sum((truth - forecasts) == 0)/length(truth) # calculate the number of correct predictions.
    return(correspondenceScore)
}

# To conduct the bias-variance analysis for binary values we need to have a vector of choices to pair-comparison tasks. The following function takes in continuous estimates about the utility of different items and transforms them to binary choices.

predictPairComparisons <- function(predictions){
    allPairs <- combn(seq(1,length(predictions),1),2) # create all possible pair-comparisons
    theJudgment2 <- as.vector(predictions[allPairs[1,]]) - as.vector(predictions[allPairs[2,]]) # subtract the value of the paired items.
    index2 <- which(theJudgment2 == 0) # find all the ties
    theJudgment2[index2] <- sample(c(0,1),length(theJudgment2[index2]),replace = TRUE) # and resolve them.
    theJudgment2[theJudgment2 < 0] <- 0 #if the right hand item had higher value set to 0.
    theJudgment2[theJudgment2 > 0] <- 1 #otherwise set to 1.
    return(theJudgment2) # return all the predictions for the pair comparisons
}

correctComparisons<- function(theData){
    numbers <- seq(1,length(theData),1)
    allPairs <- combn(numbers,2)
    theJudgment1 <- theData[allPairs[1,]] - theData[allPairs[2,]]
    index <- which(theJudgment1 == 0)
    theJudgment1[index] <- sample(c(0,1),length(theJudgment1[index]),replace = TRUE)
    theJudgment1[theJudgment1 < 0] <- 0
    theJudgment1[theJudgment1 > 0] <- 1
    return(theJudgment1)
}

removeMissingValues <-  function(traningWorld,howMany){
    removeIt <- sample(length(trainingWorld),howMany)
    trainingWorld[c(removeIt)] <- NA
    return(trainingWorld)
}


# The following function calculates the estimates of the clique strategy in datasets without missing values.

findClique <- function(communityMembersTest,similarityOrder,size){
    clique <- similarityOrder[1:size]  #keep as many agents as the size of the clique.
    review <- communityMembersTest[clique,] #find their reviews.
    smallCrowd <- colMeans(review) # aggregate their opinions.
    return(smallCrowd)
}

findCliqueNA <- function(myWorld,similarity2,size){
    clique <- similarity2[249-size:249]
    review <- myWorld[clique,]
    smallCrowd <- colMeans(review)
    return(smallCrowd)
}
findCliqueSparse <- function(communityTest,similarityValues,cliqueSize,testSetSize){
        cliqueMissing <- communityTest  # a martix with the opinions of all the individuals.
        cliqueMissing[is.na(cliqueMissing) == FALSE] <- 1 # mark all the people who have expressed an opinion as 1s.
        cliqueIndex <- cliqueMissing*as.vector(similarityValues) # mark all the people who have expressed an opinion
        cliqueIndex[is.na(cliqueIndex)] <- 0 # replace NA values with 0
        cliqueIndexMissing <- apply(cliqueIndex,2, function(x) order(x,decreasing=T)) # replace NA values with 0
        cliqueReviews <- matrix(nrow = testSetSize,c(rep(0,cliqueSize*testSetSize))) # create an opinion matrix
        for (r in 1:testSetSize){
                theClique <- cliqueIndexMissing[1:cliqueSize,r]
                cliqueReviews[r,] <- communityTestMissing[theClique,r]} # populate it with values.
        clique <- rowMeans(cliqueReviews) # aggregate the opinons
        return(clique)
}

# the following function finds and assigns weights to the most similar options to the considered ones.

findSimilarOptions <- function(similarityOptions,itemNeighbors,p){
        similarityExperiencedOptions <-  as.matrix(similarityOptions[,1:p])
        similarityOptionsOrdered <- apply(-similarityExperiencedOptions,1,order) #order items according to similarities.
        theWeights <- matrix(0,ncol = testSetSize,nrow = itemNeighbors) # create vectors to store the weight values.
        theUtilities <-  matrix(0,ncol = testSetSize,nrow = itemNeighbors) # create vectors to store the utility values.
        similarityOrder <- similarityOptionsOrdered[1:itemNeighbors,] # keep the number of neighbors that will be used.
        # theUtilities <- pickTheAgent[check]
        for (z in 1:25){
                theWeights[,z] <- similarityExperiencedOptions[z,similarityOrder[,z]]
                theUtilities[,z] <- consideredAgentExperience[similarityOrder[,z]]
        }
        similarOptions <- diag(t(theWeights)%*%theUtilities)/colSums(abs(theWeights)) # calculate the predicted values.
        return(similarOptions)
}


findDoppelgangerSparse <- function(communityTest,similarityValues,testSetSize){
        doppelgangerOpinions <- communityTest # a martix with the opinions of all the individuals.
        doppelgangerOpinions[is.na(doppelgangerOpinions) == FALSE] <- 1 # mark all the people who have expressed an opinion
        doppelgangerIndex <- doppelgangerOpinions*as.vector(similarityValues) # mark the similarity of people who have expressed an opinion with the target individual.
        doppelgangerIndex[is.na(doppelgangerIndex)] <- 0 # replace NA values with 0
        doppelgangerIndexMissing <- apply(doppelgangerIndex,2,order) # rank the agents
        selectedDoppelgangers <- as.vector(doppelgangerIndexMissing[249,]) # for each of the test items locate the agent with the highest correlation who has evaluated it.
        #doppelgangerPredictions <- diag(communityTest[selectedDoppelgangers, seq(1,10,by = 1)]) # adopt their evaluation as your own evaluation.
        doppelgangerPredictions <- diag(communityTest[selectedDoppelgangers, seq(1,testSetSize,by = 1)])
        return(doppelgangerPredictions)
}

weightedCrowdSparse <- function(communityTest,similarityValues){
        weightedScores <- communityTest*as.vector(similarityValues) #multiply scores by similarity weights.
        weightedSparseCorrelations <- weightedScores
        weightedSparseCorrelations[is.na(weightedSparseCorrelations) == FALSE] <- 1 # set all the options/user evaluation combinations to 1.
        similaritySums <- weightedSparseCorrelations*as.vector(similarityValues) # the similarities for all the evaluations.
        similaritySums[is.na(similaritySums)] <- 0 # replace NA values with 0s.
        similaritySums <- colSums(abs(similaritySums))  # add up all the weights in order to divide each item with the product of weights.
        weightedScores[is.na(weightedScores)] <- 0
        reviewSums <- colSums(weightedScores) #sum the different opinions
        weightedCrowdPredictions <- reviewSums/similaritySums #divide the review sums with similarity sum to get the score.
        return(weightedCrowdPredictions)
}



countNAs <-function(vector){sum(is.na(vector))}




