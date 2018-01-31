rm(list=ls())
library(rprojroot)
path <- function(x)
  rprojroot::find_root_file(x, criterion = has_file("social-recommendation.Rproj"))

load(path("results/missing.Rdata"))


#select the strategies to be compared

similarityWeightedCrowd <- missing[,,5]
similarityWeightedCrowd[is.na(similarityWeightedCrowd)] <- 0

wholeCrowd <- missing[,,2]
wholeCrowd[is.na(wholeCrowd)] <- 0

similarCrowd <- missing[,,6]
similarCrowd[is.na(similarCrowd)] <- 0

clique <- missing[,,3]
clique[is.na(clique)] <- 0

#define experience levels 

experience <- seq(5,90,5)



#Calculate for each individual whether strategy X outperforms strategy Y (overall or after the crossing point). 
We focus on the following comparisons:

#1.SimilarityWeightedCrowd vs wholeCrowd
similarityWeightedBetter <- sapply(1:14000, function(x) ifelse(similarityWeightedCrowd[x,18]>wholeCrowd[x,18],TRUE,FALSE)) 

#2. clique vs wholeCrowd
cliqueBetter <- sapply(1:14000, function(x) ifelse( clique[x,18]>wholeCrowd[x,18],TRUE,FALSE))

#3. clique vs similarCrowd
cliqueBetter2 <- sapply(1:14000, function(x) ifelse( clique[x,18]>similarCrowd[x,18],TRUE,FALSE))

#4. similarityWeightedCrowd vs similarCrowd
similarityWeightedBetter2 <- sapply(1:14000, function(x) ifelse(similarityWeightedCrowd[x,18]>similarCrowd[x,18],TRUE,FALSE))



#Calculate for each individual the level of experience at which strategy X outperforms strategy Y.If one strategy dominates the other over the whole range of experience, then the value is NA.

similarityWeightedNcross <- vector()
cliqueNcross <- vector()
similarityWeightedNcross2 <- vector()
cliqueNcross2 <- vector()

for(i in 1:14000){

  #similarity weighted vs whole crowd
  if(any(similarityWeightedCrowd[i,]>wholeCrowd[i,])){
    a <- similarityWeightedCrowd[i,]>wholeCrowd[i,]
    similarityWeightedNcross[i] <- experience[which(a==TRUE)[1]]
  } else{
    similarityWeightedNcross[i] <- NA
  }


  #clique vs whole crowd
  if(any(clique[i,]>wholeCrowd[i,])){
    a <- clique[i,]>wholeCrowd[i,]
    cliqueNcross[i] <- experience[which(a==TRUE)[1]]
  } else{
    cliqueNcross[i] <- NA
  }

  
  #similarity weighted vs whole crowd
  if(any(similarityWeightedCrowd[i,]>similarCrowd[i,])){
    a <- similarityWeightedCrowd[i,]>similarCrowd[i,]
    similarityWeightedNcross2[i] <- experience[which(a==TRUE)[1]]
  } else{
    similarityWeightedNcross2[i] <- NA
  }
  
  
  #clique vs whole crowd
  if(any(clique[i,]>similarCrowd[i,])){
    a <- clique[i,]>similarCrowd[i,]
    cliqueNcross2 [i] <- experience[which(a==TRUE)[1]]
  } else{
    cliqueNcross2 [i] <- NA
  }
  
  
  

}

id <- 1:14000
crossingPoint <- cbind(id,
                       similarityWeightedBetter,
                       cliqueBetter,
                       cliqueBetter2,
                       similarityWeightedBetter2,
                       similarityWeightedNcross,
                       similarityWeightedNcross2,
                       cliqueNcross,
                       cliqueNcross2)

save(crossingPoint, file="crossingPointMissing.Rdata")
