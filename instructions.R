#step 1: log in to Tardis

#step 2: download the repository: "git clone git@github.com:dnlbrkc/tardis.git"


#step 3: generate jester dataset

library(rprojroot)
path <- function(x) rprojroot::find_root_file(x, criterion = has_file("social-recommendation.Rproj"))
setwd(path("/jesterDatasets/"))
source(path("/jesterDatasets/get_data.R"))

generate_data() #this will download and save the jester dataset using a random seed

#step 4:

setwd(path("simulationCode"))

#step 5: run simulation job for 1000 repetitions



#step 6: once the job is ready, aggregate individual simulation runs using the following code:

files <- list.files(pattern="Rdata")
baseline <- array(0,dim=c(14000,15,8))

for(f in files){
  load(f)
  baseline <- baseline + memoryStrategies
}
baseline <- baseline / length(files)

save(baseline,file="baseline.Rdata")

