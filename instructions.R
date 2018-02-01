#step 1: log in to Tardis

#step 2: download the repository:

#git clone git@github.com:dnlbrkc/tardis.git"

#install packages: install.packages("rprojroot")

#step 3: generate jester dataset

library(rprojroot)
path <- function(x) rprojroot::find_root_file(x, criterion = has_file("social-recommendation.Rproj"))
setwd(path("/jesterDatasets/"))
source(path("/jesterDatasets/get_data.R"))

generate_data() #this will download and save the jester dataset using a random seed

#step 4:

setwd(path("simulationCode"))

#step 5: run simulation job for 1000 repetitions

#echo ‘export OMP_NUM_THREADS=1; Rscript jesterBaseline.R $PBS_ARRAYID’ | qsub -d. -t 1-1000 -N baseline

#step 6: once the job is ready, aggregate individual simulation runs using the following code:

files <- list.files(pattern="Rdata")
baseline <- array(0,dim=c(14000,15,8))

for(f in files){
  load(f)
  baseline <- baseline + memoryStrategies
}
baseline <- baseline / length(files)

save(baseline,file="baseline.Rdata")


#step 7: push results to the repo
# git add *
# git commit -m "."
# git push
