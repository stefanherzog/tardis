generate_data <- function(x){

#Code for obtaining the Jester dataset from Ken Goldberg's website (http://goldberg.berkeley.edu/jester-data/)
library(readxl)

#####################################################
#Download data from website and save it into a matrix
#####################################################

urls <- c("http://goldberg.berkeley.edu/jester-data/jester-data-1.zip",
          "http://goldberg.berkeley.edu/jester-data/jester-data-2.zip",
          "http://goldberg.berkeley.edu/jester-data/jester-data-3.zip")

data <- list()
for(i in 1:length(urls)){

temp <- tempfile()
temp2 <- tempfile()

download.file(urls[i],temp)
unzip(zipfile = temp, exdir = temp2)

name <- paste0("jester-data-",i,".xls")
data[[i]] <- read_excel(file.path(temp2, name), col_names = FALSE)
unlink(c(temp, temp2))


}

################################################################
#Select participants that rated all 100 jokes,
#randomly pick 14000 participants and combine them into a matrix
################################################################

ids <- sapply(1:length(urls), function(x)  which(data[[x]][,1]==100))

mat <- rbind(as.matrix(data[[1]][ids[[1]],2:length(data[[1]])]),
             as.matrix(data[[2]][ids[[2]],2:length(data[[2]])]))

set.seed(12)
select <- sample(1:nrow(mat),14000) #randomly select 14000 participants

mat <- mat[select,]

jesterData <- mat

save(jesterData,file="jesterFull.Rdata")

}
