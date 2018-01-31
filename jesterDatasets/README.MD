In this folder you can find the files that serve as input for the simulations. 

The **jester-dataset1.csv** and **jester-dataset2.csv** files are the original files provided at the page of the Jester project http://goldberg.berkeley.edu/jester-data/

In this project, we focus only on users that evaluated all the jokes. There where 14.116 people in total who evaluated all the jokes. To simplify the computations, we excluded 116 people and rounded up the number of people once and for all to 14.000. The transformed dataset is saved in the **jesterFull.Rdata** file. 

The **contextualInfo** file provides additional information about the word length of the jokes. 

The **jesterFull.Rdata** file includes the dataset that was used for the simulations. 

The **get_data.R** file includes a script that can be used to obtain the original data from the Jester website and transform it into the format used in the simulations.