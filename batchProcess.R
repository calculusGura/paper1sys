#setwd("C:/Users/buzzs/Dropbox/TE2019"); ##windows
#setwd("/home/ubuntu/test");  ##aws ssh
#setwd("/Users/Jeong/Dropbox/TE2019"); ##osx



##initialize environment
setwd("/home/buzzsongs/paper1sys"); ##the working directory for chrome OS
rm(list = ls()); ##initialize memory 
#source("./setEnvirionment.R"); ##install required packages
source("./dataClasses.R"); ##define data classes for modeling


##import data from CSV
csvData1 <- read.csv("./systemData/compDPstructure.csv");
csvData2 <- read.csv("./systemData/constDP.csv");


##generate system model from the csv data
source("./generateSystemModel.R");
systemData <- generateEntityDPModel(csvData1);
systemData <- generateConstDPModel(csvData2, systemData);


##visualize the network of a system information
source("./networkView.R");
showSystemNetwork(systemData);

ggplotly(showSystemNetwork(systemData))
##explore the paths of change propagation
source("./explorePropagationPath.R");
path <- explorePropagationPath(c("D1"), systemData);

##evaluate path 
source("./evaluatePath.R");
path <- evaluatePathRisk(path, systemData)



