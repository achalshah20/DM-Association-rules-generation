# Author: Achal Shah
# Date: 03/27/2016
# Task: Implement the Apriori algorithm

# Importing functions
source("func/utility_functions.R")
source("func/itemset_generation_funcs.R")
source("func/rules_generation_funcs.R")
source("func/execution_funcs.R")

# Data manipulation libraries
library(gdata) # For regex matching
library(dplyr) #Data frame manipulation 

# Output result directory
outputDir = "results/"

# Dataset 1
carData = read.delim2("data/car.data.txt",sep = ",",col.names = c("buying","maint","doors","persons","lug_boot","safety","class"),header = F)

# Support 0.05,0.1,0.15
# Confidence 0.7,0.8,0.9
# Lift 1,1.1,0.9
# method type
# 1. fk-1 x fk1
# 2. fk-1 x fk-1

# Pruning allowed
# False = Bruteforce
# True = Confidence based pruning

find.itemsets.and.rules(dataset = carData,methodType = 1,dataset.name = "carData",IsPruningAllowed = F,support = c(0.05,0.1,0.15),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = carData,methodType = 1,dataset.name = "carData",IsPruningAllowed = T,support = c(0.05,0.1,0.15),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = carData,methodType = 2,dataset.name = "carData",IsPruningAllowed = F,support = c(0.05,0.1,0.15),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = carData,methodType = 2,dataset.name = "carData",IsPruningAllowed = T,support = c(0.05,0.1,0.15),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))

# Dataset 2
cmcData = read.delim2("data/cmc.data.txt",sep = ",",header = F,stringsAsFactors = F)
cmcData = as.data.frame(apply(cmcData, 2,as.factor))
cmcData = as.data.frame(apply(cmcData, 2, function(x) if(length(levels(as.factor(x))) > 4)
                                                {
                                                        as.numeric(as.numeric(x) > median(as.numeric(x)))
                                                }else{x}))
cmcData = as.data.frame(apply(cmcData, 2,as.factor))
names(cmcData) = c("wife.age","wife.edu","husband.edu","children","wife.religion","wife.working","husband.occu","stand.living","media.expo","contraceptive.method")
# Support 0.05,0.1,0.15
# Confidence 0.7,0.8,0.9
# Lift 1,1.1,0.9
# method type
# 1. fk-1 x fk1
# 2. fk-1 x fk-1

# Pruning allowed
# False = Bruteforce
# True = Confidence based pruning

find.itemsets.and.rules(dataset = cmcData,methodType = 1,dataset.name = "cmcData",IsPruningAllowed = F,support = c(0.15,0.20,0.25),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = cmcData,methodType = 1,dataset.name = "cmcData",IsPruningAllowed = T,support = c(0.15,0.20,0.25),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = cmcData,methodType = 2,dataset.name = "cmcData",IsPruningAllowed = F,support = c(0.15,0.20,0.25),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = cmcData,methodType = 2,dataset.name = "cmcData",IsPruningAllowed = T,support =c(0.15,0.20,0.25),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))


# Dataset 3
nursary.data = read.delim2("data/nursery.data.txt",sep = ",",header = F,
                           col.names = c("parents","has_nurs","form","children","housing","finance","social","health","class"))

# Support 0.05,0.1,0.15
# Confidence 0.7,0.8,0.9
# Lift 1,1.1,0.9
# method type
# 1. fk-1 x fk1
# 2. fk-1 x fk-1

# Pruning allowed
# False = Bruteforce
# True = Confidence based pruning

find.itemsets.and.rules(dataset = nursary.data,methodType = 1,dataset.name = "nursaryData",IsPruningAllowed = F,support = c(0.05,0.1,0.15),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = nursary.data,methodType = 1,dataset.name = "nursaryData",IsPruningAllowed = T,support = c(0.05,0.1,0.15),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = nursary.data,methodType = 2,dataset.name = "nursaryData",IsPruningAllowed = F,support = c(0.05,0.1,0.15),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))
find.itemsets.and.rules(dataset = nursary.data,methodType = 2,dataset.name = "nursaryData",IsPruningAllowed = T,support = c(0.05,0.1,0.15),confidence = c(0.7,0.8,0.9),lift = c(1.1,1.25,1.4))

Sys.time()
