library(dplyr)

#read features, remove first column, transpose
features <- read.table("features.txt")
features <- select(features, V2)
features <- t(features)

#put the "train" and "test" pieces together, using features as column names and labeling "y" as "Activity"
xtrain <- read.table("./train/X_train.txt", col.names = features)
ytrain <- read.table("./train/y_train.txt", col.names = "Activity")

xtest <- read.table("./test/X_test.txt", col.names = features)
ytest <- read.table("./test/y_test.txt", col.names = "Activity")

subject_test <- read.table("./test/subject_test.txt", col.names = "Subjects")
subject_train <- read.table("./train/subject_train.txt", col.names = "Subjects")

#Merge "test", "train" and "subjects"
testfull <- cbind(subject_test, ytest, xtest)
trainfull <- cbind(subject_train, ytrain, xtrain)

#merge it all into a full dataset
fulldataset <- rbind(testfull, trainfull)

#rename the activitis (label them)
fulldataset <- fulldataset %>% 
  mutate(Activity = gsub("1", "WALKING", Activity)) %>%
  mutate(Activity = gsub("2", "WALKING_UPSTAIRS", Activity)) %>%
  mutate(Activity = gsub("3", "WALKING_DOWNSTAIRS", Activity)) %>%
  mutate(Activity = gsub("4", "SITTING", Activity)) %>%
  mutate(Activity = gsub("5", "STANDING", Activity)) %>% 
  mutate(Activity = gsub("6", "LAYING", Activity))

#Keep only the mean and std, without frequency (meanFreq)
subsetdataset <- select(fulldataset, 1,2, contains("mean"), contains("std"), -contains("Freq"))

#Summarise by Activity and Subjects, giving the mean of each
tidydataset <- subsetdataset %>% group_by(Activity, Subjects) %>%
  summarise_each(funs(mean), -Activity, -Subjects)

write.table(tidydataset, "tidydataset.txt", row.names = FALSE)
