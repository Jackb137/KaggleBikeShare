library(tidyverse)

library(readr)
test <- read_csv("test.csv")
sampleSubmission <- read_csv("sampleSubmission.csv")
train <- read_csv("train.csv")



# Stage all changes for commit
system("git add .")

# Commit your changes with a commit message
system("git commit -m 'Added new files'")

# Push your changes to the remote repository
system("git push origin master")