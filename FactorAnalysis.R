library(psych)
library(Information)
library(dplyr)

# effectiveness
effectiveness <- read.csv("Group_Data/EffectivenessScore.csv") 
str(effectiveness)

# removing ID 
eff <- effectiveness[, -c(1)]
str(eff)
sum(is.na(eff))

# perform Factor Analysis on the effectiveness scores
eff.fa <- fa(eff, fm="ml", scores = "tenBerge")
eff_loadings <- eff.fa$loadings
eff_loadings
# VTE scores, STK scores and IMM_2 score have high factor loadings

# find weights for the measures in effectiveness scores
eff_weights <- eff.fa$weights
eff_weights
sum(eff_weights)

# standardizing weights
eff_weights = eff_weights/sum(eff_weights)
sum(eff_weights)

# separating rows with less that 3 columns missing values
eff$invalid <- FALSE

for (row in 1:nrow(eff)){
  if (sum(!is.na(eff[row, ])) <= 3) {eff[row, c("invalid")] = TRUE}
  else {eff[row, c("invalid")] = FALSE}
}

# percentage of columns with invalid ratings
sum(eff$invalid)/nrow(eff)

# saperating rows with valid ratings
valid_indices <- which(!eff$invalid)
valid_indices

eff <- eff[which(!eff$invalid), ]
sum(eff$invalid)
eff <- eff[, -ncol(eff)]

# replacing NaN with the median value of the column
f_na <- function(measure){
  measure[which(is.na(measure))] <- median(measure, na.rm=T)
  return(measure)
}

eff <- data.frame(sapply(eff, f_na))

# calculating group scores
eff_weights
n <- ncol(eff) 
eff$score <- 0

for (row in 1:nrow(eff)){
  eff[row, c("score")] <- round(sum(eff[row, 1:n]*eff_weights)/length(eff_weights), 3)
}

effectiveness <- effectiveness[valid_indices, ]
effectiveness$score <- eff$score
effectiveness_scores <- effectiveness[, c("Provider.ID", "score")]
colnames(effectiveness_scores) <- c("Provider.ID", "eff_score")
View(effectiveness_scores)

# function to perform the same analysis on all the datasets
f_group_scores <- function(main = measures_file){
  
  
  # removing row numbers and ID 
  measures <- main[, -c(1)]
  str(measures)
  sum(is.na(measures))
  
  
  # Using fa from the psych package to find the loadings
  measures.fa <- fa(measures, fm="ml", scores = "tenBerge")
  measures_loadings <- measures.fa$loadings
  measures_loadings
  
  # weights
  measures_weights <- measures.fa$weights
  measures_weights <- measures_weights/sum(measures_weights)
  
  # handling NaN values
  measures$invalid <- FALSE
  
  for (row in 1:nrow(measures)){
    if (sum(!is.na(measures[row, ])) <= 3) {measures[row, c("invalid")] = TRUE}
    else {measures[row, c("invalid")] = FALSE}
  }
  
  sum(measures$invalid)/nrow(measures)

  valid_indices <- which(!measures$invalid)
  valid_indices
  
  measures <- measures[which(!measures$invalid), ]
  sum(measures$invalid)
  measures <- measures[, -ncol(measures)]
  
  # imputing missing values
  f_na <- function(measure){
    measure[which(is.na(measure))] <- median(measure, na.rm=T)
    return(measure)
  }
  
  measures <- data.frame(sapply(measures, f_na))
  
  # calculating group scores
  measures_weights
  n <- ncol(measures) 
  measures$score <- 0
  
  for (row in 1:nrow(measures)){
    measures[row, c("score")] <- round(sum(measures[row, 1:n]*measures_weights)/length(measures_weights), 3)
  }
  
  main <- main[valid_indices, ]
  main$score <- measures$score
  main_scores <- main[, c("Provider.ID", "score")]
  colnames(main_scores) <- c("Provider.ID", "group_score")
  group_scores <- main_scores
  
  return(list(group_scores))
  
}

# effectiveness
main <- read.csv("Group_Data/EffectivenessScore.csv")
eff_scores <- f_group_scores(main) 
colnames(eff_scores[[1]])[2] <- "effectiveness_score"
View(eff_scores[[1]])
write.csv(eff_scores[[1]], "Group_Score/EffectivenessScore.csv",row.names=FALSE)

# experience
main <- read.csv("Group_Data/ExperienceScore.csv")
exp_scores <- f_group_scores(main)
colnames(exp_scores[[1]])[2] <- "experience_score"
View(exp_scores[[1]])
write.csv(exp_scores[[1]], "Group_Score/ExperienceScore.csv",row.names=FALSE)

# mortality
main <- read.csv("Group_Data/MortalityScore.csv")
mortality_scores <- f_group_scores(main)
colnames(mortality_scores[[1]])[2] <- "mortality_score"
View(mortality_scores[[1]])
write.csv(mortality_scores[[1]], "Group_Score/MortalityScore.csv",row.names=FALSE)

# safety
main <- read.csv("Group_Data/SafetyScore.csv")
safety_scores <- f_group_scores(main)
colnames(safety_scores[[1]])[2] <- "safety_score"
View(safety_scores[[1]])
write.csv(safety_scores[[1]], "Group_Score/SafetyScore.csv",row.names=FALSE)

# timeliness
main <- read.csv("Group_Data/TimelinessScore.csv")
time_scores <- f_group_scores(main)
colnames(time_scores[[1]])[2] <- "timeliness_score"
View(time_scores[[1]])
write.csv(time_scores[[1]], "Group_Score/TimelinessScore.csv",row.names=FALSE)

# readmission
main <- read.csv("Group_Data/ReadmissionScore.csv")
read_scores <- f_group_scores(main)
colnames(read_scores[[1]])[2] <- "readmission_score"
View(read_scores[[1]])
write.csv(read_scores[[1]], "Group_Score/ReadmissionScore.csv",row.names=FALSE)

# medical
main <- read.csv("Group_Data/MedicalScore.csv")
med_scores <- f_group_scores(main)
colnames(med_scores[[1]])[2] <- "medical_score"
View(med_scores[[1]])
write.csv(med_scores[[1]], "Group_Score/MedicalScore.csv",row.names=FALSE)