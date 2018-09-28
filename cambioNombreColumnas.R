diabetic_data <- read.csv("diabetic_data.csv", na.strings = c("?"))
colnames(diabetic_data)

# All the names of the columns whose names we want to change,
# starting with the medicine ones
keys <- c("metformin", "repaglinide", "nateglinide", "chlorpropamide",
"glimepiride", "acetohexamide", "glipizide", "glyburide",
"tolbutamide", "pioglitazone", "rosiglitazone", "acarbose",
"miglitol", "troglitazone", "tolazamide", "examide",
"citoglipton", "insulin", "glyburide.metformin", "glipizide.metformin",
"glimepiride.pioglitazone", "metformin.rosiglitazone",
"metformin.pioglitazone")

shortened <- vector(mode="list", length=length(keys))
names(shortened) <- keys

# Assigning simple names to the 23 medicines
for (medIndex in 1:9){
  shortened[[medIndex]] <- paste("med_0", medIndex,sep="")
}
for (medIndex in 10:23){
  shortened[[medIndex]] <- paste("med_", medIndex,sep="")
}
# Assigning names to the rest


# Substituting the names in the original data
currCols <- colnames(diabetic_data)
for (currCol in 1:length(currCols)){
  colName <- currCols[currCol]
  if (colName %in% keys){
    colnames(diabetic_data)[currCol] <- shortened[[colName]]
  }
}

# Print current column names of the data
colnames(diabetic_data)

# Uncomment the following line to save the data
# with modified columns to a csv
# write.csv(diabetic_data, file="diabetic_data_ShortNames.csv")
