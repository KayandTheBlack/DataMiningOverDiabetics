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
"metformin.pioglitazone",
"admission_type_id", "admission_source_id","discharge_disposition_id",
"time_in_hospital", "medical_specialty", "number_emergency",
"number_inpatient", "num_lab_procedures", "num_procedures",
"num_medications", "number_outpatient","number_diagnoses", "patient_nbr")

shortenedMed <- vector(mode="list", length=23)
shortenedOther <- vector(mode="list", length=(length(keys)-23))
shortened <- vector(mode="list", length=length(keys))

# Assigning simple names to the 23 medicines
for (medIndex in 1:9){
  shortenedMed[[medIndex]] <- paste("med_0", medIndex,sep="")
}
for (medIndex in 10:23){
  shortenedMed[[medIndex]] <- paste("med_", medIndex,sep="")
}
# Assigning names to the rest
shortenedOther <- c("adm_type_id", "adm_source_id","disch_id",
"time_in_hpt", "specialty", "n_emerg",
"n_inp", "n_lab_proc", "n_proc",
"n_med", "n_outp", "n_diag","patient_n")

shortened <- c(shortenedMed, shortenedOther)
names(shortened) <- keys

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
