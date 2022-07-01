###Data Cleaning Text Fields for Aggregate Analyses
###Code Developed by Arielle Landau (alandaux.github.io)

# load required packages --------------------------------------------------

packages = c("s2", "tidyverse", "readxl", "sf", "DataCombine", "stringr", "stringi")
setdiff(packages, rownames(installed.packages()))
install.packages(setdiff(packages, rownames(installed.packages())), quietly = T)
library(DataCombine)
library(sf)
library(tidyverse)
library(readxl)
library(s2)
library(stringr)
library(stringi)

sf_use_s2(T)

# load data from csv, make sure to include the filepath if the csv is saved in a folder
# different from the root folder

generalData <- read.csv("/filepath/dataName.csv")

#sort the text fields you wish to clean, alphabetically  
sortedData <- generalData[order(generalData$your_messy_text_field),]

#remove unecessary and random quotation marks and periods from the text field
sortedData$your_messy_text_field<-gsub("'","",as.character(sortedData$your_messy_text_field))
sortedData$your_messy_text_field<-gsub('"',"",as.character(sortedData$your_messy_text_field))
sortedData$your_messy_text_field<-gsub('.',"",as.character(sortedData$your_messy_text_field))

#limit text fields to the first 20 characters to aid with word matching 
sortedData <- sortedData %>% mutate(
  your_messy_text_field = substr(your_messy_text_field, 1, 20)
)

#convert column of text field to a string vector
cleanerTextFields <- sortedData$your_messy_text_field

#fuzzy match data with agrep function based on the cleaner text fields
#outputs, for every text field, a list of other text fields that match
#the agrep function uses Levenshtein distance to decide if text fields should be matched
#see more about Levenshtein distance: https://en.wikipedia.org/wiki/Levenshtein_distance
#r documentation: https://stat.ethz.ch/R-manual/R-devel/library/base/html/agrep.html
agrepData <- sortedData %>% rowwise() %>% mutate(
  matchedTextFields = list(agrep(
    your_messy_text_field, cleanerTextFields, max.distance = 0.1, ignore.case = TRUE, value = TRUE
  ))
)

#extract the mode from each list of matched text fields
#using the mode allows us to find the most common universal name in each list of potential matches
#rows with the same mode (rows with the same, cleaned text) can now be matched together
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#use mode to match cleaned text fields
cleanedData <- agrepData %>% rowwise() %>% mutate(
  textFieldMode = Mode(matchedTextFields)
)

#aggregate data with cleaned text fields, this will tell us how often a specific text field appears
#this table counts the number of conflicts each company is involved in 
DataAggregate <- cleanedData %>% group_by(cleanedData$textFieldMode) %>% summarise(count_fields = n())

#export aggregate results as a csv
write.csv(DataAggregate,"dataAggregateTextFields.csv", row.names = FALSE)

#join cleaned data back to original dataset
joinedData <- full_join(generalData, cleanedData, by = c("your_ID_field" = "your_ID_field"), keep = TRUE, na_matches = c("na", "never") )

#export your cleaned, original dataset
write.csv(joinedData,"cleanedDataset.csv", row.names = FALSE)


