#Start with a section called "Project Info" that includes the date and link to your GitHub repository. You can add your name or username if you want to remain anonymous (remember this will be publicly accessible on GitHub).

#Load the FallopiaData.csv
library(dplyr)
FalloData<-read.csv("./InputData/FallopiaData.csv")


#Remove rows with ‘Total’ biomass < 60
#Reorder the columns so that they are in the order: ‘Total’, ‘Taxon’, ‘Senario’, ‘Nutrients’, and remove the other columns
#Make a new column TotalG, which converts the ‘Total’ column from mg to grams AND replace Total with TotalG.
FalloData<-FalloData %>%
  filter(Total>=60) %>%
  select(Total,Taxon,Scenario,Nutrients) %>%
  transmute(TotalG=Total/1000,Taxon,Scenario,Nutrients)
FalloData$Taxon
  


#Write a custom function that will take two inputs from the user: 1. a vector of data to process (e.g. column from a data.frame object) and 2. a string that defines what calculation to perform.
  #if string #2 is "Average" then calculate the average value for the column named in vector #1
  #if string #2 is "Sum" then calculate the sum of values for the column named in vector #1
  #if string #2 is "Observations" then count the number of observed values for the column named in vector #1
  #if string #2 is anything else, then output an error to the user 
  
ASOFun<-function(Column,Command){
  if(Command=="Sum"){
    return(sum(Column))}
  if(Command=="Observations"){
    return(length(Column))}
  if(Command=="Average"){
    return(sum(Column)/length(Column))}
  else(return("Error:Check Input"))
}

  
#Write some R code that uses your function to count the total number of observations in the 'Taxon' column.
ASOFun(FalloData$Taxon,"Observations")


#Write some R code that uses your function  to calculate the average TotalG for each of the two Nutrient concentrations.
FalloData %>%
  group_by(Nutrients) %>%
  summarize(Mean=ASOFun(TotalG,"Average"))
#Write (i.e. save) the new data to a file called "WrangledData.csv" in the Output folder. 
write.csv(FalloData,"./Output/WrangledData.csv")