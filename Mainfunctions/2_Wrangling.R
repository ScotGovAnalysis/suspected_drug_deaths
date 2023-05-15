##This script wrangles all the data to create three datasets - 1 for police divison, 1 for sex and 1 for age

#First - read in all the functions
source("Mainfunctions/1_Functions.R")

##Base dataset, using the function in previous chunk
##This command creates a list containing the original dataset's three tabs (Police Division, Age, Sex)
###NOTE THAT FILENAME WILL NEED TO BE CHANGED AS NEW DATASETS ARE ADDED
df <- Readingdata("Data/SuspectedDRDs_JanMar2023.xlsx")

#######################################
#######################################

##Main formatting chunk
PoliceDivision <- 
  as.data.frame(df$`A - Police Division`) %>%   #extracts first tab of spreadsheet into a data frame
  slice(3:17) %>%                               #selects only the rows needed for analysis, as the table is summarised
  Tabtidying()                                  #this is the tidying function created in "1_Functions.R"

#Peparing dataframe from 'Sex' tab
Sex <- 
  as.data.frame(df$`C - Sex`) %>% 
  slice(3:6) %>%                                              #selecting only the rows we want, as the table is summarised
  Tabtidying() %>%                                            #aforementioned tidying function
  filter(division != "Total") %>% 
  pivot_wider(names_from = division, values_from = total2)    #pivoting wider so as to calculate M/F figs


#Creating age dataframe from the Age tab in base df
Age <- 
  as.data.frame(df$`B - Age`) %>%                               
  slice(3:10) %>%                                             #selecting only the rows we want, as the table is summarised
  Tabtidying() %>%                                            #aforementioned tidying function
  pivot_wider(names_from = division, values_from = total2)    #pivoting columns wider by age bracket to do calculations

