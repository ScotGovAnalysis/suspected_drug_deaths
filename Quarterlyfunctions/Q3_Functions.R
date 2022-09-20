#3 Quarter
#This script contains functions for pulling out all the figures for the MOST RECENT THREE QUARTERS - typically this will comprise January to September of a given year

#The following code chunks produce the values needed to populate the briefing

##################
#TOP LEVEL FIGURES
##################

##Overall figure, last 3 quarters
latest_3Q <- format(round(as.numeric(PoliceDivision$total2 %>% 
                                       tail(3) %>% 
                                       sum()), 1), nsmall=0, big.mark=",")

##Figure for same period one year previous
latest_3Q_comparison <-format(round(as.numeric(PoliceDivision$total2 %>% 
                                                 tail(7) %>% 
                                                 head(3) %>% 
                                                 sum()), 1), nsmall = 0, big.mark = ",")

##difference between two period
latest_3Q_comparison_diff <- abs((PoliceDivision$total2 %>% tail(3) %>% sum())-(PoliceDivision$total2 %>% tail(7) %>% head(3) %>% sum()))

##Proportional change between current and previous period
latest_3Q_comparison_prop <- 
  abs(round(
    ((PoliceDivision$total2 %>% tail(3) %>% sum())-(PoliceDivision$total2 %>% tail(7) %>% head(3) %>% sum()))/
      (PoliceDivision$total2 %>% tail(7) %>% head(3) %>% sum())*100, 0))

#Boolean determining whether change is an increase or decrease
latest_which <- ifelse(PoliceDivision$total2 %>%  tail(3) %>% sum() > PoliceDivision$total2 %>% tail(7) %>% head(3) %>% sum(), 
                       "more", 
                       "fewer")

#Boolean determining whether aforementioned  change is an increase or decrease with synonym
latest_which2 <- ifelse(PoliceDivision$total2 %>%  tail(3) %>% sum() > PoliceDivision$total2 %>% tail(7) %>% head(3) %>% sum(), 
                       "greater", 
                       "fewer")

#Populates first month of study period (i.e. first month of latest_3Q)
first_month <- PoliceDivision$month %>% 
  tail(2)  %>% 
  tail(1) %>% 
  str_remove(pattern = "\\-.*")  ##Because the Quarter string is a monthx-monthy range, this formats the string to leave the former


#Populates last month of study period (i.e. final month of latest_3Q)
last_month <- PoliceDivision$month %>% 
  tail(1) %>% 
  str_remove(pattern = ".*-") ##Because the Quarter string is a monthx-monthy range, this formats the string to leave the latter

#Creates current year
year <- PoliceDivision$year %>% 
  tail(1)

#Creates comparison year to accompany comparison variables
year_prev <- PoliceDivision$year %>% 
  tail(5) %>% 
  head(1)

################################################################
####################
#BY SEX/AGE/LOCATION
####################
################################################################
#SEX

#Proportion of DRDs that are male
Males_latest <- 
  round(
    (Sex$Male %>% tail(3) %>% sum())/
      ((Sex$Male %>% tail(3) %>% sum()) + (Sex$Female %>% tail(3) %>% sum()))*100, 0)

#Propportion of DRDs that are male over same period in previous year
Males_prev <- 
  round(
    (Sex$Male %>% tail(7) %>% head(3) %>% sum())/
      ((Sex$Male %>% tail(7) %>% head(3) %>% sum()) + (Sex$Female %>% tail(7) %>% head(3) %>% sum()))*100, 0)

#Number of females who have had DRD in quarter of study
Females <- 
  Sex$Female %>%tail(3) %>% sum()

#Difference in number of female DRDs in quarter of study and equivalent period over the previous year
Females_prev <- 
  Females - Sex$Female %>% tail(7) %>% head(3) %>% sum()

#Proportional change in female DRDs between this year and last
Females_prop <- 
  abs(round(
    ((Females-Sex$Female %>% tail(7) %>% head(3) %>% sum())/
       Females*100), 0))

##boolean for annual increase/decrease in DRDs in females
Females_which <- ifelse(Females_prev >0, "an increase", "a decrease")

################################################################
#AGE

##Proportion of DRDs that are 'middle-aged', i.e. the largest cohort
Age_prop_middleaged <-
  round(
    ((Age$'35 – 44' %>% tail(3) %>% sum()) + (Age$'45 – 54' %>% tail(3) %>% sum()))/
      Age$Total %>% tail(3) %>% sum() * 100, 1)

#Total DRDs in young people (age =<24)
Age_young <- 
  Age$`24 & Under` %>% tail(3) %>% sum()

##Proportion change in young DRDs between this study period and the year previous
Age_young_prop <- 
  abs(round(
    (Age_young- (Age$`24 & Under` %>% tail(7) %>% head(3) %>% sum()) )/
      ((Age$`24 & Under` %>% tail(7) %>% head(3) %>% sum()))*100, 0))

## Number change in young DRDs between this study period and the year previous
Age_young_comparison <- 
  abs(Age_young - (Age$`24 & Under` %>% tail(7) %>% head(3) %>% sum()))

##boolean for annual increase/decrease in DRDs in young people
Age_which <- 
  ifelse(Age_young < (Age$`24 & Under` %>% tail(7) %>% head(3) %>% sum()), "fewer", "greater")

ranking_PoliceDiv<- PoliceDivision %>% 
  filter(division !="Total") %>% 
  select(-col, col) %>%                                 #hacky fix to send the total column to the end because....  
  top_n(39) %>%                                         #...this command judges the highest values based on the final (rightmost) column. 39 is because we want returned the most recent three quarters and there are 13 police divisions
  group_by(division) %>% 
  summarise(total = sum(total2)) %>% 
  arrange(desc(total)) %>% 
  head(4) %>%                                           #selecting the highest four values after grouping/summarising (i.e. DRDs )
  mutate(division = paste0(division, " (", total, ")")) #creating label

################################################################
#POLICE DIVISION
ranking_PoliceDiv<- PoliceDivision %>% 
  filter(division !="Total") %>% 
  select(-col, col) %>%                                 #hacky fix to send the total column to the end because....  
  top_n(39) %>%                                         #...this command judges the highest values based on the final (rightmost) column. 39 is because we want returned the most recent three quarters and there are 13 police divisions
  group_by(division) %>% 
  summarise(total = sum(total2)) %>% 
  arrange(desc(total)) %>% 
  head(3) %>%                                           #selecting the highest three values after grouping/summarising (i.e. DRDs )
  mutate(division = paste0(division, " (", total, ")")) #creating label

Ranking_PoliceDiv<- ranking_PoliceDiv$division %>%      
  str_remove(pattern = ".*:")                           #removing the damnable letters from the beginning of the string                        #removing the damnable letters from the beginning of the string
