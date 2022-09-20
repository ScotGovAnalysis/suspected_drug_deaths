#2 Quarter
#This script contains functions for pulling out all the figures for the MOST RECENT TWO QUARTERS - typically this will comprise January to June of a given year

#The following code chunks produce the values needed to populate the briefing

##################
#TOP LEVEL FIGURES
##################

#Overall figure, last 2 quarter
##NOTICE THE EXTRA FORMATTING TO ADD A COMMA IN THE OUTPUT
latest_2Q <- format(as.numeric(PoliceDivision$total2 %>% 
                                 tail(2) %>% 
                                 sum()), nsmall = 0, big.mark=",")


##Figure for same period one year previous
latest_2Q_comparison <-format(as.numeric(PoliceDivision$total2 %>%
                                           tail(6) %>%
                                           head(2) %>% 
                                           sum()),nsmall=0, big.mark=",")



#Difference  for current period and same period year previous
##NOTE THE COMMA FORMATTING DOES NOT ALLOW ARITHMETIC OPERATIONS OF VARIABLES THEMSELVES, HENCE WRITING THEM OUT AGAIN MANUALLY
latest_2Q_comparison_diff <- abs((PoliceDivision$total2 %>% tail(6) %>%head(2) %>% sum())) - 
  (PoliceDivision$total2 %>% tail(2) %>% sum())

# %change for current rolling period and previous period
##AGAIN, COMMA FORMATTING PREVENTS ARITHMETIC OPERATIONS OF VARIABLES ETC
latest_2Q_comparison_prop <-
  round(abs(
    ((PoliceDivision$total2 %>% tail(6) %>%head(2) %>% sum())) - 
      (PoliceDivision$total2 %>% tail(2) %>% sum()))/(PoliceDivision$total2 %>% tail(6) %>%head(2) %>% sum())*100,0)


#Boolean determining whether aforementioned  change is an increase or decrease
latest_which <- ifelse(
  (PoliceDivision$total2 %>% tail(2) %>% sum()) > (PoliceDivision$total2 %>% tail(6) %>%head(2) %>% sum()),
  "more", 
  "fewer")

# #Boolean determining whether aforementioned  change is an increase or decrease with synonym
latest_which2 <- ifelse((PoliceDivision$total2 %>% tail(2) %>% sum()) > (PoliceDivision$total2 %>% tail(6) %>%head(2) %>% sum()),
                        "higher",
                        "lower")

#Populates first month of study period 
first_month <- PoliceDivision$month %>% 
  tail(2)  %>% 
  tail(1) %>% 
  str_remove(pattern = "\\-.*") ##Because the Quarter string is a monthx-monthy range, this formats the string to leave the former

#Populates last month of study period (i.e. final month)
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
    (Sex$Male %>% tail(2) %>% sum())/
      ((Sex$Male %>% tail(2) %>% sum()) + (Sex$Female %>% tail(2) %>% sum()))*100, 0)

#Propportion of DRDs that are male over same period in previous year
Males_prev <- 
  round(
    (Sex$Male %>% tail(6) %>% head(2) %>% sum())/
      ((Sex$Male %>% tail(6) %>% head(2) %>% sum()) + (Sex$Female %>% tail(6) %>% head(2) %>% sum()))*100, 0)

#Number of females who have had DRD in period of study
Females <- 
  Sex$Female %>%tail(2) %>% sum()

#Difference in number of female DRDs in period of study and equivalent period over the previous year
Females_prev <- 
  abs(Females - Sex$Female %>% tail(6) %>% head(2) %>% sum())

#Proportional change in female DRDs between this year and last
Females_prop <- 
  abs(round(
    ((Females-Sex$Female %>% tail(6) %>% head(2) %>% sum())/
       (Sex$Female %>% tail(6) %>% head(2) %>% sum()))*100, 0))

##boolean for annual increase/decrease in DRDs in females
Females_which <- ifelse(Females - Sex$Female %>% tail(6) %>% head(2) %>% sum() >0, "an increase", "a decrease")

################################################################
#AGE

#Proportion of DRDs that are 'middle-aged', i.e. the largest cohort
Age_prop_middleaged <-
  round(((Age$'35 to 44' %>% tail(2) %>% sum()) + (Age$'45 to 54' %>% tail(2) %>% sum()))/(Age$Total %>% tail(2) %>% sum())*100)

#Total DRDs in young people (age =<24)
Age_young <- 
  Age$'24 & Under' %>% tail(2) %>% sum()

## Number change in young DRDs between this study period and the year previous
Age_young_comparison <- 
  abs(Age_young - (Age$'24 & Under' %>% tail(6) %>% head(2) %>% sum()))

##Proportion change in young DRDs between this study period and the year previous
Age_young_prop <- 
  abs(round((Age_young - (Age$'24 & Under' %>% tail(6) %>% head(2) %>% sum()))/
              (Age$'24 & Under' %>% tail(6) %>% head(2) %>% sum())*100,0))

##boolean for annual increase/decrease in DRDs in young people
Age_which <- 
  ifelse(round((Age_young - (Age$'24 & Under' %>% tail(6) %>% head(2) %>% sum()))/
                 (Age$'24 & Under' %>% tail(6) %>% head(2) %>% sum())*100,1) < 0, "fewer", "greater")

################################################################
#POLICE DIVISION
ranking_PoliceDiv<- PoliceDivision %>% 
  filter(division !="Total") %>% 
  select(division, total2, month, year, xaxis, Q, col) %>% 
  # select(-col, col) %>%                                
  top_n(26) %>%                                         #26 is because we want returned the most recent two quarters and there are 13 police divisions
  group_by(division) %>% 
  summarise(total = sum(total2)) %>% 
  arrange(desc(total)) %>% 
  head(3) %>%                                           #selecting the highest values after grouping/summarising (i.e. DRDs )
  mutate(division = paste0(division, " (", total, ")"))

Ranking_PoliceDiv<- ranking_PoliceDiv$division %>%      
  str_remove(pattern = ".*:")                           #removing the damnable letters from the beginning of the string
