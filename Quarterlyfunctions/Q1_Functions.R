#1 Quarter
#This script contains functions for pulling out all the figures for the MOST RECENT ONE QUARTER - typically this will comprise January to March of a given year

#The following code chunks produce the values needed to populate the briefing

##################
#TOP LEVEL FIGURES
##################

#Overall figure, last 1 quarter
latest_1Q <- PoliceDivision$total2 %>% 
  tail(1)

##Figure for same period one year previous
latest_1Q_comparison <- PoliceDivision$total2 %>%
  tail(5) %>%
  head(1)

#Difference  for current quarter and same quarter year previous
latest_1Q_comparison_diff <- abs(latest_1Q - latest_1Q_comparison)

# %change for current period and previous period
latest_1Q_comparison_prop <-
  round(abs((latest_1Q - latest_1Q_comparison) /
              latest_1Q_comparison)*100,0)

#Boolean determining whether aforementioned  change is an increase or decrease
latest_which <- ifelse(
  latest_1Q > latest_1Q_comparison,
  "more", 
  "fewer")

# #Boolean determining whether aforementioned  change is an increase or decrease with synonym
latest_which2 <- ifelse(latest_1Q > latest_1Q_comparison,
                        "higher",
                        "lower")

#Populates first month of study period (i.e. first month of latest_Q)
first_month <- PoliceDivision$month %>% 
  tail(1)  %>% 
  str_remove(pattern = "\\-.*") ##Because the Quarter string is a monthx-monthy range, this formats the string to leave the former

#Populates last month of study period (i.e. final month of latest_4Q)
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
    (Sex$Male %>% tail(1) %>% sum())/
      ((Sex$Male %>% tail(1) %>% sum()) + (Sex$Female %>% tail(1) %>% sum()))*100, 0)

#Proportion of DRDs that are male over same period in previous year
Males_prev <- 
  round(
    (Sex$Male %>% tail(5) %>% head(1) %>% sum())/
      ((Sex$Male %>% tail(5) %>% head(1) %>% sum()) + (Sex$Female %>% tail(5) %>% head(1) %>% sum()))*100, 0)

#Number of females who have had DRD in quarter of study
Females <- 
  Sex$Female %>%tail(1) %>% sum()

#Difference in number of female DRDs in quarter of study and equivalent period over the previous year
Females_prev <- 
  abs(Females - Sex$Female %>% tail(5) %>% head(1) %>% sum())

#Proportional change in female DRDs between this year and last
Females_prop <- 
  abs(round(
    ((Females-Sex$Female %>% tail(5) %>% head(1) %>% sum())/
       (Sex$Female %>% tail(5) %>% head(1) %>% sum()))*100, 0))

##boolean for annual increase/decrease in DRDs in females
Females_which <- ifelse(Females - Sex$Female %>% tail(5) %>% head(1) %>% sum() >0, "an increase", "a decrease")

################################################################
#AGE

##Proportion of DRDs that are 'middle-aged', i.e. the largest cohort
Age_prop_middleaged <-
  round(
    ((Age$'35 – 44' %>% tail(1) %>% sum()) + (Age$'45 – 54' %>% tail(1) %>% sum()))/
      Age$Total %>% tail(1) %>% sum() * 100, 0)

#Total DRDs in young people (age =<24)
Age_young <- 
  Age$`24 & Under` %>% tail(1) %>% sum()

## Number change in young DRDs between this study period and the year previous
Age_young_comparison <- 
  abs(Age_young - (Age$`24 & Under` %>% tail(5) %>% head(1) %>% sum()))

##Proportion change in young DRDs between this study period and the year previous
Age_young_prop <- 
  abs(round((Age_young - (Age$`24 & Under` %>% tail(5) %>% head(1) %>% sum()))/
              (Age$`24 & Under` %>% tail(5) %>% head(1) %>% sum())*100,0))

##boolean for annual increase/decrease in DRDs in young people
Age_which <- 
  ifelse(round((Age_young - (Age$`24 & Under` %>% tail(5) %>% head(1) %>% sum()))/
                 (Age$`24 & Under` %>% tail(5) %>% head(1) %>% sum())*100,1) < 0, "fewer", "greater")

################################################################
#POLICE DIVISION
ranking_PoliceDiv<- PoliceDivision %>% 
  filter(xaxis =="January-March 2022",
         division != "Total")  %>% 
  arrange(desc(total2)) %>% 
  head(3) %>%                                             #selecting the highest three values after grouping/summarising (i.e. drug misuse deaths)
  mutate(division = paste0(division, " (", total2, ")"))  #creating label

Ranking_PoliceDiv<- ranking_PoliceDiv$division %>%      
  str_remove(pattern = ".*:")                             #removing the damnable letters from the beginning of the string

################################################################
