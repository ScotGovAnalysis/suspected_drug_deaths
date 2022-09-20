##These functions generate two sets of figures:
###a) figures for the most recent single quarter (i.e. three-month period) 
###b) figures for the most recent twelve months (i.e. rolling period)



##a) Functions for the most recent quarter in question 

#Selects latest quarter
Quarter <- PoliceDivision %>% 
  # mutate(PoliceDivision, Quarter = paste0(Q, ", ", xaxis)) %>% 
  select(xaxis) %>% 
  tail(1) %>% 
  mutate(xaxis = str_replace(xaxis, "-", " to "))

#Selects DRD total for latest quarter
latest_fig <- PoliceDivision$total2 %>% 
  tail(1)

#Creates DRD figure for  previous calendar quarter
latest_fig_prevQ <- PoliceDivision$total2 %>% tail(2) %>% head(1)

#Creates %change in DRDs between latest quarter and previous calendar quarter
latest_fig_prevQ_prop <- abs(round((latest_fig-latest_fig_prevQ)/latest_fig_prevQ*100, 0))

#Difference in DRDs between latest quarter and previous calendar quarter
latest_fig_diff <- abs(latest_fig -latest_fig_prevQ)

#boolean for quarterly increase/decrease
latest_fig_which <- ifelse(latest_fig>latest_fig_prevQ, "more", "fewer")

#Creates latest quarter label

latest_fig_calendarQ <- PoliceDivision %>% 
  select(xaxis) %>% 
  tail(2) %>% 
  head(1) %>% 
  mutate(xaxis = str_replace(xaxis, "-", " to "))

################################################################
##b)rolling 12 month total

rolling12months <- format(as.numeric(PoliceDivision$total2 %>% 
                                       tail(4) %>% 
                                       sum()), nsmall = 0, big.mark = ",")

rolling12months_absolutechange <- 
  abs(PoliceDivision$total2 %>% 
        tail(4) %>% 
        sum() - (PoliceDivision$total2 %>% tail(8) %>% head(4) %>% sum()))

rolling12months_propchange <- 
  abs(round((PoliceDivision$total2 %>% tail(4) %>% sum() - (PoliceDivision$total2 %>% tail(8) %>% head(4) %>% sum()))/
              (PoliceDivision$total2 %>% tail(8) %>% head(4) %>% sum())*100))

rolling12months_which <- 
  ifelse(rolling12months > (PoliceDivision$total2 %>% tail(8) %>% head(4) %>% sum()),
         "more",
         "fewer")
