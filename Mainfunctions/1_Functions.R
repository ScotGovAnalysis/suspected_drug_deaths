##FUNCTIONS
#This script contains the functions used to wrangle the summary data and create visualisations for the suspected drug deaths publication - RUN THIS SCRIPT FIRST

######
#libraries
library(tidyverse)
library(flextable)


#1) Function for reading in MAIN DATASET
Readingdata <- function(filepath) {
  readxl::excel_sheets(filepath) %>%
    set_names() %>%
    map(readxl::read_excel, path = filepath)
}
#######################################################################

#2) MINI TIDYING FUNCTIONS TO ACCOUNT FOR SUMMARISED DATASET
##To tranform the appropriate columns to Quarter
Month_fun <- function(x){
  seq(x, 10000000, by=4)
}

##To transform the appropriate columns to Year
Year_fun <- function(x,y){
  seq(x, y, by=1)
}
########################################################################

#3) #MAIN TIDYING FUNCTION
Tabtidying <- function(x){
  
  #to fix the first column name from the long, unformatted name to 'division'
  colnames_x <- colnames(x)
  colnames_x[1] <- "division"
  colnames(x) <- colnames_x
  
  #wrangling the rest
  pivot_longer(x, 2:last_col(), names_to = "col", values_to = "total") %>%  
    mutate(col = str_remove(col, "..."),
           col = as.numeric(col),
           total2 = case_when(total == "x" ~ 0,
                              T~as.numeric(total)),
           month = case_when(col %in% c(Month_fun(2)) ~"January-March", ###Aforementioned function for quarters
                             col %in% c(Month_fun(3)) ~"April-June",
                             col %in% c(Month_fun(4)) ~"July-September",
                             T~"October-December"),
           year = case_when(col %in% c(Year_fun(2,5))~2017, ###Aforementioned function for years
                            col %in% c(Year_fun(6,9))~2018, ### WILL NEED TO MAKE ADDITIONAL CHANGES IN SUBSEQUENT YEARS
                            col %in% c(Year_fun(10,13))~2019,
                            col %in% c(Year_fun(14,17))~2020,
                            col %in% c(Year_fun(18,21))~2021,
                            col %in% c(Year_fun(22,25))~2022,
                            T~2023),
           xaxis = paste(month,year),
           Q = case_when(month == "January-March"~"Q1",
                         month == "April-June"~"Q2",
                         month == "July-September"~"Q3",
                         T~"Q4")) %>%
    select(-total) %>% 
    drop_na()  
}
########################################################################

#4). FUNCTION TO CREATE FIG 1
Plot_fun <- function(x){
  x %>% 
    mutate(rolling = zoo::rollapplyr(total2, 4, sum, partial = TRUE),##zoo::rollapplyr is a nifty command for creating rolling totals (in this case, last 4 observations)
           month2 = case_when(month == "January-March"~"Jan-Mar",
                              month == "April-June"~"Apr-Jun",
                              month == "July-September"~"Jul-Sep",
                              T~"Oct-Dec"),
           xaxis = paste(month2, year),
           rolling = case_when(rolling<1000 ~ 0,                                    ##
                               T~rolling)) %>%                                      ##
    select(total2, rolling, everything()) %>%                                       ##
    pivot_longer(total2:rolling, names_to ="category", values_to = "total") %>%     ## 
    filter(division == "Total",                                                     ##
           total !=0) %>%                                                           ##
    pivot_wider(names_from = category, values_from = total) %>%                     ##These lines are a hacky fix to ensure the line only comes in when there are four months of rolling data
    ggplot() +
    geom_col(aes(x=reorder(xaxis,col), y=total2, fill = "Police Scotland suspected drug deaths"))+
    geom_line(aes(x=reorder(xaxis,col), y=rolling, color = str_wrap("Rolling annual suspected drug deaths (sum of last 4 quarters)", 35)), group=1, size =1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = .95,
                                     vjust = .5,
                                     size=12,
                                     colour = "black"),
          axis.text.y=element_text(size=12,
                                   colour = "black"),
          axis.title = element_text(size=12, 
                                    face = "plain",
                                    colour = "black"),
          legend.text = element_text(size=12,
                                     colour="black"),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.caption = element_text(size=12)) +
    labs(x="",
         y="",
         title = "Figure 1: Number of Police Scotland suspected drug deaths by quarter \nand year, January 2017 to June 2022",      #######NOTE THAT MONTHS IN TITLE WILL NEED TO BE CHANGED DEPENDING ON WHAT QUARTER IT IS
         caption = "Source: Police Scotland") +
    scale_fill_manual(values = "#0065bd") +
    scale_color_manual(values = "black") +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0,1600, by=200),
                       breaks = seq(0,1600, by=200),
                       expand = c(0,10)) 
}