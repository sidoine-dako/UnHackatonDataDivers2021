############  CODE FOR COVID-19 IMPACT ON SOCIO-ECONOMIC DEVELOPMENT ##########
## TEAM : DATA DIVERS 

rm(list=ls())

#set working directory 

setwd("E:/Scripts/R/Hackathon/Hackathon")

#import data 
dat <-  read.delim("data1.txt")

##PRE PROCESSING DATA 
#coerce the data as tibble 
dat <- as_tibble(dat)

#remove NA's

dat <-  na.omit(dat)

#import libraries 
#install.packages("tidyverse")
library(tidyverse)

dat.1 <- dat %>%
         mutate(
           continent = factor(continent),
           year      = factor(year)
         ) %>%
         group_by(continent, year) %>%
         summarise( 
            ave.strin       = mean(stringency_index,   na.rm = TRUE), 
            ave.gdp         = mean(gdp_per_capita,     na.rm = TRUE), 
            ave.poverty     = mean(extreme_poverty,    na.rm = TRUE)
         )

## PLOT : STRINGENCY INDEX 
ggplot(dat.1) +
       aes(y = ave.strin, x = continent, fill = year) + 
       geom_bar(
         stat  ='identity', position = "dodge",
         width = 0.6
       )+ 
       labs(
         title = "Stringency index",
         subtitle = "2020 and 2021",
         caption = "data::  https://covid.ourworldindata.org/",
         y        = " ",
         x        = " ", 
         fill     = " "
       ) +
       scale_fill_grey(
         label    = c("Year 2020","Year 2021")
       ) + 
       theme_linedraw() +
       theme(
          axis.text.x = element_text(size = 10, colour = "black"),
          axis.text.y = element_text(size = 10, colour = "black")
       )
       
##PLOT : GDP PER CAPITA
ggplot(dat.1) +
       aes(y = ave.gdp, x = continent, fill = year) + 
       geom_bar(
       stat  ='identity', position = "dodge",
       width = 0.6
      )+ 
       labs(
       title = "Gross domestic product per capita",
       caption = "data::  https://covid.ourworldindata.org/",
       y        = " ",
       x        = " ", 
       fill     = " "
      ) +
      scale_fill_grey(
      label    = c("Year 2020","Year 2021")
      ) + 
      theme_linedraw()+
      theme(
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")
      )

##PLOT : EXTREME POVERTY 
ggplot(dat.1) +
       aes(y = ave.poverty, x = continent, fill = year) + 
       geom_bar(
       stat  ='identity', position = "dodge",
       width = 0.6
      )+ 
      labs(
       title = "Extreme poverty",
       caption = "data::  https://covid.ourworldindata.org/",
       y        = " ",
       x        = " ", 
       fill     = " "
     ) +
     scale_fill_grey(
      label    = c("Year 2020","Year 2021")
     ) + 
     theme_linedraw()+
     theme(
       axis.text.x = element_text(size = 10, colour = "black"),
       axis.text.y = element_text(size = 10, colour = "black")
     )



