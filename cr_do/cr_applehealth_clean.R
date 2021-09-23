## Apple health data ##

# https://www.ryanpraski.com/apple-health-data-how-to-export-analyze-visualize-guide/ 
# https://github.com/deepankardatta/AppleHealthAnalysis
# https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d

# SECTION A: CONVERT XML DATA #

rm(list = ls())
library(dplyr)
library(ggplot2)
library(lubridate)
#library(XML)


## SECTION A: CONVERT XML DATA ####
#
#getwd()
### load apple health export.xml file ====
#  xml <- xmlParse("/Users/gerardchung/Google Drive/longitudinal_datasets_USA   /apple_healthdata#/data/apple_health_export/export.xml")
#
### transform xml file to data frame - select the Record rows from the xml file ====
#df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
#
### save as RData ====
#getwd()
#save(df, file = "data/applehealth.RData" )

rm(list = ls())
load("data/applehealth.RData")

str(df)
tibble(df)

# SECTION B: CLEAN VARS ####
summary(df)
unique(df$type)
unique(df$sourceName)
unique(df$sourceVersion)
unique(df$unit)
unique(df$creationDate)
unique(df$startDate)


## make value variable numeric ====
df$value <- as.numeric(as.character(df$value))
df$value <- as.numeric(df$value)
range(df$value , na.rm = T)
mean(df$value , na.rm = T)
sd(df$value , na.rm = T)
str(df)

## make endDate in a date time variable POSIXct using lubridate with eastern time zone ====
df$endDate <-ymd_hms(df$endDate,tz="America/New_York")
str(df)

## add in year month date dayofweek hour columns ====
    # the following will extract the dates and create a numerical var
df$month<-format(df$endDate,"%m")
df$year<-format(df$endDate,"%Y")
df$date<-format(df$endDate,"%Y-%m-%d")
df$dayofweek <-wday(df$endDate, label=TRUE, abbr=FALSE)
df$hour <-format(df$endDate,"%H")
str(df)

## Clean type varible ====
library(stringr)
unique(df$type)

#str_view_all(df$type[1:10], pattern = "(HKQuantityTypeIdentifier)")
df$type <- 
    str_replace_all(df$type, 
                pattern = "(HKQuantityTypeIdentifier)|(HKCategoryTypeIdentifier)",
                replacement = "")
unique(df$type)




## show steps by month by year using dplyr then graph using ggplot2 ====
unique(df$type)
class(df$month)

df %>%
    filter(year > 2017) %>%
    filter(type == 'StepCount') %>%
    group_by(year,month) %>%
    summarize(steps=sum(value)) %>%
    #print table steps by month by year
    print (n=100) %>%
    ungroup() %>% 
    #graph data by month by year
    ggplot(aes(x=month, y=steps, fill=year)) + 
    geom_bar(position='dodge', stat='identity') +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_brewer() +
    theme_bw() +  
    theme(panel.grid.major = element_blank()) 




df1 <- 
    df %>% 
    filter(year > 2018) %>% 
    filter(type == 'StepCount') %>%
    group_by(year,month) %>% 
    summarize(steps=sum(value)) %>% 
    ungroup() 


#p <- ggplot(data = df1, aes(x = month, fill = year))
#p + geom_area(aes(y = steps))

unique(df1$month)
df1$month <- factor(df1$month,
                    labels = c("Jan",
                               "Feb",
                               "Mar",
                               "Apr",
                               "May",
                               "Jun",
                               "Jul",
                               "Aug",
                               "Sep",
                               "Oct",
                               "Nov",
                               "Dec"))


# line graph
p <- 
    df1 %>% 
    filter(month!= "Dec" & month != "Nov" & month != "Oct") %>%
    ggplot(df1, mapping = aes(x = month, y = steps, group = year))

p + geom_line(aes(color = year)) +
    scale_y_continuous("# of steps",
                       labels = scales::comma) +
    theme_minimal()





df1_wide <- 
    df1 %>% 
    filter(month!= "Dec" & month != "Nov" & month != "Oct" & month != "Sep") %>%
    tidyr::pivot_wider(values_from ="steps", names_from = "year", names_prefix = "yr")

# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Stacked%20Area%20Chart
# Stacked Area Chart
p <- ggplot(df1_wide, aes( x = as.numeric(month)))

library(showtext)
# You will need to have internet connection
# If you restart R you will need to execute this code again to use the font
font_add_google(name = "Roboto Condensed",   # Name of the font on the Google Fonts site
                family = "Roboto Condensed") # Name you want to use to call the font

font_add_google(name = "Noto Sans",   # Name of the font on the Google Fonts site
                family = "Noto Sans") # Name you want to use to call the font


# Load the fonts for all graphic devices
showtext_auto() # have to do this everytime

p + geom_area(aes(y = yr2019,
                  fill = "2019"), # this fill is the name 
              alpha = .6, 
              color = "black") +
    geom_area(aes(y = yr2020, 
                  fill = "2020"), 
              alpha = .7,
              color = "darkred") +
    geom_area(aes(y = yr2021, 
                  fill = "2021"), 
              alpha = .8,
              color = "darkblue") +
    labs(title="Number of steps monthly from 2019-2021",
         subtitle = "Exploring my health data",
         caption = "Codes: https://github.com/gerardchung/apple_healthdata | gerardchung.com"
         ) +
    scale_y_continuous("# of steps",
                       labels = scales::comma) +
    scale_x_continuous("Month",
                      breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                      labels = c("Jan",
                                 "Feb",
                                "Mar",
                                "Apr",
                                "May",
                                "Jun",
                                "Jul",
                                "Aug",
                                "Sep",
                                "Oct",
                                "Nov",
                                "Dec")) +
    theme_minimal() +
    theme(text = element_text(family = "Noto Sans"),
          plot.title = element_text(face="bold", size = 28),
          plot.caption  = element_text(size = 15, family = "Noto Sans", color = "#595959"),
          axis.text.y = element_text(size = 15,  family = "Noto Sans", color = "black"),
          axis.text.x = element_text(size = 15,  family = "Noto Sans", color = "black"),
          axis.title.x = element_text(size = 15,family = "Noto Sans", color = "black"),
          axis.title.y = element_text(size = 15,family = "Noto Sans", color = "black")) +
    scale_fill_manual(name = "", values=c('#999999','#E69F00', '#56B4E9')
                       ) + 
    geom_text(x=5, y=435300, 
              label="May 2021: Lots of walking at DisneyWrld",
              size = 5,fontface = "plain",
              family = "Noto Sans") -> plot_final

getwd()
ggsave("plots/stepsmthly.png", plot = plot_final, type = 'cairo', width = 4, height = 4, dpi = 300, units = "in", bg = "#ffffff")    


    

#heatmap day of week hour of day
df %>%
    filter(type == 'StepCount') %>%
    filter(year > 2018) %>% 
    group_by(date,dayofweek,hour) %>% 
    summarize(steps=sum(value)) %>% 
    group_by(hour,dayofweek) %>% 
    summarize(steps=sum(steps)) %>% 
    arrange(desc(steps)) %>%
    #print table steps by date by month by year
    print (n=100) %>%
    ggplot(aes(x=dayofweek, y=hour, fill=steps)) + 
    geom_tile() + 
    scale_fill_continuous(labels = scales::comma, low = 'white', high = 'red') +
    theme_classic() + 
    theme(
        panel.grid.major = element_blank(),
        text = element_text(family = "Noto Sans"),
        plot.title = element_text(face="bold", size = 20),
        plot.caption  = element_text(size = 8, family = "Noto Sans", color = "#595959"),
        axis.text.y = element_text(size = 10,  family = "Noto Sans", color = "black"),
        axis.text.x = element_text(size = 10,  family = "Noto Sans", color = "black"),
        axis.title.x = element_text(size = 10,family = "Noto Sans", color = "black"),
        axis.title.y = element_text(size = 10,family = "Noto Sans", color = "black") 
    ) + 
    labs(title = "Number of steps moved from 2019-2021",
         subtitle = "Heatmap to identify time and day of week for most/least steps",
         caption = "Codes: https://github.com/gerardchung/apple_healthdata | gerardchung.com",
         x = "Day of the week",
         y = "time of the day") -> plot_final1

getwd()
ggsave("plots/heatmap_steps.png", plot = plot_final1, type = 'cairo', width = 3, height = 3, dpi = 300, units = "in", bg = "#ffffff")  

