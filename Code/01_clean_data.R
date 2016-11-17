# Load Packages
list.of.packages <- c("pitchRx", "dplyr","RSQLite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(pitchRx)
library(RSQLite)
library(dplyr)

# Set Working Directory

my_db <- src_sqlite("Data/pitchRx.sqlite3", create = FALSE)

at_bat <- tbl(my_db, "atbat")

at_bat <- collect(at_bat)

at_bat <- at_bat %>%
  arrange(gameday_link, desc(inning_side), inning, o) %>%
  select(gameday_link, batter_name, pitcher_name, b, s, o, inning, inning_side,
         stand, p_throws, event, pitcher, batter, date)

at_bat <- at_bat %>%
  filter(!duplicated(at_bat)) 

at_bat <- at_bat %>%
  filter(!(event %in% c("Batter Interference", "Catcher Interference",
                        "Fan interference", "Runner Out")))

# Classify events into six categories, removing irrelevant events
convertEvent <- function(event){
  if (event %in% c("Walk", "Hit By Pitch", "Intent Walk")){
    return("Walk")
  }
  else if (event %in% c("Double", "Triple")){
    return("XBH")
  }
  else if (event %in% c("Single", "Home Run")){
    return(as.character(event))
  }
  else{
    return("Out")
  }
}

outcome <- sapply(at_bat$event, convertEvent)

at_bat <- at_bat %>%
  mutate(outcome = ordered(outcome, levels = 
                             c("Out", "Walk","Single","XBH",
                               "Home Run"))) %>%
  mutate(on_base = as.numeric((outcome != "Out")))

at_bat$date <- as.Date(gsub("_", "-", at_bat$date))

write.csv(at_bat, file = "Data/at_bat.csv")
    
  
  
