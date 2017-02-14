#####################################################################################################
#                                                                                                   #
# R program: FluNearYou.R                                                                           #
#                                                                                                   #
# Description: Descriptive analyses for Greater-Boston Flu near You data                            #
#                                                                                                   #
# Creates datasets: rawFNY; FNY                                                                     #
#                                                                                                   #
# Original Variables: week_of   state  zip   participants  users   household   ili                  # 
#         gastro other  no_symptoms fever cough sore_throat chills  fatigue nausea                  #
#         diarrhea  bodyache  headache                                                              #
#                                                                                                   #
#####################################################################################################
#####################################################################################################
#                                                                                                   #
# Purpose: Import and analyze Flu Near You Data                                                     #
#                                                                                                   #
# Date of Program Start: 10/27/2015                                                                 #
#                                                                                                   #
# Steps to Program:                                                                                 #
#     Step 1: Install and Import libraries                                                          #
#     Step 2: Jurisdiction Defines 3 Variables                                                      #
#     Step 3: Import data                                                                           #
#     Step 4: Clean data/ create variables                                                          #
#     Step 5: Subset summary dataset                                                                #  
#     Step 6: Export dataset                                                                        #
#                                                                                                   #
# Pre-requisite: FluNearYou data from datasets dashboard                                            #
#                                                                                                   #
#####################################################################################################


#################################################
#     Step 1: Install and Import Libraries      #
#################################################
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("plyr", "ggplot2")

ipak(packages)

#################################################
#   Step 2: Jurisdiction Defines 3 Variables    #
#################################################

# Jurisdiction defined. Place file named "FNY" in a folder and indicate path to file below
datasource<-"~/Documents/FNY/FNY_ZIPDATA_from_2012-08-27_to_2015-09-27.csv"

# Jurisdiction defined. Change state and county information to meet needs
my_state<-"MA"
my_county<-"Suffolk"

# Jurisdiction defined. List zip codes

# From CSV file (label zip code column as "ZIP")
zipsource<-"~/Documents/FNY/boston/Boston_zips.csv"
my_zips<-read.csv(zipsource, header=T)

# From list
# ZIP<-c("02310", ...)
# my_zips<-data.frame(ZIP)

# Jurisdiction defined. Time period
# Time period options: 
#  - FNY week of (FNY_week_of): FNY week starting
#  - year (FNY_yr): 2012, 2013, 2014, 2015, 2016
#  - flu season (FNY_fluseason): 2012-2013, 2013-2014, 2014-2015, 2015-2016
#  - mmwr week (FNY_mmwr_wk): 1 - 52/53
#  - mmwr yearweek(FNY_yrweek): 201234-201537

# Change time variable and value to meet needs
time_variable<-FNY$FNY_fluseason
time_value<-"2014-2015"

# If time_variable is FNY_mmwr_wk or FNY_yrweek then use time_start and time_stop in place of time_value
#time_start==201234
#time_stop==201537

# Jurisdiction defined. Regional variables
# Regional variable options: 
#  - National
#    -- FNY_national_entries: number of national entries
#    -- FNY_national_ili: number of national incidences of ili
#    -- FNY_national_per_ili: percent ili at national level
#  - State
#    -- FNY_state_entries: number of state entries
#    -- FNY_state_ili: number of state incidences of ili
#    -- FNY_state_per_ili: percent ili at state level
#  - County
#    -- FNY_county_entries: number of county entries
#    -- FNY_county_ili: number of county incidences of ili
#    -- FNY_county_per_ili: percent ili at county level

variables<-c("FNY_week_of", "FNY_yr", "FNY_mmwr_wk","FNY_yrweek","FNY_fluseason", "FNY_national_entries", "FNY_national_ili", "FNY_national_per_ili")

outsource<-"~/Documents/FNY/FNY_final.csv"

#################################################
#            Step 3: Import Data                #
#################################################

rawFNY<-read.csv(datasource, header=T)

# Rename variables with "FNY" prefix
rawFNY<-rename(rawFNY, c("week_of" = "FNY_week_of", 
                 "state" = "FNY_state",
                 "zip" = "FNY_zip",
                 "participants" = "FNY_participants",
                 "users" = "FNY_users",
                 "household" = "FNY_household",
                 "ili" = "FNY_ili",
                 "other" = "FNY_other",
                 "no_symptoms" = "FNY_no_symptoms",
                 "fever" = "FNY_fever",
                 "cough" = "FNY_cough",
                 "sore_throat" = "FNY_sorethroat",
                 "chills" = "FNY_chills",
                 "fatigue" = "FNY_fatigue",
                 "nausea" = "FNY_nausea",
                 "diarrhea" = "FNY_diarrhea",
                 "bodyache" = "FNY_bodyache",
                 "headache" = "FNY_headache"))


#################################################
#    Step 4: Clean data/ create variables       #
#################################################

# Create MMWR variable
#Format FNY_week_of as date
rawFNY$FNY_week_of<-as.Date(rawFNY$FNY_week_of, "%Y-%m-%d")
rawFNY<- rawFNY[order(rawFNY$FNY_week_of),]

datas<-function(data, level){
  #sum entries over each week
  tot<-ddply(data, c("FNY_week_of"), summarize, entries = sum(FNY_participants))
  
  #Sum ili over each week
  ilis<-ddply(data, c("FNY_week_of"), summarize, ili = sum(FNY_ili))
  
  #Merge entries and ili
  fny_ili<-merge(tot, ilis, by="FNY_week_of", all.x=T)
  
  #Calculate percent ili
  fny_ili$per_ili<-fny_ili$ili/fny_ili$entries*100
  
  #Reformat week_of as a date
  fny_ili$FNY_week_of<-as.Date(fny_ili$FNY_week_of, "%Y-%m-%d")
  
  fny_ili<-rename(fny_ili, c("entries" = as.character(paste("FNY", level, "entries", sep = "_")),
                 "ili" = as.character(paste("FNY", level, "ili", sep = "_")),
                 "per_ili" = as.character(paste("FNY", level, "per_ili", sep = "_"))))

  return(fny_ili)
}
# National
FNY_national_ili<-datas(rawFNY, "national")

# State
FNY_state<-subset(rawFNY, FNY_state==my_state)
FNY_state_ili<-datas(FNY_state, "state")

# County/ zip
# Fix formats of zip codes - if needed
rawFNY$FNY_zip<-as.numeric(as.character(rawFNY$FNY_zip))
my_zips$FNY_zip<-as.numeric(as.character(my_zips$ZIP))

# merge list with FNY
FNY_county<-merge(rawFNY, my_zips, by="FNY_zip")
FNY_county_ili<-datas(FNY_county, "county")

# merge national, state, and county data
FNY<-Reduce(function(x, y) merge(x, y, all=TRUE), list(FNY_national_ili, FNY_state_ili, FNY_county_ili))

# Create data frame to convert FNY week of to CDC week
cdcf<-function(year, num, start){
  d<-data.frame(matrix(nrow=num, ncol=3))
  d[,1]<-year
  d[1,2]<-as.Date(start,"%Y-%m-%d")
  d[1,3]<-1
  for (i in 2:num) {
    d[i,2]<-d[i-1,2]+7
    d[i,3]<-i
  }
  d<-rename(d, c("X1" = "FNY_yr",
                 "X2" = "FNY_week_of",
                 "X3" = "FNY_mmwr_wk"))
  return(d)
}

d12<-cdcf(2012, 52, "2012-01-02")
d13<-cdcf(2013, 52, "2012-12-31")
d14<-cdcf(2014, 53, "2013-12-30")
d15<-cdcf(2015, 52, "2015-01-05")
d16<-cdcf(2016, 52, "2016-01-04")

# set all years into one dataset
week<-rbind(d12, d13, d14, d15, d16)
week$FNY_week_of<-as.Date(as.numeric(week$FNY_week_of),origin = "1970-01-01")

# Merge to FNY
FNY<-merge(FNY, week, by="FNY_week_of", all.x=T)

# create MMWR year and week variable
FNY$FNY_yrweek <- c()
FNY$FNY_yrweek <- ifelse(FNY$FNY_mmwr_wk<10, as.character(paste(FNY$FNY_yr, "0", FNY$FNY_mmwr_wk, sep = "")),
                            as.character(paste(FNY$FNY_yr, FNY$FNY_mmwr_wk, sep = "")))
FNY$FNY_yrweek <- as.numeric(as.character(FNY$FNY_yrweek))

# create flu season variable
FNY$FNY_fluseason<-ifelse(FNY$FNY_yrweek>=201240 & FNY$FNY_yrweek<= 201339, "2012-2013", 
                             ifelse(FNY$FNY_yrweek>=201340 & FNY$FNY_yrweek<= 201439, "2013-2014",
                                    ifelse(FNY$FNY_yrweek>=201440 & FNY$FNY_yrweek<= 201539, "2014-2015",
                                           ifelse(FNY$FNY_yrweek>=201540 & FNY$FNY_yrweek<= 201639, "2015-2016", NA))))

#################################################
#        Step 5: Subset summary dataset         #
#################################################

FNY_final<-subset(FNY, time_variable==time_value, select=variables)

# If time_variable is FNY_mmwr_wk or FNY_yrweek
#FNY_final<-subset(FNY, time_variable>=time_start & time_variable<=time_stop, select=variables)

#################################################
#           Step 6: Export dataset              #
#################################################

write.csv(FNY_final, outsource, row.names=F)
                                                         