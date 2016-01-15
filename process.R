starttime <- proc.time()
data_file_url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
data_file_local<-"storm.data.csv.bz2"
data_file_dir<-"data/"
if (!dir.exists(data_file_dir)) {
  dir.create(data_file_dir)
}
data_file_path<-paste0(data_file_dir,data_file_local)
if (!file.exists(data_file_path)) {
  download.file(data_file_url,data_file_path,mode = "wb")
}
# no need to unzip it - bz2 files can be read directly by read.csv and read.table
if (!exists("data_raw")) {
  data_raw <- read.csv(data_file_path)
}
library(dplyr)
library(stringr)
# Many entries for EVTYPE, PROPDMGEXP and CROPDMGEXP are duplicated with different cases, set all to upper case
data_raw$EVTYPE <- as.factor(toupper(data_raw$EVTYPE))
data_raw$PROPDMGEXP <- as.factor(toupper(data_raw$PROPDMGEXP))
data_raw$CROPDMGEXP <- as.factor(toupper(data_raw$CROPDMGEXP))

# first, remove all fields, keeping only the ones that we'll need
wanted_fields <- c("BGN_DATE","STATE","EVTYPE","FATALITIES","INJURIES",
                   "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
data_raw_wanted_fields <- data_raw %>%
  select(one_of(wanted_fields))

# first pass - filter out records that have at least 1 FATALITY, 1 INJURY,
# or at least $1 Property or Crop damage
data_filtered <- data_raw_wanted_fields %>%
  filter(FATALITIES > 0 |
           INJURIES > 0 |
           PROPDMG > 0 |
           CROPDMG > 0)

data_munged <- data_filtered

## Fix a data error in one record - this one was verified independently to be $115M, not $115B
## Credit: Robert Carman,
##  Class Discussion forums: https://www.coursera.org/learn/reproducible-research/discussions/DrCe_bl7EeWjxw7W9fJX5Q
data_munged$PROPDMGEXP[data_munged$PROPDMGEXP == 'B' & data_munged$BGN_DATE == '1/1/2006 0:00:00']<- as.factor("M")


########## START EVENT TYPE MUNGING #########
# Strategy to clean up messy event types:
# 1. Records with events that cannot be matched at all will be ignored
# 2. Attempt to reduce number of distinct events by removing non-letters,
#     clean up spacing, expand abbreviations and fix obvious misspellings
# 3. Explicitly replace events that are mentioned in the NWS documentation
#     as being counted as another event
# 4. Loop through official event list, replacing record events if the official
#     event is a substring of the recorded event (e.g. "TORNADO F1" -> "TORNADO")

message("[Initial] #Levels :: ", length(levels(data_munged$EVTYPE)))
data_munged<-droplevels(data_munged) # drop unused levels
message("[Drop 1 ] #Levels :: ", length(levels(data_munged$EVTYPE)))

# load master event types
et <- read.delim("Event_Types.txt",header = FALSE,col.names = "EventType")
et$EventType <- as.factor(toupper(et$EventType))

data_munged<-droplevels(data_munged) # drop unused levels

# some basic replacements to standardize some terms and misspellings
levels(data_munged$EVTYPE) <- gsub("  "," ",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("\\\\","/",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub(" AND ","/",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub(" FLD$","",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("/ ","/",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("//","/",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("/SML","/SMALL",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("/SMALL"," SMALL",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("AVALANCE","AVALANCHE",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("COASTALSTORM","COASTAL STORM",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("HVY","HEAVY",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("ICE ON ROAD","ICY ROADS",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("ICE ROADS","ICY ROADS",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("BLACK ICE","ICY ROADS",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("LAKE EFFECT","LAKE-EFFECT",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("LIGHTING","LIGHTNING",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("LIGNTNING","LIGHTNING",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("PRECIP$","PRECIPITATION",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("SQUALL$","SQUALLS",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("THUDER","THUNDER",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("THUNDEER","THUNDER",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("THUNDERE","THUNDER",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("THUNDERT","THUNDERS",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("THUNER","THUNDER",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("THUNDERTSORM","THUNDERSTORM",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("THUNDERSORM","THUNDERSTORM",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("TORNDAO","TORNADO",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("TROM","TORM",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("TSTM","THUNDERSTORM",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("TUNDER","THUNDER",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("UNSEASONABLE","UNSEASONABLY",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("WINDCHILL","WIND CHILL",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("WINDS","WIND",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("COLD/WIND","COLD/WIND CHILL",levels(data_munged$EVTYPE))

# Special case - explicitly renamed in NWS documentation
levels(data_munged$EVTYPE) <- gsub("LANDSLIDE","DEBRIS FLOW",levels(data_munged$EVTYPE))

# Special cases - explicitly mentioned in event type description in NWS documentation
data_munged$EVTYPE[grepl("HYPOTHERMIA",data_munged$EVTYPE)]<-as.factor("COLD/WIND CHILL")
data_munged$EVTYPE[grepl("SLIDE",data_munged$EVTYPE)]<-as.factor("DEBRIS FLOW")
data_munged$EVTYPE[grepl("FREEZE",data_munged$EVTYPE)]<-as.factor("FROST/FREEZE")
levels(data_munged$EVTYPE) <- gsub("ASTRONOMICAL HIGH TIDE","STORM SURGE/TIDE",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("HIGH TIDES","STORM SURGE/TIDE",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("STORM SURGE","STORM SURGE/TIDE",levels(data_munged$EVTYPE))
levels(data_munged$EVTYPE) <- gsub("BEACH EROSION","HIGH SURF",levels(data_munged$EVTYPE))
data_munged$EVTYPE[grepl("SURF",data_munged$EVTYPE)]<-as.factor("HIGH SURF")
levels(data_munged$EVTYPE) <- gsub("DROWNING","RIP CURRENT",levels(data_munged$EVTYPE))
data_munged$EVTYPE[grepl("BURST",data_munged$EVTYPE)]<-as.factor("THUNDERSTORM WIND")
data_munged$EVTYPE[grepl("GUSTNADO",data_munged$EVTYPE)]<-as.factor("THUNDERSTORM WIND")
data_munged$EVTYPE[grepl("LANDSPOUT",data_munged$EVTYPE)]<-as.factor("TORNADO")
data_munged$EVTYPE[grepl("FIRE",data_munged$EVTYPE)]<-as.factor("WILDFIRE")
data_munged$EVTYPE[grepl("WINTRY",data_munged$EVTYPE)]<-as.factor("WINTER WEATHER")
data_munged$EVTYPE[grepl("DAM BREAK",data_munged$EVTYPE)]<-as.factor("FLASH FLOOD")

data_munged<-droplevels(data_munged) # drop unused levels
message("[Drop 2 ] #Levels :: ", length(levels(data_munged$EVTYPE)))

# temporarily rename MARINE THUNDERSTORM WIND to mtsw so that we don't lose it
levels(data_munged$EVTYPE) = c(levels(data_munged$EVTYPE),"mtsw")
data_munged$EVTYPE[grepl("MARINE THUNDERSTORM WIND",data_munged$EVTYPE)]<-as.factor("mtsw")

# this is specifically NOT a thunderstorm, so rename it
data_munged$EVTYPE[grepl("NON[- ]THUNDERSTORM WIND",data_munged$EVTYPE)]<-as.factor("HIGH WIND")

# some special cases where the official event doesn't match up exactly
levels(data_munged$EVTYPE) = c(levels(data_munged$EVTYPE),"HURRICANE (TYPHOON)")
data_munged$EVTYPE[grepl("HURRICANE",data_munged$EVTYPE)]<-as.factor("HURRICANE (TYPHOON)")
data_munged$EVTYPE[grepl("THUNDERSTORM",data_munged$EVTYPE)]<-as.factor("THUNDERSTORM WIND")

# restore MARINE THUNDERSTORM WIND
data_munged$EVTYPE[grepl("mtsw",data_munged$EVTYPE)]<-as.factor("MARINE THUNDERSTORM WIND")

# cycle through the official event list, and rename the recorded events to match
#   the official one IF the official one is a substring of the recorded event
for(i in 1:nrow(et)) {
  thisone<-levels(et$EventType)[i]
  levels(data_munged$EVTYPE) = c(levels(data_munged$EVTYPE),thisone)
  data_munged$EVTYPE[grepl(thisone,data_munged$EVTYPE)]<-as.factor(thisone)
}

data_munged<-droplevels(data_munged) # drop unused levels
message("[Drop 3 ] #Levels :: ", length(levels(data_munged$EVTYPE)))
########## FINISH EVENT TYPE MUNGING #########

EVTYPE_TABLE <- data.frame(levels(data_munged$EVTYPE))
unmatched<-data.frame(setdiff(data_munged$EVTYPE, et$EventType))
names(unmatched)<-c("EVTYPE")

levels(data_munged$EVTYPE) = c(levels(data_munged$EVTYPE),"_ALL_OTHERS")
for(i in 1:nrow(unmatched)) {
  thisone<-levels(unmatched$EVTYPE)[i]
  levels(data_munged$EVTYPE)[levels(data_munged$EVTYPE)==thisone] <- "_ALL_OTHERS"
#  levels(data_munged$EVTYPE) <- gsub(thisone,"_ALL_OTHERS",levels(data_munged$EVTYPE))
}
data_munged<-droplevels(data_munged) # drop unused levels
message("[Final  ] #Levels :: ", length(levels(data_munged$EVTYPE)))

####
replaceMultiplier <- function(x){
  # this function will convert the code given in PROPDMGEXP and CROPDMGEXP to the
  #   full multiplier that it represents
  #   A numeric field is an exponent of 10, while H,K,M,B represent Hundred, Thousand,
  #   Million and Billion. Anything else cannot be interpreted, so we'll set it to 0
  x_num <- suppressWarnings(as.numeric(x))
  if (is.numeric(x_num) && !(is.na(x_num))) {
    ret<-10 ** x_num
    #    print(paste("8 ret:",ret))
  } else if(x=="H") {
    ret<-100
  } else if(x=="K") {
    ret<-1000
  } else if(x=="M") {
    ret<-1000000
  } else if(x=="B") {
    ret<-1000000000
  } else { # anything else is garbage, set to zero so that calculation is zero
    ret<-0
  }
  ret
}

data_pass3 <- data_munged

data_pass3$PROPMULT<-
  sapply(as.character(data_pass3$PROPDMGEXP), FUN=replaceMultiplier)
data_pass3$PROPVAL<-data_pass3$PROPDMG * data_pass3$PROPMULT

data_pass3$CROPMULT<-
  sapply(as.character(data_pass3$CROPDMGEXP), FUN=replaceMultiplier)
data_pass3$CROPVAL<-data_pass3$CROPDMG * data_pass3$CROPMULT


# crunch it up
data_crunch <- data_pass3 %>%
group_by(EVTYPE) %>%
summarise(sumF=sum(FATALITIES),
          sumI=sum(INJURIES),
          sumP=sum(PROPVAL),
          sumC=sum(CROPVAL),
          meanF=round(mean(FATALITIES),3),
          meanI=round(mean(INJURIES),3),
          meanP=round(mean(PROPVAL),3),
          meanC=round(mean(CROPVAL),3))

# Population health = FATALITIES, INJURIES

# economic consequences: PROPDMG, CROPDMG hold dollar amount, PROPDMGEXP, CROPDMGEXP
# hold character showing magnitude of PROPDMG/CROPDMG (i.e. "B","H")

# output proc time
print(proc.time() - starttime)

cleanAllButDR <- function(){
  rm(list=ls()[ls()!="data_raw" & ls()!="cleacleanAllButDR"])
}