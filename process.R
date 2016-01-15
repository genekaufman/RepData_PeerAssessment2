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
data_pass1 <- data_raw %>%
  select(one_of(wanted_fields))

# first pass - filter out records that have at least 1 FATALITY, 1 INJURY,
# or at least $1 Property or Crop damage
data_pass1a <- data_pass1 %>%
  filter(FATALITIES > 0 |
           INJURIES > 0 |
           PROPDMG > 0 |
           CROPDMG > 0)

# filter out unmatchable records
# data_pass2 <- data_pass1a %>%
#   filter(!grepl("WET ",EVTYPE))  %>%
#   filter(!grepl("\\?",EVTYPE))  %>%
#   filter(!grepl("APACHE ",EVTYPE))  %>%
#   filter(!grepl("MISHAP",EVTYPE))  %>%
#   filter(!grepl("ACCIDENT",EVTYPE))  %>%
#   filter(!grepl("RECORD ",EVTYPE))
data_pass2 <- data_pass1a
data_pass2a<-data_pass2

########## START EVENT TYPE MUNGING #########
# Strategy to clean up messy event types:
# 1. Records with events that cannot be matched at all will be ignored
# 2. Attempt to reduce number of distinct events by removing non-letters,
#     clean up spacing, expand abbreviations and fix obvious misspellings
# 3. Explicitly replace events that are mentioned in the NWS documentation
#     as being counted as another event
# 4. Loop through official event list, replacing record events if the official
#     event is a substring of the recorded event (e.g. "TORNADO F1" -> "TORNADO")

message("[Initial] #Levels :: ", length(levels(data_pass2$EVTYPE)))
data_pass2<-droplevels(data_pass2) # drop unused levels
message("[Drop 1 ] #Levels :: ", length(levels(data_pass2$EVTYPE)))

# load master event types
et <- read.delim("Event_Types.txt",header = FALSE,col.names = "EventType")
et$EventType <- as.factor(toupper(et$EventType))
#levels(data_pass2$EVTYPE) <- as.factor(str_trim(unique(c(levels(data_pass2$EVTYPE),levels(et)))))

data_pass2<-droplevels(data_pass2) # drop unused levels

# some basic replacements to standardize some terms and misspellings
levels(data_pass2$EVTYPE) <- gsub("  "," ",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("\\\\","/",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub(" AND ","/",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub(" FLD$","",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("/ ","/",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("//","/",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("/SML","/SMALL",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("/SMALL"," SMALL",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("AVALANCE","AVALANCHE",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("COASTALSTORM","COASTAL STORM",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("HVY","HEAVY",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("ICE ON ROAD","ICY ROADS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("ICE ROADS","ICY ROADS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("BLACK ICE","ICY ROADS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("LAKE EFFECT","LAKE-EFFECT",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("LIGHTING","LIGHTNING",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("LIGNTNING","LIGHTNING",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("PRECIP$","PRECIPITATION",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("SQUALL$","SQUALLS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUDER","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNDEER","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNDERE","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNDERT","THUNDERS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNER","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNDERTSORM","THUNDERSTORM",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNDERSORM","THUNDERSTORM",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("TORNDAO","TORNADO",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("TROM","TORM",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("TSTM","THUNDERSTORM",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("TUNDER","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("UNSEASONABLE","UNSEASONABLY",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("WINDCHILL","WIND CHILL",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("WINDS","WIND",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("COLD/WIND","COLD/WIND CHILL",levels(data_pass2$EVTYPE))

# Special case - explicitly renamed in NWS documentation
levels(data_pass2$EVTYPE) <- gsub("LANDSLIDE","DEBRIS FLOW",levels(data_pass2$EVTYPE))

# Special cases - explicitly mentioned in event type description in NWS documentation
data_pass2$EVTYPE[grepl("HYPOTHERMIA",data_pass2$EVTYPE)]<-as.factor("COLD/WIND CHILL")
data_pass2$EVTYPE[grepl("SLIDE",data_pass2$EVTYPE)]<-as.factor("DEBRIS FLOW")
data_pass2$EVTYPE[grepl("FREEZE",data_pass2$EVTYPE)]<-as.factor("FROST/FREEZE")
levels(data_pass2$EVTYPE) <- gsub("ASTRONOMICAL HIGH TIDE","STORM SURGE/TIDE",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("HIGH TIDES","STORM SURGE/TIDE",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("STORM SURGE","STORM SURGE/TIDE",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("BEACH EROSION","HIGH SURF",levels(data_pass2$EVTYPE))
data_pass2$EVTYPE[grepl("SURF",data_pass2$EVTYPE)]<-as.factor("HIGH SURF")
levels(data_pass2$EVTYPE) <- gsub("DROWNING","RIP CURRENT",levels(data_pass2$EVTYPE))
data_pass2$EVTYPE[grepl("BURST",data_pass2$EVTYPE)]<-as.factor("THUNDERSTORM WIND")
data_pass2$EVTYPE[grepl("GUSTNADO",data_pass2$EVTYPE)]<-as.factor("THUNDERSTORM WIND")
data_pass2$EVTYPE[grepl("LANDSPOUT",data_pass2$EVTYPE)]<-as.factor("TORNADO")
data_pass2$EVTYPE[grepl("FIRE",data_pass2$EVTYPE)]<-as.factor("WILDFIRE")
data_pass2$EVTYPE[grepl("WINTRY",data_pass2$EVTYPE)]<-as.factor("WINTER WEATHER")
data_pass2$EVTYPE[grepl("DAM BREAK",data_pass2$EVTYPE)]<-as.factor("FLASH FLOOD")

data_pass2<-droplevels(data_pass2) # drop unused levels
message("[Drop 2 ] #Levels :: ", length(levels(data_pass2$EVTYPE)))

# temporarily rename MARINE THUNDERSTORM WIND to mtsw so that we don't lose it
levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),"mtsw")
data_pass2$EVTYPE[grepl("MARINE THUNDERSTORM WIND",data_pass2$EVTYPE)]<-as.factor("mtsw")

# this is specifically NOT a thunderstorm, so rename it
data_pass2$EVTYPE[grepl("NON[- ]THUNDERSTORM WIND",data_pass2$EVTYPE)]<-as.factor("HIGH WIND")

# some special cases where the official event doesn't match up exactly
levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),"HURRICANE (TYPHOON)")
data_pass2$EVTYPE[grepl("HURRICANE",data_pass2$EVTYPE)]<-as.factor("HURRICANE (TYPHOON)")
data_pass2$EVTYPE[grepl("THUNDERSTORM",data_pass2$EVTYPE)]<-as.factor("THUNDERSTORM WIND")

# restore MARINE THUNDERSTORM WIND
data_pass2$EVTYPE[grepl("mtsw",data_pass2$EVTYPE)]<-as.factor("MARINE THUNDERSTORM WIND")

# cycle through the official event list, and rename the recorded events to match
#   the official one IF the official one is a substring of the recorded event
for(i in 1:nrow(et)) {
  thisone<-levels(et$EventType)[i]
  levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),thisone)
  data_pass2$EVTYPE[grepl(thisone,data_pass2$EVTYPE)]<-as.factor(thisone)
}

data_pass2<-droplevels(data_pass2) # drop unused levels
message("[Drop 3 ] #Levels :: ", length(levels(data_pass2$EVTYPE)))
########## FINISH EVENT TYPE MUNGING #########

EVTYPE_TABLE <- data.frame(levels(data_pass2$EVTYPE))
unmatched<-data.frame(setdiff(data_pass2$EVTYPE, et$EventType))
names(unmatched)<-c("EVTYPE")

levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),"_ALL_OTHERS")
for(i in 1:nrow(unmatched)) {
  thisone<-levels(unmatched$EVTYPE)[i]
  levels(data_pass2$EVTYPE)[levels(data_pass2$EVTYPE)==thisone] <- "_ALL_OTHERS"
#  levels(data_pass2$EVTYPE) <- gsub(thisone,"_ALL_OTHERS",levels(data_pass2$EVTYPE))
}
data_pass2<-droplevels(data_pass2) # drop unused levels
message("[Final  ] #Levels :: ", length(levels(data_pass2$EVTYPE)))

#### NEED TO FIX DMG NUMBERS FIRST!
####

replaceMultiplier <- function(x){
  #  print(paste("2 x:",x))
  ret<-0
  x_num <- suppressWarnings(as.numeric(x))
  #  print(paste("5 x_num:",x_num))
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
    #    print(paste("17 ret:",ret))

  } else { # anything else is garbage, set to zero so that calculation is zero
    ret<-0
  }
#   ret<-paste(x," -> ",ret)
#   print(ret)
  ret
}

bad.prop<-data_pass2 %>%
#filter(PROPDMGEXP == "-" | PROPDMGEXP == "+" | PROPDMGEXP == "") %>%
group_by(PROPDMGEXP) %>%
  summarize(ps = sum(PROPDMG), pc = n(), pm = mean(PROPDMG))

bad.crop<-data_pass2 %>%
#  filter(CROPDMGEXP == "-" | CROPDMGEXP == "+" | CROPDMGEXP == "") %>%
  group_by(CROPDMGEXP) %>%
  summarize(cs = sum(CROPDMG), cc = n(), cm = mean(CROPDMG))

data_pass3 <- data_pass2
# %>%
#   mutate(PROPMULT=as.character(PROPDMGEXP), CROPMULT=as.character(CROPDMGEXP))
#
data_pass3$PROPMULT<-
  sapply(as.character(data_pass3$PROPDMGEXP), FUN=replaceMultiplier)
#data_pass3$PROPMULT<-replaceMultiplier(data_pass3$PROPDMGEXP)
 # sapply(as.character(data_pass3$PROPDMGEXP), FUN=replaceMultiplier)
data_pass3$PROPVAL<-data_pass3$PROPDMG * data_pass3$PROPMULT

#data_pass3$CROPMULT<-replaceMultiplier(data_pass3$CROPDMGEXP)

data_pass3$CROPMULT<-
  sapply(as.character(data_pass3$CROPDMGEXP), FUN=replaceMultiplier)
data_pass3$CROPVAL<-data_pass3$CROPDMG * data_pass3$CROPMULT

#
# levels(data_pass3$PROPMULT) = c(levels(data_pass3$PROPMULT),100)
# data_pass3$PROPMULT[data_pass2$PROPDMGEXP=="H"]<-as.factor(100)
#
# data_pass3$PROPMULT[data_pass2$PROPDMGEXP=="H"]<-as.factor(100)

#sapply(data_pass3$PROPMULT,FUN = replaceMultiplier)

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