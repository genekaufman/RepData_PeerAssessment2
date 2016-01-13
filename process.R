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
data_pass2 <- data_pass1a %>%
  filter(!grepl("WET ",EVTYPE))  %>%
  filter(!grepl("\\?",EVTYPE))  %>%
  filter(!grepl("APACHE ",EVTYPE))  %>%
  filter(!grepl("MISHAP",EVTYPE))  %>%
  filter(!grepl("ACCIDENT",EVTYPE))  %>%
  filter(!grepl("RECORD ",EVTYPE))

data_pass2a<-data_pass2

message("34:: ", length(levels(data_pass2$EVTYPE)))
data_pass2<-droplevels(data_pass2)
message("36:: ", length(levels(data_pass2$EVTYPE)))

# load master event types
et <- read.delim("Event_Types.txt",header = FALSE,col.names = "EventType")
et$EventType <- as.factor(toupper(et$EventType))
#levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),"thunderstorm")
levels(data_pass2$EVTYPE) <- as.factor(str_trim(unique(c(levels(data_pass2$EVTYPE),levels(et)))))

#data_pass2$EVTYPE[grepl("THUNDERSTORM",data_pass2$EVTYPE)]<-as.factor("thunderstorm")
data_pass2<-droplevels(data_pass2)
levels(data_pass2$EVTYPE) <- gsub("  "," ",levels(data_pass2$EVTYPE))

levels(data_pass2$EVTYPE) <- gsub("FIRES","FIRE",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("UNSEASONABLE","UNSEASONABLY",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("TUNDER","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNDERT","THUNDERS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("TORNDAO","TORNADO",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNDERE","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNDEER","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUNER","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("THUDER","THUNDER",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("WINDS","WIND",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("SQUALL$","SQUALLS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("TROM","TORM",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("SORM","STORM",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("MUD ","MUD",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("SLIDES","SLIDE",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub(" FLD","",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("/ ","/",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub(" AND ","/",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("ICE ON ROAD","ICY ROADS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("ICE ROADS","ICY ROADS",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("HVY","HEAVY",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("\\\\","/",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("//","/",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("/SML","/SMALL",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("/SMALL"," SMALL",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("COASTALSTORM","COASTAL STORM",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("AVALANCE","AVALANCHE",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("LAKE EFFECT","LAKE-EFFECT",levels(data_pass2$EVTYPE))
levels(data_pass2$EVTYPE) <- gsub("WINDCHILL","WIND CHILL",levels(data_pass2$EVTYPE))

#gsub("FIRE","FIREXXXX",levels(data_pass2$EVTYPE))
data_pass2<-droplevels(data_pass2)
message("48:: ", length(levels(data_pass2$EVTYPE)))

# temporarily rename marine t** Wind to MTW so that we don't lose them
levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),"mtsw")
data_pass2$EVTYPE[grepl("MARINE THUNDERSTORM WIND",data_pass2$EVTYPE)]<-as.factor("mtsw")
data_pass2$EVTYPE[grepl("MARINE TSTM WIND",data_pass2$EVTYPE)]<-as.factor("mtsw")

#levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),"ntw")
data_pass2$EVTYPE[grepl("NON[- ]TSTM WIND",data_pass2$EVTYPE)]<-as.factor("HIGH WIND")
#data_pass2$EVTYPE[grepl("MARINE TSTM WIND",data_pass2$EVTYPE)]<-as.factor("MTSW")

# some special cases
levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),"HURRICANE (TYPHOON)")
data_pass2$EVTYPE[grepl("HURRICANE",data_pass2$EVTYPE)]<-as.factor("HURRICANE (TYPHOON)")
data_pass2$EVTYPE[grepl("THUNDERSTORM",data_pass2$EVTYPE)]<-as.factor("THUNDERSTORM WIND")
data_pass2$EVTYPE[grepl("TSTM",data_pass2$EVTYPE)]<-as.factor("THUNDERSTORM WIND")

data_pass2$EVTYPE[grepl("LIGNTNING",data_pass2$EVTYPE)]<-as.factor("LIGHTNING")
data_pass2$EVTYPE[grepl("LIGHTING",data_pass2$EVTYPE)]<-as.factor("LIGHTNING")

data_pass2$EVTYPE[grepl("MUD",data_pass2$EVTYPE)]<-as.factor("MUDSLIDE")
data_pass2$EVTYPE[grepl("MIXED PRECIPITATION",data_pass2$EVTYPE)]<-as.factor("MIXED PRECIP")

#data_pass2$EVTYPE[gsub("  ",data_pass2$EVTYPE)]<-as.factor("MIXED PRECIP")

for(i in 1:nrow(et)) {
  thisone<-levels(et$EventType)[i]
  levels(data_pass2$EVTYPE) = c(levels(data_pass2$EVTYPE),thisone)
  data_pass2$EVTYPE[grepl(thisone,data_pass2$EVTYPE)]<-as.factor(thisone)

}


data_pass2<-droplevels(data_pass2)
message("56:: ", length(levels(data_pass2$EVTYPE)))

table(levels(data_pass2$EVTYPE))





data_pass3<-droplevels(data_pass3)

# Population health = FATALITIES, INJURIES

# economic consequences: PROPDMG, CROPDMG hold dollar amount, PROPDMGEXP, CROPDMGEXP
# hold character showing magnitude of PROPDMG/CROPDMG (i.e. "B","H")
