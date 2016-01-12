data_file_url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
data_file_local<-"storm.data.csv.bz2"
data_file_dir<-"data/"
data_file_path<-paste0(data_file_dir,data_file_local)
if (!file.exists(data_file_path)) {
  download.file(data_file_url,data_file_path,mode = "wb")
}
# no need to unzip it - bz2 files can be read directly by read.csv and read.table
if (!exists("data_raw")) {
  data_raw <- read.csv(data_file_path)
}
library(dplyr)
# Many entries for EVTYPE, PROPDMGEXP and CROPDMGEXP are duplicated with different cases, set all to upper case
data_raw$EVTYPE <- as.factor(toupper(data_raw$EVTYPE))
data_raw$PROPDMGEXP <- as.factor(toupper(data_raw$PROPDMGEXP))
data_raw$CROPDMGEXP <- as.factor(toupper(data_raw$CROPDMGEXP))

wanted_fields <- c("BGN_DATE","STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROP","CROPDMGEXP")
wanted_fields <- c("BGN_DATE","STATE","EVTYPE")
data_pass1<- data_raw %>%
  select(one_of(wanted_fields))

# Population health = FATALITIES, INJURIES

# economic consequences: PROPDMG, CROPDMG hold dollar amount, PROPDMGEXP, CROPDMGEXP
# hold character showing magnitude of PROPDMG/CROPDMG (i.e. "B","H")
