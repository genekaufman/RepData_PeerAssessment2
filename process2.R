
bigamount<-function(amt) {
  if(length(amt)>1) {
    amt=amt[2]
  }

  if (amt > 10^9) {
    lpart<-format(as.integer(amt/(10^9)),scientific=FALSE)
    ret<-paste0(lpart,"B")
  } else if (amt > 10^6) {
    lpart<-format(as.integer(amt/(10^6)),scientific=FALSE)
    ret<-paste0(lpart,"M")
  } else if (amt > 10^3) {
    lpart<-format(as.integer(amt/(10^3)),scientific=FALSE)
    ret<-paste0(lpart,"K")
  } else {
    ret<-format(amt,scientific = FALSE)
  }
  ret
}

data_by_sumF <- data_crunch %>%
  arrange(desc(sumF))
data_by_sumF<-data_by_sumF[1:10,]

data_by_sumI <- data_crunch %>%
  arrange(desc(sumI))
data_by_sumI<-data_by_sumI[1:10,]

data_by_sumPC <- data_crunch %>%
  arrange(desc(sumPC))
data_by_sumPC<-data_by_sumPC[1:10,]

data_by_sumP <- data_crunch %>%
  arrange(desc(sumP))
data_by_sumP<-data_by_sumP[1:10,]

data_by_sumC <- data_crunch %>%
  arrange(desc(sumC))
data_by_sumC<-data_by_sumC[1:10,]

library(ggplot2)

thisbreaks=seq(0,7000,1000)
thislabels=lapply(thisbreaks,FUN = bigamount)
plot_sumF<-ggplot(data_by_sumF, aes(EVTYPE,sumF)) +
  geom_bar(stat="identity") +
  ggtitle("Total Fatalities by Weather Event Type") +
  xlab("Weather Event") +
  scale_y_continuous(name="Fatalities",
                     breaks=thisbreaks,
                     labels=thislabels) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.text.y=element_text(angle=0))
print(plot_sumF)

thisbreaks=seq(0,100000,10000)
thislabels=lapply(thisbreaks,FUN = bigamount)
plot_sumI<-ggplot(data_by_sumI, aes(EVTYPE,sumI)) +
  geom_bar(stat="identity") +
  ggtitle("Total Injuries by Weather Event Type") +
  xlab("Weather Event") +
  scale_y_continuous(name="Injuries",
                     breaks=thisbreaks,
                     labels=thislabels) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.text.y=element_text(angle=0))
print(plot_sumI)


maxPC <- max(data_by_sumPC$sumPC)
thisbreaks=seq(0,10^11,10^10)
thislabels=lapply(thisbreaks,FUN = bigamount)
plot_sumPC<-ggplot(data_by_sumPC, aes(EVTYPE,sumPC)) +
  geom_bar(stat="identity") +
  ggtitle("Total Property + Crop Damage by Weather Event Type") +
  xlab("Weather Event") +
  scale_y_continuous(name="US $",
                     breaks=thisbreaks,
                     labels=thislabels
                     ) +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.text.y=element_text(angle=0))
print(plot_sumPC)
