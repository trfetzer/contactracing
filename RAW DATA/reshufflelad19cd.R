#reshuffle missing case measure
library(Laurae)


setwd("/Users/thiemo/Dropbox/Research/Impact of contact tracing/11 Replication/")
DAT<-read.dta(file="ANALYSIS DATA/VINTAGE-CROSS-2020-11-15-lad19cd.dta")
DAT<-data.table(DAT)

DAT[, missed2027 := missedcases20200920+missedcases20200921+missedcases20200922+missedcases20200923+missedcases20200924+missedcases20200925+missedcases20200926+missedcases20200927]


DAT <-DTUniqueBy(DAT, "lad19cd")

TMP<-DAT[, list(rgn11cd, lad19cd, utla19cd, nuts306cd, nuts206cd, missed2027)]


set.seed(20201119)

SAMPLES<-lapply(1:100, function(x) sample(TMP$missed2027, nrow(TMP)) )

for(i in 1:length(SAMPLES)) { 

TMP[, temp:= SAMPLES[[i]]]

setnames(TMP, "temp",paste("reshuffled", i, sep=""))
}


set.seed(20201120)

#reshuffle within region
RGN<-TMP[, .N, by=rgn11cd]$rgn11cd

OUT<-NULL

for(jj in 1:length(RGN)) {

TMP.REG<-TMP[rgn11cd==RGN[jj]]
SAMPLES<-lapply(1:100, function(x) data.frame(rgn11cd=RGN[jj], "temp"=sample(TMP.REG$missed2027, nrow(TMP.REG)) ))

SAMPLES.WIDE<-SAMPLES[[1]]
setnames(SAMPLES.WIDE, "temp", "reshufflereg1")
for(k in 2:length(SAMPLES)) {

SAMPLES.WIDE<-cbind(SAMPLES.WIDE, "temp"=SAMPLES[[k]]$temp)
setnames(SAMPLES.WIDE, "temp", paste("reshufflereg",k, sep=""))

}

OUT[[jj]] <- SAMPLES.WIDE

}

TMP<-cbind(TMP, rbindlist(OUT))



set.seed(20201121)

#reshuffle within region
RGN<-TMP[, .N, by=nuts206cd]$nuts206cd

OUT<-NULL

for(jj in 1:length(RGN)) {

TMP.REG<-TMP[nuts206cd==RGN[jj]]
SAMPLES<-lapply(1:100, function(x) data.frame(nuts206cd=RGN[jj], "temp"=sample(TMP.REG$missed2027, nrow(TMP.REG)) ))

SAMPLES.WIDE<-SAMPLES[[1]]
setnames(SAMPLES.WIDE, "temp", "reshufflenuts1")
for(k in 2:length(SAMPLES)) {

SAMPLES.WIDE<-cbind(SAMPLES.WIDE, "temp"=SAMPLES[[k]]$temp)
setnames(SAMPLES.WIDE, "temp", paste("reshufflenuts",k, sep=""))

}

OUT[[jj]] <- SAMPLES.WIDE

}

TMP<-cbind(TMP, rbindlist(OUT))


write.dta(TMP, file="ANALYSIS DATA/RESHUFFLED.dta")
