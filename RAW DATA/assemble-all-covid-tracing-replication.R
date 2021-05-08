library(stringr)
library(data.table)
library(plyr)
library(lubridate)
library(foreign)
library(operator.tools)
library(nlstimedist)
library(attempt)
library(zoo)
library(openxlsx)
library(rgdal)
library(rgeos)
library(ggplot2)

DTUniqueBy <- function(data, varvec) {
  data <- as.data.table(data)
  data[!duplicated(data.frame(data[, varvec, with=F]))]
}


#fix this date as the geographic attribution of cases changed in a way that we can not replicate for past data vintages after Nov 16
ddate = "2020-11-05"
setwd("/Users/thiemo/Dropbox/Research/Impact of contact tracing/11 Replication/")

ffs<-grep("\\.csv",list.files("RAW DATA/Legacy_cases_files-main", full.names=TRUE, recursive=TRUE),value=TRUE)
ffs<-setdiff(ffs, grep("NOT USE",list.files("RAW DATA/Legacy_cases_files-main", full.names=TRUE, recursive=TRUE),value=TRUE))


VINTAGES<-lapply(ffs, function(x) data.table(vintage = str_extract(x,"[0-9]{8}"), read.csv(file=x)))


VINTAGES<-lapply(VINTAGES, function(x) x[, list(vintage, Area.code, Area.type, date=Specimen.date, cases=Daily.lab.confirmed.cases)])

VINTAGES<-lapply(VINTAGES, function(x) x[Area.type %in% c("ltla","utla","Upper tier local authority","Lower tier local authority")])

VINTAGES<-rbindlist(VINTAGES)
VINTAGES[, vintage :=ymd(vintage)]
VINTAGES<-VINTAGES[vintage<=ddate]

VINTAGES[, vintage := as.character(ymd(vintage))]

VINTAGES[Area.type %in% c("Upper tier local authority","Lower tier local authority"), Area.type := "utla"]
VINTAGES[Area.type %in% c("Lower tier local authority"), Area.type := "ltla"]


#LTLA=UTLA
VINTAGES<-VINTAGES[Area.type=="ltla"]
setnames(VINTAGES, "Area.code","lad19cd")

load("RAW DATA/CROSSWALK.OA.rdata")
setnames(OA, names(OA), tolower(names(OA)))


VINTAGES <-join(VINTAGES, DTUniqueBy(OA[, .N, by=c("lad19cd","rgn11cd")][, 1:2],"lad19cd"))
VINTAGES <-join(VINTAGES, DTUniqueBy(OA[, .N, by=c("lad19cd","nuts206cd")][, 1:2],"lad19cd"))
VINTAGES <-join(VINTAGES, DTUniqueBy(OA[, .N, by=c("lad19cd","nuts306cd")][, 1:2],"lad19cd"))
VINTAGES <-join(VINTAGES, DTUniqueBy(OA[, .N, by=c("lad19cd","utla19cd")][, 1:2],"lad19cd"))
VINTAGES <-join(VINTAGES, OA[, .N, by=c("rgn11cd","rgn11nm")][, 1:2][, list(rgn11cd, rgn11name=rgn11nm)])



##ATTACH CONTACT TRACING STATISTICS
sheetnames<-c("Cases Transferred", "Cases Transferred - Completed","Cases - Completed In 24 Hours","Cases - Completed In 48 Hours","Cases - Completed In 72 Hours","Contacts Transferred","Contacts Transferred Completed","Contacts - Completed In 24 Hrs","Contacts - Completed In 48 Hrs","Contacts - Completed In 72 Hrs")
parseTNTFOIA<-function(sheet, name) {

TMP<-read.xlsx("RAW DATA/Test and Trace/FOI RESULTS/FOI 1273834.xlsx", startRow=2, sheet=sheet, colNames=FALSE)
utlas<-as.character(TMP[1,3:ncol(TMP)])

TMP<-TMP[3:nrow(TMP),2:ncol(TMP)]

setnames(TMP, names(TMP), c("week",utlas) )

DAT<-melt(data.table(TMP), id.vars="week")
setnames(DAT, c("variable","value"), c("utla", gsub("[^A-z0-9]","", name)))
DAT
}


DATS<-lapply(1:length(sheetnames), function(x) parseTNTFOIA(x, sheetnames[x]))

TNT<-DATS[[1]]
for(i in 2:length(DATS)) {

TNT<-join(TNT, DATS[[i]])


}

TNT <-join(TNT, OA[, .N, by=c("utla19cd","utla19nm")][, list(utla= utla19nm, utla19cd)])
TNT <-TNT[!is.na(utla19cd)]


#AGGRETATE TEST AND TRACE PERFORMANCE
WEEKS<-data.table(read.csv(file="RAW DATA/Test and Trace/weeks.csv"))
TNT[, from := gsub("-","",as.character(dmy(substr(week,1,8))))]
TNT[, to := gsub("-","",as.character(dmy(substr(week,11,19))))]

TNT<-join(TNT, WEEKS[, list(from, calender_week)])
TNT <-TNT[!is.na(calender_week)]

TNT[, week := NULL]
TNT[, from  := NULL]
TNT[, to  := NULL]
TNT[, utla  := NULL]

setnames(TNT, "calender_week","week")

GM<-data.table(read.csv(file="RAW DATA/google-mobility-reports-data-master/2020_GB_Region_Mobility_Report.csv"))
GM[sub_region_1=="", sub_region_1:= NA]
GM[sub_region_2=="", sub_region_2:= NA]
GM[, regid := paste(sub_region_1, sub_region_2,sep="-")]

#SHP<-readOGR(dsn="RAW DATA/google-mobility-reports-data-master/geography/",layer="google_mobility_lad_boundary_200903")
LOCS<-data.table(read.dbf(file="RAW DATA/google-mobility-reports-data-master/geography/google_mobility_lad_boundary_200903.dbf"))


IDS<-LOCS[, list(lad19cd, lad19nm, sb_rg_1, sb_rg_2)]
IDS[, regid := paste(sb_rg_1, sb_rg_2,sep="-")]

GM<-join(GM[, c("date","regid", grep("percent",names(GM),value=TRUE)), with=F],IDS[, list(lad19cd, regid)])
GM <-GM[!is.na(lad19cd)]
GM[, regid := NULL]

setnames(GM, grep("percent_change_from_baseline", names(GM), value=TRUE), gsub("percent_change_from_baseline","pctch", grep("percent_change", names(GM), value=TRUE)))


GM<-join(GM,DTUniqueBy(OA[, .N, by=c("lad19cd","nuts206cd")],"lad19cd")[, 1:2])
GM<-join(GM,DTUniqueBy(OA[, .N, by=c("lad19cd","nuts306cd")],"lad19cd")[, 1:2])
GM<-join(GM,DTUniqueBy(OA[, .N, by=c("lad19cd","utla19cd")],"lad19cd")[, 1:2])
GM<-join(GM,DTUniqueBy(OA[, .N, by=c("lad19cd","rgn11cd")],"lad19cd")[, 1:2])

GM<-GM[!is.na(utla19cd)]


load(paste("RAW DATA/Daily Deaths/DEATHS-2020-11-16.rdata",sep=""))
VINTAGES<-join(VINTAGES, DEATHS)

#DEATHS FROM ONS
ONSWEEKLY<-data.table(read.csv(file="https://download.beta.ons.gov.uk/downloads/datasets/weekly-deaths-local-authority/editions/time-series/versions/4.csv"))
ONSWEEKLY[place.of.death=="care-home", place.of.death := "carehome"]

VINTAGES[, year := year(date)]
VINTAGES[, week := week(date)]

VINTAGES[, date := as.character(date)]
VINTAGES[, td := ymd(date)]
VINTAGES[, ymd := as.numeric(gsub("-","",date))]


WEEKS<-data.table(read.csv(file="RAW DATA/Test and Trace/weeks.csv"))
VINTAGES<-join(VINTAGES, WEEKS[, list(week= calender_week, tnt_performance)])

TEST<-lapply(list.files("RAW DATA/Test and Trace/data files", full.names=TRUE), function(x) data.table(week= str_extract(x, "w[0-9]+"), read.csv(file=x)))

TEST<-rbindlist(TEST, use.names=TRUE, fill=TRUE)
TEST[, week := as.numeric(gsub("w","",week))]


MAP<-TEST[, .N, by=Upper.Tier.Local.Authority][, list(utla=Upper.Tier.Local.Authority)]

TIERS<-data.table(read.csv(file="RAW DATA/Cross walks/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(April_2019)_Lookup_in_England_and_Wales.csv"))
UTLAS<-TIERS[, .N, by=c("UTLA19CD","UTLA19NM")][, 1:2]

MAP <-join(MAP,UTLAS[, list(utla= UTLA19NM, UTLA19CD)])
MAP[utla %in% c("Cornwall & Isles of Scilly","Cornwall and Isles of Scilly"), UTLA19CD := "E06000052"]
MAP[utla %in% c("City of London & Hackney","Hackney and City of London"), UTLA19CD := "E09000012"]
MAP<-MAP[!is.na(UTLA19CD)]

TEST[, utla:=Upper.Tier.Local.Authority]
TEST<-join(TEST, MAP)
TEST <-TEST[!is.na(UTLA19CD)]
setnames(TEST, "UTLA19CD", "utla19cd")

TEST<-join(TEST, WEEKS)
TEST<-TEST[order(Category,utla19cd,week)]

TEST.REF<-TEST[Category=="People transferred to contact tracing system", list(tntcumref=sum(Total), tntcumreach=sum(Total.reached)), by=c("utla19cd","calender_week")][,  list(utla19cd,week=calender_week, tntcumref, tntcumreach)]
TEST.CON<-TEST[Category %in% c("Close contacts identified who were not managed by local health protection teams","Close contacts identified who were not managed by local health protection teams","Contacts identified who were not managed by local health protection teams","Non-complex close contacts identified"), list( tntcumcont=sum(Total), tntcumcontreach=sum(Total.reached)), by=c("utla19cd","calender_week")][, list(utla19cd,week=calender_week, tntcumcont, tntcumcontreach)]

TEST.CON<-TEST.CON[order(utla19cd,week)]
TEST.REF<-TEST.REF[order(utla19cd,week)]

TEST.REF[, l1tntcumref :=shift(tntcumref,type="lag"), by ="utla19cd"]
TEST.REF[, l1tntcumreach :=shift(tntcumreach,type="lag"), by ="utla19cd"]
TEST.REF[, dchtntcumref := tntcumref-l1tntcumref]
TEST.REF[, dchtntcumreach := tntcumreach-l1tntcumreach]

TEST.CON[, l1tntcumcont :=shift(tntcumcont,type="lag"), by ="utla19cd"]
TEST.CON[, l1tntcumcontreach :=shift(tntcumcontreach,type="lag"), by ="utla19cd"]

TEST.CON[, dchtntcumcont := tntcumcont-l1tntcumcont]
TEST.CON[, dchtntcumcontreach := tntcumcontreach-l1tntcumcontreach ]


LA<-data.table(read.xlsx("RAW DATA/Test statistics/Demographic_LA_tables_w23.xlsx", sheet=8, startRow=3))
LA<-melt(LA[, c(1,7:ncol(LA)),with=F])
LA[, start := dmy(substr(variable,1,8))]
LA[, end := dmy(substr(variable,12,20))]
LA[, week_start := week(start)]
LA[, week_end := week(end)]

LA<-LA[, list(lad19cd=LTLA,week=week_start, tests_performed=value)]


CASES<-data.table(read.xlsx("RAW DATA/Test statistics/Demographic_LA_tables_w23.xlsx", sheet=9, startRow=3))
CASES<-melt(CASES[, c(1,7:ncol(CASES)),with=F], id.vars="LTLA")
CASES[, start := dmy(substr(variable,1,8))]
CASES[, end := dmy(substr(variable,12,20))]
CASES[, week_start := week(start)]
CASES[, week_end := week(end)]

CASES<-CASES[, list(lad19cd=LTLA,week=week_start, positive_cases=value)]
CASES[, positive_cases := as.numeric(positive_cases)]



SPATIAL<-NULL
SPATIAL[[1]]<-c("lad19cd","utla19cd","nuts206cd","nuts306cd","rgn11cd")
SPATIAL[[2]]<-c("utla19cd","nuts206cd","nuts306cd","rgn11cd")
SPATIAL[[3]]<-c("nuts306cd","nuts206cd","rgn11cd")
SPATIAL[[4]]<-c("nuts206cd","rgn11cd")
SPATIAL[[5]]<-c("rgn11cd")


varagg<-c("cases","newcoviddeaths")


OUT<-NULL
for(kk in 1:length(SPATIAL[[1]])) {
splevel<-SPATIAL[[kk]]

VINTUSE<-VINTAGES[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=varagg, by=c(splevel,"vintage","date","week","td","ymd")]
VINTUSE[, day := ymd(vintage)-ymd(date)]
VINTUSE[, day := as.numeric(day)]
VINTUSE[, max_cases := max(cases,na.rm=TRUE), by=c(splevel,"date")]
VINTUSE[, propMax := cases/max_cases]



#THE VERY NAIVE ESTIMATION 
COLLAPSE<-VINTUSE[, list(cases=sum(cases)), by=c("vintage","date","day")]
COLLAPSE[, vintage := ymd(vintage)]
COLLAPSE[, max_cases := max(cases), by=c("date")]
COLLAPSE[, day := ymd(vintage)-ymd(date)]
COLLAPSE[, day := as.numeric(day)]
COLLAPSE[, propMax := cases/max_cases]


TMP<-COLLAPSE[date<="2020-09-19"][month(date)==9][, list(propMax =round(mean(propMax),digits=3)), by=day][order(day)][day<=14]

model <- timedist(
  TMP, x = "day", y = "propMax", r = 0.2, c = 0.5, t = 5
)
TMP[, fitted := predict(model, TMP)]


TMP2<-COLLAPSE[vintage=="2020-10-02"][date>="2020-09-20" & date<="2020-10-01"][, list(propMax =round(mean(propMax),digits=3)), by=day][order(day)][day<=14]
TMP2[, fitted := predict(model, TMP2)]


TMP[, fitted2 :=  1 - (1 - (0.3904/(1 + exp(-1.8975 * (day - 2.1697)))))^day]

TMP<-join(TMP, COLLAPSE[date=="2020-09-24"][order(day)][day>0, list(day, propMax24Sep=propMax)])


pdf("DRAFT/figures/example-curve-fit.pdf",width = 6, height = 4 )

 plot(TMP[day>0, list(day, propMax)], ylab="% of positive cases reported", xlab="Days since test taken")
 lines(TMP[, list(day, fitted)],col="red")
lines(TMP[, list(day, propMax24Sep)], col="blue", lty="dashed")
dev.off()


TMP3<-COLLAPSE[vintage=="2020-10-19"][date>"2020-10-05" & date<="2020-10-18"][, list(propMax =round(mean(propMax),digits=3)), by=day][order(day)][day<=14]


#aspect_ratio <- 1.75
#height <- 4

ggplot(dat=TMP[day>0]) + geom_point(aes(x=day,y=propMax),alpha=1, col="maroon")  + geom_line(aes(x=day,y=propMax),alpha=1, col="maroon", linetype="dashed") + geom_point(data=TMP3,aes(x=day,y=propMax),alpha=1, col="black") + geom_line(data=TMP3,aes(x=day,y=propMax),alpha=1, col="black", linetype="dotted") + geom_point(data=TMP2[day>0], aes(x=day,y=propMax),alpha=1, col="navy") + geom_line(data=TMP2[day>0], aes(x=day,y=propMax),alpha=1, col="navy") + ylab("% of positive test results reported")  + xlab("# days after test taken")+ theme_bw()
#ggsave(filename=paste("04 Draft/figures/fraction-cases-reported",splevel[1],".pdf",sep=""),height = height , width = height * aspect_ratio)


VINTUSE[, fitted := predict(model, VINTUSE)]
VINTUSE[, fitted_cases := round(fitted * max_cases)]
VINTUSE[, resid_missed_cases := fitted_cases-cases]

#Between Sept. 25 and Oct. 2, 15,841 cases went unreported in the government database.

MISSINGCASES.NAIVE<-VINTUSE[date>="2020-09-20" & date<="2020-10-02" , list(missing_cases_naive=sum(resid_missed_cases), frac_missing_cases_naive= sum(resid_missed_cases,na.rm=TRUE)/sum(max_cases,na.rm=TRUE)), by=c(splevel,"vintage")]

MISSINGCASES.NAIVEPLACEBP<-VINTUSE[date>="2020-09-01" & date<="2020-09-10" , list(missing_cases_naive=sum(resid_missed_cases), frac_missing_cases_naive= sum(resid_missed_cases,na.rm=TRUE)/sum(max_cases,na.rm=TRUE)), by=c(splevel,"vintage")]



#MORE SOPHISTICATED BY REGION AND CASES
COLLAPSE<-VINTUSE[, list(cases=sum(cases)), by=c("vintage","rgn11cd","date","day")]
COLLAPSE[, group := paste(rgn11cd,date,sep="-")]

COLLAPSE[, vintage := ymd(vintage)]
COLLAPSE[, max_cases := max(cases), by=c("date","rgn11cd")]
COLLAPSE[, day := ymd(vintage)-ymd(date)]
COLLAPSE[, day := as.numeric(day)]
COLLAPSE[, propMax := cases/max_cases]

COLLAPSE[, propMax := round(propMax,1)]

COLLAPSE <-COLLAPSE[max_cases>0]
COLLAPSE<-COLLAPSE[(date>="2020-09-01" & date<="2020-09-15") ]
COLLAPSE<-COLLAPSE[order(rgn11cd, day)]


COLLAPSE<-COLLAPSE[vintage!=date]


GROUPS<-COLLAPSE[, .N, by=group]$group

MODELS<-NULL

for(i in 1:length(GROUPS)) {

#priors hard coded
test<-try_catch(expr = MODELS[[i]]<- timedist(COLLAPSE[group==GROUPS[[i]]], x = "day", y = "propMax", r = 0.2, c = 0.5, t = 5), 
          .e = ~ NULL) 

if(is.null(test)) {
#adjust priors
test<-try_catch(expr = MODELS[[i]]<- timedist(COLLAPSE[group==GROUPS[[i]]], x = "day", y = "propMax", r = 0.2, c = 0.5, t = 10), 
          .e = ~ NULL) 
}


}

MODELS.WIDE<-NULL
for(i in 1:length(GROUPS)) {
 
 if(!is.null(MODELS[[i]])) {
 
 
 MODELS.WIDE[[i]]<-data.table(group=GROUPS[[i]], t(data.table(summary(MODELS[[i]])$coefficients)[,1]))
 }
 
}  


MODELS.WIDE<-rbindlist(MODELS.WIDE)
setnames(MODELS.WIDE, names(MODELS.WIDE), c("group", "r_hat","c_hat","t_hat"))
MODELS.WIDE[, rgn11cd := substr(group,1,9)]
MODELS.WIDE[, date := substr(group,11,20)]


#VINTUSE<-join(VINTUSE, MODELS.WIDE[, list(rgn11cd, date, r_hat,c_hat,t_hat)])
VINTUSE<-join(VINTUSE, MODELS.WIDE[, list(r_hat=mean(r_hat), c_hat=mean(c_hat), t_hat=mean(t_hat)), by="rgn11cd"])

VINTUSE[, fitted_sophisticated := 1 - (1 - (r_hat/(1 + exp(-c_hat * (day - t_hat)))))^day]
VINTUSE[is.na(fitted_sophisticated), fitted_sophisticated := 0]

VINTUSE[, fitted_sophisticated_cases := round(fitted_sophisticated * max_cases)]
VINTUSE[, resid_sophisticated_missed_cases := fitted_sophisticated_cases-cases]




NOM<-lapply(list.files("RAW DATA/NOMIS/",full.names=TRUE), function(x) data.table(read.csv(file=x, skip=9,nrows=34753)))
NOM<-lapply(NOM, function(x) setnames(x, names(x), substr(gsub("\\.","",names(x)),1,35)))

NOM<-lapply(NOM, function(x) x[,c(2:ncol(x)),with=F])

NOM<-lapply(NOM, function(x) x[, Total := rowSums(x[,c(2:ncol(x)),with=F])])
NOM<-lapply(NOM, function(x) setnames(x, "mnemonic","lsoa11cd"))

NOM<-lapply(NOM, function(x) join(x,DTUniqueBy(OA[, .N, by=c("lsoa11cd",splevel[1])], "lsoa11cd")[, 1:2]))
NOM<-lapply(NOM, function(x) join(x,DTUniqueBy(OA[, .N, by=c("lsoa11cd",splevel[1])], "lsoa11cd")[, 1:2]))
 
 
NOM<-lapply(NOM, function(x) x[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=names(x)[2:(ncol(x)-1)], by=c(splevel[1])])
NOM<-lapply(NOM, function(x) data.table(x[, 1, with=F], x[, 2:(ncol(x)-1),with=F]/x$Total))


for(i in 1:length(NOM)) {
VINTUSE<-join(VINTUSE, NOM[[i]])
}



DEATHSMSOA<-data.table(read.xlsx("RAW DATA/Deaths first wave/referencetables1.xlsx", sheet=8, startRow=12))
DEATHSMSOA <-DEATHSMSOA[, c(1,9,15)]
setnames(DEATHSMSOA, names(DEATHSMSOA), c("msoa11cd","coviddeathsMarJul","otherdeathsMarJul"))
DEATHSMSOA[, coviddeathsMarJul := as.numeric(coviddeathsMarJul)]
DEATHSMSOA[, otherdeathsMarJul := as.numeric(otherdeathsMarJul)]
DEATHSMSOA<-DEATHSMSOA[!is.na(coviddeathsMarJul)]
DEATHSMSOA<-join(DEATHSMSOA, DTUniqueBy(OA[, .N, by=c("msoa11cd",splevel[1])], "msoa11cd")[, 1:2])
DEATHSMSOA<-DEATHSMSOA[, list(coviddeathsMarJul =sum(coviddeathsMarJul), otherdeathsMarJul = sum(otherdeathsMarJul)), by=c(splevel[1])]

VINTUSE<-join(VINTUSE,DEATHSMSOA)


if(splevel[1] != "lad19cd") {
LA<-join(LA, DTUniqueBy(OA[, .N, by=c("lad19cd",splevel[1])], "lad19cd")[, 1:2])
}
LAM<-LA[, list(tests_performed=sum(as.numeric(tests_performed))), by=c(splevel[1],"week")]
VINTUSE<-join(VINTUSE,LAM)


if(splevel[1] != "lad19cd") {
CASES<-join(CASES, DTUniqueBy(OA[, .N, by=c("lad19cd",splevel[1])], "lad19cd")[, 1:2])
}
CASESM<-CASES[, list(positive_cases=sum(as.numeric(positive_cases))), by=c(splevel[1],"week")]
VINTUSE<-join(VINTUSE,CASESM)



POP<-read.xlsx("RAW DATA/Area characteristics/SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx", sheet=4, startRow=5)
POP<-data.table(POP)
POP<-POP[, c(1,7:ncol(POP)), with=F]
setnames(POP, "MSOA.Code", "msoa11cd")
POP<-join(POP, DTUniqueBy(OA[, .N, by=c("msoa11cd",splevel[1])], "msoa11cd")[, 1:2])
POP<-POP[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), .SDcols=names(POP)[3:(ncol(POP)-1)], by=c(splevel[1])]

POP.LONG<-data.table(melt(POP))

POP.LONG<-join(POP.LONG, data.frame(variable=POP.LONG[, .N, by=variable]$variable , group=c(sort(rep(1:9, 10)),9)))
 POP.LONG <-join(POP.LONG[, list(pop_age=sum(value)), by=c(splevel[1],"group")],POP.LONG[, list(pop_all=sum(value)), by=c(splevel[1])])

POP.LONG[, age:= paste((group-1)*10,"_",(group)*10,sep="")]
POP.LONG[, pop_age_sh := pop_age/pop_all ]
setnames(POP.LONG, splevel[1], "temp")

POP.WIDE<-dcast(POP.LONG, temp ~ age, value.var="pop_age_sh")
setnames(POP.WIDE, "temp", splevel[1])
setnames(POP.WIDE, names(POP.WIDE)[2:ncol(POP.WIDE)], paste("agesh_", names(POP.WIDE)[2:ncol(POP.WIDE)],sep=""))
VINTUSE <-join(VINTUSE, POP.WIDE) 
POP$population<-rowSums(POP[, 2:ncol(POP)])

VINTUSE<-join(VINTUSE, POP[, c(splevel[1], "population"), with=F])

DENSITY<-data.table(read.xlsx("RAW DATA/Area characteristics/SAPE22DT11-mid-2019-lsoa-population-density.xlsx", sheet=4, startRow=5))
setnames(DENSITY, c("LSOA.Code","Mid-2019.population"), c("lsoa11cd","population"))


DENSITY<-join(DENSITY, DTUniqueBy(OA[, .N, by=c(splevel[1],"lsoa11cd")][, 1:2], "lsoa11cd"))

VINTUSE <-join(VINTUSE, DENSITY[, list(area_lad=sum(Area.Sq.Km), sdpopdens=sd(People.per.Sq.Km)), by=c(splevel[1])])

#exposure to 

COMMUTING<-data.table(read.csv(file="RAW DATA/Commuting/wu03ew_v2.csv"))
setnames(COMMUTING, names(COMMUTING), gsub("\\.","",names(COMMUTING)))
COMMUTING[, all_commuters := sum(AllcategoriesMethodoftraveltowork), by= Areaofresidence]
COMMUTING[Areaofresidence!= Areaofworkplace, out_commuters:= sum(AllcategoriesMethodoftraveltowork), by= Areaofresidence]
COMMUTING[Areaofresidence!= Areaofworkplace, in_commuters :=  sum(AllcategoriesMethodoftraveltowork), by=c("Areaofworkplace")]

COM<-join(DTUniqueBy(COMMUTING[Areaofresidence!=Areaofworkplace], "Areaofresidence")[, list(msoa11cd= Areaofresidence, all_commuters, out_commuters )], DTUniqueBy(COMMUTING[Areaofresidence!=Areaofworkplace], "Areaofworkplace")[, list(msoa11cd= Areaofworkplace, in_commuters)])
COM<-join(COM, OA[, .N, by=c("msoa11cd",splevel[1])][, 1:2])
COM<- COM[, list(all_commuters=sum(all_commuters), out_commuters=sum(out_commuters), in_commuters=sum(in_commuters)), by=c(splevel[1])]

VINTUSE<-join(VINTUSE,COM)


REG.WEEKLY<-ONSWEEKLY[cause.of.death =="covid-19"][registration.or.occurrence=="registrations", list(deaths=v4_0, year=time, lad19cd=administrative.geography,week=as.numeric(str_extract(week.number,"[0-9]+")), place.of.death )]
REG.WEEKLY<-join(REG.WEEKLY, OA[, .N, by=c("lad19cd",splevel[1])][, 1:2,with=F])
REG.WEEKLY<-REG.WEEKLY[, list(deaths=sum(deaths)), by=c(splevel[1],"week","place.of.death")]
setnames(REG.WEEKLY, splevel[1], "temp")
REG.WEEKLY<-dcast(REG.WEEKLY, temp +week ~ place.of.death, value.var="deaths")
setnames(REG.WEEKLY, names(REG.WEEKLY)[3:ncol(REG.WEEKLY)], paste("onsdrw_",substr(gsub("-","", names(REG.WEEKLY)[3:ncol(REG.WEEKLY)]),1,15),sep=""))
setnames(REG.WEEKLY,  "temp",splevel[1])

VINTUSE<-join(VINTUSE, REG.WEEKLY)


REG.WEEKLY<-ONSWEEKLY[cause.of.death =="covid-19"][registration.or.occurrence=="occurrences", list(deaths=v4_0, lad19cd=administrative.geography,week=as.numeric(str_extract(week.number,"[0-9]+")), place.of.death )]
REG.WEEKLY<-join(REG.WEEKLY, OA[, .N, by=c("lad19cd",splevel[1])][, 1:2,with=F])
REG.WEEKLY<-REG.WEEKLY[, list(deaths=sum(deaths)), by=c(splevel[1],"week","place.of.death")]
setnames(REG.WEEKLY, splevel[1], "temp")
REG.WEEKLY<-dcast(REG.WEEKLY, temp +week ~ place.of.death, value.var="deaths")
setnames(REG.WEEKLY, names(REG.WEEKLY)[3:ncol(REG.WEEKLY)], paste("onsdow_",substr(gsub("-","", names(REG.WEEKLY)[3:ncol(REG.WEEKLY)]),1,15),sep=""))
setnames(REG.WEEKLY,  "temp",splevel[1])

VINTUSE<-join(VINTUSE, REG.WEEKLY)


if(splevel[1] %in%c("lad19cd","utla19cd")) {
VINTUSE<-join(VINTUSE, TEST.CON[,list(utla19cd,week,dchtntcumcont,dchtntcumcontreach)])
VINTUSE<-join(VINTUSE, TEST.REF[, list(utla19cd,week,dchtntcumref,dchtntcumreach)])
VINTUSE<-join(VINTUSE, TNT)

}  else {
TEST.REF2<-join(TEST.REF, OA[, .N, by=c(splevel[1], "utla19cd")][,1:2])
TEST.REF2<-TEST.REF2[, list(dchtntcumref=sum(dchtntcumref,na.rm=TRUE), dchtntcumreach=sum(dchtntcumreach,na.rm=TRUE)), by=c(splevel[1],"week")]
TEST.CON2<-join(TEST.CON, OA[, .N, by=c(splevel[1], "utla19cd")][,1:2])
TEST.CON2<-TEST.CON2[, list(dchtntcumcont=sum(dchtntcumcont,na.rm=TRUE), dchtntcumcontreach=sum(dchtntcumcontreach,na.rm=TRUE)), by=c(splevel[1],"week")]
VINTUSE<-join(VINTUSE, TEST.CON2)
VINTUSE<-join(VINTUSE, TEST.REF2)
}


VINTUSE<-join(VINTUSE, WEEKS[, list(week= calender_week, tnt_performance)])
write.dta(VINTUSE, file=paste("ANALYSIS DATA/VINTAGE-LONG-",ddate,"-",splevel[1],".dta",sep=""))



MISSINGCASES.SOPH<-VINTUSE[date>="2020-09-20" & date<="2020-10-02" , list(missing_cases_soph=sum(resid_sophisticated_missed_cases),frac_missing_cases_soph= sum(resid_sophisticated_missed_cases,na.rm=TRUE)/sum(max_cases,na.rm=TRUE)), by=c(splevel[1],"vintage")]
MISSINGCASES.SOPH.EARLY<-VINTUSE[date>="2020-09-20" & date<="2020-09-27" , list(missing_cases_soph_early=sum(resid_sophisticated_missed_cases), frac_missing_cases_soph_early=sum(resid_sophisticated_missed_cases)/sum(max_cases)), by=c(splevel[1],"vintage")]
MISSINGCASES.NAIVE.EARLY<-VINTUSE[date>="2020-09-20" & date<="2020-09-27" , list(missing_cases_naive_early=sum(resid_missed_cases), frac_missing_cases_naive_early=sum(resid_missed_cases)/sum(max_cases)), by=c(splevel[1],"vintage")]



MISSING.CHRIS.TOTO<-VINTUSE[date>="2020-09-20" & date<="2020-09-25", list(cases =sum(cases)), by=c(splevel[1],"vintage")]

MISSING.CHRIS.TOTO<-join(MISSING.CHRIS.TOTO[vintage==ddate][, c(splevel[1],"cases"),with=F],MISSING.CHRIS.TOTO[vintage=="2020-10-02"][, list(cases20201002=cases), by=c(splevel[1])])
MISSING.CHRIS.TOTO[, missing_cases_toto := cases-cases20201002]
MISSING.CHRIS.TOTO[, frac_missing_cases_toto := (cases-cases20201002)/cases]

MISSING.CHRIS.TOTO2<-VINTUSE[, list(cases =sum(cases)), by=c(splevel[1],"vintage","date")]
MISSING.CHRIS.TOTO2<-join(MISSING.CHRIS.TOTO2[vintage==ddate][, c(splevel[1],"date","cases"),with=F],MISSING.CHRIS.TOTO2[vintage=="2020-10-02"][, list(cases20201002=cases), by=c(splevel[1],"date")])
MISSING.CHRIS.TOTO2<- MISSING.CHRIS.TOTO2[date>="2020-09-07" & date<="2020-10-02"]
MISSING.CHRIS.TOTO2[, missed_cases := cases-cases20201002]

setnames(MISSING.CHRIS.TOTO2,splevel[1],"ind")
MISSED.WIDE<-dcast(MISSING.CHRIS.TOTO2[, list(ind,date, missed_cases)], ind ~ date, value.var="missed_cases" )

setnames(MISSED.WIDE, names(MISSED.WIDE)[1],splevel[1])

setnames(MISSED.WIDE, names(MISSED.WIDE)[2:ncol(MISSED.WIDE)],paste("missedcases", gsub("-","",names(MISSED.WIDE)[2:ncol(MISSED.WIDE)]),sep=""))


MISSING.CHRIS.TOTO3<-VINTUSE[, list(cases =sum(cases)), by=c(splevel[1],"vintage","date")]

MISSING.CHRIS.TOTO3<-join(MISSING.CHRIS.TOTO3[vintage==ddate][, c(splevel[1],"date","cases"),with=F],MISSING.CHRIS.TOTO3[vintage=="2020-10-16"][, list(cases20201002=cases), by=c(splevel[1],"date")])
 MISSING.CHRIS.TOTO3<- MISSING.CHRIS.TOTO3[date>="2020-09-20" & date<="2020-10-12"]
MISSING.CHRIS.TOTO3[, missed_cases := cases-cases20201002]

setnames(MISSING.CHRIS.TOTO3,splevel[1],"ind")
MISSED.WIDE.FUT<-dcast(MISSING.CHRIS.TOTO3[, list(ind,date, missed_cases)], ind ~ date, value.var="missed_cases" )

setnames(MISSED.WIDE.FUT, names(MISSED.WIDE.FUT)[1],splevel[1])

setnames(MISSED.WIDE.FUT, names(MISSED.WIDE.FUT)[2:ncol(MISSED.WIDE.FUT)],paste("missedcasesfut", gsub("-","",names(MISSED.WIDE.FUT)[2:ncol(MISSED.WIDE.FUT)]),sep=""))


MISSING.CHRIS.TOTO4<-VINTUSE[, list(cases =sum(cases)), by=c(splevel[1],"vintage","date")]
MISSING.CHRIS.TOTO4 <-join(MISSING.CHRIS.TOTO4, MISSING.CHRIS.TOTO4[vintage==ddate][, list(date, cases_recent=cases), by=c(splevel[1])])
MISSING.CHRIS.TOTO4[, datediff := ymd(vintage)-ymd(date)]
MISSING.CHRIS.TOTO4[, missingl5 := cases_recent-cases  ]
MISSED<-MISSING.CHRIS.TOTO4[datediff==8][date>="2020-09-01" & date<="2020-10-21", list(date, missingl5), by=c(splevel[1])]
setnames(MISSED,splevel[1],"ind")

MISSED.WIDE5day<-dcast(MISSED[,list(ind,date, missingl5)], ind ~ date, value.var="missingl5" )
setnames(MISSED.WIDE5day, names(MISSED.WIDE5day)[2:ncol(MISSED.WIDE5day)],paste("missingl5", gsub("-","",names(MISSED.WIDE5day)[2:ncol(MISSED.WIDE5day)]),sep=""))

setnames(MISSED.WIDE5day,"ind",splevel[1])


MISSING.CHRIS.PLACEBO1<-VINTUSE[date<="2020-10-12" & date>="2020-10-05", list(cases =sum(cases)), by=c(splevel[1],"vintage")]
MISSING.CHRIS.PLACEBO1<-join(MISSING.CHRIS.PLACEBO1[vintage==ddate][, c(splevel[1],"cases"),with=F],MISSING.CHRIS.PLACEBO1[vintage=="2020-10-19"][, list(cases20201002=cases), by=c(splevel[1])])
MISSING.CHRIS.PLACEBO1[, missing_cases_toto_pl1 := cases-cases20201002]
MISSING.CHRIS.PLACEBO1[, frac_missing_cases_toto_pl1 := (cases-cases20201002)/cases]

MISSING.CHRIS.PLACEBO2<-VINTUSE[date>="2020-09-06" & date<="2020-09-12", list(cases =sum(cases)), by=c(splevel[1],"vintage")]
MISSING.CHRIS.PLACEBO2<-join(MISSING.CHRIS.PLACEBO2[vintage==ddate][, c(splevel[1],"cases"),with=F],MISSING.CHRIS.PLACEBO2[vintage=="2020-09-18"][, list(cases20201002=cases), by=c(splevel[1])])
MISSING.CHRIS.PLACEBO2[, missing_cases_toto_pl2 := cases-cases20201002]
MISSING.CHRIS.PLACEBO2[, frac_missing_cases_toto_pl2 := (cases-cases20201002)/cases]


FORCROSS_SECTION<-VINTUSE[vintage==ddate]

#FORCROSS_SECTION<-join(FORCROSS_SECTION, VINTUSE[vintage=="2020-10-03"][, list(cases20201003=cases,fitted_naive_cases20201003=fitted_cases,fitted_soph_cases20201003=fitted_sophisticated_cases, fitted_sophisticated_prob=fitted_sophisticated, fitted_naive_prob=fitted ), by=c(splevel[1],"date")])
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSING.CHRIS.TOTO[, c(splevel[1],"missing_cases_toto","frac_missing_cases_toto"), with=F] )
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSING.CHRIS.PLACEBO1[, c(splevel[1],"missing_cases_toto_pl1","frac_missing_cases_toto_pl1"), with=F] )
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSING.CHRIS.PLACEBO2[, c(splevel[1],"missing_cases_toto_pl2","frac_missing_cases_toto_pl2"), with=F] )
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSED.WIDE[, c(splevel[1],grep("missedcases",names(MISSED.WIDE),value=TRUE)), with=F] )
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSED.WIDE.FUT[, c(splevel[1],grep("missedcases",names(MISSED.WIDE.FUT),value=TRUE)), with=F] )
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSED.WIDE5day )


#
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSINGCASES.SOPH[vintage=="2020-10-02"][,c(splevel[1],"missing_cases_soph", "frac_missing_cases_soph"),with=F])
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSINGCASES.NAIVE[vintage=="2020-10-02"][,c(splevel[1], "missing_cases_naive","frac_missing_cases_naive"), with=F])
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSINGCASES.NAIVE.EARLY[vintage=="2020-10-02"][,c(splevel[1], "missing_cases_naive_early","frac_missing_cases_naive_early"),with=F])
FORCROSS_SECTION<-join(FORCROSS_SECTION, MISSINGCASES.SOPH.EARLY[vintage=="2020-10-02"][,c(splevel[1], "missing_cases_soph_early","frac_missing_cases_soph_early"), with=F])

FORCROSS_SECTION[, ymd := as.numeric(gsub("-","",date))]


GMMEAN<-GM[, list(retail_and_recreation_pctch=mean(retail_and_recreation_pctch,na.rm=TRUE), grocery_and_pharmacy_pctch=mean(grocery_and_pharmacy_pctch,na.rm=TRUE), parks_pctch=mean(parks_pctch,na.rm=TRUE), transit_stations_pctch=mean(transit_stations_pctch,na.rm=TRUE), workplaces_pctch=mean(workplaces_pctch,na.rm=TRUE), residential_pctch=mean(residential_pctch,na.rm=TRUE) ), by=c(splevel[1],"date")]
FORCROSS_SECTION<-join(FORCROSS_SECTION, GMMEAN)

setnames(FORCROSS_SECTION, names(FORCROSS_SECTION), gsub("missing_cases","miss_",names(FORCROSS_SECTION)))


write.dta(FORCROSS_SECTION, file=paste("ANALYSIS DATA/VINTAGE-CROSS-",ddate,"-",splevel[1],".dta",sep=""))


}

