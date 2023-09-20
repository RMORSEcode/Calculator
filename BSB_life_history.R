library(tidyverse)
library(FSA)
library(car)
### Bottom trawl survey strata shapefile
bts=sf::read_sf("C:/Users/ryan.morse/Desktop/shapefiles/BTS/BTS_Strata.shp")
## load bottom trawl survey data
load("C:/Users/ryan.morse/Downloads/NEFSC_BTS_2021.RData")
load("C:/Users/ryan.morse/Downloads/SurvdatBio.RData")
survdat <- survey$survdat

#filter to black sea bass #141
bsb.sp=survdat.bio %>% dplyr::filter(SVSPP==141, SEASON=="SPRING")
bsb.fl=survdat.bio %>% dplyr::filter(SVSPP==141, SEASON=="FALL")
plot(LENGTH~AGE,data=bsb.sp, main="BSB Spring", ylim=c(0,60), xlim=c(0,12))
plot(LENGTH~AGE,data=bsb.fl, main="BSB Fall", ylim=c(0,60), xlim=c(0,12))
boxplot(LENGTH~AGE,data=bsb.sp, main="BSB Spring", ylim=c(0,60), xlim=c(0,12))
boxplot(LENGTH~AGE,data=bsb.fl, main="BSB Fall", ylim=c(0,60), xlim=c(0,12))


#Spring
test.s=bsb.sp %>% count(YEAR, STRATUM)
t2.s=test.s[test.s$n>=50,]
barplot(table(t2.s$STRATUM), main="Spring Strata >=50")
#Fall
test.f=bsb.fl %>% count(YEAR, STRATUM)
t2.f=test.f[test.f$n>=50,]
barplot(table(t2.f$STRATUM),main="Fall Strata >=50")

## spatial view of high abundance strata
bts.s=bts %>% filter(STRATA %in% t2.s$STRATUM)
bts.f=bts %>% filter(STRATA %in% t2.f$STRATUM)
plot(bts[2], axes=T, reset = FALSE, main="Spring Strata")
plot(bts.s[2], add=T, col='red')
plot(bts[2], axes=T, reset = FALSE, main="Fall Strata")
plot(bts.f[2], add=T, col='red')

test=bsb %>% count(YEAR, SEASON)

## single year -- last year with both seasons
plot(LENGTH~AGE,data=bsb.sp[which(bsb.sp$YEAR==2016),], main="BSB Spring 2016", ylim=c(0,60), xlim=c(0,12))
plot(LENGTH~AGE,data=bsb.fl[which(bsb.fl$YEAR==2015),], main="BSB Fall 2015", ylim=c(0,60), xlim=c(0,12))
boxplot(LENGTH~AGE,data=bsb.sp[which(bsb.sp$YEAR==2016),], main="BSB Spring 2016", ylim=c(0,60), xlim=c(0,12))
boxplot(LENGTH~AGE,data=bsb.fl[which(bsb.fl$YEAR==2015),], main="BSB Fall 2015", ylim=c(0,60), xlim=c(0,12))

agesum <- group_by(bsb,SEX) %>%
  summarize(minage=min(AGE, na.rm = T),maxage=max(AGE, na.rm = T))
agesum

## drop missing data
bsb.sp2=bsb.sp[complete.cases(bsb.sp$AGE),]
bsb.fl2=bsb.fl[complete.cases(bsb.fl$AGE),]

vb <- vbFuns(param="Typical")
f.starts.sp <- vbStarts(LENGTH~AGE,data=bsb.sp2) 
f.fit.sp <- nls(LENGTH~vb(AGE,Linf,K,t0),data=bsb.sp2,start=f.starts.sp)
coef(f.fit.sp)
ages <- seq(-1,12,by=0.2)
f.boot1.sp <- Boot(f.fit.sp)  # Be aware of some non-convergence
confint(f.boot1.sp)

predict(f.fit.sp,data.frame(AGE=2:7))
predict2 <- function(x) predict(x,data.frame(AGE=ages))
ages <- 2:7
predict2(f.fit.sp)  
ages <- seq(-1,12,by=0.2)
f.boot2 <- Boot(f.fit.sp,f=predict2)

preds1 <- data.frame(ages,
                     predict(f.fit.sp,data.frame(AGE=ages)),
                     confint(f.boot2))
preds2 <- filter(preds1,AGE>=agesum$minage[2],AGE<=agesum$maxage[2])
