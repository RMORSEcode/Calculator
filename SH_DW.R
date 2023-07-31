library(ggplot2)
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
Rutgers <- read_excel_allsheets(Janine)
Janine="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Oyster morphometrics/Janine_s data/Rutgers Oyster Farm Data from 2020-2021 (2023-01-16).xlsx"
Janine_RM="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Oyster morphometrics/Janine_s data/Rutgers_RM.xlsx"

Rutgers_RM <- sapply(readxl::excel_sheets(Janine_RM), simplify = F, USE.NAMES = T,
                    function(X) readxl::read_excel(Janine, sheet = X))
test=Rutgers_RM$`BarnBay-Sep 2020`[11:29,]
test2=test[2:19,];colnames(test2)=test[1,]
test2$date='9/02/2020'
test=Rutgers_RM$`BarnBay-Nov 2020`[11:29,]
test3=test[2:19,]; colnames(test3)=test[1,]
test3$date='11/09/2020'
BarnBay=rbind(test2,test3)
test=Rutgers_RM$`BarnBay-July 2021`[11:29,]
test3=test[2:19,];colnames(test3)=test[1,]
test3$date='7/07/2021'
BarnBay=rbind(BarnBay,test3)
BarnBay$Location='Barnegat Bay'
BarnBay$Sublocation='Rose Cove'
BarnBay$State='NJ'
BarnBay=BarnBay[,-c(9,10,11)]
# BarnBay[,1:7]=as.numeric(matrix(BarnBay[,1:7]))

test=Rutgers_RM$`DelBay-Oct 2020`[11:29,]
test2=test[2:19,];colnames(test2)=test[1,]
test2$date='10/19/2020'
test=Rutgers_RM$`DelBay-Nov 2020`[11:29,]
test3=test[2:19,]; colnames(test3)=test[1,]
test3$date='11/21/2020'
DelBay=rbind(test2,test3)
test=Rutgers_RM$`DelBay-April 2021`[13:31,]
test3=test[2:19,];colnames(test3)=test[1,]
test3$date='4/13/2021'
DelBay=rbind(DelBay,test3)
test=Rutgers_RM$`DelBay- June 2021`[11:29,]
test3=test[2:19,];colnames(test3)=test[1,]
test3$date='6/14/2021'
DelBay=rbind(DelBay,test3)
test=Rutgers_RM$`DelBay- Aug 2021`[11:29,]
test3=test[2:19,];colnames(test3)=test[1,]
test3$date='8/09/2021'
DelBay=rbind(DelBay,test3)
DelBay$Location='Delaware Bay'
DelBay$Sublocation=NA
DelBay$State='NJ'
DelBay=DelBay[,-c(9,10,11)]

test=Rutgers_RM$`RehoBay- Nov 2020`[12:30,]
test2=test[2:19,];colnames(test2)=test[1,]
test2$date='11/6/2020'
test=Rutgers_RM$`RehoBay- April 2021`[11:29,]
test3=test[2:19,]; colnames(test3)=test[1,]
test3$date='4/28/2021'
RehoBay=rbind(test2,test3)
test=Rutgers_RM$`RehoBay- July 2021`[11:29,]
test3=test[2:19,];colnames(test3)=test[1,]
test3$date='7/12/2021'
RehoBay=rbind(RehoBay,test3)
test=Rutgers_RM$`RehoBay- Sept 2021`[11:29,]
test3=test[2:19,];colnames(test3)=test[1,]
test3$date='9/10/2021'
RehoBay=rbind(RehoBay,test3)
RehoBay$Location='Rehobath Bay'
RehoBay$Sublocation='Sally Cove'
RehoBay$State='DE'
RehoBay=RehoBay[,-c(9,10,11)]

Barr=rbind(BarnBay, DelBay) 
Barr=rbind(Barr, RehoBay)

plot(as.numeric(RehoBay$`Dry Tissue Weight (g)`) ~  
       as.numeric(RehoBay$`Shell Height (mm)`), type='p', ylim=c(0,5), xlim=c(20,150), 
     col='red', pch=19, xlab='SH (mm)', ylab='Tissue DW (g)', main='RehoBay')
rblm=lm(log(as.numeric(RehoBay$`Dry Tissue Weight (g)`))~log(as.numeric(RehoBay$`Shell Height (mm)`)))
summary(rblm)
xval=seq(20,140,by=.5)
yval=exp(rblm$coefficients[1])*xval^rblm$coefficients[2]
lines(xval, yval, col='blue')
text(90,4.5, paste('y=',round(exp(rblm$coefficients[1]),9),'x^',round(rblm$coefficients[2],3), sep='' ))

plot(as.numeric(BarnBay$`Dry Tissue Weight (g)`) ~  
       as.numeric(BarnBay$`Shell Height (mm)`), type='p', ylim=c(0,5), xlim=c(20,150), 
     col='red', pch=19, xlab='SH (mm)', ylab='Tissue DW (g)', main='BarnBay')
bblm=lm(log(as.numeric(BarnBay$`Dry Tissue Weight (g)`))~log(as.numeric(BarnBay$`Shell Height (mm)`)))
summary(bblm)
xval=seq(20,140,by=.5)
yval=exp(bblm$coefficients[1])*xval^bblm$coefficients[2]
lines(xval, yval, col='blue')
text(90,4.5, paste('y=',round(exp(bblm$coefficients[1]),9),'x^',round(bblm$coefficients[2],3), sep='' ))

plot(as.numeric(DelBay$`Dry Tissue Weight (g)`) ~  
       as.numeric(DelBay$`Shell Height (mm)`), type='p', ylim=c(0,5), xlim=c(20,150), 
     col='red', pch=19, xlab='SH (mm)', ylab='Tissue DW (g)', main='DelBay')
dblm=lm(log(as.numeric(DelBay$`Dry Tissue Weight (g)`))~log(as.numeric(DelBay$`Shell Height (mm)`)))
summary(dblm)
xval=seq(20,140,by=.5)
yval=exp(dblm$coefficients[1])*xval^dblm$coefficients[2]
lines(xval, yval, col='blue')
text(90,4.5, paste('y=',round(exp(dblm$coefficients[1]),9),'x^',round(dblm$coefficients[2],3), sep='' ))

plot(as.numeric(RehoBay$`Dry Tissue Weight (g)`) ~  
       as.numeric(RehoBay$`Shell Height (mm)`), type='p', ylim=c(0,5), xlim=c(20,150), 
     col='red', pch=19, xlab='SH (mm)', ylab='Tissue DW (g)', main='DE-NJ')
points(as.numeric(BarnBay$`Dry Tissue Weight (g)`) ~  
         as.numeric(BarnBay$`Shell Height (mm)`), type='p', pch=19, col='blue')
points(as.numeric(DelBay$`Dry Tissue Weight (g)`) ~  
         as.numeric(DelBay$`Shell Height (mm)`), type='p', pch=19, col='gray')
xval=seq(20,140,by=.5)
yval=exp(rblm$coefficients[1])*xval^rblm$coefficients[2]
lines(xval, yval, col='red')
xval=seq(20,140,by=.5)
yval=exp(bblm$coefficients[1])*xval^bblm$coefficients[2]
lines(xval, yval, col='blue')
xval=seq(20,140,by=.5)
yval=exp(dblm$coefficients[1])*xval^dblm$coefficients[2]
lines(xval, yval, col='black')
legend('topleft', bty = 'n', horiz = F, legend=c("RehoBay", "BarnBay", "DelBay"), text.col = c('red', 'blue', 'gray'))

### Maine data from Tom Kiffney UMaine
plot(Kiffney$DryTissueWeight_g ~Kiffney$ShellHeight_mm, 
     las=1, xlab='SH (mm)', ylab='DW (g)', ylim=c(0,8), xlim=c(0,140), type='p', col='gray')
klm=lm(log(Kiffney$DryTissueWeight_g)~log(Kiffney$ShellHeight_mm))
xval=seq(0,120,by=.5)
yval=exp(klm$coefficients[1])*xval^klm$coefficients[2]
lines(xval, yval, col='black', lwd=2)

ggplot(Kiffney, aes(x=`ShellHeight_mm`, y=`DryTissueWeight_g`)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,120) + ylim(0,7) +
  geom_point(aes(color=`Site`)) 
## diploids vs triploids
plot(Kiffney2$Net_dry_wt_mg[Kiffney2$Ploidy=='2N']/1000~Kiffney2$ShellHeight_mm[Kiffney2$Ploidy=='2N'], 
     las=1, xlab='SH (mm)', ylab='DW (g)', ylim=c(0,8), xlim=c(0,140), type='p', col='gray')
klm2=lm(log(Kiffney2$Net_dry_wt_mg[Kiffney2$Ploidy=='2N']/1000)~log(Kiffney2$ShellHeight_mm[Kiffney2$Ploidy=='2N']))
xval=seq(0,90,by=.5)
yval=exp(klm2$coefficients[1])*xval^klm2$coefficients[2]
lines(xval, yval, col='black', lwd=2)
points(Kiffney2$Net_dry_wt_mg[Kiffney2$Ploidy=='3N']/1000~Kiffney2$ShellHeight_mm[Kiffney2$Ploidy=='3N'], col='red')
klm3=lm(log(Kiffney2$Net_dry_wt_mg[Kiffney2$Ploidy=='3N']/1000)~log(Kiffney2$ShellHeight_mm[Kiffney2$Ploidy=='3N']))
xval=seq(0,90,by=.5)
yval=exp(klm3$coefficients[1])*xval^klm3$coefficients[2]
lines(xval, yval, col='purple', lwd=2)
# legend('topleft', bty = 'n', horiz = F, legend=c("RehoBay", "BarnBay", "DelBay"), text.col = c('red', 'blue', 'gray'))

ggplot(Kiffney2[Kiffney2$Ploidy=='2N',], aes(x=`ShellHeight_mm`, y=`Net_dry_wt_mg`)) +  labs(y='DW (mg)', x='SH (mm)') + xlim(0,120) + ylim(0,700) +
  geom_point(aes(color=`Site`))+ ggtitle('Diploid')
ggplot(Kiffney2[Kiffney2$Ploidy=='3N',], aes(x=`ShellHeight_mm`, y=`Net_dry_wt_mg`)) +  labs(y='DW (mg)', x='SH (cm)') + xlim(0,120) + ylim(0,700) +
  geom_point(aes(color=`Site`))+ ggtitle('Triploid')
ggplot(Kiffney2, aes(x=`ShellHeight_mm`, y=`Net_dry_wt_mg`)) +  labs(y='DW (mg)', x='SH (mm)') + xlim(0,120) + ylim(0,700) +
  geom_point(aes(color=`Ploidy`))


# ## CBP data for Chester River and Rappahannock River near farms for Poach data
# # WQ=read.csv("C:/Users/ryan.morse/Downloads/WaterQualityWaterQualityStation.csv")
# wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
# WQ=read.csv(paste(wd, 'WaterQualityWaterQualityStation.csv', sep=''))
#   
# WQ$SampleDate=mdy(WQ$SampleDate)
# WQ2=WQ[order(WQ$Station, WQ$SampleDate),] %>% filter(Layer=="S " | Layer=="B ")
# WQ3=WQ2 %>% filter(Station=="LE3.4", Parameter=="SALINITY", Layer=="S ")
# plot(WQ3$MeasureValue~ WQ3$SampleDate, type='l', ylab='Salinity', xlab='Date', main='CBP LE3.4 Rapp', las=1)
# WQ3=WQ2 %>% filter(Station=="LE3.4", Parameter=="SALINITY", Layer=="B ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='red')
# legend('bottomleft', bty='n', lty=c(1,1), col=c('black', 'red'), legend=c('S', 'B'))
# 
# WQ3=WQ2 %>% filter(Station=="ET4.2", Parameter=="SALINITY", Layer=="S ")
# plot(WQ3$MeasureValue~ WQ3$SampleDate, type='l', ylab='Salinity', xlab='Date', las=1, main='CBP ET4.2 Chester')
# WQ3=WQ2 %>% filter(Station=="ET4.2", Parameter=="SALINITY", Layer=="B ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='red')
# legend('bottomleft', bty='n', lty=c(1,1), col=c('black', 'red'), legend=c('S', 'B'))
# ## plot both stations S
# WQ3=WQ2 %>% filter(Station=="LE3.4", Parameter=="SALINITY", Layer=="S ")
# plot(WQ3$MeasureValue~ WQ3$SampleDate, type='l', col='blue', ylab='Salinity', xlab='Date', ylim=c(4,20),las=1)
# WQ3=WQ2 %>% filter(Station=="LE3.4", Parameter=="SALINITY", Layer=="B ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='blue', lty=3)
# WQ3=WQ2 %>% filter(Station=="ET4.2", Parameter=="SALINITY", Layer=="S ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='red', lty=1)
# WQ3=WQ2 %>% filter(Station=="ET4.2", Parameter=="SALINITY", Layer=="B ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='red', lty=3)
# abline(h=14, lty=2)
# legend('bottomleft', bty='n', lty=c(1,3), legend=c('S', 'B'))
# legend('bottomright', bty='n', text.col=c('blue', 'red'), legend=c('Rapp', 'Chest'))
# 
# 
# WQ3=WQ2 %>% filter(Station=="LE3.4", Parameter=="WTEMP", Layer=="S ")
# plot(WQ3$MeasureValue~ WQ3$SampleDate, type='l', col='blue', ylab='WTEMP', xlab='Date', ylim=c(0,30),las=1)
# WQ3=WQ2 %>% filter(Station=="LE3.4", Parameter=="WTEMP", Layer=="B ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='blue', lty=3)
# WQ3=WQ2 %>% filter(Station=="ET4.2", Parameter=="WTEMP", Layer=="S ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='red', lty=1)
# WQ3=WQ2 %>% filter(Station=="ET4.2", Parameter=="WTEMP", Layer=="B ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='red', lty=3)
# # abline(h=14, lty=2)
# legend('bottomleft', bty='n', lty=c(1,3), legend=c('S', 'B'))
# legend('bottomright', bty='n', text.col=c('blue', 'red'), legend=c('Rapp', 'Chest'))
# 
# ## limit time 2015-2017
# WQf=WQ2 %>% filter(year(SampleDate)<2018, year(SampleDate)>2014)
# ## choose to plot:
# k <- "mu"
# param="SALINITY"; plab="Salinity"; pt="S.tiff"
# param="WTEMP"; plab="Water temperature (C)"; pt="T.tiff"
# param="CHLA"; plab=expression(paste("Chlorophyll a (", mu, "g/L)", sep='')); pt='Chl.tiff'
# param="DIN"; plab="DIN (mg/L)"; pt="DIN.tiff"
# param="DO"; plab="Dissolved oxygen (mg/L)"; pt="DO.tiff"
# WQ2 %>% filter(Parameter==param, year(SampleDate)<2018, year(SampleDate)>2014) %>%
#   ggplot(mapping=aes(x=SampleDate, y=MeasureValue)) + 
#   theme_bw() +
#   theme(
#     panel.border = element_blank(), 
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(), 
#     axis.line = element_line(colour = "black")
#   ) +
#   labs(y=plab, x='Date') +
#        geom_line(aes(color=Station, linetype=Layer)) + 
#                    geom_point(aes(color=Station, shape=Layer))
# ggsave(filename = pt, plot=last_plot(), path=wd, device='tiff', width = 4, height=3, units="in", dpi=300, bg='white')
# 
# 
# WQ2 %>% filter(Parameter=="WTEMP") %>%
#   ggplot(mapping=aes(x=SampleDate, y=MeasureValue)) +
#   geom_line(aes(color=Station, linetype=Layer))
# 
# WQ2 %>% filter(Parameter=="SALINITY") %>%
#   ggplot(mapping=aes(x=SampleDate, y=MeasureValue)) +
#   geom_line(aes(color=Station, linetype=Layer))
# 
# WQ2 %>% filter(Parameter=="CHLA") %>%
#   ggplot(mapping=aes(x=SampleDate, y=MeasureValue)) +
#   geom_line(aes(color=Station, linetype=Layer))
# 
# WQ2 %>% filter(Parameter=="DIN") %>%
#   ggplot(mapping=aes(x=SampleDate, y=MeasureValue)) +
#   geom_line(aes(color=Station, linetype=Layer))
# 
# WQ2 %>% filter(Parameter=="DO") %>%
#   ggplot(mapping=aes(x=SampleDate, y=MeasureValue)) +
#   geom_line(aes(color=Station, linetype=Layer))
# 
# 
# WQ3=WQ2 %>% filter(Station=="LE3.4", Parameter=="WTEMP", Layer=="B ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='blue', lty=3)
# WQ3=WQ2 %>% filter(Station=="ET4.2", Parameter=="WTEMP", Layer=="S ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='red', lty=1)
# WQ3=WQ2 %>% filter(Station=="ET4.2", Parameter=="WTEMP", Layer=="B ")
# lines(WQ3$MeasureValue~ WQ3$SampleDate, col='red', lty=3)
# abline(h=14, lty=2)
# legend('bottomleft', bty='n', lty=c(1,3), legend=c('S', 'B'))
# legend('bottomright', bty='n', text.col=c('blue', 'red'), legend=c('Rapp', 'Chest'))

poachdiploid=poach %>% filter(Ploidy=='Diploid')
poachtriploid=poach %>% filter(Ploidy=='Triploid')

plot(poach$Tissue_Dry_Weight_g[which(poach$Location_Index=="RAPP")] ~ 
       poach$Total_Shell_Height_Length_mm[which(poach$Location_Index=="RAPP")], type='p', ylim=c(0,5), xlim=c(20,150), 
     col='red', pch=19, xlab='SH (mm)', ylab='Tissue DW (g)', main='CB Poach')

points(poach$Tissue_Dry_Weight_g[which(poach$Location_Index=="Chester")] ~ 
         poach$Total_Shell_Height_Length_mm[which(poach$Location_Index=="Chester")], type='p', pch=19, col='gray')
clm=lm(log(poach$Tissue_Dry_Weight_g)~log(poach$Total_Shell_Height_Length_mm))
test=summary(clm)
xval=seq(20,140,by=.5)
yval=exp(clm$coefficients[1])*xval^clm$coefficients[2]
lines(xval, yval, col='blue')
text(50,4.5, paste('y=',round(exp(clm$coefficients[1]),9),'x^',round(clm$coefficients[2],3), sep='' ))
legend('bottomright', bty = 'n', horiz = F, legend=c("Rappahannock", "Chester"), text.col = c('red', 'gray'))
# text(40,3, expression(paste('Adj. R^2~=',round(test$adj.r.squared,2), sep='')))
# text(40,2, text=parse(paste('Adj. R^2~=',round(test$adj.r.squared,2), sep='')))
# text(60,3, expression(paste(plain("Adj. R")^plain("2"),plain('='),round(test$adj.r.squared,2), sep='')))
# text(60,2, parse(text=(paste("Adj. R^2~","=",round(test$adj.r.squared,2), sep=''))))
# text(60,1, expression(paste("Adj. R"^"2","=",test$adj.r.squared, sep='')))
rval=round(test$adj.r.squared,2)
text(20,5, labels = bquote(R^2 == .(rval)), adj = 0, cex = 0.85)


plot(poachtriploid$Tissue_Dry_Weight_g[which(poachtriploid$Location_Index=="Chester")] ~ 
       poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="Chester")], 
     ylim=c(0,5), xlim=c(20,150), type='p', pch=19, col='gray', ylab='DW (g)', xlab='SH (mm)', main='Triploid')
points(poachtriploid$Tissue_Dry_Weight_g[which(poachtriploid$Location_Index=="RAPP")] ~ 
       poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="RAPP")], 
     ylim=c(0,5), xlim=c(20,150), type='p', pch=19, col='red')
clm=lm(log(poachtriploid$Tissue_Dry_Weight_g)~log(poachtriploid$Total_Shell_Height_Length_mm))
test=summary(clm)
xval=seq(20,140,by=.5)
yval=exp(clm$coefficients[1])*xval^clm$coefficients[2]
lines(xval, yval, col='blue')
text(50,4.5, paste('y=',round(exp(clm$coefficients[1]),9),'x^',round(clm$coefficients[2],3), sep='' ))
legend('bottomright', bty = 'n', horiz = F, legend=c("Rappahannock", "Chester"), text.col = c('red', 'gray'))
# legend('topleft', bty='n', horiz = F, legend=c('Chester', "Rapp"), text.col=c('gray', 'red'))
rval=round(test$adj.r.squared,2)
text(20,5, labels = bquote(R^2 == .(rval)), adj = 0, cex = 0.85)

plot(poachdiploid$Tissue_Dry_Weight_g[which(poachdiploid$Location_Index=="Chester")] ~ 
       poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="Chester")], 
     ylim=c(0,5), xlim=c(20,150), type='p', pch=19, col='gray', ylab='DW (g)', xlab='SH (mm)', main='Diploid')
points(poachdiploid$Tissue_Dry_Weight_g[which(poachdiploid$Location_Index=="RAPP")] ~ 
         poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="RAPP")], 
       ylim=c(0,5), xlim=c(20,150), type='p', pch=19, col='red')
clm=lm(log(poachdiploid$Tissue_Dry_Weight_g)~log(poachdiploid$Total_Shell_Height_Length_mm))
test=summary(clm)
xval=seq(20,140,by=.5)
yval=exp(clm$coefficients[1])*xval^clm$coefficients[2]
lines(xval, yval, col='blue')
text(50,4.5, paste('y=',round(exp(clm$coefficients[1]),9),'x^',round(clm$coefficients[2],3), sep='' ))
legend('bottomright', bty = 'n', horiz = F, legend=c("Rappahannock", "Chester"), text.col = c('red', 'gray'))
# legend('topleft', bty='n', horiz = F, legend=c('Chester', "Rapp"), text.col=c('gray', 'red'))
rval=round(test$adj.r.squared,2)
text(20,5, labels = bquote(R^2 == .(rval)), adj = 0, cex = 0.85)

### plot %N vs SH by ploidy
plot(poachtriploid$Percent_N_content[which(poachtriploid$Location_Index=="Chester")] ~ 
       poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="Chester")], 
     ylim=c(4,11), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Tissue %N', xlab='SH (mm)', main='Triploid')
points(poachtriploid$Percent_N_content[which(poachtriploid$Location_Index=="RAPP")] ~ 
         poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="RAPP")], 
       ylim=c(4,11), xlim=c(20,150), type='p', pch=19, col='red')
plot(poachdiploid$Percent_N_content[which(poachdiploid$Location_Index=="Chester")] ~ 
       poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="Chester")], 
     ylim=c(4,11), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Tissue %N', xlab='SH (mm)', main='Diploid')
points(poachdiploid$Percent_N_content[which(poachdiploid$Location_Index=="RAPP")] ~ 
         poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="RAPP")], 
       ylim=c(4,11), xlim=c(20,150), type='p', pch=19, col='red')

### by site
plot(poachtriploid$Percent_N_content[which(poachtriploid$Location_Index=="Chester")] ~ 
       poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="Chester")], 
     ylim=c(4,11), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Tissue %N', xlab='SH (mm)', main='Chester')
points(poachdiploid$Percent_N_content[which(poachdiploid$Location_Index=="Chester")] ~ 
         poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="Chester")], 
       ylim=c(4,11), xlim=c(20,150), type='p', pch=19, col='red')
legend('topright', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))
plot(poachtriploid$Percent_N_content[which(poachtriploid$Location_Index=="RAPP")] ~ 
       poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="RAPP")], 
     ylim=c(4,11), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Tissue %N', xlab='SH (mm)', main='Rappahannock')
points(poachdiploid$Percent_N_content[which(poachdiploid$Location_Index=="RAPP")] ~ 
         poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="RAPP")], 
       ylim=c(4,11), xlim=c(20,150), type='p', pch=19, col='red')
legend('topleft', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))



### Shell N %
plot(PCBVA$Shell_N_Percent[which(PCBVA$Ploidy=="Triploid")] ~ 
       PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Triploid")], 
     ylim=c(0,0.5), xlim=c(20,150), type='p', pch=17, col='black', ylab='Shell %N', xlab='SH (mm)', las=1, main='Shell N')
points(PCBVA$Shell_N_Percent[which(PCBVA$Ploidy=="Diploid")] ~ 
         PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Diploid")],
       pch=17, col='orange')
points(PCBMD$Shell_N_Percent[which(PCBMD$Ploidy=="Triploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Triploid")],  pch=19, col='gray')
points(PCBMD$Shell_N_Percent[which(PCBMD$Ploidy=="Diploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Diploid")], pch=19, col='red')
# legend('topright', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))
# legend('topleft', bty = 'n', horiz = F, legend=c("VA", "MD"), pch=c(17,19))
legend('topright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))

### Tissue N %
plot(PCBVA$Tissue_N_Percent[which(PCBVA$Ploidy=="Triploid")] ~ 
       PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Triploid")], 
     ylim=c(4,11), xlim=c(20,150), type='p', pch=17, col='black', ylab='Tissue %N', xlab='SH (mm)', las=1, main='Tissue N')
points(PCBVA$Tissue_N_Percent[which(PCBVA$Ploidy=="Diploid")] ~ 
         PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Diploid")],
       pch=17, col='orange')
points(PCBMD$Tissue_N_Percent[which(PCBMD$Ploidy=="Triploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Triploid")],  pch=19, col='gray')
points(PCBMD$Tissue_N_Percent[which(PCBMD$Ploidy=="Diploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Diploid")], pch=19, col='red')
# legend('topright', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))
# legend('topleft', bty = 'n', horiz = F, legend=c("VA", "MD"), pch=c(17,19))
legend('bottomright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))

### Shell P %
plot(PCB$Shell_TP_Percent[which(PCB$Ploidy=="Triploid" & PCB$Location_Index=="RAPP")] ~ 
       PCB$Total_Shell_Height_Length_mm[which(PCB$Ploidy=="Triploid" & PCB$Location_Index=="RAPP")], 
     ylim=c(0.03,0.06), xlim=c(20,150), type='p', pch=17, col='black', ylab='Shell %P', xlab='SH (mm)', las=1, main='Shell P')
points(PCB$Shell_TP_Percent[which(PCB$Ploidy=="Diploid"& PCB$Location_Index=="RAPP")] ~ 
         PCB$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Diploid" & PCB$Location_Index=="RAPP")],
       pch=17, col='orange')
points(PCB$Shell_TP_Percent[which(PCBMD$Ploidy=="Triploid"& PCB$Location_Index=="Chester")] ~ 
         PCB$Total_Shell_Height_Length_mm[which(PCB$Ploidy=="Triploid"& PCB$Location_Index=="Chester")],  pch=19, col='gray')
points(PCB$Shell_TP_Percent[which(PCBMD$Ploidy=="Diploid"& PCB$Location_Index=="Chester")] ~ 
         PCB$Total_Shell_Height_Length_mm[which(PCB$Ploidy=="Diploid"& PCB$Location_Index=="Chester")], pch=19, col='red')
# legend('topright', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))
# legend('topleft', bty = 'n', horiz = F, legend=c("VA", "MD"), pch=c(17,19))
legend('topright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))

### Tissue P %
plot(PCBVA$Tissue_TP_Percent[which(PCBVA$Ploidy=="Triploid")] ~ 
       PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Triploid")], 
     ylim=c(0,2), xlim=c(20,150), type='p', pch=17, col='black', ylab='Tissue %P', xlab='SH (mm)', las=1, main='Tissue P')
points(PCBVA$Tissue_TP_Percent[which(PCBVA$Ploidy=="Diploid")] ~ 
         PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Diploid")],
       pch=17, col='orange')
points(PCBMD$Tissue_TP_Percent[which(PCBMD$Ploidy=="Triploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Triploid")],  pch=19, col='gray')
points(PCBMD$Tissue_TP_Percent[which(PCBMD$Ploidy=="Diploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Diploid")], pch=19, col='red')
# legend('topright', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))
# legend('topleft', bty = 'n', horiz = F, legend=c("VA", "MD"), pch=c(17,19))
legend('topright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))


### Tissue P
# plot(poachtriploid$Percent_P_content[which(poachtriploid$Location_Index=="Chester")] ~ 
#        poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="Chester")], 
#      ylim=c(0,2), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Tissue %P', xlab='SH (mm)', main='Chester')
# points(poachdiploid$Percent_P_content[which(poachdiploid$Location_Index=="Chester")] ~ 
#          poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="Chester")], 
#        ylim=c(0,2), xlim=c(20,150), type='p', pch=19, col='red')
# legend('topright', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))
# plot(poachtriploid$Percent_P_content[which(poachtriploid$Location_Index=="RAPP")] ~ 
#        poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="RAPP")], 
#      ylim=c(0,2), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Tissue %P', xlab='SH (mm)', main='Rappahannock')
# points(poachdiploid$Percent_P_content[which(poachdiploid$Location_Index=="RAPP")] ~ 
#          poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="RAPP")], 
#        ylim=c(0,2), xlim=c(20,150), type='p', pch=19, col='red')
# legend('topleft', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))
# test=poach[which(month(poach$Date_Oysters_Removed)==2),]

# ### Tissue P
# plot(poachtriploid$Percent_P_content[which(poachtriploid$Location_Index=="Chester")] ~ 
#        poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="Chester")], 
#      ylim=c(0,2), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Tissue %P', xlab='SH (mm)', main='Tissue P')
# points(poachdiploid$Percent_P_content[which(poachdiploid$Location_Index=="Chester")] ~ 
#          poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="Chester")], 
#        pch=19, col='red')
# points(poachtriploid$Percent_P_content[which(poachtriploid$Location_Index=="RAPP")] ~ 
#        poachtriploid$Total_Shell_Height_Length_mm[which(poachtriploid$Location_Index=="RAPP")], 
#      pch=17, col='black')
# points(poachdiploid$Percent_P_content[which(poachdiploid$Location_Index=="RAPP")] ~ 
#          poachdiploid$Total_Shell_Height_Length_mm[which(poachdiploid$Location_Index=="RAPP")], 
#        pch=17, col='orange')
# legend('topright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))

# ### Shell P
# plot(PCB2$`Shell P content mg/g`[which(PCB2$Location_Index=="Chester" & PCB2$Ploidy=="Triploid")] ~ 
#        PCB2$Total_Shell_Height_Length_mm[which(PCB2$Location_Index=="Chester"& PCB2$Ploidy=="Triploid")], 
#      ylim=c(0,0.6), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Shell %P', xlab='SH (mm)', main='Chester')
# points(PCB2$`Shell P content mg/g`[which(PCB2$Location_Index=="Chester" & PCB2$Ploidy=="Diploid")] ~ 
#          PCB2$Total_Shell_Height_Length_mm[which(PCB2$Location_Index=="Chester" & PCB2$Ploidy=="Diploid")], 
#        ylim=c(0,0.6), xlim=c(20,150), type='p', pch=19, col='red')
# legend('topright', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))
# plot(PCB2$`Shell P content mg/g`[which(PCB2$Location_Index=="RAPP" & PCB2$Ploidy=="Triploid")] ~ 
#        PCB2$Total_Shell_Height_Length_mm[which(PCB2$Location_Index=="RAPP"& PCB2$Ploidy=="Triploid")], 
#      ylim=c(0,0.6), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Shell %P', xlab='SH (mm)', main='Rappahannock')
# points(PCB2$`Shell P content mg/g`[which(PCB2$Location_Index=="RAPP" & PCB2$Ploidy=="Diploid")] ~ 
#          PCB2$Total_Shell_Height_Length_mm[which(PCB2$Location_Index=="RAPP" & PCB2$Ploidy=="Diploid")], 
#        ylim=c(0,0.6), xlim=c(20,150), type='p', pch=19, col='red')
# legend('topright', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('red', 'gray'))

plot(PCB2$shell_percent_P[which(PCB2$Location_Index=="Chester" & PCB2$Ploidy=="Triploid")] ~ 
       PCB2$Total_Shell_Height_Length_mm[which(PCB2$Location_Index=="Chester"& PCB2$Ploidy=="Triploid")], 
     ylim=c(0.03,0.06), xlim=c(20,150), type='p', pch=19, col='gray', ylab='Shell %P', xlab='SH (mm)', las=1, main='Shell P')
points(PCB2$shell_percent_P[which(PCB2$Location_Index=="Chester" & PCB2$Ploidy=="Diploid")] ~ 
         PCB2$Total_Shell_Height_Length_mm[which(PCB2$Location_Index=="Chester" & PCB2$Ploidy=="Diploid")], 
       pch=19, col='red')
points(PCB2$shell_percent_P[which(PCB2$Location_Index=="RAPP" & PCB2$Ploidy=="Triploid")] ~ 
       PCB2$Total_Shell_Height_Length_mm[which(PCB2$Location_Index=="RAPP"& PCB2$Ploidy=="Triploid")], 
      pch=17, col='black')
points(PCB2$shell_percent_P[which(PCB2$Location_Index=="RAPP" & PCB2$Ploidy=="Diploid")] ~ 
         PCB2$Total_Shell_Height_Length_mm[which(PCB2$Location_Index=="RAPP" & PCB2$Ploidy=="Diploid")], 
       pch=17, col='orange')
legend('topright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))





# testing ploidy on fits from CBay Poach data
plot(poachdiploid$Tissue_Dry_Weight_g ~ poachdiploid$Total_Shell_Height_Length_mm, type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, main='CB Poach')
points(poachtriploid$Tissue_Dry_Weight_g ~ poachtriploid$Total_Shell_Height_Length_mm, type='p', col='purple')
clmd=lm(log(poachtriploid$Tissue_Dry_Weight_g)~log(poachtriploid$Total_Shell_Height_Length_mm))
summary(clmd)
xval=seq(20,140,by=.5)
yval=exp(clmd$coefficients[1])*xval^clmd$coefficients[2]
lines(xval, yval, col='red', lwd=2)
clmt=lm(log(poachdiploid$Tissue_Dry_Weight_g)~log(poachdiploid$Total_Shell_Height_Length_mm))
summary(clmt)
xval=seq(20,140,by=.5)
yval=exp(clmt$coefficients[1])*xval^clmt$coefficients[2]
lines(xval, yval, col='black', lwd=2)
legend('topleft', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('gray', 'purple'))
## add CBP data fits
xval=seq(20,140,by=.5)
yval=exp(cbdlm$coefficients[1])*xval^cbdlm$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=2)
xval2=seq(20,140,by=.5)
yval2=exp(cbtlm$coefficients[1])*xval^cbtlm$coefficients[2]
lines(xval2, yval2, col='red', lwd=2, lty=2)

anova(clmd, clmt, test="Chisq")


barplot(table(round(poach$Total_Shell_Height_Length_mm,-1)), main="All")
barplot(table(round(MD$Total_Shell_Height_Length_mm,-1)), main="Chester")
barplot(table(round(VA$Total_Shell_Height_Length_mm,-1)), main="Rappahannock")
barplot(table(round(poachdiploid$Total_Shell_Height_Length_mm,-1)), main="diploid")
barplot(table(round(poachtriploid$Total_Shell_Height_Length_mm,-1)), main="triploid")

### look at distribution of SH by site and ploidy
tbl=with(poach, table(round(Total_Shell_Height_Length_mm,-1), Waterbody_Name))
# barplot(tbl, beside = TRUE, legend = TRUE, bty='n')
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Waterbody_Name)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Site")
  
tbl=with(poach, table(round(Total_Shell_Height_Length_mm,-1), Waterbody_Name, Ploidy))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Ploidy)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Ploidy")

### compare distribution of SH by ploidy
tbl=with(poachdiploid, table(round(Total_Shell_Height_Length_mm,-1), Waterbody_Name))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Waterbody_Name)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Site", title="Diploid")
tbl=with(poachtriploid, table(round(Total_Shell_Height_Length_mm,-1), Waterbody_Name))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Waterbody_Name)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Site", title="Triploid")
### compare distribution of SH by month removed
tbl=with(poach, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="Size at removal")
tbl=with(MD, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="MD Size at removal")
tbl=with(VA, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="VA Size at removal")
tbl=with(poachdiploid, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="Dip Size at removal")
tbl=with(poachtriploid, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="Trip Size at removal")


# lines(expmod$fitted.values,lwd=2, col = "red")
# SH=seq(20,150,.1)
# expline=predict(expmod, list('poach$Total_Shell_Height_Length_mm'=SH))
# lines(SH, expline, lwd=2, col='red')

# https://rpubs.com/mengxu/exponential-model
df=data.frame(poach$Tissue_Dry_Weight_g,poach$Total_Shell_Height_Length_mm);colnames(df)=c('y', 'x')
data.df=df[complete.cases(df),]
# m=coef(lm(log(df$y)~df$x))[2]
# https://rpubs.com/mengxu/exponential-model

## now try with MA samples, shell Length > shell height
reitsma.all=reitsma
reitsma=reitsma[reitsma$Species=='Cv',]
df=data.frame(reitsma$`Dry Tiss Mass (g)`,reitsma$`Shell Length (mm)`,reitsma$Town); colnames(df)=c('y', 'x', 'loc')
data.df=df[complete.cases(df),]
plot(df$y[df$loc=='Bourne']~df$x[df$loc=='Bourne'])
plot(df$y[df$loc=='Wellfleet']~df$x[df$loc=='Wellfleet'])
plot(df$y[df$loc=='Wareham']~df$x[df$loc=='Wareham'])

# + xlim(0,120) + ylim(0,7)
ggplot(df, aes(x=log(x), y=log(y))) +  labs(x='DW (g)', y='SH (cm)') + 
  geom_point(aes(color=loc)) 

ggplot(df, aes(x=y, y=x)) +  labs(x='DW (g)', y='SH (cm)') + ylim(0,120) + xlim(0,7) +
  geom_point(aes(color=loc)) 
ggplot(df, aes(x=x, y=y)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,120) + ylim(0,7) +
     geom_point(aes(color=loc))   
  
ggplot(df, aes(x=x, y=y)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,120) + ylim(0,7) +
     geom_point(aes(color=loc))   

ggplot(reitsma, aes(x=`Shell Length (mm)`, y=`Dry Tiss Mass (g)`)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,120) + ylim(0,7) +
  geom_point(aes(color=`off/on bottom`))   

theta.0 <- min(data.df$y) * 0.25  
model.0 <- lm(log(y - theta.0) ~ x, data=data.df)  
alpha.0 <- exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]
start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
start
model <- nls(y ~ alpha * exp(beta * x) + theta , data = data.df, start = start,trace = TRUE, control = list(maxiter = 300))
plot(data.df$x, data.df$y)
lines(data.df$x, predict(model, list(x = data.df$x)), col = 'skyblue', lwd = 3)
summary(model)
plot(profile(model))




ggplot(df, aes(x=x, y=y)) + 
  geom_point() +
  geom_smooth(method="lm", formula= (y ~ m*exp(x)), se=FALSE, color=1)


# nls(y~alpha * exp(beta*x)+theta, data=df, start=list(alpha=0.001, beta=))

plot(log(reitsma$`Dry Tiss Mass (g)`)~reitsma$`Shell Height (mm)`, type='p')

# plot(reitsma$`Dry Tiss Mass (g)`~reitsma$`Shell Height (mm)`, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
#      ylim=c(0,6), xlim=c(10,40), main='MA')
plot(reitsma$`Dry Tiss Mass (g)`~reitsma$`Shell Length (mm)`, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(50,140), main='MA')
rlm=lm(log(reitsma$`Dry Tiss Mass (g)`)~log(reitsma$`Shell Length (mm)`))
xval=seq(0,140,by=.5)
yval=exp(rlm$coefficients[1])*xval^rlm$coefficients[2]
lines(xval, yval)
text(70,6.8, paste('y=',round(exp(rlm$coefficients[1]),9),'x^',round(rlm$coefficients[2],3), sep='' ))




### plot all model lines
rxval=seq(0,140,by=.5)
ryval=exp(rlm$coefficients[1])*rxval^rlm$coefficients[2]
bxval=seq(0,140,by=.5)
byval=exp(blm$coefficients[1])*bxval^blm$coefficients[2]
cxval=seq(0,140,by=.5)
cyval=exp(clm$coefficients[1])*cxval^clm$coefficients[2]
sxval=seq(0,140,by=.5)
syval=exp(slm$coefficients[1])*sxval^slm$coefficients[2]
gxval=seq(0,140,by=.5)
gyval=exp(glm$coefficients[1])*gxval^glm$coefficients[2]

kxval=seq(0,140,by=.5)
kyval=exp(klm$coefficients[1])*kxval^klm$coefficients[2] # ME
k2yval=exp(klm2$coefficients[1])*kxval^klm2$coefficients[2] #diploid ME
k3yval=exp(klm3$coefficients[1])*kxval^klm3$coefficients[2] #triploid ME


njxval=seq(0,140,by=.5)
dbyval=exp(dblm$coefficients[1])*njxval^dblm$coefficients[2]
rbyval=exp(rblm$coefficients[1])*njxval^rblm$coefficients[2]
bbyval=exp(bblm$coefficients[1])*njxval^bblm$coefficients[2]

plot(Seb2$`Cage Mean (dry weight)` ~Seb2$MeanSH, type='n', xlab='SH (mm)', ylab='DW (g)', las=1, 
     ylim=c(0,7), xlim=c(0,150), main='All')
lines(gxval, gyval, col='black') #NH
lines(rxval, ryval, col='red') #MA
lines(cxval, cyval, col='blue')#CB
lines(sxval, syval, col='purple') #NY
lines(bxval, byval, col='green') #CT
lines(njxval, dbyval, col='yellow') #NJ Delaware Bay
lines(njxval, bbyval, col='orange') #NJ Barnegat Bay
lines(njxval, rbyval, col='brown') #DE Rehobath Bay
legend('topleft', bty='n', 
       legend = c('NH', 'MA', 'CB', 'NY', 'CT', 'NJ Del', 'NJ Barn', 'DE Reho'), 
       text.col=c('black', 'red', 'blue', 'purple', 'green', 'yellow', 'orange', 'brown'))
lines(kxval,kyval, lty=2, col='black')
lines(kxval,k2yval, lty=3, col='black') # need to fix scale (mm, mg)
lines(kxval,k3yval, lty=4, col='black') # need to fix scale (mm, mg)



plot(Seb2$`Cage Mean (dry weight)`~Seb2$MeanSH, type='n', xlab='SH (mm)', ylab='DW (g)', las=1, 
     ylim=c(0,7), xlim=c(0,150), main='All')
points(CB$Tissue_Dry_Weight_g ~ CB$Total_Shell_Height_Length_mm,  pch=16,col='gray')
points(grz$DW~grz$`Shell Height (mm)`, pch=16,col='black')
points(reitsma$`Dry Tiss Mass (g)`~reitsma$`Shell Length (mm)`, pch=16, col='red')
points(poach$`Tissue_Dry_Weight_g` ~ poach$`Total_Shell_Height_Length_mm`, pch=16, col='blue')
points(Seb2$`Cage Mean (dry weight)`~Seb2$MeanSH, pch=16, col='orange')
points(bayer2$`dry tissue weight (g)` ~ bayer2$SH, pch=16, col='brown')
legend('topleft', bty='n', horiz=F, legend = c('NH', 'MA', 'CB-P', 'NY', 'CT', 'CB-C'), 
       text.col=c('black', 'red', 'blue', 'orange', 'brown', 'gray'))

df=reitsma[reitsma$`Water Body`=='Wellfleet Harbor',]# %>%
ggplot(df, aes(x=`Shell Height (mm)`, y=`Dry Tiss Mass (g)`)) +  labs(y='DW (g)', x='SH (mm)') + xlim(0,60) + ylim(0,7) + geom_point(aes(color=`OysterGrp`))



df=reitsma %>% select(`Water Body`=='Wellfleet Harbor') %>%
ggplot(aes(x=`Shell Height (mm)`, y=`Dry Tiss Mass (g)`)) +  
  labs(y='DW (g)', x='SH (cm)') + xlim(0,120) + ylim(0,700) + 
  geom_point(aes(color=`OysterGrp`))

### Cornwell CBP data
CB.2023=readxl::read_xlsx("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Oyster morphometrics/Data_compiled_MASTER_updated on11-15-17_2.xlsx",
                          sheet='Compiled_With_Formulas', skip=0, col_names = T)
CB.2023$Tissue_Dry_Weight_g[which(CB.2023$Tissue_Dry_Weight_g<=0)]=NA

CB.tissue=readxl::read_xlsx("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Oyster morphometrics/Data_compiled_MASTER_updated on11-15-17_2.xlsx",
                          sheet='Used_Tissue Analysis', skip=0, col_names = T)
CB.shell=readxl::read_xlsx("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Oyster morphometrics/Data_compiled_MASTER_updated on11-15-17_2.xlsx",
                          sheet='Used_Shell Analysis', skip=0, col_names = T)

CB=readxl::read_xlsx("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Oyster morphometrics/CB-oyster expert panel-compiled data 9.xlsx",
                     sheet='All_Shell Height_Tissue_DryWT', skip=0, col_names = T)
CBd=CB %>% filter(Ploidy=='Diploid')
CBt=CB %>% filter(Ploidy=='Triploid')
CBd %>% ggplot(aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (mm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=Waterbody_Name))   
CBt %>% ggplot(aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (mm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=Waterbody_Name)) 
boxplot(CB$Tissue_N_Percent ~ CB$Month_Oysters_Removed, ylim=c(0,10))
ggplot(CB, aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=Waterbody_Name)) 
ggplot(CB, aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=State)) 
ggplot(CB, aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=Relevant_Oyster_Practice_Aquaculture)) 
ggplot(CB, aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=Subtidal_Intertidal_WaterColumn_Other)) 

ggplot(CBd, aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=Waterbody_Name)) 
ggplot(CBt, aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (cm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=Waterbody_Name)) 

Subtidal_Intertidal_WaterColumn_Other
plot(CBd$Tissue_Dry_Weight_g ~ CBd$Total_Shell_Height_Length_mm, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='CB Cornwell')
cbdlm=lm(log(CBd$Tissue_Dry_Weight_g) ~ log(CBd$Total_Shell_Height_Length_mm))
xval=seq(0,140,by=.5)
yval=exp(cbdlm$coefficients[1])*xval^cbdlm$coefficients[2]
lines(xval, yval, col='red')
text(70,6.8, paste('y=',round(exp(cbdlm$coefficients[1]),9),'x^',round(cbdlm$coefficients[2],3), sep='' ))

plot(CBt$Tissue_Dry_Weight_g ~ CBt$Total_Shell_Height_Length_mm, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='CB Cornwell')
cbtlm=lm(log(CBt$Tissue_Dry_Weight_g) ~ log(CBt$Total_Shell_Height_Length_mm))
xval=seq(0,140,by=.5)
yval=exp(cbtlm$coefficients[1])*xval^cbtlm$coefficients[2]
lines(xval, yval, col='red')
text(70,6.8, paste('y=',round(exp(cbtlm$coefficients[1]),9),'x^',round(cbtlm$coefficients[2],3), sep='' ))
## plot both triploid and diploid
plot(CBd$Tissue_Dry_Weight_g ~ CBd$Total_Shell_Height_Length_mm, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='CB Cornwell', col='gray')
points(CBt$Tissue_Dry_Weight_g ~ CBt$Total_Shell_Height_Length_mm, type='p', col='purple')
xval=seq(0,140,by=.5)
yval=exp(cbdlm$coefficients[1])*xval^cbdlm$coefficients[2]
lines(xval, yval, col='black', lwd=2)
xval2=seq(0,140,by=.5)
yval2=exp(cbtlm$coefficients[1])*xval^cbtlm$coefficients[2]
lines(xval2, yval2, col='red', lwd=2)
text(70,6.8, paste('y=',round(exp(cbtlm$coefficients[1]),9),'x^',round(cbtlm$coefficients[2],3), sep='' ))


plot(Seb3$`DW (g)`~Seb3$`Site mean`, type='n', xlab='SH (mm)', ylab='DW (g)', las=1, 
     ylim=c(0,7), xlim=c(0,150), main='All')
lines(gxval, gyval, col='black') #NH
lines(rxval, ryval, col='red') #MA
lines(cxval, cyval, col='blue')#CB Poach
lines(sxval, syval, col='purple') #NY
lines(bxval, byval, col='green') #CT
lines(xval, yval, col='black', lwd=2) # triploid CB
lines(xval2, yval2, col='red', lwd=2) # diploid CB
legend('topleft', bty='n', legend = c('NH', 'MA', 'CB P', 'NY', 'CT'), text.col=c('black', 'red', 'blue', 'purple', 'green'))
legend('bottomright', bty='n', legend = c('CB C_di', 'CB C_tri'), text.col=c('red','black'))
