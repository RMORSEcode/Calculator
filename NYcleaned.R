library(tidyverse)
library(ggExtra)
library(ggpubr)
library(lubridate)
library(quantreg)

wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
s1=match("Poach et al. in prep 2023", Main$Data_Source) # start of Poach data (after end of CB)
MainNoCB=Main[s1:dim(Main)[1],]
RegionFarm=MainNoCB %>% filter(!(Waterbody_Region %in% c("Jamaica Bay", "Hudson River", "Raritan Bay")))
## fix second column name assignment for Tissue Carbon
# test=as.matrix(RegionFarm$Tissue_TC_g_C_per_g_dw)
# colnames(test)=NULL
# RegionFarm=RegionFarm %>% dplyr::select(-Tissue_TC_g_C_per_g_dw)
# RegionFarm$Tissue_TC_g_C_per_g_dw=test[,1]
# ## reorder columns
# colnames(RegionFarm)
# RegionFarm=RegionFarm[,c(1:5,58,6:8,61,9:28,62,59,29:34,56,35:55,57,60)]
outfile=RegionFarm %>% select(-c(Raw_Data_File, Number_ID, Condition_Index:Shell_Wet_Weight_g))
# colnames(outfile)[15]="Total_Shell_Height_mm"
dt=lubridate::now()
dat=format(dt,"%Y%m%d")
write.csv(outfile, file=paste(wd,dat,'RegionFarm.csv', sep=''),row.names=FALSE)

## Write out CB data for Zenodo inclusion
CB2=Main[1:s1-1,]
CB2=CB2 %>% filter(Data_Source!="Powell_Mann") # drop Powell_Mann ( Mann, Southworth, and Wesson unpubl.)
# test=as.matrix(CB2$Tissue_TC_g_C_per_g_dw)
# colnames(test)=NULL
# CB2=CB2 %>% dplyr::select(-Tissue_TC_g_C_per_g_dw)
# CB2$Tissue_TC_g_C_per_g_dw=test[,1]
### fix entries
CB2$Site[which(CB2$Site=="budder")]="budden"
CB2$Subtidal_Intertidal_WaterColumn_Other[which(CB2$Subtidal_Intertidal_WaterColumn_Other=="Other")]=NA
CB2$Hatchery_produced_or_Wild[which(CB2$Hatchery_produced_or_Wild=="?")]=NA
CB2$Season_Oysters_Removed[which(CB2$Season_Oysters_Removed=="?")]=NA
CB2$Oyster_Stock[which(CB2$Oyster_Stock=="?")]=NA

### See CBzenodo.R for updated file handling for Zenodo submission 20240516 ###
# outfile=CB2 %>% select(-(c(Near_Waterbody_General_Location, Shell_Organic_C_Percent, Raw_Data_File, Number_ID,Location_Index,Site_within_Study,Month_Oysters_Removed,Year_Oysters_Removed,
#       Total_Shell_Height_Length_Inches , Waterbody_Region,Volume_ml:Gear_Class)))
# outfile=outfile[,c(1:24,26,25,28,27,29,31,30,33,32,35,34,36:38)]
# colnames(outfile)
# colnames(outfile)[9]
# colnames(outfile)[9]="Subtidal_Intertidal_WaterColumn"
# outfile$Data_Source[which(outfile$Data_Source=="Parker-unpubl")]="Parker-Bricker"
# colnames(outfile)[15]="Total_Shell_Height_mm"
# write.csv(outfile, file=paste(wd,'CB2023oysterBMP.csv', sep=''),row.names=FALSE)


Main[1:s1-1,] %>% select(Total_Shell_Height_Length_mm, Tissue_Dry_Weight_g, ) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=st_abrv)) + 
  geom_boxplot(color = "black", notch=T, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(0, 15))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 


## tables used in paper
# with(RegionFarm, table(st_abrv, Gear_Class,Ploidy))
# with(RegionFarm, table(st_abrv,Ploidy))
# with(RegionFarm, table(st_abrv,Ploidy))
# with(RegionFarm, table(Data_Source, Waterbody_Region ))
## Tissue
with(RegionFarm, table(complete.cases(Tissue_N_Percent), Ploidy))
with(RegionFarm, table(complete.cases(Tissue_N_Percent), Gear_Class))
with(RegionFarm, table(complete.cases(Tissue_N_Percent), st_abrv))
## Shell
with(RegionFarm, table(complete.cases(Shell_N_Percent), Ploidy))
with(RegionFarm, table(complete.cases(Shell_N_Percent), Gear_Class))
with(RegionFarm, table(complete.cases(Shell_N_Percent), st_abrv))

with(RegionFarm[RegionFarm$st_abrv=="ME",], table(Site, Ploidy, st_abrv))


### summarize data for tables
## overall
test=RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(Tissue_N_Percent, Shell_N_Percent) %>% 
    summarise(mnNt=mean(Tissue_N_Percent, na.rm=T), sdNt=sd(Tissue_N_Percent, na.rm=T), nNT=n(),
            mnNs=mean(Shell_N_Percent, na.rm=T), sdNs=sd(Shell_N_Percent, na.rm=T), nNS=n())
write.csv(test, file=paste(wd,"overall_mean_N_",dat,".csv", sep=''))
## state
test=RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(st_abrv, Ploidy, Tissue_N_Percent, Shell_N_Percent) %>% 
  group_by(st_abrv) %>%
  summarise(mnNt=mean(Tissue_N_Percent, na.rm=T), sdNt=sd(Tissue_N_Percent, na.rm=T),
            nNT=n(), mnNs=mean(Shell_N_Percent, na.rm=T), sdNs=sd(Shell_N_Percent, na.rm=T))
write.csv(test, file=paste(wd,"state_mean_N_",dat,".csv", sep=''))
### ploidy
test=RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(Ploidy, Tissue_N_Percent, Shell_N_Percent) %>% 
  group_by(Ploidy) %>%
  summarise(mnNt=mean(Tissue_N_Percent, na.rm=T), sdNt=sd(Tissue_N_Percent, na.rm=T),
            nNT=n(), mnNs=mean(Shell_N_Percent, na.rm=T), sdNs=sd(Shell_N_Percent, na.rm=T))
write.csv(test, file=paste(wd,"ploidy_mean_N_",dat,".csv", sep=''))
### gear
test=RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(Gear_Class, Tissue_N_Percent, Shell_N_Percent) %>% 
  group_by(Gear_Class) %>%
  summarise(mnNt=mean(Tissue_N_Percent, na.rm=T), sdNt=sd(Tissue_N_Percent, na.rm=T),
            nNT=n(), mnNs=mean(Shell_N_Percent, na.rm=T), sdNs=sd(Shell_N_Percent, na.rm=T))
write.csv(test, file=paste(wd,"gear_mean_N_",dat,".csv", sep=''))
with(RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),], table(st_abrv, Gear_Class))
### overlay with states
### regressions for gear
### regressions for states
### compare N reduction with and w/o NY data
with(RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),], table(Location_Index,st_abrv))
with(RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),], table(Near_Waterbody_General_Location,st_abrv))
with(RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),], table(Waterbody_Name,st_abrv))



mcol=c('cyan','black', 'red', 'blue', 'orange', 'brown', 'yellow', 'purple', 'green', 'gray50', 'maroon')
colrs=col2rgb(mcol)
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
t.col=t_col(mcol, 50)

stt=sort(unique(RegionFarm$st_abrv))
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,10), xlim=c(0,200), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
for(i in 1:length(stt)){
  dataa2=RegionFarm[RegionFarm$st_abrv==stt[i],]
  mcol2=data.frame(col2rgb(mcol))
  colr=rgb(red = mcol2[1,i], green=mcol2[2,i], blue=mcol2[3,i], alpha=30, maxColorValue = 255)
  points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=colr, ylim=c(0,8), xlim=c(0,200))
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  a=summary(qrx)$coefficients[1,1]
  b=summary(qrx)$coefficients[2,1]
  x=seq(0, 150, length = 250)
  yval=(a*(x^b))
  lines(x, yval, col=ifelse(summary(qrx)$coefficients[1,4]<0.05, mcol[i], mcol[i]), lwd=ifelse(summary(qrx)$coefficients[1,4]<0.05, 2, 0))
  # lines(x, yval, col=mcol[i], lwd=2)
  text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste(stt[i],'*', sep=' '),''), col=mcol[i])
  # text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
  
}
legend('topleft', bty='n', 
       legend = stt, 
       text.col=mcol)

plot(RegionFarm$Shell_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,200), xlim=c(0,200), ylab="Shell dry weight (g)", xlab="Shell height (mm)", las=1)
for(i in c(1,3:6,9:length(stt))){
  dataa2=RegionFarm[RegionFarm$st_abrv==stt[i],]
  points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=mcol[i], ylim=c(0,200), xlim=c(0,200))
  qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
  a=summary(qrx)$coefficients[1,1]
  b=summary(qrx)$coefficients[2,1]
  x=seq(0, 150, length = 250)
  yval=(a*(x^b))
  lines(x, yval, col=ifelse(summary(qrx)$coefficients[1,4]<0.05, mcol[i], mcol[i]), lwd=ifelse(summary(qrx)$coefficients[1,4]<0.05, 2, 1))
  # lines(x, yval, col=mcol[i], lwd=2)
  text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste(stt[i],'*', sep=' '),''), col=mcol[i])
  # text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
  
}
legend('topleft', bty='n', 
       legend = stt, 
       text.col=mcol)


## compare diploid and triploid in ME, floating gear
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, ylim=c(0,10), xlim=c(0,200), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1, type='n')
dataa2=RegionFarm %>% filter(Site=="Pemaquid Oyster Company",Ploidy=="Diploid", st_abrv=="ME")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col='pink', ylim=c(0,8), xlim=c(0,200))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='red', lwd=2)
dataa2=RegionFarm %>% filter(Site=="Pemaquid Oyster Company", Ploidy=="Triploid", st_abrv=="ME")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col='gray30', ylim=c(0,8), xlim=c(0,200))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='black', lwd=2)


### Gear Class w/ polygons for Tissue###
### add polygons of upper and lower fit (change tau for upper and lower)
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, ylim=c(0,10), xlim=c(0,200), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)

# points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.72, blue = 0.72, alpha = 0.1))
# points(Main[1:s1-1,]$Tissue_Dry_Weight_g ~ Main[1:s1-1,]$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))

plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     pch=19, ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)

dataa2=RegionFarm %>% filter(Gear_Class=="Floating")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))

dataa2=RegionFarm %>% filter(Gear_Class=="Floating")
qrxu=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.75)
qrxl=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.25)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
au=summary(qrxu)$coefficients[1,1]
bu=summary(qrxu)$coefficients[2,1]
yvalu=(au*(x^bu))
al=summary(qrxl)$coefficients[1,1]
bl=summary(qrxl)$coefficients[2,1]
yvall=(al*(x^bl))
polygon(c(x,rev(x)),c(yvalu, rev(yval)), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
polygon(c(x,rev(x)),c(yvall, rev(yval)), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
text(180,yval[250], labels="Floating")
text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
# points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
### Bottom
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
# points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
qrxu=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.75)
qrxl=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.25)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
au=summary(qrxu)$coefficients[1,1]
bu=summary(qrxu)$coefficients[2,1]
yvalu=(au*(x^bu))
al=summary(qrxl)$coefficients[1,1]
bl=summary(qrxl)$coefficients[2,1]
yvall=(al*(x^bl))
polygon(c(x,rev(x)),c(yvalu, rev(yval)), col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
polygon(c(x,rev(x)),c(yvall, rev(yval)), col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.9), lwd=2)
text(180,yval[250], labels="Bottom")
text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
# points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
### No Gear
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
# points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
qrxu=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.75)
qrxl=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.25)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
au=summary(qrxu)$coefficients[1,1]
bu=summary(qrxu)$coefficients[2,1]
yvalu=(au*(x^bu))
al=summary(qrxl)$coefficients[1,1]
bl=summary(qrxl)$coefficients[2,1]
yvall=(al*(x^bl))
polygon(c(x,rev(x)),c(yvalu, rev(yval)), col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
polygon(c(x,rev(x)),c(yvall, rev(yval)), col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.9), lwd=2)
text(180,yval[250], labels="No Gear")
text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
legend('topleft', pch=19, col=c('red','green','blue'), legend=c("Floating", "On-bottom", "Off-bottom"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)

### Gear Class w/ polygons For Shells###
plot(Main$Shell_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, ylim=c(0,200), xlim=c(0,200), ylab="Shell dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)

plot(Main$Shell_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='n', 
     pch=19, ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)

points(RegionFarm$Shell_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.72, blue = 0.72, alpha = 0.1))
points(Main[1:s1-1,]$Shell_Dry_Weight_g ~ Main[1:s1-1,]$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))

dataa2=RegionFarm %>% filter(Gear_Class=="Floating")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))

dataa2=RegionFarm %>% filter(Gear_Class=="Floating")
qrxu=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.75)
qrxl=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.25)
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
au=summary(qrxu)$coefficients[1,1]
bu=summary(qrxu)$coefficients[2,1]
yvalu=(au*(x^bu))
al=summary(qrxl)$coefficients[1,1]
bl=summary(qrxl)$coefficients[2,1]
yvall=(al*(x^bl))
polygon(c(x,rev(x)),c(yvalu, rev(yval)), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
polygon(c(x,rev(x)),c(yvall, rev(yval)), col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
text(180,yval[250], labels="Floating")
text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
# points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
### Bottom
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
# points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
qrxu=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.75)
qrxl=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.25)
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
au=summary(qrxu)$coefficients[1,1]
bu=summary(qrxu)$coefficients[2,1]
yvalu=(au*(x^bu))
al=summary(qrxl)$coefficients[1,1]
bl=summary(qrxl)$coefficients[2,1]
yvall=(al*(x^bl))
polygon(c(x,rev(x)),c(yvalu, rev(yval)), col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
polygon(c(x,rev(x)),c(yvall, rev(yval)), col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.9), lwd=2)
text(180,yval[250], labels="Bottom")
text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
# points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
### No Gear
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
# points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
qrxu=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.75)
qrxl=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.25)
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
au=summary(qrxu)$coefficients[1,1]
bu=summary(qrxu)$coefficients[2,1]
yvalu=(au*(x^bu))
al=summary(qrxl)$coefficients[1,1]
bl=summary(qrxl)$coefficients[2,1]
yvall=(al*(x^bl))
polygon(c(x,rev(x)),c(yvalu, rev(yval)), col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
polygon(c(x,rev(x)),c(yvall, rev(yval)), col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.9), lwd=2)
text(180,yval[250], labels="No Gear")
text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)

legend('topleft', pch=19, col=c('red','green','blue'), legend=c("Floating", "On-bottom", "Off-bottom"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
# points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))

#add all points light gray
points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
points(Main$Tissue_Dry_Weight_g[1:s1-1] ~Main$Total_Shell_Height_Length_mm[1:s1-1], pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
points(Main$Tissue_Dry_Weight_g ~Main$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))



### Ploidy Tissue
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
dataa2=RegionFarm %>% filter(Ploidy=="Diploid")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
qrxu=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b =1.83359), tau=0.75)
qrxl=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b =1.83359), tau=0.25)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
au=summary(qrxu)$coefficients[1,1]
bu=summary(qrxu)$coefficients[2,1]
yvalu=(au*(x^bu))
al=summary(qrxl)$coefficients[1,1]
bl=summary(qrxl)$coefficients[2,1]
yvall=(al*(x^bl))
polygon(c(x,rev(x)),c(yvalu, rev(yval)), col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
polygon(c(x,rev(x)),c(yvall, rev(yval)), col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))

points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 0, alpha = 0.9), lwd=2)
# text(180,yval[250], labels="Diploid")
# text(180,yval[250], labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=.75)
dataa2=RegionFarm %>% filter(Ploidy=="Triploid")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
legend('topleft', pch=19, col=c('blue', 'red'), legend=c("Diploid", "Triploid"), bty='n')
# text(180,yval[250], labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=.75)
# text(180,yval[250], labels="Triploid")
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)


table(RegionFarm$st_abrv[RegionFarm$Ploidy=="Triploid"])
table(RegionFarm$st_abrv[RegionFarm$Ploidy=="Diploid"])

### Ploidy Shells
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab="Shell height (mm)", las=1)
dataa2=RegionFarm %>% filter(Ploidy=="Diploid")
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
summary(qrx)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 0, alpha = 0.9), lwd=2)
# text(180,yval[250], labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=.75)
dataa2=RegionFarm %>% filter(Ploidy=="Triploid")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
summary(qrx)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
legend('topleft', pch=19, col=c('blue', 'red'), legend=c("Diploid", "Triploid"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
# text(180,yval[250], labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=.75)

# plot(Main$Tissue_Dry_Weight_g ~ Main$Total_Shell_Height_Length_mm, type='n', 
#      ylim=c(0,10), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
# points(RegionFarm$Tissue_Dry_Weight_g[RegionFarm$Ploidy=="Diploid"] ~RegionFarm$Total_Shell_Height_Length_mm[RegionFarm$Ploidy=="Diploid"], pch=19, col=rgb(red = 0.72, green = 0.52, blue = 0.52, alpha = 0.1))
# points(RegionFarm$Tissue_Dry_Weight_g[RegionFarm$Ploidy=="Triploid"] ~RegionFarm$Total_Shell_Height_Length_mm[RegionFarm$Ploidy=="Triploid"], pch=19, col='black')

### save SH:DW by state for SHELL
stt=sort(unique(RegionFarm$st_abrv))
SHDWf=data.frame(matrix(NA, nrow=length(stt)+1, ncol=7))
colnames(SHDWf)=c("State", "a", "b", "SEa", "SEb","Pa","Pb")
SHDWf[,1]=c("Avg.",stt)
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.0007, b = 2), tau=0.5)
SHDWf[1,2]=summary(qrx)$coefficients[1,1]
SHDWf[1,3]=summary(qrx)$coefficients[2,1]
SHDWf[1,4]=summary(qrx)$coefficients[1,2]
SHDWf[1,5]=summary(qrx)$coefficients[2,2]
SHDWf[1,6]=summary(qrx)$coefficients[1,4]
SHDWf[1,7]=summary(qrx)$coefficients[2,4]
for(i in c(1,3:6,9:length(stt))){
  dataa2=RegionFarm[RegionFarm$st_abrv==stt[i],]
  qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
  if(summary(qrx)$coefficients[1,4]>0.05){
    next
  }
  else{
    SHDWf[i+1,2]=summary(qrx)$coefficients[1,1]
    SHDWf[i+1,3]=summary(qrx)$coefficients[2,1]
    SHDWf[i+1,4]=summary(qrx)$coefficients[1,2]
    SHDWf[i+1,5]=summary(qrx)$coefficients[2,2]
    SHDWf[i+1,6]=summary(qrx)$coefficients[1,4]
    SHDWf[i+1,7]=summary(qrx)$coefficients[2,4]
    }
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
SHDWf2=SHDWf[order(SHDWf$State),]
write.csv(SHDWf2, file=paste(wd,dat,"shell_SH_DW_qr50_state_data.csv"))
### save SH:DW by state for TISSUE
SHDWf=data.frame(matrix(NA, nrow=length(stt)+1, ncol=7))
colnames(SHDWf)=c("State", "a", "b", "SEa", "SEb","Pa","Pb")
SHDWf[,1]=c("Avg.",stt)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.00037, b = 1.83359), tau=0.5)
SHDWf[1,2]=summary(qrx)$coefficients[1,1]
SHDWf[1,3]=summary(qrx)$coefficients[2,1]
SHDWf[1,4]=summary(qrx)$coefficients[1,2]
SHDWf[1,5]=summary(qrx)$coefficients[2,2]
SHDWf[1,6]=summary(qrx)$coefficients[1,4]
SHDWf[1,7]=summary(qrx)$coefficients[2,4]
for(i in c(1:length(stt))){
  dataa2=RegionFarm[RegionFarm$st_abrv==stt[i],]
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  if(summary(qrx)$coefficients[1,4]>0.05){
    next
  }
  else{
    SHDWf[i+1,2]=summary(qrx)$coefficients[1,1]
    SHDWf[i+1,3]=summary(qrx)$coefficients[2,1]
    SHDWf[i+1,4]=summary(qrx)$coefficients[1,2]
    SHDWf[i+1,5]=summary(qrx)$coefficients[2,2]
    SHDWf[i+1,6]=summary(qrx)$coefficients[1,4]
    SHDWf[i+1,7]=summary(qrx)$coefficients[2,4]
  }
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
SHDWf2=SHDWf[order(SHDWf$State),]
write.csv(SHDWf2, file=paste(wd,dat,"Tissue_SH_DW_qr50_state_data.csv"))

### Tissue Shell height to dry weight relationship for gear class
SHDW=data.frame(matrix(NA, nrow=length(unique(RegionFarm$Gear_Class)), ncol=7))
colnames(SHDW)=c("GearClass", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in(1:3)){
  j=unique(RegionFarm$Gear_Class)[i]
  dataa2=RegionFarm %>% filter(Gear_Class==j)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  SHDW[i,1]=j
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
write.csv(SHDW, file=paste(wd,dat,"Tissue_SH_DW_qr50_gear_data.csv"))
### SHELLS Shell height to dry weight relationship for gear class
SHDW=data.frame(matrix(NA, nrow=length(unique(RegionFarm$Gear_Class)), ncol=7))
colnames(SHDW)=c("GearClass", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in(1:3)){
  j=unique(RegionFarm$Gear_Class)[i]
  dataa2=RegionFarm %>% filter(Gear_Class==j)
  qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
  SHDW[i,1]=j
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
write.csv(SHDW, file=paste(wd,dat,"Shells_SH_DW_qr50_gear_data.csv"))

### Ploidy
SHDW=data.frame(matrix(NA, nrow=length(unique(RegionFarm$Gear_Class)), ncol=7))
colnames(SHDW)=c("GearClass", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in(1:2)){
  j=unique(RegionFarm$Ploidy)[i]
  dataa2=RegionFarm %>% filter(Ploidy==j)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  SHDW[i,1]=j
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
write.csv(SHDW, file=paste(wd,dat,"Tissue_SH_DW_qr50_ploidy_data.csv"))
## Ploidy shells
SHDW=data.frame(matrix(NA, nrow=length(unique(RegionFarm$Gear_Class)), ncol=7))
colnames(SHDW)=c("GearClass", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in(1:2)){
  j=unique(RegionFarm$Ploidy)[i]
  dataa2=RegionFarm %>% filter(Ploidy==j)
  qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
  SHDW[i,1]=j
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
write.csv(SHDW, file=paste(wd,dat,"Shells_SH_DW_qr50_ploidy_data.csv"))

### Full data sets
SHDW=data.frame(matrix(NA, nrow=1, ncol=7))
colnames(SHDW)=c("Div", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in(1)){
  # j=unique(RegionFarm$Ploidy)[i]
  dataa2=RegionFarm #%>% filter(Ploidy==j)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  SHDW[i,1]="All data"
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
write.csv(SHDW, file=paste(wd,dat,"Tissue_SH_DW_qr50_all.csv"))
### Shell Full data set
SHDW=data.frame(matrix(NA, nrow=1, ncol=7))
colnames(SHDW)=c("Div", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in(1)){
  # j=unique(RegionFarm$Ploidy)[i]
  dataa2=RegionFarm #%>% filter(Ploidy==j)
  qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
  SHDW[i,1]="All data"
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
write.csv(SHDW, file=paste(wd,dat,"Shell_SH_DW_qr50_all.csv"))

## Just CB Panel
SHDW=data.frame(matrix(NA, nrow=3, ncol=7))
colnames(SHDW)=c("Div", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in(1)){
  # j=unique(RegionFarm$Ploidy)[i]
  dataa2=Main %>% filter(Panel==T)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  SHDW[i,1]="All data"
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
for(i in(2:3)){
  j=unique(Main$Ploidy)[i-1]
  dataa2=Main %>% filter(Panel==T, Ploidy==j)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  SHDW[i,1]=j
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
write.csv(SHDW, file=paste(wd,dat,"Tissue_SH_DW_qr50_Panel.csv"))
# Shell
SHDW=data.frame(matrix(NA, nrow=3, ncol=7))
colnames(SHDW)=c("Div", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in(1)){
  # j=unique(RegionFarm$Ploidy)[i]
  dataa2=Main %>% filter(Panel==T)
  qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
  SHDW[i,1]="All data"
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
for(i in(2:3)){
  j=unique(Main$Ploidy)[i-1]
  dataa2=Main %>% filter(Panel==T, Ploidy==j)
  qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
  SHDW[i,1]=j
  SHDW[i,2]=summary(qrx)$coefficients[1,1]
  SHDW[i,3]=summary(qrx)$coefficients[2,1]
  SHDW[i,4]=summary(qrx)$coefficients[1,2]
  SHDW[i,5]=summary(qrx)$coefficients[2,2]
  SHDW[i,6]=summary(qrx)$coefficients[1,4]
  SHDW[i,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=as.character(dt, format="%Y%m%d")
write.csv(SHDW, file=paste(wd,dat,"Shell_SH_DW_qr50_Panel.csv"))


plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,10), xlim=c(0,200), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
for(i in(1:2)){
  j=unique(Main$Ploidy)[i]
  dataa2=Main %>% filter(Panel==T, Ploidy==j)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=mcol[i], ylim=c(0,8), xlim=c(0,200))
  a=summary(qrx)$coefficients[1,1]
  b=summary(qrx)$coefficients[2,1]
  x=seq(0, 150, length = 250)
  yval=(a*(x^b))
  lines(x, yval, col='black', lwd=1)
  lines(x, yval, col='red', lwd=1)
  
  # lines(x, yval, col=mcol[i], lwd=2)
  # text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste(stt[i],'*', sep=' '),''), col=mcol[i])

}

# RegionFarm Tissue (all)
plot(Main$Tissue_Dry_Weight_g[1:s1-1] ~ Main$Total_Shell_Height_Length_mm[1:s1-1], type='p', pch=19, col='lightblue', 
     ylim=c(0,10), xlim=c(0,200), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
points(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, pch=21, col='gray20')
a=0.0000142135070166104
b=2.60727827
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='red', lwd=2)

test=RegionFarm[RegionFarm$st_abrv=='ME',]
table(test$Waterbody_Name, test$Ploidy)
test2=test[test$Waterbody_Name=="Damariscotta River",]
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
# points(test2$Tissue_Dry_Weight_g[test2$Ploidy=="Diploid"]~test2$Total_Shell_Height_Length_mm[test2$Ploidy=="Diploid"], pch=19, col='black')
# points(test2$Tissue_Dry_Weight_g[test2$Ploidy=="Triploid"]~test2$Total_Shell_Height_Length_mm[test2$Ploidy=="Triploid"], pch=21, col='red')
dataa2=RegionFarm %>% filter (st_abrv=='ME', Waterbody_Name=="Damariscotta River", Ploidy=="Diploid")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col='black', ylim=c(0,8), xlim=c(0,150))
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
# lines(x, yval, col='black', lwd=2)
dataa2=RegionFarm %>% filter (st_abrv=='ME', Waterbody_Name=="Damariscotta River", Ploidy=="Triploid")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=21, col='red', ylim=c(0,8), xlim=c(0,150))
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
lines(x, yval, col='black', lwd=2)
yval=(a*(x^b))
lines(x, yval, col='red', lwd=2)

dataa2=RegionFarm %>% filter (Data_Source=="Poach et al. in prep 2023", Ploidy=="Diploid")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=21, col='gray80', ylim=c(0,8), xlim=c(0,150))
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
dataa2=RegionFarm %>% filter (Data_Source=="Poach et al. in prep 2023", Ploidy=="Triploid")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=21, col='purple', ylim=c(0,8), xlim=c(0,150))
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
lines(x, yval, col='black', lwd=1, lty=2)
yval=(a*(x^b))
lines(x, yval, col='red', lwd=1, lty=2)
# lines(x, yval, col=mcol[i], lwd=2)
# text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste(stt[i],'*', sep=' '),''), col=mcol[i])

a = 0.00037
ax = 9.25e-05
a2 = ax*1
a2=ax*2
a2=ax*3
a2=ax*4
a2=ax*5

## testing sensitivity to start values for nlrq
# ax = 9.25e-05
# plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
# dataa2=RegionFarm %>% filter (Data_Source=="Poach et al. in prep 2023", Ploidy=="Triploid")
# for(i in (1:100)){
#   a2 = ax*i
#   qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = a2, b = 1.83359), tau=0.5)
#   a=summary(qrx)$coefficients[1,1]
#   b=summary(qrx)$coefficients[2,1]
#   yval=(a*(x^b))
#   lines(x, yval, col=i, lwd=1)
# }
# bx=1.0359
# for(i in (1:4)){
#   b2 = bx*i
#   qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = b2), tau=0.5)
#   a=summary(qrx)$coefficients[1,1]
#   b=summary(qrx)$coefficients[2,1]
#   yval=(a*(x^b))
#   lines(x, yval, col=i, lwd=1)
# }

my_comparisons=list(unique(RegionFarm$State[complete.cases(RegionFarm$Tissue_TP_Percent)]))
RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>% select(State, Ploidy, Tissue_TP_Percent) %>% 
  ggboxplot(y='Tissue_TP_Percent', x='State', fill='Ploidy',ylab = 'Tissue P Percent', xlab='', ylim=c(0.25,2)) +
  stat_compare_means(label = "p.signif", method = "anova", ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, label.y = c(1.75), label= "p.signif")

my_comparisons=list(unique(RegionFarm$State[complete.cases(RegionFarm$Tissue_N_Percent)]))
RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggboxplot(y='Tissue_N_Percent', x='State', fill='Ploidy', ylab = 'Tissue N Percent', xlab='', ylim=c(2.5,14)) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = ".all.") #+
# stat_compare_means(comparisons = my_comparisons, method = "anova", label.y = c(12.5),label= "p.signif") #+

## Tissue N
RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=st_abrv)) + 
  geom_boxplot(color = "black", notch=T, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(0, 15))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 
## Tissue N (gear class)
RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=Gear_Class)) + 
  geom_boxplot(color = "black", notch=T, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(0, 15))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  theme(text = element_text(size = 20)) +
  theme(
    # legend.position = c(0.5,0.15),
    # legend.text = element_text(size = 20),
    legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

## Tissue N (ploidy)
RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=Ploidy)) + 
  geom_boxplot(color = "black", notch=T, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(0, 15))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4,position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  theme(
    # legend.position = c(0.5,0.15),
    # legend.text = element_text(size = 20),
    legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))+
  theme(text = element_text(size = 20))  

## Shell N 
RegionFarm[complete.cases(RegionFarm$Shell_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Shell_N_Percent) %>% 
  ggplot(aes(y=Shell_N_Percent, x=st_abrv)) + 
  geom_boxplot(color = "black", notch=T, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Shell N Percent') +
  coord_cartesian(ylim = c(0, 1))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))
## Shell N (gear class)
RegionFarm[complete.cases(RegionFarm$Shell_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Shell_N_Percent) %>% 
  ggplot(aes(y=Shell_N_Percent, x=Gear_Class)) + 
  geom_boxplot(color = "black", notch=T, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Shell N Percent') +
  coord_cartesian(ylim = c(0, 1))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4,position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(
    # legend.position = c(0.5,0.95),
    # legend.text = element_text(size = 20),
    legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))+
  theme(text = element_text(size = 20))  
## Shell N (ploidy)
RegionFarm[complete.cases(RegionFarm$Shell_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Shell_N_Percent) %>% 
  ggplot(aes(y=Shell_N_Percent, x=Ploidy)) + 
  geom_boxplot(color = "black", notch=T, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Shell N Percent') +
  coord_cartesian(ylim = c(0, 1))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4,position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(
    # legend.position = c(0.5,0.95),
    # legend.text = element_text(size = 20),
    legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))+
  theme(text = element_text(size = 20))  


library(maps)
library(mapdata)
library(marmap)
nesbath=getNOAA.bathy(lon1=-79,lon2=-68,lat1=31,lat2=45, resolution=10, keep=F)
data(stateMapEnv)
wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
stations=readxl::read_xlsx(paste(wd, "Location_data.xlsx", sep=''),sheet='final2', range='A1:F33')

par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
map("worldHires", xlim=c(-79,-68),ylim=c(33,45), fill=T,border=0,col="gray70")
map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')
map.axes(las=1)
map('state', fill = F, add=T) # add state lines
points(stations$Longitude, stations$Latitude, pch=19, col='red')
points(stations$Longitude, stations$Latitude, pch=21, col='black', bg='red', cex=1.15)
# plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col="gray80",lty=1)
# plot(nesbath,deep=-50, shallow=-50, step=1,add=T,lwd=1,col=addTrans('blue',125),lty=1)
# plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col=addTrans('blue',50),lty=1)
# plot(nesbath,deep=-100, shallow=-100, step=1,add=T,lwd=1,col=addTrans('blue',75),lty=1)

## NY and New England
map("worldHires", xlim=c(-75,-68),ylim=c(40,45), fill=T,border=0,col="gray70")
map.axes(las=1)
map('state', fill = F, add=T) # add state lines
points(stations$Longitude, stations$Latitude, pch=19, col='red')
points(stations$Longitude, stations$Latitude, pch=21, col='black', bg='red', cex=1.15)

# plot stations with N in different color
stations2=stations %>% filter(Waterbody_Name %in% RegionFarm$Waterbody_Name[complete.cases(RegionFarm$Tissue_N_Percent)])
stations3=stations %>% filter(!(Waterbody_Name %in% RegionFarm$Waterbody_Name[complete.cases(RegionFarm$Tissue_N_Percent)]))

sta1=!(stations2 %in% stations)
par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
map("worldHires", xlim=c(-79,-68),ylim=c(33,45), fill=T,border=0,col="gray90")
map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')
map.axes(las=1)
map('state', fill = F, add=T) # add state lines
# points(stations$Longitude, stations$Latitude, pch=19, col='orange')
# points(stations$Longitude, stations$Latitude, pch=21, col='black', bg='orange', cex=1.15)
# points(stations2$Longitude, stations2$Latitude, pch=19, col='lightblue')
# points(stations2$Longitude, stations2$Latitude, pch=21, col='black', bg='lightblue', cex=1.15)
# points(stations2$Longitude, stations2$Latitude, pch=19, col='pink')
# points(stations2$Longitude, stations2$Latitude, pch=21, col='black', bg='pink', cex=1.15)
# points(stations$Longitude, stations$Latitude, pch=19, col='darkorange')
points(stations3$Longitude, stations3$Latitude, pch=21, col='black', bg='darkorange', cex=1.15)
# points(stations2$Longitude, stations2$Latitude, pch=19, col='lightgreen')
points(stations2$Longitude, stations2$Latitude, pch=21, col='black', bg='purple', cex=1.15)
legend(-75,36, pch=c(21,21), col='black', pt.bg=c('darkorange','purple'), legend=c('Morphometric data', 'Morphometric + N'), bty='n')

RegionFarm %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=Oyster_Growth_Location_Type))+ 
  geom_point()+
  ylim(0,8) +
  xlim(0, 150)
RegionFarm %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=Gear_Class))+ 
  geom_point()+
  ylim(0,8) +
  xlim(0, 150)

library(tidyverse)
library(ggExtra)
library(ggpubr)

mcol=c('cyan','black', 'red', 'blue', 'orange', 'brown', 'yellow', 'purple', 'green', 'gray50', 'maroon')

stt=sort(unique(RegionFarm$st_abrv))
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
points(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
       pch=19, col='gray10')

for(i in 1:length(stt)){
  dataa2=RegionFarm[RegionFarm$st_abrv==stt[i],]
  points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=mcol[i], ylim=c(0,8), xlim=c(0,200))
}
legend('topleft', bty='n', 
       legend = stt, 
       text.col=mcol)
RegionFarm %>% ggplot(aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g)) +  labs(y='DW (g)', x='SH (mm)') + xlim(0,140) + ylim(0,7) +
  geom_point(aes(color=st_abrv))   

### compare mid-Atlantic vs New England
newEng=RegionFarm %>% filter(st_abrv %in% c("ME", "NH", "MA", "RI", "CT"))
midAtl=RegionFarm %>% filter(!(st_abrv %in% c("ME", "NH", "MA", "RI", "CT")))
###
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
points(newEng$Tissue_Dry_Weight_g ~newEng$Total_Shell_Height_Length_mm, pch=19, col='gray30', ylim=c(0,8), xlim=c(0,200))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = newEng, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
points(midAtl$Tissue_Dry_Weight_g ~midAtl$Total_Shell_Height_Length_mm, pch=19, col=azul, ylim=c(0,8), xlim=c(0,200))
lines(x, yval, col='black', lwd=2)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = midAtl, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='blue', lwd=2)
legend('topleft', pch=c(19,19), lty=c(1,1), lwd=2, col=c('gray30',azul),  legend=c("New England","Mid-Atlantic"), bty='n')
legend('topleft', pch=c(NA,NA), lty=c(1,1), lwd=2, col=c('black', 'blue'), legend=c("New England","Mid-Atlantic"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
  
col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1)

"#00AFBB"
col2rgb(azul, alpha = T)
col2rgb('gray30', alpha = T)

rgb(0, 175/255, 187/255)
"#00AFBB"
rgb(77/255, 77/255, 77/255)
"#4D4D4D"


### testing effect of removing data sources on Panel 
CB2=Main[1:s1-1,] %>% filter(Data_Source!="Powell_Mann") 
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
points(CB$Tissue_Dry_Weight_g ~CB$Total_Shell_Height_Length_mm, pch=19, col='red')# rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
points(Main$Tissue_Dry_Weight_g[1:s1-1] ~Main$Total_Shell_Height_Length_mm[1:s1-1], pch=19, col='yellow')# rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
points(CB2$Tissue_Dry_Weight_g ~CB2$Total_Shell_Height_Length_mm, pch=19, col='gray70')# rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
legend('topleft', pch=c(19,19,19), col=c('red', 'yellow', 'gray70'), legend=c('All', '-K_H & K_O', '- P_M'), bty='n')
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = Main[1:s1-1,], start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrxs99)$coefficients[1,1]
b=summary(qrxs99)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='red', lwd=2, lty=1)

## plot CB data with RegionFarm overlay
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
points(Main$Tissue_Dry_Weight_g[1:s1-1] ~Main$Total_Shell_Height_Length_mm[1:s1-1], pch=19, col='gray40')# rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
# points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=24, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.2))
points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=17, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.2))
legend('topleft', pch=c(19, 17), col=c('gray70',rgb(red = 1, green = 0, blue = 0, alpha = 0.2)), legend=c("CBP 2023", "This study"), bty='n')
# text(25,6, labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=1)
## plot 50th quantile for all data in each data set
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = Main[1:s1-1,], start = list(a = 0.00037, b = 1.83359), tau=0.5)
print(summary(qrx))
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='red', lwd=2, lty=1)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.00037, b = 1.83359), tau=0.5)
print(summary(qrx))
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='black', lwd=2, lty=1)
a2=round(a,6)
b2=round(b,3)
text(25,6, labels=bquote(Y==.(a2)*X^.(b2)), cex=1)
## add error
ae=summary(qrx)$coefficients[1,2]
be=summary(qrx)$coefficients[2,2]  
yvalu=((a+ae)*(x^(b+be)))
yvall=((a-ae)*(x^(b-be)))
lines(x, yvalu, col='black', lwd=1, lty=2)
lines(x, yvall, col='black', lwd=1, lty=2)
## add interquatile range
qrxu=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.00037, b = 1.83359), tau=0.75)
qrxl=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.00037, b = 1.83359), tau=0.25)
au=summary(qrxu)$coefficients[1,1]
bu=summary(qrxu)$coefficients[2,1]
yvaluq=(au*(x^bu))
al=summary(qrxl)$coefficients[1,1]
bl=summary(qrxl)$coefficients[2,1]
yvallq=(al*(x^bl))  
lines(x, yvaluq, col='blue', lwd=1)
lines(x, yvallq, col='blue', lwd=1)
# CB diploid 2023 No Gear:
a=0.00037
b=1.83359
yval=(a*(x^b))
ae=0.00005
be=0.02896
lines(x, yval, col='green', lwd=2, lty=1)
yvalu=((a+ae)*(x^(b+be)))
yvall=((a-ae)*(x^(b-be)))
lines(x, yvalu, col='green', lwd=2, lty=2)
lines(x, yvall, col='green', lwd=2, lty=2)

legend('topleft', lty=c(1,2,1,2),col=c('red','red','black', 'black'), legend=c("99 no P-M","99 Full", "95 no P-M", "95 Full"), bty='n')


## plot CB data with RegionFarm overlay (shells)
plot(RegionFarm$Shell_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab="Shell height (mm)", las=1)
points(Main$Shell_Dry_Weight_g[1:s1-1] ~Main$Total_Shell_Height_Length_mm[1:s1-1], pch=19, col='gray70')# rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
# points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=24, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.2))
points(RegionFarm$Shell_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=17, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.2))
legend('topleft', pch=c(19, 17), col=c('gray70',rgb(red = 1, green = 0, blue = 0, alpha = 0.2)), legend=c("CBP 2023", "This study"), bty='n')
# text(25,6, labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=1)
## plot 50th quantile for all data in each data set
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = Main[1:s1-1,], start = list(a = 0.0007, b = 2), tau=0.5)
print(summary(qrx))
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='black', lwd=2, lty=2)
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.0007, b = 2), tau=0.5)
print(summary(qrx))
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='black', lwd=2, lty=1)
a2=round(a,6)
b2=round(b,3)
text(25,120, labels=bquote(Y==.(a2)*X^.(b2)), cex=1)

# create scatter plot using ggplot() function
plot <- ggplot(RegionFarm, aes(x=Total_Shell_Height_Length_mm, y=Tissue_Dry_Weight_g))+
  geom_point()+
  theme(legend.position="none")
# use ggMarginal function to create marginal histogram
ggMarginal(plot, type="histogram")

"#00AFBB"
ggscatterhist(RegionFarm, x = "Total_Shell_Height_Length_mm", y = "Tissue_Dry_Weight_g",
              color = 'gray30',
              margin.params = list(fill = "lightgray"),
              xlab="Shell height (mm)",
              ylab="Tissue dry weight (g)",
              ylim(0,7))

ggscatterhist(Main[1:s1-1,], x = "Total_Shell_Height_Length_mm", y = "Tissue_Dry_Weight_g",
              color = 'gray30',
              margin.params = list(fill = "lightgray"),
              xlab="Shell height (mm)",
              ylab="Tissue dry weight (g)")

### save SH:DW by state for TISSUE
SHDWt=data.frame(matrix(NA, nrow=3, ncol=7))
colnames(SHDWt)=c("quantile", "a", "b", "SEa", "SEb","Pa","Pb")
SHDWs=data.frame(matrix(NA, nrow=3, ncol=7))
colnames(SHDWs)=c("quantile", "a", "b", "SEa", "SEb","Pa","Pb")
for(i in c(0.25,0.5, 0.75)){
  if(i==0.25){
    j=1
  }
  else if(i==0.5){
    j=2
  }
  else{
    j=3
  }
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.00037, b = 1.83359), tau=i)
  SHDWt[j,1]=i
  SHDWt[j,2]=summary(qrx)$coefficients[1,1]
  SHDWt[j,3]=summary(qrx)$coefficients[2,1]
  SHDWt[j,4]=summary(qrx)$coefficients[1,2]
  SHDWt[j,5]=summary(qrx)$coefficients[2,2]
  SHDWt[j,6]=summary(qrx)$coefficients[1,4]
  SHDWt[j,7]=summary(qrx)$coefficients[2,4]
  qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.0007, b = 2), tau=i)
  SHDWs[j,1]=i
  SHDWs[j,2]=summary(qrx)$coefficients[1,1]
  SHDWs[j,3]=summary(qrx)$coefficients[2,1]
  SHDWs[j,4]=summary(qrx)$coefficients[1,2]
  SHDWs[j,5]=summary(qrx)$coefficients[2,2]
  SHDWs[j,6]=summary(qrx)$coefficients[1,4]
  SHDWs[j,7]=summary(qrx)$coefficients[2,4]
}
dt=lubridate::now()
dat=format(dt, "%Y%m%d")
write.csv(SHDWt, file=paste(wd,dat,"Tissue_SH_DW_qr50_interquartile_data.csv"))
write.csv(SHDWs, file=paste(wd,dat,"Shell_SH_DW_qr50_interquartile_data.csv"))

