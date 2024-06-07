### RM 20240423 code to plot figures for Calculator manuscript submitted to PlosOne ###
library(tidyverse)
library(ggExtra)
library(ggpubr)
library(lubridate)
library(quantreg)
library(mapdata)
library(marmap)
library(patchwork)

# Arial font 12 point
# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device = "win")

#_________________________________________________________________
### Figure 1 ### Map of region and samples
data(stateMapEnv)
wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
stations=readxl::read_xlsx(paste(wd, "Location_data.xlsx", sep=''),sheet='final2')
par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
map("worldHires", xlim=c(-79,-68),ylim=c(33,45), fill=T,border=0,col="gray70", xlab="Lon")
map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')
map.axes(las=1)
mtext(c("Longitude", "Latitude"), side=c(1,2), line = 2.5)
map('state', fill = F, add=T) # add state lines
# plot stations with N in different color
stations2=stations %>% filter(Waterbody_Name %in% RegionFarm$Waterbody_Name[complete.cases(RegionFarm$Tissue_N_Percent)])
stations3=stations %>% filter(!(Waterbody_Name %in% RegionFarm$Waterbody_Name[complete.cases(RegionFarm$Tissue_N_Percent)]))
## plot
tiff("Fig1_Rose_et_al_12_17.tiff", height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = 300)
par(mar = c(2,2,0,0)) #(b,l,t,r)
par(oma = c(2,2,0,0))
par(family = "Arial")
map("worldHires", xlim=c(-79,-68),ylim=c(33,45), fill=T,border=0,col="gray90")
map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')
map.axes(las=1)
map('state', fill = F, add=T) # add state lines
mtext(c("Longitude", "Latitude"), side=c(1,2), line = 2.5)
points(stations3$Longitude, stations3$Latitude, pch=23, col='black', bg='orange', cex=1.15)
points(stations2$Longitude, stations2$Latitude, pch=21, col='black', bg='green', cex=1.15)
legend(-76,36, pch=c(23,21), col='black', pt.bg=c('orange', 'green'),legend=c('Morphometric data', 'Morphometric + N'), bty='n', )
dev.off()
#_________________________________________________________________
### Figure 2 ### boxplot of N data for tissue
# A - Tissue N

p1=RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=st_abrv)) + 
  geom_boxplot(color = "black", notch=F, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(0, 15))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 12, family="Arial")) +
  theme(legend.text = element_text(size = 12, family="Arial")) 
# B - Tissue N (gear class)
p2=RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=Gear_Class)) + 
  geom_boxplot(color = "black", notch=F, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(0, 15))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  theme(text = element_text(size = 12, family="Arial")) +
  theme(
    # legend.position = c(0.5,0.15),
    # legend.text = element_text(size = 20),
    legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))
# C - Tissue N (ploidy)
p3=RegionFarm[complete.cases(RegionFarm$Tissue_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=Ploidy)) + 
  geom_boxplot(color = "black", notch=F, fill="gray") +
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
  theme(text = element_text(size = 12, family="Arial"))  
tiff("Fig2_Rose_et_al_15_10.tiff", height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
p1 / p2 / p3 + plot_annotation(tag_levels = 'A')
dev.off()
#_________________________________________________________________
### Figure 3 ### boxplot of N data for Shell
# A - Shell N 
p1=RegionFarm[complete.cases(RegionFarm$Shell_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Shell_N_Percent) %>% 
  ggplot(aes(y=Shell_N_Percent, x=st_abrv)) + 
  geom_boxplot(color = "black", notch=F, fill="gray") +
  theme_classic()+
  labs(x='', y = 'Shell N Percent') +
  coord_cartesian(ylim = c(0, 1))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 12, family="Arial")) +
  theme(legend.text = element_text(size = 12, family="Arial"))
# B - Shell N (gear class)
p2=RegionFarm[complete.cases(RegionFarm$Shell_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Shell_N_Percent) %>% 
  ggplot(aes(y=Shell_N_Percent, x=Gear_Class)) + 
  geom_boxplot(color = "black", notch=F, fill="gray") +
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
  theme(text = element_text(size = 12, family="Arial"))  
# C - Shell N (ploidy)
p3=RegionFarm[complete.cases(RegionFarm$Shell_N_Percent),] %>% select(st_abrv, Ploidy, Gear_Class, Shell_N_Percent) %>% 
  ggplot(aes(y=Shell_N_Percent, x=Ploidy)) + 
  geom_boxplot(color = "black", notch=F, fill="gray") +
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
  theme(text = element_text(size = 12, family="Arial"))  
tiff("Fig3_Rose_et_al_15_10.tiff", height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
p1/p2/p3 + plot_annotation(tag_levels = 'A')
dev.off()
#_________________________________________________________________
### Figure 4 ### plot CB data with RegionFarm overlay
# A - Tissue
tiff("Fig4_Rose_et_al_15_10.tiff", height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
par(oma=c(2,2,0,0)) 
par(mar=c(4,4,2,2) + 0.1)
par(mfrow = c(2, 1))
par(family = "Arial")
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.2))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='black', lwd=2, lty=1)
a2=round(a,6)
b2=round(b,3)
text(0,7, labels=bquote(Y==.(a2)*X^.(b2)), cex=1, pos = 4)
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,8, labels="A", cex=1)
# B - Shell
plot(RegionFarm$Shell_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab="Shell height (mm)", las=1)
points(RegionFarm$Shell_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.2))
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.0007, b = 2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='black', lwd=2, lty=1)
a2=round(a,6)
b2=round(b,3)
text(0,140, labels=bquote(Y==.(a2)*X^.(b2)), cex=1, pos = 4)
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,150, labels="B", cex=1)
dev.off()
#_______________________________________________________
### Figure 5 ### 
# A - Ploidy Tissue
tiff("Fig5_Rose_et_al_15_10.tiff", height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
par(oma=c(2,2,0,0)) 
par(mar=c(4,4,2,2) + 0.1)
par(mfrow = c(2, 1))
par(family = "Arial")
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
dataa2=RegionFarm %>% filter(Ploidy=="Diploid")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
dataa2=RegionFarm %>% filter(Ploidy=="Triploid")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 0, alpha = 0.9), lwd=2)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
legend('topleft', pch=19, col=c('blue', 'red'), legend=c("Diploid", "Triploid"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,8, labels="A", cex=1)
# B-Ploidy Shells
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
dataa2=RegionFarm %>% filter(Ploidy=="Triploid")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 0, alpha = 0.9), lwd=2)
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
text(140,150, labels="B", cex=1)
dev.off()
#____________________________________________________________________
### Figure 6 ### Gear Class w/ polygons
# A - Tissue
tiff("Fig6_Rose_et_al_15_10.tiff", height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
par(oma=c(0,0,0,0)) 
par(mar=c(4,4,2,2) + 0.1)
par(mfrow = c(2, 1))
par(family = "Arial")
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     pch=19, ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)
dataa2=RegionFarm %>% filter(Gear_Class=="Floating")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="Floating")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.9), lwd=2)
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.9), lwd=2)
legend('topleft', pch=19, col=c('red','green','blue'), legend=c("Floating", "No Gear", "Bottom"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,8, labels="A", cex=1)
# B - Gear Class w/ polygons For Shells
plot(Main$Shell_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='n', 
     pch=19, ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)
dataa2=RegionFarm %>% filter(Gear_Class=="Floating")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))
dataa2=RegionFarm %>% filter(Gear_Class=="Floating")
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.9), lwd=2)
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b =2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.9), lwd=2)
legend('topleft', pch=19, col=c('red','green','blue'), legend=c("Floating", "No Gear", "Bottom"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,150, labels="B", cex=1)
dev.off()
#_________________________________________________________________
### Figure 7  compare mid-Atlantic vs New England
newEng=RegionFarm %>% filter(st_abrv %in% c("ME", "NH", "MA", "RI", "CT"))
midAtl=RegionFarm %>% filter(!(st_abrv %in% c("ME", "NH", "MA", "RI", "CT")))
###
# A - Tissue
tiff("Fig7_Rose_et_al_15_10.tiff", height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
par(oma=c(2,2,0,0)) 
par(mar=c(4,4,2,2) + 0.1)
par(mfrow = c(2, 1))
par(family = "Arial")
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
points(newEng$Tissue_Dry_Weight_g ~newEng$Total_Shell_Height_Length_mm, pch=19, col=rgb(77/255, 77/255, 77/255, 46/255), ylim=c(0,8), xlim=c(0,200))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = newEng, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
points(midAtl$Tissue_Dry_Weight_g ~midAtl$Total_Shell_Height_Length_mm, pch=19, col=rgb(0, 175/255, 187/255, 36/255), ylim=c(0,8), xlim=c(0,200))
lines(x, yval, col='black', lwd=2)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = midAtl, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='blue', lwd=2)
legend('topleft', pch=c(19,19), lty=c(1,1), lwd=2, col=c(rgb(77/255, 77/255, 77/255, 46/255),rgb(0, 175/255, 187/255, 36/255)),  legend=c("New England","Mid-Atlantic"), bty='n')
legend('topleft', pch=c(NA,NA), lty=c(1,1), lwd=2, col=c('black', 'blue'), legend=c("New England","Mid-Atlantic"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,8, labels="A", cex=1)
# B - Shell
plot(Main$Shell_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='n', 
     pch=19, ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)
points(newEng$Shell_Dry_Weight_g ~newEng$Total_Shell_Height_Length_mm, pch=19, col=rgb(77/255, 77/255, 77/255, 46/255))
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = newEng, start = list(a = 0.0007, b = 2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
points(midAtl$Shell_Dry_Weight_g ~midAtl$Total_Shell_Height_Length_mm, pch=19, col=rgb(0, 175/255, 187/255, 36/255), ylim=c(0,8), xlim=c(0,200))
lines(x, yval, col='black', lwd=2)
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = midAtl, start = list(a = 0.0007, b = 2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='blue', lwd=2)
legend('topleft', pch=c(19,19), lty=c(1,1), lwd=2, col=c(rgb(77/255, 77/255, 77/255, 46/255),rgb(0, 175/255, 187/255, 36/255)),  legend=c("New England","Mid-Atlantic"), bty='n')
legend('topleft', pch=c(NA,NA), lty=c(1,1), lwd=2, col=c('black', 'blue'), legend=c("New England","Mid-Atlantic"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,150, labels="B", cex=1)
dev.off()
# azul="#00AFBB"
# col2rgb(azul)
# rgb(0, 175/255, 187/255, 26/255)
# col2rgb('gray30')
# rgb(77/255, 77/255, 77/255, 26/255)
#____________________________________________________________
### Figure 8 ### plot CB data with RegionFarm overlay no regression line
# A - Tissue
tiff("Fig8_Rose_et_al_15_10.tiff", height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
par(family = "Arial")
par(oma=c(2,2,0,0)) 
par(mar=c(4,4,2,2) + 0.1)
par(mfrow = c(2, 1))
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
points(Main$Tissue_Dry_Weight_g[1:s1-1] ~Main$Total_Shell_Height_Length_mm[1:s1-1], pch=19, col='gray70')
points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=17, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.2))
legend('topleft', pch=c(19, 17), col=c('gray70',rgb(red = 1, green = 0, blue = 0, alpha = 0.2)), legend=c("CBP 2023", "This study"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,8, labels="A", cex=1)
# B - Shell
plot(RegionFarm$Shell_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab="Shell height (mm)", las=1)
points(Main$Shell_Dry_Weight_g[1:s1-1] ~Main$Total_Shell_Height_Length_mm[1:s1-1], pch=19, col='gray70')# rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
points(RegionFarm$Shell_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=17, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.2))
legend('topleft', pch=c(19, 17), col=c('gray70',rgb(red = 1, green = 0, blue = 0, alpha = 0.2)), legend=c("CBP 2023", "This study"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,150, labels="B", cex=1)
dev.off()
#_______________________________________________________
### Supplementary S1 ### interquartile range on qr50 to show overlap
# A -  Gear Class w/ polygons for Tissue###
tiff("FigS1_Rose_et_al_15_10.tiff", height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
par(oma=c(2,2,0,0)) 
par(mar=c(4,4,2,2) + 0.1)
par(mfrow = c(2, 1))
par(family = "Arial")
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     pch=19, ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)
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
### Bottom
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
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
### No Gear
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
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
legend('topleft', pch=19, col=c('red','green','blue'), legend=c("Floating", "No Gear", "Bottom"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,8, labels="A", cex=1)

# B - Gear Class w/ polygons For Shells###
plot(Main$Shell_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='n', 
     pch=19, ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)
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
### Bottom
dataa2=RegionFarm %>% filter(Gear_Class=="Bottom")
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
### No Gear
dataa2=RegionFarm %>% filter(Gear_Class=="No Gear")
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
legend('topleft', pch=19, col=c('red','green','blue'), legend=c("Floating", "No Gear", "Bottom"), bty='n')
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,150, labels="B", cex=1)
dev.off()