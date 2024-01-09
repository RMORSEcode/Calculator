s1=match("Poach et al. in prep 2023", Main$Data_Source) # start of Poach data (after end of CB)
MainNoCB=Main[s1:dim(Main)[1],]
RegionFarm=MainNoCB %>% filter(!(Waterbody_Region %in% c("Jamaica Bay", "Hudson River", "Raritan Bay")))

### overlay with states
### regressions for gear
### regressions for states
### compare N reduction with and w/o NY data

mcol=c('cyan','black', 'red', 'blue', 'orange', 'brown', 'yellow', 'purple', 'green', 'gray50', 'maroon')

stt=sort(unique(RegionFarm$st_abrv))
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,10), xlim=c(0,200), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
for(i in 1:length(stt)){
  dataa2=RegionFarm[RegionFarm$st_abrv==stt[i],]
  points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=mcol[i], ylim=c(0,8), xlim=c(0,200))
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  a=summary(qrx)$coefficients[1,1]
  b=summary(qrx)$coefficients[2,1]
  x=seq(0, 150, length = 250)
  yval=(a*(x^b))
  lines(x, yval, col=ifelse(summary(qrx)$coefficients[1,4]<0.05, mcol[i], mcol[i]), lwd=ifelse(summary(qrx)$coefficients[1,4]<0.05, 2, 1))
  # lines(x, yval, col=mcol[i], lwd=2)
  # text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste(stt[i],'*', sep=' '),''), col=mcol[i])
  text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
  
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
  # text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste(stt[i],'*', sep=' '),''), col=mcol[i])
  text(180,yval[250], labels=ifelse(summary(qrx)$coefficients[1,4]<0.05, paste('Y=',round(a,6),'*X^',round(b,3),sep=''),''), cex=.75)
  
}
legend('topleft', bty='n', 
       legend = stt, 
       text.col=mcol)

### Gear Class w/ polygons for Tissue###
### add polygons of upper and lower fit (change tau for upper and lower)
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, ylim=c(0,10), xlim=c(0,200), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)

# points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.72, blue = 0.72, alpha = 0.1))
# points(Main[1:s1-1,]$Tissue_Dry_Weight_g ~ Main[1:s1-1,]$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))

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

### Gear Class w/ polygons For Shells###
plot(Main$Shell_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, ylim=c(0,200), xlim=c(0,200), ylab="Shell dry weight (g)", xlab="Shell height (mm)", col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1), las=1)

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

# points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 1, blue = 0, alpha = 0.1))

#add all points light gray
points(RegionFarm$Tissue_Dry_Weight_g ~RegionFarm$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
points(Main$Tissue_Dry_Weight_g ~Main$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))
points(Main$Tissue_Dry_Weight_g ~Main$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0.52, green = 0.52, blue = 0.52, alpha = 0.1))




### Ploidy Tissue
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,10), xlim=c(0,200), ylab="Tissue dry weight (g)", xlab="Shell height (mm)", las=1)
dataa2=RegionFarm %>% filter(Ploidy=="Diploid")
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 0, alpha = 0.9), lwd=2)
text(180,yval[250], labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=.75)
dataa2=RegionFarm %>% filter(Ploidy=="Triploid")
points(dataa2$Tissue_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
legend('topleft', pch=19, col=c('blue', 'red'), legend=c("2N", "3N"), bty='n')
text(180,yval[250], labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=.75)

table(RegionFarm$st_abrv[RegionFarm$Ploidy=="Triploid"])
table(RegionFarm$st_abrv[RegionFarm$Ploidy=="Diploid"])

### Ploidy Shells
plot(RegionFarm$Tissue_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,200), xlim=c(0,200), ylab="Shell dry weight (g)", xlab="Shell height (mm)", las=1)
dataa2=RegionFarm %>% filter(Ploidy=="Diploid")
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
summary(qrx)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.1))
lines(x, yval, col=rgb(red = 0, green = 0, blue = 0, alpha = 0.9), lwd=2)
text(180,yval[250], labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=.75)
dataa2=RegionFarm %>% filter(Ploidy=="Triploid")
points(dataa2$Shell_Dry_Weight_g ~dataa2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.1))
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa2, start = list(a = 0.0007, b = 2), tau=0.5)
summary(qrx)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col=rgb(red = 1, green = 0, blue = 0, alpha = 0.9), lwd=2)
legend('topleft', pch=19, col=c('blue', 'red'), legend=c("2N", "3N"), bty='n')
text(180,yval[250], labels=paste('Y=',round(a,6),'*X^',round(b,3),sep=''), cex=.75)

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
