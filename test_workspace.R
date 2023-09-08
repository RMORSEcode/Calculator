### space for testing ideas and making plots before moving code into more structured forms ###
### see Load_date.R, build_main_file.R SH_DW.R 
library(rstatix)
library(tidyverse)
library(ggpubr)
library(quantreg)
library(colorspace)


print(sapply(CB, class))

xx=(sapply(CB, is.numeric))
for(i in 1:ncol(CB)){
  if(colnames(CB)[i] %in% colnames(CB)[xx]){
    print(paste(i, 'of', ncol(CB), 'is numeric', sep=' '))
    next
  }
  print(paste(i, 'of', ncol(CB), sep=' '))
  print(sapply(CB[,i], class))
  print(unique(CB[,i]))
}
# newdf=CB %>% select(-c(Original_Order, Number_ID, Analysis_ID, Yates_Bar_Name, Location_Index_Raw_Data, 
                       # Near_Waterbody__General_Location, Waterbody_Type,.,Ross_Project:Quantile_Grouping))
# to match PCB
newdf=CB %>% select(-c(Original_Order, Analysis_ID, Yates_Bar_Name, .,Ross_Project:Quantile_Grouping))
colnames(newdf)[5]="Location_Index"
newdf$Number_ID=as.numeric(newdf$Number_ID)

for(i in 1:ncol(newdf)){
  if(colnames(newdf)[i] %in% colnames(newdf)[xx]){
    print(paste(i, 'of', ncol(newdf), 'is numeric', sep=' '))
    next
  }
  print(paste(i, 'of', ncol(newdf), sep=' '))
  print(sapply(newdf[,i], class))
  print(unique(newdf[,i]))
}


# data verification
test=identify_outliers(CB.2023, variabe=Tissue_N_Percent)
# t2=PCB$Number_ID %in% test$Number_ID 
# PCB2=PCB[-c(which(t2==T)),]

### remove outliers -> NA; negative vals -> NA
newdf=CB.2023
for(i in 25:46){
  varn=colnames(CB.2023)[i]
  newdf[,varn]=as.numeric(newdf[,varn])
  newdf[which(newdf[,varn]<0),varn]=NA
  test=identify_outliers(CB.2023, variabe=varn)
  for(j in 1:dim(test)[1]){
    # reprow=test$Original_Order[j]
    newdf[which(newdf$Original_Order==test$Original_Order[j]),varn]=NA
  }
  print(varn)
  print('new = ')
  print(range(newdf[,i],na.rm = T))
  print('original =')
  print(range(CB.2023[,i],na.rm=T))
}


range(newdf[,25],na.rm = T)
range(CB.2023[,25],na.rm=T)




dim(data)
## [1] 150   4
quartiles <- quantile(data$Sepal.Width, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$Sepal.Width)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data_no_outlier <- subset(data, data$Sepal.Width > Lower & data$Sepal.Width < Upper)
dim(data_no_outlier)


quartiles=quantile(CB.2023[,varn], probs=c(.25,0.75),na.rm=T)
IQR <- IQR(CB.2023[,varn])
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data_no_outlier <- subset(CB.2023, CB.2023[,varn] > Lower & CB.2023[,varn] < Upper)

# checking for differences in wild vs cultured
unique(CB.2023$`Hatchery-produced_or_Wild`) # "Hatchery-produced", "Wild" , "Mixed" ,"?"

wCB=CB.2023 %>% filter(`Hatchery-produced_or_Wild`=='Wild', Oyster_Growth_Location_Type=="Reef")
hCB=CB.2023 %>% filter(`Hatchery-produced_or_Wild`=="Hatchery-produced", Oyster_Growth_Location_Type=="Reef")
# hCB <- hCB[complete.cases(hCB$Total_Shell_Height_Length_mm),]
# hCB <- hCB[complete.cases(hCB$Tissue_Dry_Weight_g),]

plot(wCB$Tissue_Dry_Weight_g ~ wCB$Total_Shell_Height_Length_mm, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='Wild CB Cornwell')
plot(hCB$Tissue_Dry_Weight_g ~ hCB$Total_Shell_Height_Length_mm, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='Hatchery CB Cornwell')
points(wCB$Tissue_Dry_Weight_g ~ wCB$Total_Shell_Height_Length_mm, col='red', pch=19)

wCB=CB.2023 %>% filter(`Hatchery-produced_or_Wild`=='Wild')
hCB=CB.2023 %>% filter(`Hatchery-produced_or_Wild`=="Hatchery-produced", Oyster_Growth_Location_Type=="Reef")

plot(wCB$Tissue_Dry_Weight_g ~ wCB$Total_Shell_Height_Length_mm, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='Wild CB Cornwell')
qrw=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=wCB, tau=0.5, na.action = 'na.omit')
xval=seq(20,140,by=.5)
yval=exp(qrw$coefficients[1])*xval^qrw$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
plot(hCB$Tissue_Dry_Weight_g ~ hCB$Total_Shell_Height_Length_mm, type='p', xlab='SH (mm)', col='gray', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='Hatchery CB Cornwell')
qrh=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=hCB, tau=0.5, na.action = 'na.omit')
yvalh=exp(qrh$coefficients[1])*xval^qrh$coefficients[2]
lines(xval, yvalh, col='blue', lwd=2, lty=1)

plot(wCB$Tissue_Dry_Weight_g ~ wCB$Total_Shell_Height_Length_mm, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='CB Cornwell')
points(hCB$Tissue_Dry_Weight_g ~ hCB$Total_Shell_Height_Length_mm, col='gray', pch=21)
lines(xval, yvalh, col='blue', lwd=2, lty=1)
lines(xval, yval, col='red', lwd=2, lty=1)
legend('topleft', bty='n', horiz = T, pch=c(19,19), col=c('black', 'gray'), legend = c('Wild', 'Hatchery'), text.col = c('red', 'blue'))

### Reitsma MA
unique(reitsma$`Culture/wild`) #[1] "wild"     "cultured"
wR=reitsma %>% filter(`Culture/wild`=='wild')
hR=reitsma %>% filter(`Culture/wild`=='cultured')

plot(wR$`Dry Tiss Mass (g)` ~ wR$`Shell Length (mm)`, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='Wild Reitsma')
qrw=rq(log(`Dry Tiss Mass (g)`) ~ log(`Shell Length (mm)`), data=wR, tau=0.5, na.action = 'na.omit')
xval=seq(20,140,by=.5)
yval=exp(qrw$coefficients[1])*xval^qrw$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)

plot(hR$`Dry Tiss Mass (g)` ~ hR$`Shell Length (mm)`, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='Hatchery Reitsma')
qrh=rq(log(`Dry Tiss Mass (g)`) ~ log(`Shell Length (mm)`), data=hR, tau=0.5, na.action = 'na.omit')
yvalh=exp(qrh$coefficients[1])*xval^qrh$coefficients[2]
lines(xval, yvalh, col='blue', lwd=2, lty=1)

plot(hR$`Dry Tiss Mass (g)` ~ hR$`Shell Length (mm)`, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(0,140), main='MA Reitsma')
points(wR$`Dry Tiss Mass (g)` ~ wR$`Shell Length (mm)`, pch=21, col='red')
qrb=rq(log(`Dry Tiss Mass (g)`) ~ log(`Shell Length (mm)`), data=reitsma, tau=0.5, na.action = 'na.omit')
yvalb=exp(qrb$coefficients[1])*xval^qrb$coefficients[2]
lines(xval, yvalb, col='black', lwd=2, lty=1)
lines(xval, yvalh, col='blue', lwd=2, lty=1)
legend('topleft', bty='n', horiz = F, pch=c(19,19), col=c('black', 'red'), legend = c('Hatchery','Wild (q50 combined)'), text.col = c('blue','black'))


#original and edited
# poach=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited_Nov2020.xlsx', sep=''), sheet='Compiled Data')
# poach2=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='compiled')

pairwise.t.test(pwc$`Tissue %N`, pwc$mon, p.adjust.method = "bonf")


# remove rows with outliers from
test=identify_outliers(PCB2, variabe=Tissue_N_Percent)
t2=PCB$Number_ID %in% test$Number_ID 
PCB2=PCB[-c(which(t2==T)),]
test=identify_outliers(PCB2, variabe=Total_Shell_Height_Length_mm)
t2=PCB2$Number_ID %in% test$Number_ID 
PCB2=PCB2[-c(which(t2==T)),]

# Now check for normality by state
PCB2[PCB2$State=="Virginia",] %>% shapiro_test(`Tissue_N_Percent`, `Total_Shell_Height_Length_mm`)
PCB2[PCB2$State=="Maryland",] %>% shapiro_test(`Tissue_N_Percent`, `Total_Shell_Height_Length_mm`)

ggqqplot(PCB2$Tissue_N_Percent)
ggqqplot(log(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Virginia"]))
ggqqplot(log(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Maryland"]))

barplot(table(round(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Virginia"],-1)))
barplot(table(round(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Maryland"],-1)))

# pairwise comparisons
pwc <- selfesteem %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
bxp + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


### test for outliers
x=PCB %>% select(Tissue_N_Percent, Month_Oysters_Removed, State, Ploidy) %>%
  group_by(State, Month_Oysters_Removed) %>%
  identify_outliers(Tissue_N_Percent)

### test for normal distribution
x=PCB %>% select(Tissue_N_Percent, Month_Oysters_Removed, State, Ploidy, Total_Shell_Height_Length_mm) %>%
  group_by(State, Month_Oysters_Removed, Ploidy) %>%
  shapiro_test(Tissue_N_Percent, Total_Shell_Height_Length_mm)

ggqqplot(VA, "Tissue_N_Percent", facet.by = "Month_Oysters_Removed")
ggqqplot(MD, "Tissue_N_Percent", facet.by = "Month_Oysters_Removed")


# build artificial data with multiplicative error
Dat <- NULL; Dat$x <- rep(1:25, 20)
set.seed(1)
Dat$y <- SSlogis(Dat$x, 10, 12, 2)*rnorm(500, 1, 0.1)
plot(Dat)
# fit first a nonlinear least-square regression
Dat.nls <- nls(y ~ SSlogis(x, Asym, mid, scal), data=Dat); Dat.nls
lines(1:25, predict(Dat.nls, newdata=list(x=1:25)), col=1)
# then fit the median using nlrq
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.5, trace=TRUE)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=2)
# the 1st and 3rd quartiles regressions
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.25, trace=TRUE)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=3)
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.75, trace=TRUE)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=3)
# and finally "external envelopes" holding 95 percent of the data
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.025, trace=TRUE)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=4)
Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.975, trace=TRUE)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=4)
leg <- c("least squares","median (0.5)","quartiles (0.25/0.75)",".95 band (0.025/0.975)")
legend(1, 12.5, legend=leg, lty=1, col=1:4)

x=c(1,2,3,4,5,6,7,8,9,0)
y=c(13,28,43,35,96,84,101,110,108,13)
lm.out <- lm(y ~ x)
newx = seq(min(x),max(x),by = 0.05)
conf_interval <- predict(lm.out, newdata=data.frame(x=newx), interval="confidence",
                         level = 0.95)
plot(x, y, xlab="x", ylab="y", main="Regression")
abline(lm.out, col="lightblue")
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)



### 2 line plots by state with error bars for %N
t2=test %>% filter(Ploidy=="Diploid", State=='Virginia')
t3=test %>% filter(Ploidy=="Triploid", State=='Virginia')
plot(t2$mnN~t2$Month_Oysters_Removed, type='b', pch=19, ylim=c(5,10), 
     las=1, ylab='Percent N', xlab='Month')
lines(t3$mnN~t3$Month_Oysters_Removed, col='red')
points(t3$mnN~t3$Month_Oysters_Removed, col='red', pch=19)
segments(t2$Month_Oysters_Removed, t2$mnN-t2$sdN, t2$Month_Oysters_Removed,t2$mnN+t2$sdN, col='black')
segments(t3$Month_Oysters_Removed, t3$mnN-t3$sdN, t3$Month_Oysters_Removed,t3$mnN+t3$sdN, col='red')
legend('topleft', lty = c(1,1), pch=c(19, 19), col = c('black', 'red'), 
       legend=c('VA diploid', 'VA triploid'), horiz = F, bty='n')

t2=test %>% filter(Ploidy=="Diploid", State=='Maryland')
t3=test %>% filter(Ploidy=="Triploid", State=='Maryland')
plot(t2$mnN~t2$Month_Oysters_Removed, type='b', lty=3, pch=17, ylim=c(5,10), 
     las=1, ylab='Percent N', xlab='Month')
lines(t3$mnN~t3$Month_Oysters_Removed, col='red', lty=3)
points(t3$mnN~t3$Month_Oysters_Removed, col='red', pch=17)
segments(t2$Month_Oysters_Removed, t2$mnN-t2$sdN, t2$Month_Oysters_Removed,t2$mnN+t2$sdN, col='black')
segments(t3$Month_Oysters_Removed, t3$mnN-t3$sdN, t3$Month_Oysters_Removed,t3$mnN+t3$sdN, col='red')
legend('topleft', lty = c(3,3), pch=c(17, 17), col = c('black', 'red'), 
       legend=c('MD diploid', 'MD triploid'), horiz = F, bty='n')


### 2 line plots median by state with quantile error bars for Tissue %P


plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid" & PCB$State=='Virginia'] ~ 
       PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"& PCB$State=='Virginia'], type='p', 
     col='red', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1)
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid" & PCB$State=='Maryland'] ~ 
         PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid" & PCB$State=='Maryland'], pch=19, col='gray')
clmd=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"]))
yval=exp(clmd$coefficients[1])*xval^clmd$coefficients[2]
lines(xval, yval, col='black', lwd=2)
legend('topleft', bty = 'n', horiz = F, legend=c("VA Diploid", "MD Diploid"), text.col = c('red','gray'))

plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid" & PCB$State=='Virginia'] ~ 
       PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"& PCB$State=='Virginia'], type='p', 
     col='red', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1)
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid" & PCB$State=='Maryland'] ~ 
         PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid" & PCB$State=='Maryland'], pch=19, col='gray')
clmt=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"]))
xval=seq(20,140,by=.5)
yval=exp(clmt$coefficients[1])*xval^clmt$coefficients[2]
lines(xval, yval, col='black', lwd=2)
legend('topleft', bty = 'n', horiz = F, legend=c("VA Triploid", "MD Triploid"), text.col = c('red','gray'))


## add CBP data fits
# yval=exp(cbdlm$coefficients[1])*xval^cbdlm$coefficients[2]
yval=(0.00037)*xval^1.83359 # CBP all data BMP Second Report Appendix
# yval=(0.0004)*xval^1.82 # CBP bay data (first report diploid)
yval2=(0.00005)*xval^2.39 # CBP bay data (first report triploid)
lines(xval, yval, col='black', lwd=2, lty=2)
# yval2=exp(cbtlm$coefficients[1])*xval^cbtlm$coefficients[2]
lines(xval, yval2, col='red', lwd=2, lty=2)


PCB %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggboxplot(y='Tissue_N_Percent', x='State', fill='Ploidy',ylab = 'Tissue P Percent', xlab='', ylim=c(0,15)) + 
stat_compare_means(method = "anova", label.y = 0.2)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")




test=PCB %>% select(State, Month_Oysters_Removed, Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent) %>% 
  group_by(Month_Oysters_Removed, State, Ploidy) %>%
  summarise(mnN=mean(Tissue_N_Percent, na.rm=T), mnP=mean(Tissue_TP_Percent, na.rm=T),
            mnC=mean(Tissue_C_Percent, na.rm=T), sdC=sd(Tissue_C_Percent, na.rm=T),
            sdN=sd(Tissue_N_Percent, na.rm=T),sdP=sd(Tissue_TP_Percent, na.rm=T)) #%>%
## add stats to plots
my_comparisons=list( c("Virginia", "Maryland"))

PCB %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggboxplot(y='Tissue_N_Percent', x='State', fill='Ploidy', ylab = 'Tissue N Percent', xlab='', ylim=c(2.5,14)) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "anova", label.y = c(12.5),label= "p.signif") #+
# stat_compare_means(label.y = 2.5)

PCB %>% select(State, Ploidy, Tissue_TP_Percent) %>% 
  ggboxplot(y='Tissue_TP_Percent', x='State', fill='Ploidy',ylab = 'Tissue P Percent', xlab='', ylim=c(0.25,2)) +
  stat_compare_means(label = "p.signif", method = "anova", ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, label.y = c(1.75), label= "p.signif")



plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"], type='p', 
           col='red', ylim=c(0,7), xlim=c(0,140), ylab='Dry weight (g)', xlab='Shell height (mm)', pch=4, cex=1.5,las=1,
         cex.axis=1.5,cex.lab=1.5)
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], cex=1.5,type='p', col='gray', pch=3)
clmt=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"]))
xval=seq(20,140,by=.5)
yval=exp(clmt$coefficients[1])*xval^clmt$coefficients[2]
lines(xval, yval, col='black', lwd=2)
clmd=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"]))
yval=exp(clmd$coefficients[1])*xval^clmd$coefficients[2]
lines(xval, yval, col='purple', lwd=2)
legend('topleft', lty=c(1,1,2,2), pch=c(NA,NA,NA,NA),lwd=c(2,2,2,2),col=c('purple', 'black','purple','black'), 
       bty = 'n', horiz = F, legend=c("Diploid", "Triploid","CBP Diploid", "CBP Triploid"), 
       text.col = c('red', 'gray', 'black', 'black'), cex=c(1.5, 1.5, 1.5, 1.5))
# legend('topleft', pch=c(4,3,NA,NA),col=c('red', 'gray',NA, NA), 
       # bty = 'n', horiz = F, legend=c(NA,NA,NA,NA),cex=c(1.5, 1.5, 1.5, 1.5)) 
yval=(0.00037)*xval^1.83359 # CBP all data BMP Second Report Appendix
yval2=(0.00005)*xval^2.39 # CBP bay data (first report triploid)
lines(xval, yval, col='purple', lwd=2, lty=2)
lines(xval, yval2, col='black', lwd=2, lty=2)



plot(CB$Shell_Dry_Weight_g[CB$Ploidy=="Diploid"] ~ CB$Total_Shell_Height_Length_mm[CB$Ploidy=="Diploid"], type='p', 
     col='gray', ylim=c(0,300), xlim=c(0,140), ylab='Shell dry weight (g)', xlab='Shell height (mm)', pch=19, las=1,
     cex.axis=1.5,cex.lab=1.5)
points(CB$Shell_Dry_Weight_g[CB$Ploidy=="Triploid"] ~ CB$Total_Shell_Height_Length_mm[CB$Ploidy=="Triploid"], type='p', col='purple')
t=CB %>% filter(Ploidy=="Diploid")
qr2=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
xval=seq(20,140,by=.5)
yval=exp(qr2$coefficients[1])*xval^qr2$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=1)
t=PCB %>% filter(Ploidy=="Triploid")
qr3=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
yval=exp(qr3$coefficients[1])*xval^qr3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
qr1=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=PCB, tau=0.5, na.action = 'na.omit')
yval=exp(qr1$coefficients[1])*xval^qr1$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=2)
legend('topleft', lty=c(1,1,2), lwd=c(2,2,2),col=c('black', 'red','black'), 
       bty = 'n', horiz = F, legend=c("Diploid", "Triploid","Combined"), 
       text.col = c('gray','purple','black'), cex=c(1.5, 1.5, 1.5))


## CB updated 2023 data (new source loaded in SH_DW.R)
plot(CB.tissue$Tissue_Dry_Weight_g[CB.tissue$Ploidy=="Triploid"]~
       CB.tissue$Total_Shell_Height_Length_mm[CB.tissue$Ploidy=="Triploid"], 
     type='p', ylim=c(0,8), xlim=c(0,180))
qrx3=rq(log(Tissue_Dry_Weight_g[CB.tissue$Ploidy=="Triploid"]) ~ log(Total_Shell_Height_Length_mm[CB.tissue$Ploidy=="Triploid"]), data=CB.tissue, tau=0.5, na.action = 'na.omit')
yval=exp(qrx3$coefficients[1])*xval^qrx3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)

plot(CB.tissue$Tissue_Dry_Weight_g[CB.tissue$Ploidy=="Diploid"]~
       CB.tissue$Total_Shell_Height_Length_mm[CB.tissue$Ploidy=="Diploid"], 
     type='p', ylim=c(0,8), xlim=c(0,180))
qrx2=rq(log(Tissue_Dry_Weight_g[CB.tissue$Ploidy=="Diploid"]) ~ log(Total_Shell_Height_Length_mm[CB.tissue$Ploidy=="Diploid"]), data=CB.tissue, tau=0.5, na.action = 'na.omit')
yval=exp(qrx2$coefficients[1])*xval^qrx2$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
## both
plot(CB.tissue$Tissue_Dry_Weight_g[CB.tissue$Ploidy=="Diploid"]~
       CB.tissue$Total_Shell_Height_Length_mm[CB.tissue$Ploidy=="Diploid"], 
     type='p', ylim=c(0,8), xlim=c(0,180))
points(CB.tissue$Tissue_Dry_Weight_g[CB.tissue$Ploidy=="Triploid"]~
       CB.tissue$Total_Shell_Height_Length_mm[CB.tissue$Ploidy=="Triploid"], col='red', pch=1) 
yval=exp(qrx2$coefficients[1])*xval^qrx2$coefficients[2]
lines(xval, yval, col='blue', lwd=2, lty=1)
yval=exp(qrx3$coefficients[1])*xval^qrx3$coefficients[2]
lines(xval, yval, col='brown', lwd=2, lty=1)

test=CB.tissue %>% filter(Ploidy=="Triploid")
t=test %>% filter(Tissue_Dry_Weight_g >4)
t2=test #%>% filter(Tissue_Dry_Weight_g <4)
qrx4=rq(log(Tissue_Dry_Weight_g) ~ 
          log(Total_Shell_Height_Length_mm), 
        data=t2, tau=0.5, na.action = 'na.omit')
yval=exp(qrx4$coefficients[1])*xval^qrx4$coefficients[2]
lines(xval, yval, col='brown', lwd=2, lty=1)

plot(CB.tissue$Tissue_Dry_Weight_g[CB.tissue$Ploidy=="Triploid"]~
       CB.tissue$Total_Shell_Height_Length_mm[CB.tissue$Ploidy=="Triploid"], 
     ylim=c(0,8), xlim=c(0,180), type='n')
points(test$Tissue_Dry_Weight_g[test$Location_Index=="PX"]~
          test$Total_Shell_Height_Length_mm[test$Location_Index=="PX"], pch=1, col='red')
points(test$Tissue_Dry_Weight_g[test$Location_Index=="SV"]~
         test$Total_Shell_Height_Length_mm[test$Location_Index=="SV"], pch=1, col='blue')
points(test$Tissue_Dry_Weight_g[test$Location_Index=="YR"]~
         test$Total_Shell_Height_Length_mm[test$Location_Index=="YR"], pch=1, col='green')
plot(CB.shell$Shell_Dry_Weight_g[CB.shell$Ploidy=="Diploid"] ~ CB.shell$Total_Shell_Height_Length_mm[CB.shell$Ploidy=="Diploid"], type='p', 
     col='gray', ylim=c(0,200), xlim=c(0,140), ylab='Shell dry weight (g)', xlab='Shell height (mm)', pch=1, las=1,
     cex.axis=1.5,cex.lab=1.5)

t2=test %>% filter(Location_Index=="YR" | Location_Index=="SV")
qrx4=rq(log(Tissue_Dry_Weight_g) ~ 
          log(Total_Shell_Height_Length_mm), 
        data=t2, tau=0.5, na.action = 'na.omit')
yval=exp(qrx4$coefficients[1])*xval^qrx4$coefficients[2]
lines(xval, yval, col='brown', lwd=2, lty=3)

t2=test %>% filter(Location_Index=="PX")
qrx4=rq(log(Tissue_Dry_Weight_g) ~ 
          log(Total_Shell_Height_Length_mm), 
        data=t2, tau=0.5, na.action = 'na.omit')
yval=exp(qrx4$coefficients[1])*xval^qrx4$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=3)

test=WQ2 %>% filter(Parameter==param, year(SampleDate)<2018, year(SampleDate)>2014) %>%
  group_by(Station, Layer)%>% 
  summarize(min = min(MeasureValue),
            median = median(MeasureValue),
            mean = mean(MeasureValue),
            max = max(MeasureValue))


test=WQ2 %>%                               # Summary by group using purrr
  purr::split(.$MeasureValue) %>%
  map(summary)





PCB %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggboxplot(y='Tissue_N_Percent', x='Ploidy', xlab=F, fill='Ploidy', color = "black", 
             palette = c("white", "gray"),ylim=c(5,12), notch=T, facet.by = 'State' ,
             add="mean", ylab = 'Tissue N Percent', short.panel.labs = FALSE)
## try with base ggplot
PCB %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=Ploidy, fill=Ploidy)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(5, 12))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  facet_wrap(~State) +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = "all") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label= "p.signif") 


PCB %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggboxplot(y='Tissue_N_Percent', x='State', fill='Ploidy', ylab = 'Tissue N Percent', xlab='', ylim=c(5,12), 
          color = "black", palette = c("white", "gray"), notch=T) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", aes(group='State'),
               position = position_dodge2(width = 0.75, preserve = "single")) +
  theme(strip.text.y = element_blank())


plot_grid(
  p1, p2, p3, p4, p5, p6,
  nrow=3,
  align="hv"
)


test=PCB %>% select(State, Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State, Ploidy) %>%
  summarise(across(everything(), mean,na.rm=T))



### summary stats
test=PCB %>% select(State, Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State, Ploidy) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
write.csv(test, file='mean_by_state_and_ploidy.csv')

testsd=PCB %>% select(State, Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State, Ploidy) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))
write.csv(testsd, file='sd_by_state_and_ploidy.csv')

test=PCB %>% select(State, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
write.csv(test, file='mean_by_state.csv')

test=PCB %>% select(Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(Ploidy) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
write.csv(test, file='mean_by_ploidy.csv')

testsd=PCB %>% select(State, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))
write.csv(testsd, file='sd_by_state.csv')

testsd=PCB %>% select(Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(Ploidy) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))
write.csv(testsd, file='sd_by_ploidy.csv')


test=PCB %>% select(Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
write.csv(test, file='mean.csv')
testsd=PCB %>% select(Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))
write.csv(testsd, file='sd.csv')


### 20230807 plot all values SH:DW for Julie
plot(Seb2$`Cage Mean (dry weight)`~Seb2$MeanSH, type='n', xlab='Shell Height (mm)', ylab='Dry Weight (g)', las=1, 
     ylim=c(0,7), xlim=c(0,150))
points(CB$Tissue_Dry_Weight_g ~ CB$Total_Shell_Height_Length_mm,  pch=16,col='gray')
points(grizzle.all$`Soft Tissue DW (g)`~grizzle.all$`Shell Height (mm)`, pch=16,col='black')
points(reitsma$`Dry Tiss Mass (g)`~reitsma$`Shell Length (mm)`, pch=16, col='red')
points(poach$`Tissue_Dry_Weight_g` ~ poach$`Total_Shell_Height_Length_mm`, pch=16, col='blue')
points(Seb2$`Cage Mean (dry weight)`~Seb2$MeanSH, pch=16, col='orange')
points(bayer2$`dry tissue weight (g)` ~ bayer2$SH, pch=16, col='brown')
points(Barr$`Dry Tissue Weight (g)` ~Barr$`Shell Height (mm)`, pch=16, col='yellow')
points(Ayvazian$`Tissue Dry Weight (g)` ~ Ayvazian$`Shell Height  (mm)` , pch=16, col='purple')
points(Kiffney$DryTissueWeight_g ~ Kiffney$ShellHeight_mm, pch=16, col='green')
points(Darrow$`Tissue Dry Wt (g)`~Darrow$`Length (mm)`, pch=16, col='gray50')
legend('topleft', 
       bty='n', 
       horiz=F, 
       legend = c('CB-C','NH', 'MA', 'CB-P', 'NY', 'CT', 'NJ-DE', 'RI', 'ME', 'NC'), 
       text.col=c('gray','black', 'red', 'blue', 'orange', 'brown', 'yellow', 'purple', 'green', 'gray50'))



plot(Seb.f$Dry_tissue_wt_g ~ Seb.f$Shell_height_mm, type='p', xlab='SH (mm)', ylab='DW (g)', las=1, 
     ylim=c(0,7), xlim=c(0,150), main='All')
Seb.f %>% ggplot(aes(y=Dry_tissue_wt_g, x=Shell_height_mm, color=Site)) + geom_point()


### plotting Main dataframe, options to:
# 1) plot all together (full)
# 2) remove CB - (not include in regression of rest)
# 3) remove CB and Levinton NY
# plot states independently
# plot regions


s1=match("Poach et al. in prep 2023", Main$Data_Source) # start of Poach data (after end of CB)
s2=match("Levinton J, et al. 2011 PLoS ONE 6(4)", Main$Data_Source)-1 #Last data before Levinton
s3=match("Barr et al. submitted 2022", Main$Data_Source) # Start of Barr (after end of Levinton)
s4=match("Sebastiano et al 2015", Main$Data_Source)-1 #Last data before Sebastiano
vec=c(s1:s2, s3:dim(Main)[1]) # No CB, No Levinton
vecNoCBNY=c(s1:s4, s3:dim(Main)[1]) # No CB, Sebastiano, Levinton
# 1) plot all together (full)
P=Main %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=State))+ 
  geom_point()+
  ylim(0,8) +
  xlim(0, 200)
# No CB
P1=Main[s1:dim(Main)[1],] %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=State)) +
  geom_point() +
  ylim(0,8) +
  xlim(0, 200)
# Just CB
P2=Main[1:s1,] %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=State)) +
  geom_point() +
  ylim(0,8) +
  xlim(0, 200)

# No CB no Levinton
P3=Main[vec,] %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=State)) +
  geom_point() +
  ylim(0,8) +
  xlim(0, 200)

# No Levinton or Sebastiano or CB
P4=Main[vecNoCBNY,] %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=State)) +
  geom_point() +
  ylim(0,8) +
  xlim(0, 200)

# All data
qr.main=rq(log(Tissue_Dry_Weight_g) ~ 
             log(Total_Shell_Height_Length_mm), 
           data=Main, tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.main$coefficients[1])*xval^qr.main$coefficients[2]
mod.all=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)

qr.main=lm(log(Tissue_Dry_Weight_g) ~ 
             log(Total_Shell_Height_Length_mm), 
           data=Main, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.main$coefficients[1])*xval^qr.main$coefficients[2]
mod.all.lm=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)

# CB only
qr.x=rq(log(Tissue_Dry_Weight_g) ~ 
          log(Total_Shell_Height_Length_mm), 
        data=Main[1:9727,], tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
mod.CB=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)

# No CB, No Levinton

qr.x=rq(log(Tissue_Dry_Weight_g) ~ 
          log(Total_Shell_Height_Length_mm), 
        data=Main[vec,], tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
mod.noCBnoLev=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)

# No CB
qr.x=rq(log(Tissue_Dry_Weight_g) ~ 
          log(Total_Shell_Height_Length_mm), 
        data=Main[s1:dim(Main)[1],], tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
mod.noCB=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)

# No Levinton
qr.x=rq(log(Tissue_Dry_Weight_g) ~ 
          log(Total_Shell_Height_Length_mm), 
        data=Main[Main$Data_Source!="Levinton J, et al. 2011 PLoS ONE 6(4)",], tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
mod.noLev=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)

### CB Panel report 2nd model
xval=seq(0,180,by=.5)
yval=(0.00037)*xval^1.83359 # CBP all data BMP Second Report Appendix
mod.BMP=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)

P + 
  geom_line(data=mod.all, color='black', size=1.25) +
  geom_line(data=mod.CB, color='black', linetype = "dashed", size=1.25) +
  geom_line(data=mod.noLev, color='red', size=1.25) +
  geom_line(data=mod.noCB, color='gray', size=1.25) +
  geom_line(data=mod.noCBnoLev, color='blue', size=1.25)+
  geom_line(data=mod.BMP, color='black', linetype = "dotted", size=1.25)+
  geom_line(data=mod.all.lm, color='green', size=1.25)

P3 + geom_line(data=mod.noCBnoLev, color='blue', size=1.25) +
  geom_line(data=mod.BMP, color='black', linetype = "dotted", size=1.25)

plot(Main$Tissue_Dry_Weight_g ~ Main$Total_Shell_Height_Length_mm, type='n', ylim=c(0,8), xlim=c(0,200))
lines(mod.all$Tissue_Dry_Weight_g ~mod.all$Total_Shell_Height_Length_mm, col='black', lwd=2)
lines(mod.all.lm$Tissue_Dry_Weight_g ~mod.all.lm$Total_Shell_Height_Length_mm, col='black')
lines(mod.BMP$Tissue_Dry_Weight_g ~mod.BMP$Total_Shell_Height_Length_mm, col='black', lty=2, lwd=2)

lines(gxval, gyval, col='gray70', lwd=2) #NH
lines(rxval, ryval, col='red', lwd=2) #MA
lines(cxval, cyval, col='blue', lwd=2)#CB
lines(sxval, syval, col='purple', lwd=2) #NY
lines(bxval, byval, col='green', lwd=2) #CT
lines(njxval, dbyval, col='yellow', lwd=2) #NJ Delaware Bay
lines(njxval, bbyval, col='orange', lwd=2) #NJ Barnegat Bay
lines(njxval, rbyval, col='brown', lwd=2) #DE Rehobath Bay
legend('topleft', bty='n', 
       legend = c('NH', 'MA', 'CB', 'NY', 'CT', 'NJ Del', 'NJ Barn', 'DE Reho'), 
       text.col=c('gray70', 'red', 'blue', 'purple', 'green', 'yellow', 'orange', 'brown'))


q11 <- qualitative_hcl(11, "Dark3")
q11 <- diverging_hcl(11, "Berlin")
q11 <- diverging_hcl(11, "Tofino")
# qr By state
plot(Main$Tissue_Dry_Weight_g ~ Main$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
#plot with CB
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, col='gray70', ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
#plot with CB inches
# plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_Inches[1:9727], type='p', 
#      pch=19, col='gray70', ylim=c(0,8), xlim=c(0,7.87402), ylab="Dry weight (g)", xlab="Shell height (in)", las=1)

7.87402
stt=sort(unique(Main$State))
MainNoCB=Main[s1:dim(Main)[1],]
MainNoCBLev=Main[vec,]
for(i in 1:length(unique(Main$State))){
  # dataa=Main[Main$State==stt[i],]
  dataa=MainNoCB[MainNoCB$State==stt[i],]
  # dataa=MainNoCBLev[MainNoCBLev$State==stt[i],]
  qr.x=rq(log(Tissue_Dry_Weight_g) ~  log(Total_Shell_Height_Length_mm),
          data=dataa, tau=0.5, na.action = 'na.omit')
  xval=seq(0,180,by=.5)
  yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
  # points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col=q11[i], ylim=c(0,8), xlim=c(0,200))
  lines(yval ~xval, col=q11[i], lwd=2)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, 
           data = dataa, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  x <- seq(0, 180, length = 250)
  # lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 2, lwd=2, col = q11[i])
  }
legend('topleft', bty='n', 
       legend = c('CT', 'DE', 'ME', 'MD', 'MA', 'NH', 'NJ', 'NY', 'NC', 'RI', 'VA'), 
       text.col=q11)
# Add in qr50 for all included data (change data=...)
qr.x=rq(log(Tissue_Dry_Weight_g) ~  log(Total_Shell_Height_Length_mm), 
        data=Main[1:s1-1,], tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
lines(yval ~xval, col='black', lwd=2)
lines(mod.BMP$Tissue_Dry_Weight_g ~mod.BMP$Total_Shell_Height_Length_mm, col='black', lty=2, lwd=2)

plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
    pch=19, col='gray70', ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
# try lm fit instead of qr 
cb2023lm=lm(log(Main$Tissue_Dry_Weight_g[1:s1-1])~log(Main$Total_Shell_Height_Length_mm[1:s1-1]))
yval=exp(cb2023lm$coefficients[1])*xval^cb2023lm$coefficients[2]
lines(yval ~xval, col='blue', lwd=2)
### adding in routine from BMP
# cb2023qr=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = Main[1:s1-1,], start = list(a = 0.0001, b = 2.1056), tau=0.5)
cb2023qr=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = Main[1:s1-1,], start = list(a = 0.00037, b = 1.83359), tau=0.5)
x <- seq(0, 180, length = 250)
lines(predict(cb2023qr, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 2, lwd=2, col = "red")


plot(Main$Tissue_Dry_Weight_g ~ Main$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
for(i in 1:length(unique(Main$State))){
  # dataa=Main[Main$State==stt[i],]
  dataa=MainNoCB[MainNoCB$State==stt[i],]
  # dataa=MainNoCBLev[MainNoCBLev$State==stt[i],]
  qr.x=rq(log(Tissue_Dry_Weight_g) ~  log(Total_Shell_Height_Length_mm),
          data=dataa, tau=0.5, na.action = 'na.omit')
  xval=seq(0,180,by=.5)
  yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
  points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col=q11[i], ylim=c(0,8), xlim=c(0,200))
  lines(yval ~xval, col=q11[i], lwd=2)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  x <- seq(0, 180, length = 250)
  lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 2, lwd=2, col = q11[i])
}
legend('topleft', bty='n', 
       legend = c('CT', 'DE', 'ME', 'MD', 'MA', 'NH', 'NJ', 'NY', 'NC', 'RI', 'VA'), 
       text.col=q11)
summary(qrx)
summary(qr.x)

## nlrq for all data (no CB)
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, col='gray70', ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
dataa=MainNoCB
points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col='gray30', ylim=c(0,8), xlim=c(0,200))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa, start = list(a = 0.00037, b = 1.83359), tau=0.5)
x <- seq(0, 180, length = 250)
lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 1, lwd=2, col = 'black')
summary(qrx)
## effect of removing Levinton
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, col='gray70', ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
dataa=MainNoCBLev
points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col='gray30', ylim=c(0,8), xlim=c(0,200))
lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 1, lwd=2, col = 'black')
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa, start = list(a = 0.00037, b = 1.83359), tau=0.5)
x <- seq(0, 180, length = 250)
lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 2, lwd=2, col = 'black')
summary(qrx)


## OLS starting points for 'a' and 'b' for nlrq (not working yet)
y=log(dataa$Tissue_Dry_Weight_g)
x=log(dataa$Total_Shell_Height_Length_mm)
slope <- cor(x, y) * (sd(y) / sd(x))
intercept <- mean(y) - (slope * mean(x))
## get values of power model:
#Diploids: (qr2)
exp(qr2$coefficients[1])
qr2$coefficients[2]

### checking on quantile regression rq vs nlrq
plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"], type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='Tissue dry weight (g)', xlab='Shell height (mm)', pch=19, las=1,
     cex.axis=1.5,cex.lab=1.5)
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], pch=1, col='purple', )
clmt=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"]))
clmd=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"]))
t=PCB %>% filter(Ploidy=="Diploid")
qr2=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
xval=seq(20,140,by=.5)
yval=exp(qr2$coefficients[1])*xval^qr2$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=1)
qrxd=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = t, start = list(a = 0.00037, b = 1.83359), tau=0.5)
x <- seq(0, 180, length = 250)
lines(predict(qrxd, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 2, lwd=2, col = 'yellow')

t=PCB %>% filter(Ploidy=="Triploid")
qr3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
yval=exp(qr3$coefficients[1])*xval^qr3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
qrxt=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = t, start = list(a = 0.00037, b = 1.83359), tau=0.5)
x <- seq(0, 180, length = 250)
lines(predict(qrxt, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 2, lwd=2, col = 'green')


qrxt=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = t, start = list(a = 0.0005, b = 2), tau=0.5)
x <- seq(0, 180, length = 250)
lines(predict(qrxt, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 1, lwd=2, col = 'yellow')

## 2023 report updates
yvalcbdng=(0.00037)*xval^1.83359 # CB BMP diploids w/out gear BMP Second Report Appendix 2023
yvalcbdg=(0.00016)*xval^2.07714 # CB BMP diploids w/ gear BMP Second Report Appendix 2023
yvalcbtg=(0.00002)*xval^2.607 # CB BMP triploids w/ gear BMP Second Report Appendix 2023
lines(xval, yvalcbdng, col='black', lwd=2, lty=3)
lines(xval, yvalcbdg, col='black', lwd=2, lty=2)
lines(xval, yvalcbtg, col='red', lwd=2, lty=2)

s5=match("Reitsma et al. 2017", Main$Data_Source)-1 #end of Poach
q10 <- qualitative_hcl(10, "Dark3")
## plot CB BMP by source (10)
plot(Main$Tissue_Dry_Weight_g ~ Main$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
dss=sort(unique(Main$Data_Source[1:s1-1]))
for(i in 1:length(unique(Main$Data_Source[1:s1-1]))){
  # dataa=Main[Main$State==stt[i],]
  dataa=Main[Main$Data_Source==dss[i],]
  # dataa=MainNoCBLev[MainNoCBLev$State==stt[i],]
  qr.x=rq(log(Tissue_Dry_Weight_g) ~  log(Total_Shell_Height_Length_mm),
          data=dataa, tau=0.5, na.action = 'na.omit')
  xval=seq(0,180,by=.5)
  yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
  points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col=q10[i], ylim=c(0,8), xlim=c(0,200))
  lines(yval ~xval, col=q10[i], lwd=2)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  x <- seq(0, 180, length = 250)
  lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 3, lwd=2, col = q10[i])
}

legend('topleft', bty='n', legend = dss, text.col=q11)
legend('topleft', bty='n', legend = dss[i], text.col=q11[i])

### CB BMP by gear and ploidy
dataa=Main[1:s1-1,]
table(dataa$Representative_Aquaculture_Oyster_Practice)
plot(Main$Tissue_Dry_Weight_g ~ Main$Total_Shell_Height_Length_mm, type='n', 
     ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
points(dataa$Tissue_Dry_Weight_g[which(dataa$Ploidy=="Diploid" & dataa$Representative_Aquaculture_Oyster_Practice=="On-Bottom without Gear")] ~ 
         dataa$Total_Shell_Height_Length_mm[which(dataa$Ploidy=="Diploid" & dataa$Representative_Aquaculture_Oyster_Practice=="On-Bottom without Gear")], 
       pch=21, col='gray70', ylim=c(0,8), xlim=c(0,200))
points(dataa$Tissue_Dry_Weight_g[which(dataa$Ploidy=="Triploid" & dataa$Representative_Aquaculture_Oyster_Practice=="Off-Bottom with Gear")] ~ 
         dataa$Total_Shell_Height_Length_mm[which(dataa$Ploidy=="Triploid" & dataa$Representative_Aquaculture_Oyster_Practice=="Off-Bottom with Gear")], 
       pch="x", col='red', ylim=c(0,8), xlim=c(0,200))
points(dataa$Tissue_Dry_Weight_g[which(dataa$Ploidy=="Diploid" & dataa$Representative_Aquaculture_Oyster_Practice=="Off-Bottom with Gear")] ~ 
         dataa$Total_Shell_Height_Length_mm[which(dataa$Ploidy=="Diploid" & dataa$Representative_Aquaculture_Oyster_Practice=="Off-Bottom with Gear")], 
       pch="+", col='black', ylim=c(0,8), xlim=c(0,200))
lines(xval, yvalcbdng, col='gray50', lwd=2, lty=2)
lines(xval, yvalcbdg, col='black', lwd=2, lty=2)
lines(xval, yvalcbtg, col='red', lwd=2, lty=2)

ssdng=dataa[which(dataa$Ploidy=="Diploid" & dataa$Representative_Aquaculture_Oyster_Practice=="On-Bottom without Gear"),]
ssdg=dataa[which(dataa$Ploidy=="Diploid" & dataa$Representative_Aquaculture_Oyster_Practice=="Off-Bottom with Gear"),]
sstg=dataa[which(dataa$Ploidy=="Triploid" & dataa$Representative_Aquaculture_Oyster_Practice=="Off-Bottom with Gear"),]
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = ssdng, start = list(a = 0.00037, b = 1.83359), tau=0.5)
lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 3, lwd=2, col = 'gray50')
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = ssdg, start = list(a = 0.00037, b = 1.83359), tau=0.5)
lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 3, lwd=2, col = 'black')
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = sstg, start = list(a = 0.00037, b = 1.83359), tau=0.5)
lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 3, lwd=2, col = 'red')





#plot gear on bottom 
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, col='gray70', ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1, main="On-bottom no gear")
dataa=Main[Main$Representative_Aquaculture_Oyster_Practice=="On-Bottom without Gear",]
points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col=q11[i], ylim=c(0,8), xlim=c(0,200))
qr.x=rq(log(Tissue_Dry_Weight_g) ~  log(Total_Shell_Height_Length_mm), 
        data=dataa, tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
lines(yval ~xval, col='black', lwd=2)
lines(mod.BMP$Tissue_Dry_Weight_g ~mod.BMP$Total_Shell_Height_Length_mm, col='black', lty=2, lwd=2)


#plot off bottom
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, col='gray70', ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1, main="Off-bottom with gear")
dataa=Main[Main$Representative_Aquaculture_Oyster_Practice=="Off-Bottom with Gear",]
points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col=q11[i], ylim=c(0,8), xlim=c(0,200))
qr.x=rq(log(Tissue_Dry_Weight_g) ~  log(Total_Shell_Height_Length_mm), 
        data=dataa, tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
lines(yval ~xval, col='black', lwd=2)
lines(mod.BMP$Tissue_Dry_Weight_g ~mod.BMP$Total_Shell_Height_Length_mm, col='black', lty=2, lwd=2)

# plot SH:DW by growth location type for all data
Main %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=Oyster_Growth_Location_Type))+ 
  geom_point()+
  ylim(0,8) +
  xlim(0, 200)

# plot SH:DW by growth location type for all data besides CB BMP and Levinton NY
Main[vecNoCBNY,] %>% ggplot(aes(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm, color=Oyster_Growth_Location_Type))+ 
  geom_point()+
  ylim(0,8) +
  xlim(0, 200)

Main[complete.cases(Main$Tissue_N_Percent),] %>% select(State, Month_Oysters_Removed, Tissue_N_Percent) %>% 
  ggplot(aes(x=Month_Oysters_Removed, y = Tissue_N_Percent, color = State, shape = State)) +
  xlim(1,12) +
  ylim(5,11)+
  geom_point()

Main[complete.cases(Main$Tissue_C_Percent),] %>% select(State, Month_Oysters_Removed, Tissue_C_Percent) %>% 
  ggplot(aes(x=Month_Oysters_Removed, y = Tissue_C_Percent, color = State, shape = State)) +
  xlim(1,12) +
  ylim(15,60)+
  geom_point()



## Tissue P
Main[complete.cases(Main$Tissue_TP_Percent),] %>% select(State, Ploidy, Tissue_TP_Percent) %>% 
  ggplot(aes(y=Tissue_TP_Percent, x=State)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Tissue P Percent') +
  coord_cartesian(ylim = c(0.5, 1.5))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 


## Tissue C
Main[complete.cases(Main$Tissue_C_Percent),] %>% select(State, Ploidy, Tissue_C_Percent) %>% 
  ggplot(aes(y=Tissue_C_Percent, x=State)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Tissue C Percent') +
  coord_cartesian(ylim = c(15, 55))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 


## Tissue N
Main[complete.cases(Main$Tissue_N_Percent),] %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=State)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(0, 15))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 


## Shell N
Main[complete.cases(Main$Shell_N_Percent),] %>% select(State, Ploidy, Shell_N_Percent) %>% 
  ggplot(aes(y=Shell_N_Percent, x=State)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Shell N Percent') +
  coord_cartesian(ylim = c(0, 1))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 

## Shell P
Main[complete.cases(Main$Shell_TP_Percent),] %>% select(State, Shell_TP_Percent) %>% 
  ggplot(aes(y=Shell_TP_Percent, x=State)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Shell P Percent') +
  coord_cartesian(ylim = c(0.02, 0.06))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 

# testing shell P 
boxplot(PCB$Shell_TP_Percent~PCB$State)
PCB %>% ggplot(aes(y=Shell_TP_Percent, x=State)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Shell P Percent') +
  coord_cartesian(ylim = c(0.02, 0.06))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))
Main[complete.cases(Main$Shell_TP_Percent),] %>% ggplot(aes(y=Shell_TP_Percent, x=State)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Shell P Percent') +
  coord_cartesian(ylim = c(0.02, 0.06))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))
test=Main[complete.cases(Main$Shell_TP_Percent),]
boxplot(test$Shell_TP_Percent~test$State)
test2=test[test$Data_Source!="Poach et al. in prep 2023",]
boxplot(test2$Shell_TP_Percent~test2$State)


## Shell C #Reitsma data issue, half values around 15, rest around 0.5-1.5.
# Joshua Reitsma:
  # Organic Carbon values shown for Spring, missing values were run so inorganic was included, fall includes all Carbon
Main[complete.cases(Main$Shell_C_Percent),] %>% select(State, Ploidy, Shell_C_Percent) %>% 
  ggplot(aes(y=Shell_C_Percent, x=State)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Shell C Percent') +
  coord_cartesian(ylim = c(10, 15))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 


### looking into CBP monitoring for stations in CB BMP data
xx=unique(Main$Data_Source[1:s1-1])
# [1] "Higgins-2011"      "Parker-unpubl"     "Kingsley-Smith"    "Higgins-Choptank"  "Higgins-Lynnhaven"
# [6] "Ross-LA"           "Ross-Monitor"      "Kellogg-Choptank"  "Liddel-2008"       "Powell_Mann" 
for(i in (1:length(xx))){
print(paste("Data Source:", xx[i]))
# print(unique(Main$Location_Index[Main$Data_Source==xx[i]]))
# print(table(Main$Location_Index[Main$Data_Source==xx[i]]))
print(unique(Main$Near_Waterbody__General_Location[Main$Data_Source==xx[i]]))
print(table(Main$Near_Waterbody__General_Location[Main$Data_Source==xx[i]]))
}
# Spencer's St. Jerome 
#         46         38 
unique(Main$Location_Index[Main$Data_Source=="Higgins-2011"])
table(Main$Location_Index[Main$Data_Source=="Higgins-2011"])



unique(CB2016$Data_Source)



