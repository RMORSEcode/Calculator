
names=data.frame(matrix(NA, nrow = ncol(CB), ncol=1)); colnames(names)[1]='cornwell'
names$cornwell=colnames(CB)

names$poach=NA
names$poach[1:ncol(poach)]=colnames(poach)

names$reitsma=NA
names$reitsma[1:ncol(reitsma)]=colnames(reitsma)

names$grizzle=NA
names$grizzle[1:ncol(grizzle.all)]=colnames(grizzle.all)

names$bayer=NA
names$bayer[1:ncol(bayer)]=colnames(bayer)

names$seb2=NA
names$seb2[1:ncol(Seb2)]=colnames(Seb2)

names$Barr=NA
names$Barr[1:ncol(Barr)]=colnames(Barr)

names$Kiffney=NA
names$Kiffney[1:ncol(Kiffney)]=colnames(Kiffney)

names$Darrow=NA
names$Darrow[1:ncol(Darrow)]=colnames(Darrow)

names$Ayvazian=NA
names$Ayvazian[1:ncol(Ayvazian)]=colnames(Ayvazian)

date='20230810'
write.csv(names, file=paste(wd, 'Join/', date,'_data_names.csv', sep=''))

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
### add Poach data PCB
colnames(newdf)[!(colnames(newdf) %in% colnames(PCB))]
colnames(PCB)[!(colnames(PCB) %in% colnames(newdf))]
colnames(PCB)[1]="Representative_Aquaculture_Oyster_Practice"
PCB2=select(PCB, -is.outlier)
newdf2=bind_rows(newdf, PCB2)
colnames(newdf)[!(colnames(newdf) %in% colnames(r2))]


## now fix names and add Reitsma (C virginica only)
# drop derived columns
r2=reitsma %>% select(-c(Analysis, GroupID, Species, OysterGrp,`off/on bottom`, ))
colnames(newdf)[!(colnames(newdf) %in% colnames(r2))]
#fix column names
colnames(r2)[1]="Number_ID"
colnames(r2)[2]="Season_Oysters_Removed"
colnames(r2)[3]="Date_Oysters_Removed"
colnames(r2)[4]="Near_Waterbody__General_Location"
colnames(r2)[5]="Waterbody_Name"
colnames(r2)[7]="Hatchery-produced_or_Wild"
colnames(r2)[8]="Volume_ml" #"Volume (ml)" 
r2$Ploidy="Diploid"
r2$Ploidy[reitsma$`off/on bottom`=='off-triploid']="Triploid"
r2$Representative_Aquaculture_Oyster_Practice=NA
r2$Representative_Aquaculture_Oyster_Practice[reitsma$OysterGrp=="W"]="On-Bottom without Gear" 
r2$Representative_Aquaculture_Oyster_Practice[reitsma$OysterGrp=="Con"]="On-Bottom without Gear"
r2$Representative_Aquaculture_Oyster_Practice[reitsma$OysterGrp=="Coff"]="Off-Bottom with Gear"
r2$Representative_Aquaculture_Oyster_Practice[reitsma$OysterGrp=="Coff-T"]="Off-Bottom with Gear"
colnames(r2)[9]="Total_Shell_Height_Length_mm" #"Shell Length (mm)"
colnames(r2)[10]="Total_Shell_Width_mm" #"Shell Width (mm)"
colnames(r2)[11]="Total_Shell_Depth_mm" #[11] "Shell Height (mm)"
colnames(r2)[12]="Shell&Tissue_Total_Wet_Weight_g" #"Whole Weight (g)"
colnames(r2)[13]="Shell_Dry_Weight_g" #Dry Shell Mass (g)"
colnames(r2)[14]="Tissue_Dry_Weight_g" #"Dry Tiss Mass (g)"
colnames(r2)[15]="Condition_Index" #"Condition Index"    
colnames(r2)[18]="Tissue_N_Percent" #Meat %N"
colnames(r2)[19]="Tissue_C_Percent"#"Meat % C"
colnames(r2)[22]="Shell_N_Percent"#"Shell %N" 
colnames(r2)[23]="Shell_C_Percent"#"Shell % C" 
#[16] "Shell/Length (g/mm)"
#"DryT/Length (g/mm)"
# #"Tissue N (g)"       
# [21] "Tissue C (g)"
# "Shell N (g)"
# "Shell C (g)"      
#[26] "gN/animal"   
#"gC/animal"   
#"gN/gAnimal"     
#"%N/animal"    
#"gC/gAnimal"         
#[31] "%C/animal"
colnames(r2)[!(colnames(r2) %in% colnames(newdf))]
r3=r2 %>% select(-c(`Shell/Length (g/mm)`, `DryT/Length (g/mm)`,`Tissue N (g)`,
                    `Tissue C (g)`,`Shell N (g)`:`%C/animal`))#, `Shell C (g)`))
colnames(r3)[!(colnames(r3) %in% colnames(newdf))]
### now add in missing cols that will not be NA:
colnames(newdf)[!(colnames(newdf) %in% colnames(r3))]
# [1] "Raw_Data_File"                         "Data_Source"                          
# [3] "Location_Index"                        "State"                                
# [5] "Waterbody_Type"                        "Site_within_Study"                    
# [7] "Oyster_Growth_Location_Type"           "Subtidal_Intertidal_WaterColumn_Other"
# [9] "Oyster_Stock"                          "Date_Oysters_Deployed"                
# [11] "Month_Oysters_Removed"                 "Year_Oysters_Removed"                 
# [13] "Tissue_AFDW_g"                         "Tissue_CN_molar"                      
# [15] "Tissue_TC_g_C_per_ g_dw"               "Tissue_TN_g_N_per_g_dw"               
# [17] "Tissue_TP_Percent"                     "Tissue_TP_g_P_per_g_dw"               
# [19] "Shell_CN_molar"                        "Shell_TC_g_C_per_ g_dw"               
# [21] "Shell_TN_g_N_per_g_dw"                 "Shell_TP_Percent"                     
# [23] "Shell_TP_g_P_per_g_dw"                 "Total_Shell_Height_Length_Inches"     
# [25] "Oyster_Size_Class"                     "Habitat_Group"                        
r3$Raw_Data_File="Reitsma Shellfish N Sample data.xlsx"
r3$Data_Source='Reitsma et al. 2017'
r3$State="Massachusetts"
newdf2=bind_rows(newdf2, r3)

 




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

library(rstatix)
library(tidyverse)
library(ggpubr)

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




