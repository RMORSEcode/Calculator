# library("googledrive")
library(dplyr)
library(lubridate)
library(readxl)

wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Nitrogen data"
files=list.files(wd, pattern='.xls')

reitsma.nms=readxl::read_xlsx(paste(wd,'/', 'Reitsma Shellfish N Sample data.xlsx', sep=''), skip=0, n_max=5, col_names = F)
reitsma=readxl::read_xlsx(paste(wd,'/', 'Reitsma Shellfish N Sample data.xlsx', sep=''), skip=4)
poach=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited_Nov2020.xlsx', sep=''), sheet='Compiled Data')
poach2=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='compiled')
# create better version with all vars
PCBVA=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='Original Data Virginia RM')
PCBVA=PCBVA[-1,]
PCBMD=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='Original Data Maryland RM')
PCBMD=PCBMD[-1,]
colnames(PCBVA)[!(colnames(PCBVA) %in% colnames(PCBMD))]
PCBVA=subset(PCBVA, select=-c(`bad_Shell_TC_g_C_per_ g_dw`, `bad_Shell_TN_g_N_per_g_dw`))
PCB=rbind(PCBVA, PCBMD)
PCB=subset(PCB, select=-c(`Shell_TP_g_P_per_g_dw`, `Shell_TP_Percent`))
## add shell P, dates are from first sampling date only, merge on sampled ID 
pcbshellPO4=read_xlsx("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Nitrogen data/Poach/Daily Shell Phosphate Tests_Dec_2020b.xlsx", sheet='Data_Summary')
pcbshellPO4=pcbshellPO4[,c(1,5,6)]
colnames(pcbshellPO4)[1]="Number_ID"
pcbshellPO4$Number_ID=as.character(pcbshellPO4$Number_ID)
# pcbshellPO4$shell_percent_P=pcbshellPO4$`Shell P content mg/g`/10
PCB=left_join(PCB, pcbshellPO4, by="Number_ID", keep=F)

bayer=readxl::read_xlsx(paste(wd,'/','RM.xlsx', sep=''), sheet='Bayer')
higgins=readxl::read_xlsx(paste(wd,'/','Higgins_data.xlsx', sep=''), sheet='Sheet1')
higgins=higgins %>% filter(Tissue_N_Percent <15) #drop bad sample
grizzle.dep=readxl::read_xlsx(paste(wd,'/','Grizzle_2011-data.xlsx', sep=''), sheet='post')#deployment
grizzle.init=readxl::read_xlsx(paste(wd,'/','Grizzle_2011-data.xlsx', sep=''), sheet='pre') #initial values
grizzle=readxl::read_xlsx(paste(wd,'/','Grizzle_2011-data.xlsx', sep=''), sheet='all') #initial values
## correct for mislabeling of larger oysters mg->g (small oysters appear OK)
grizzle.all$DW=NA
grizzle.all$DW[grizzle.all$`Shell Height (mm)`>20]=grizzle.all$`Soft Tissue DW (g)`[grizzle.all$`Shell Height (mm)`>20]*10
grizzle.all$DW[grizzle.all$`Shell Height (mm)`<20]=grizzle.all$`Soft Tissue DW (g)`[grizzle.all$`Shell Height (mm)`<20]
# grz=grizzle.all %>% filter(`Shell Height (mm)`>20)
# plot((10*grz$`Soft Tissue DW (g)`)~grz$`Shell Height (mm)`, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
#      ylim=c(0,5), xlim=c(20,80), main='NH')
# glm=lm(log(10*grz$`Soft Tissue DW (g)`)~log(grz$`Shell Height (mm)`))
# xval=seq(20,80,by=.5)
# yval=exp(glm$coefficients[1])*xval^glm$coefficients[2]
# lines(xval, yval)
# text(40,4.5, paste('y=',round(exp(glm$coefficients[1]),9),'x^',round(glm$coefficients[2],3), sep='' ))
grz=grizzle.all
plot((grz$DW)~grz$`Shell Height (mm)`, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,5), xlim=c(0,80), main='NH')
glm=lm(log(grz$DW)~log(grz$`Shell Height (mm)`))
xval=seq(0,80,by=.5)
yval=exp(glm$coefficients[1])*xval^glm$coefficients[2]
lines(xval, yval)
text(20,4, paste('y=',round(exp(glm$coefficients[1]),9),'x^',round(glm$coefficients[2],3), sep='' ))

## Maine data from Tom Kiffney
Kiff1=read.csv(paste(wd,'Oyster morphometrics/Kiffney/tissue2021.csv', sep=''))
Kiff2=read.csv(paste(wd,'Oyster morphometrics/Kiffney/shell2021.csv', sep=''))
Kiffney=left_join(Kiff1, Kiff2, by=c('Site', 'ShellHeight_mm', 'WholeWetWeight_g'))
Kiffney2=read.csv(paste(wd,'Oyster morphometrics/Kiffney/tissuePloidy2022.csv', sep=''))

## NC data from Beth Darrow
Darrow=read_xlsx(paste(wd,"Oyster morphometrics/NC/NCOysterSizeData_DarrowKInsella_forJulieRose.xlsx", sep=''), sheet='RM_morpho')




Sebastiano=readxl::read_xlsx(paste(wd,'/','RM.xlsx', sep=''), sheet='SebMeanSH')
Sebastiano2=readxl::read_xlsx(paste(wd,'/','RM.xlsx', sep=''), sheet='SebDW')
Seb.1=Sebastiano %>% group_by(Site, Date, Cage) %>%
  mutate(MeanSH=mean(`Bag Mean (height)`)) %>% 
  select(-Bag, -`Bag Mean (height)`, -`Site mean`) %>%
  distinct(.)
Seb=Seb.1 %>% left_join(Sebastiano2, by=c('Date', 'Site'))


Sebtest=Sebastiano[complete.cases(Sebastiano$`Site mean`),]
# Seb=Sebtest %>% left_join(Sebastiano2, by=c('Date', 'Site'))
Seb2=Seb[complete.cases(Seb$`Cage Mean (dry weight)`),]
plot(Seb2$`Cage Mean (dry weight)`~Seb2$MeanSH, type='p', xlab='SH (mm)', ylab='DW (g)', las=1, ylim=c(0,3), xlim=c(45,80))
#drop Shelter Island sites
# Seb3=Seb2 %>% filter(Site != 'SI')
# plot(Seb3$`DW (g)`~Seb3$`Site mean`, type='p', xlab='SH (mm)', ylab='DW (g)', las=1, 
#      ylim=c(0,3), xlim=c(45,80), main='NY')
# slm=lm(log(Seb3$`DW (g)`)~log(Seb3$`Site mean`))
# xval=seq(45,80,by=.5)
# yval=exp(slm$coefficients[1])*xval^slm$coefficients[2]
# lines(xval, yval)
# text(60,2.5, paste('y=',round(exp(slm$coefficients[1]),9),'x^',round(slm$coefficients[2],3), sep='' ))
# with SI sites
plot(Seb2$`Cage Mean (dry weight)`~Seb2$MeanSH, type='p', xlab='SH (mm)', ylab='DW (g)', las=1, ylim=c(0,3), xlim=c(45,80))
slm=lm(log(Seb2$`Cage Mean (dry weight)`)~log(Seb2$MeanSH))
xval=seq(45,80,by=.5)
yval=exp(slm$coefficients[1])*xval^slm$coefficients[2]
lines(xval, yval)
text(60,2.5, paste('y=',round(exp(slm$coefficients[1]),9),'x^',round(slm$coefficients[2],3), sep='' ))


## NH (no dates) ~ fall 3 month deplyment
plot(grizzle$`%N` ~ grizzle$`Shell Height (mm)`, typ='p') #after deployment
plot(grizzle2$`%N` ~ grizzle2$`Height (mm)`, typ='p') # pre-deployment
plot(grizzle.all$`%N` ~ grizzle.all$`Shell Height (mm)`, typ='p') #all together


## Greenwich CT
btest=bayer[complete.cases(bayer$`Tissue %N`),]
plot(btest$`Tissue %N`~month(btest$Date))
plot(btest$`Tissue %N`~(btest$Date))
boxplot(btest$`Tissue %N`~(btest$Date), main='Greenwhich CT', ylab='Tissue %N', xlab='')
boxplot(btest$`Tissue %N`~month(btest$Date), main='Greenwhich CT', ylab='Tissue %N', xlab='')

bshell=bayer[complete.cases(bayer$`Shell %N`),]
plot(bshell$`Shell C(mg/mg)` ~ month(bshell$Date), main='Greenwhich CT', ylab='Shell %C', xlab='', type='p')

Bayer.sum=btest %>% select(`Tissue %N`, Date)%>%
  mutate(mon=month(Date), .keep='unused') %>%
  group_by(mon) %>% 
  summarise(cnt=n(), mean=mean(`Tissue %N`, na.rm=T),med=median(`Tissue %N`, na.rm=T),sd=sd(`Tissue %N`, na.rm=T)) 
pwc=btest %>% select(`Tissue %N`, Date) %>% mutate(mon=month(Date), .keep='unused') #%>%
pairwise.t.test(pwc$`Tissue %N`, pwc$mon, p.adjust.method = "bonf")

pairwise.t.test(bayer$`Tissue %N`, bayer$Date, p.adjust.method = "bonf")



bayer2=bayer
bayer2$SH=bayer$`Length (mm)`
bayer2$SH[which(is.na(bayer$`Length (mm)`))]=bayer$`IJ Length (mm)`[which(is.na(bayer$`Length (mm)`))]
plot(bayer2$`dry tissue weight (g)` ~ bayer2$SH, type='p', xlab='SH (mm)', ylab='DW (g)', las=1,
     ylim=c(0,7), xlim=c(30,150), main='CT')
blm=lm(log(bayer2$`dry tissue weight (g)`) ~ log(bayer2$SH))
xval=seq(0,150,by=.5)
yval=exp(blm$coefficients[1])*xval^blm$coefficients[2]
lines(xval, yval)
text(70,6, paste('y=',round(exp(blm$coefficients[1]),9),'x^',round(blm$coefficients[2],3), sep='' ))



## Chesapeake Bay Tissue N
boxplot(poach$Percent_N_content ~ poach$Date_Oysters_Removed, main='CBay', ylab='Tissue %N', xlab='')
boxplot(poach$Percent_N_content ~ poach$Month_Oysters_Removed, main='CBay', ylab='Tissue %N', xlab='')

boxplot(poach$Percent_N_content[poach$Ploidy=='Triploid'] ~ poach$Month_Oysters_Removed[poach$Ploidy=='Triploid'], main='CBay Triploid', ylab='Tissue %N', xlab='')
boxplot(poach$Percent_N_content[poach$Ploidy=='Diploid'] ~ poach$Month_Oysters_Removed[poach$Ploidy=='Diploid'], main='CBay Diploid', ylab='Tissue %N', xlab='')

# By state
# VA=poach %>% filter(State == 'Virginia')
# MD=poach %>% filter(State == 'Maryland')
# boxplot(VA$Percent_N_content[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], main='VA Triploid', ylab='Tissue %N', xlab='Month', ylim=c(5,12), las=1)
# boxplot(VA$Percent_N_content[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], main='VA Diploid', ylab='Tissue %N', xlab='Month', ylim=c(5,12), las=1)
# boxplot(MD$Percent_N_content[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], main='MD Triploid', ylab='Tissue %N', xlab='Month', ylim=c(5,12), las=1)
# boxplot(MD$Percent_N_content[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], main='MD Diploid', ylab='Tissue %N', xlab='Month', ylim=c(5,12), las=1)

# ggplot(VA, aes(x = Month_Oysters_Removed, y = Percent_N_content, fill = Ploidy)) +
#   geom_boxplot()

# ggplot(VA, aes(x = as.factor(Month_Oysters_Removed), y = Percent_N_content, fill = Ploidy)) +
#   geom_boxplot()+ 
#   stat_boxplot(geom = "errorbar") +
#   geom_boxplot(outlier.colour = NA) +
#   xlab("Month") + 
#   ylab("Percent N") + 
#   ylim(5,11) +
#   labs(title = "Rappahannock River") + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()
# 
# ggplot(MD, aes(x = as.factor(Month_Oysters_Removed), y = Percent_N_content, fill = Ploidy)) +
#   geom_boxplot()+ 
#   stat_boxplot(geom = "errorbar") +
#   geom_boxplot(outlier.colour = NA) +
#   xlab("Month") + 
#   ylab("Percent N") + 
#   ylim(5,11) +
#   labs(title = "Chester River") + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()
# 
# ggplot(VA, aes(x = as.factor(Month_Oysters_Removed), y = Percent_P_content, fill = Ploidy)) +
#   geom_boxplot()+ 
#   stat_boxplot(geom = "errorbar") +
#   geom_boxplot(outlier.colour = NA) +
#   xlab("Month") + 
#   ylab("Percent P") + 
#   ylim(0.5,1.5) +
#   labs(title = "Rappahannock River") + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()
# ggplot(MD, aes(x = as.factor(Month_Oysters_Removed), y = Percent_P_content, fill = Ploidy)) +
#   geom_boxplot()+ 
#   stat_boxplot(geom = "errorbar") +
#   geom_boxplot(outlier.colour = NA) +
#   xlab("Month") + 
#   ylab("Percent P") + 
#   ylim(0.5,1.5) +
#   labs(title = "Chester River") + 
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme_bw()
plot(VA$Percent_N_content[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], type='p', main='VA Triploid', ylab='Tissue %N', xlab='', ylim=c(5,11))
plot(VA$Percent_N_content[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], type='p', main='VA Diploid', ylab='Tissue %N', xlab='', ylim=c(5,11))
plot(MD$Percent_N_content[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], type='p', main='MD Triploid', ylab='Tissue %N', xlab='',ylim=c(5,11))
plot(MD$Percent_N_content[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], type='p', main='MD Diploid', ylab='Tissue %N', xlab='',ylim=c(5,11))

# Phosphorous
boxplot(VA$Percent_P_content[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], main='VA Triploid', ylab='Tissue %P', xlab='', ylim=c(0,1.5))
boxplot(VA$Percent_P_content[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], main='VA Diploid', ylab='Tissue %P', xlab='', ylim=c(0,1.5))
boxplot(MD$Percent_P_content[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], main='MD Triploid', ylab='Tissue %P', xlab='', ylim=c(0,1.5))
boxplot(MD$Percent_P_content[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], main='MD Diploid', ylab='Tissue %P', xlab='', ylim=c(0,1.5))

# N:P molar
boxplot(VA$Tissue_NP_Molar[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], main='VA Triploid', ylab='Tissue N:P', xlab='', ylim=c(10,30))
boxplot(VA$Tissue_NP_Molar[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], main='VA Diploid', ylab='Tissue N:P', xlab='', ylim=c(10,30))
boxplot(MD$Tissue_NP_Molar[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], main='MD Triploid', ylab='Tissue N:P', xlab='', ylim=c(10,30))
boxplot(MD$Tissue_NP_Molar[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], main='MD Diploid', ylab='Tissue N:P', xlab='', ylim=c(10,30))

# N:P molar
boxplot(VA$Percent_C_content[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], main='VA Triploid', ylab='Tissue %C', xlab='', ylim=c(30,50))
boxplot(VA$Percent_C_content[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], main='VA Diploid', ylab='Tissue %C', xlab='', ylim=c(30,50))
boxplot(MD$Percent_C_content[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], main='MD Triploid', ylab='Tissue %C', xlab='', ylim=c(30,50))
boxplot(MD$Percent_C_content[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], main='MD Diploid', ylab='Tissue %C', xlab='', ylim=c(30,50))


## MA
MA=reitsma %>% filter(Species=='Cv')
boxplot(MA$`Meat %N`~month(MA$Date))
hist(MA$`Meat %N`)
#Spring
MAs=MA %>% filter(Sampling=='Spring')
MAs %>% count(`Water Body`)
boxplot(MAs$`Meat %N`~(MAs$`Water Body`), ylim=c(5,11), main="MA Spr", ylab='Tissue %N')
MAs.sum=MAs %>% select(`Meat %N`, `Water Body`)%>% group_by(`Water Body`) %>% 
  summarise(cnt=n(), mean=mean(`Meat %N`, na.rm=T),med=median(`Meat %N`, na.rm=T),sd=sd(`Meat %N`, na.rm=T)) 
#Fall
MAf=MA %>% filter(Sampling=='Fall')
MAf %>% count(`Water Body`) 
boxplot(MAf$`Meat %N`~(MAf$`Water Body`), ylim=c(5,11), main="MA Fall", ylab='Tissue %N')
MAf.sum=MAf %>% select(`Meat %N`, `Water Body`)%>% group_by(`Water Body`) %>% 
 summarise(cnt=n(), mean=mean(`Meat %N`, na.rm=T),med=median(`Meat %N`, na.rm=T),sd=sd(`Meat %N`, na.rm=T)) 

# pwc %>% pairwise.t.test(mean ~ mon)
pairwise.t.test(MAf$`Meat %N`, MAf$`Water Body`, p.adjust.method = "bonf")
pairwise.t.test(MAs$`Meat %N`, MAs$`Water Body`, p.adjust.method = "bonf")


## plot all as points
# plot(btest$`Tissue %N`~(btest$Date), ylab='Tissue %N', xlab='', type='p', col='green', pch=19, ylim=c(4,12))
plot(btest$`Tissue %N`~month(btest$Date), ylab='Tissue %N', xlab='', type='p', col='green', pch=19, ylim=c(4,11))
points()
points(MAs$`Meat %N`~month(MAs$Date), col='red')
points(MAf$`Meat %N`~month(MAf$Date), col='red')
points(VA$Percent_N_content[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], col='purple')
points(MD$Percent_N_content[VA$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[VA$Ploidy=='Diploid'], col='yellow')
legend('bottomleft', pch=19, col=c('green', 'red', 'purple', 'yellow'), legend=c('CT', 'MA', 'VA', 'MD'), horiz = T, bty='n')

###
pctN=data.frame(c(btest$`Tissue %N`)); colnames(pctN)='Tissue %N'
pctN$month=month(btest$Date)
pctN$loc='CT'

df=data.frame(MAs$`Meat %N`); colnames(df)='Tissue %N'
df$month=month(MAs$Date)
df$loc='MA'

pctN=rbind(pctN, df)

df=data.frame(MAf$`Meat %N`); colnames(df)='Tissue %N'
df$month=month(MAf$Date)
df$loc='MA'

pctN=rbind(pctN, df)

df=data.frame(VA$Percent_N_content[VA$Ploidy=='Diploid']); colnames(df)='Tissue %N'
df$month=VA$Month_Oysters_Removed[VA$Ploidy=='Diploid']
df$loc='VA'

pctN=rbind(pctN, df)

df=data.frame(MD$Percent_N_content[MD$Ploidy=='Diploid']); colnames(df)='Tissue %N'
df$month=MD$Month_Oysters_Removed[MD$Ploidy=='Diploid']
df$loc='MD'

pctN=rbind(pctN, df)

df=data.frame(higgins$Tissue_N_Percent); colnames(df)='Tissue %N'
df$month=month(higgins$Date_Oysters_Removed)
df$loc='VA'
pctN=rbind(pctN, df)

boxplot(pctN$`Tissue %N`~pctN$month)
library(ggplot2)
ggplot(pctN, aes(x= month, y = `Tissue %N`, color = loc, shape = loc)) +
  xlim(1,12) +
  ylim(5,11)+
  geom_point()

#drop CT
pctN2=pctN %>% filter(loc!='CT')
boxplot(pctN2$`Tissue %N`~pctN2$month)

#drop 6,7,8
pctN2=pctN %>% filter(month %in% c(1,2,3,4,5,9,10,11,12))
boxplot(pctN2$`Tissue %N`~pctN2$month)

###




