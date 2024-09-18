library(tidyverse)
library(FSA)
library(car)
library(lubridate)
library(ggpubr)
library(gplots)
library(maps)
library(mapdata)
library(marmap)
library(readxl)

### Bottom trawl survey strata shapefile
wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
bts=sf::read_sf("C:/Users/ryan.morse/Desktop/shapefiles/BTS/BTS_Strata.shp")

### EFH shapefiles from Mid-Atlantic and New England Councils FMP
# https://www.habitat.noaa.gov/application/efhinventory/
NEWENGefh=sf::read_sf(paste(wd,"/Habitat/EFH/neweng_efh/neweng_efh.shp",sep=''))
MIDATLefh=sf::read_sf(paste(wd,"/Habitat/EFH/midatl_efh/midatl_efh.shp",sep=''))
MIDefh=st_transform(MIDATLefh, "WGS84")
NEWefh=st_transform(NEWENGefh, "WGS84")
map("worldHires", xlim=c(-78,-68),ylim=c(36.5,45), fill=T,border=0,col="gray70")
map.axes(las=1)
plot(MIDefh$geometry[39], col='green', add=T) #BSB Juv
plot(MIDefh$geometry[40], col='blue', add=T) #BSB adult
plot(MIDefh$geometry[3], col='red', add=T) # BSB eggs

# Load Barrett et al 2022 data
Barrett=readxl::read_xlsx(paste(wd, "Habitat/Barrett_2022_suppl3.xlsx", sep=''), sheet = 'fish abundance data')

## rfishbase data
fb.results=rfishbase::popgrowth(species_list = c("Tautogolabrus adspersus", "Centropristis striata", "Tautoga onitis", "Stenotomus chrysops"))

### predict lengths and weights
x.a=0.015372
x.b=2.96
# estimate Fall BSB BTS
x.loo=52.6
x.lk=0.219
x.lt=-0.805
x.Mlinf=0.229372
x.yrs=c(0.5,seq(from=1, to=12,1))
x.len=x.loo*(1-exp(-x.lk*(x.yrs-x.lt))) #length cm
x.wgt=x.a*(x.len^x.b) # weight g
x.Mlorenz=x.Mlinf*((1-exp(-x.lk*(x.yrs-x.lt)))^-1)
L.his=data.frame(x.yrs); colnames(L.his)="Age"
L.his$L_cm=x.len
L.his$W_g=x.wgt
L.his$M_lorenzen=x.Mlorenz
x.mnAbnFarm=6.038795
x.mnAbnRef=0.22
x.enAbn=x.mnAbnFarm-x.mnAbnRef
x.cageArea=1.1148 #m2
L.his$Ni=NA #Ni per m2
L.his$Ni[1]=(x.enAbn/x.cageArea)*exp(-L.his$M_lorenzen[1])
for(i in 2:13){
L.his$Ni[i]=L.his$Ni[i-1]*exp(-L.his$M_lorenzen[i])
}
L.his$B=L.his$Ni*L.his$W_g
# lower CI
x.loo=51.04
x.lk=0.2068
x.lt=-0.837
x.len=x.loo*(1-exp(-x.lk*(x.yrs-x.lt))) #length cm
x.wgt=x.a*(x.len^x.b) # weight g
x.Mlorenz_lower=x.Mlinf*((1-exp(-x.lk*(x.yrs-x.lt)))^-1)
L.his$L_cm_lower=x.len
L.his$W_g_lower=x.wgt
L.his$M_lorenzen_lower=x.Mlorenz_lower
L.his$Ni_lower=NA #Ni per m2
L.his$Ni_lower[1]=(x.enAbn/x.cageArea)*exp(-L.his$M_lorenzen_lower[1])
for(i in 2:13){
  L.his$Ni_lower[i]=L.his$Ni[i-1]*exp(-L.his$M_lorenzen_lower[i])
}
L.his$B_lower=L.his$Ni_lower*L.his$W_g_lower
# upper CI
x.loo=54.46
x.lk=0.2318
x.lt=-0.774
x.len=x.loo*(1-exp(-x.lk*(x.yrs-x.lt))) #length cm
x.wgt=x.a*(x.len^x.b) # weight g
x.Mlorenz_upper=x.Mlinf*((1-exp(-x.lk*(x.yrs-x.lt)))^-1)
L.his$L_cm_upper=x.len
L.his$W_g_upper=x.wgt
L.his$M_lorenzen_upper=x.Mlorenz_upper
L.his$Ni_upper=NA #Ni per m2
L.his$Ni_upper[1]=(x.enAbn/x.cageArea)*exp(-L.his$M_lorenzen_upper[1])
for(i in 2:13){
  L.his$Ni_upper[i]=L.his$Ni[i-1]*exp(-L.his$M_lorenzen_upper[i])
}
L.his$B_upper=L.his$Ni_upper*L.his$W_g_upper
write.csv(L.his, file=paste0(wd,'Habitat/BSB_production_CI_Milford_20240814.csv'))

## load bottom trawl survey data
# load ("C:/Users/ryan.morse/Downloads/NEFSC_BTS_2021.RData")
# load ("C:/Users/ryan.morse/Downloads/SurvdatBio.RData") 2018 is last year
## load survdat 2024 https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw/survdat.rds
survey=readRDS("C:/Users/ryan.morse/Downloads/survdat.rds")
survdat <- survey$survdat
## load survdat.bio from Andy 2024
survey.bio=readRDS("C:/Users/ryan.morse/Downloads/SurvdatBio.rds")
survdat.bio <- survey.bio$survdat


### Species List ###
## black sea bass - 141
## scup - 143
## cunner - 176
## tautog - 177

### Load energy density data
# dens=readxl::read_xlsx(paste(wd,"/Habitat/FINALBSBDATA.xlsx", sep=''), sheet='BSB Data - original') # energy density
dens=readxl::read_xlsx(paste(wd,"/Habitat/BSB_Dry_Weights_Final.xlsx", sep=''), sheet='Energy Density BSB') # energy density
colnames(dens)
colnames(dens)[2]="Farm_Reef"
colnames(dens)[5]="NOAA_WW_g"
colnames(dens)[6]="Total_Length_mm" 
colnames(dens)[20]="Energy_Density_kJ_g"
dens$Total_Length_cm=dens$Total_Length_mm/10
dens$logL=log(dens$Total_Length_cm) # now in cm
dens$logW=log(dens$NOAA_WW_g/1000) # now in kg
dens.wgt=readxl::read_xlsx(paste(wd,"/Habitat/BSB_Dry_Weights_Final.xlsx", sep=''), sheet='BSB Data') # energy density
colnames(dens.wgt)
colnames(dens.wgt)[2]="TL_mm"
colnames(dens.wgt)[3]="SL_mm"
colnames(dens.wgt)[4]="Wet_wgt_g"
colnames(dens.wgt)[19]="Dry_wgt_g"
test=substr(dens.wgt$`Unique ID`,1,1)
dens.wgt$Site=test
dens.wgt$Site[which(dens.wgt$Site=="F")]="M" # F is for cages from Milford, vs traps and cages
dens.wgt$Farm_Reef=NA
test=substr(dens.wgt$`Unique ID`,3,3)
t1=as.numeric(test)
dens.wgt$Farm_Reef[which(is.na(t1))]=test[which(is.na(t1))]
test1=substr(dens.wgt$`Unique ID`,4,4)
t2=as.numeric(test1)
dens.wgt$Farm_Reef[which(is.na(t2))]=test1[which(is.na(t2))]
table(dens.wgt$Farm_Reef)
dens.wgt$TL_cm=dens.wgt$TL_mm/10
dens.wgt$logL=log(dens.wgt$TL_cm) # now in cm
dens.wgt$logW=log(dens.wgt$Wet_wgt_g/1000) # now in kg
dens.wgt$percent_DW=(dens.wgt$Dry_wgt_g/dens.wgt$Wet_wgt_g)*100
# Calculated weights from W-L fall BTS all data
dens.wgt$Wcalc=0.015372*(dens.wgt$TL_cm^2.96) # Calculated weights from W-L fall BTS all data
dens.wgt$Kn=dens.wgt$Wet_wgt_g/dens.wgt$Wcalc
# clean up and add dens (measured values)
ED=dens.wgt %>% left_join(dens, join_by(`Unique ID`==`Fish ID`))
# Calculate total energy for those with dry weights
dens.wgt$TotalEnergy=NA
dens.wgt$TotalEnergy[which(!(is.na(dens.wgt$Dry_wgt_g)))]=(20.265*dens.wgt$Dry_wgt_g[which(!(is.na(dens.wgt$Dry_wgt_g)))])-0.4825 # y=20.265x-0.4825 R2=0.998
dens.wgt$Energy_Density_kJ_g=dens.wgt$TotalEnergy/dens.wgt$Wet_wgt_g



## Estimated and measured ED 
plot(dens.wgt$Energy_Density_kJ_g~dens.wgt$percent_DW)
lm1=lm(dens.wgt$Energy_Density_kJ_g~dens.wgt$percent_DW, data=dens.wgt)
abline(lm1, col='black', lw=1)
points(dens$`Energy Density (kJ/g)`, dens$`% Dry Weight`, pch=1, col='red')

## Measured ED only -> Y=26.46x-1.513
plot(dens$Energy_Density_kJ_g~dens$`% Dry Weight`, ylab="Energy density (kJ/g)", xlab="Percent dry weight")
lm1=lm(dens$Energy_Density_kJ_g~dens$`% Dry Weight`)
abline(lm1, col='black', lw=1)
text(0.22, 6, labels=paste0("Y = ", round(lm1$coefficients[2],2), "x", round(lm1$coefficients[1],3)))
text(0.22, 5.65, labels=paste0("R.sq=",round(summary(lm1)$r.squared,4)))
## Measured Total Energy
plot(dens$`Total Energy`~dens$`Final Dry Weight (g)`, ylab='Total Energy', xlab='Final dry weight (g)')
lm1=lm(dens$`Total Energy`~dens$`Final Dry Weight (g)`)
abline(lm1, col='black', lw=1)
text(5, 300, labels=paste0("Y = ", round(lm1$coefficients[2],2), "x", round(lm1$coefficients[1],3)))
text(5, 250, labels=paste0("R.sq=",round(summary(lm1)$r.squared,4)))

## clean up, remove bad values, excludes, outliers
# dens.wgt2=dens.wgt %>% filter(Dry_wgt_g >0)
dens.wgt=dens.wgt %>% filter(!(`Unique ID` %in% c('M9R17', 'C8F66', 'M8F74', 'C9F19')))
dens=dens%>% filter(!(`Fish ID` %in% c('M9R17','C9F19', 'C9R14', 'M10F127', 'M10F135', 'M9F126', 'M9F127', 'M9R22')))

ED=ED %>% filter(!(`Unique ID` %in% c('M9R17', 'C8F66', 'M8F74', 'C9F19','C9R14', 'M10F127', 'M10F135', 'M9F126', 'M9F127', 'M9R22')))

## checking
t1=NA
t1=dens.wgt$Dry_wgt_g[dens.wgt$TL_mm>57]/dens.wgt$Wet_wgt_g[dens.wgt$TL_mm>57]
test=(25.85*(t1))-1.37
plot(dens.wgt$Energy_Density_kJ_g[dens.wgt$TL_mm>57]~test[dens.wgt$TL_mm>57], ylim=c(3.8, 5.8), xlim=c(3.8, 5.8), col=ifelse(dens.wgt$`Shipped to SMAST`==1, 'red', 'black'))

# Load GoPro data
gpd=read.csv(paste(wd,"/Habitat/2018-Bsb-MaxN-LHS.csv", sep=''))
gpd$DateTime=as_date(gpd$DateTime, format = "%m/%d/%Y")
gpd$month=month(gpd$DateTime)
gpd2=gpd %>% filter(MaxN>0)
boxplot(gpd2$MaxN ~ gpd2$Treatment)
boxplot(gpd$MaxN ~ gpd$Treatment)

# read all sheets and concatenate
gp2018sheets=excel_sheets(paste0(wd,"Habitat/bsb 2021/2018 farm raw data compiled.xlsx"))
gp2018all=lapply(gp2018sheets, function(x) read_excel(paste0(wd,"/Habitat/bsb 2021/2018 farm raw data compiled.xlsx"), sheet=x))
# str(gp2018all)
gpd2018=plyr::rbind.fill(gp2018all)
gpdbsb2018=gpd2018 %>% dplyr::filter(Behavior=="Black sea bass")
max(gpdbsb2018$Modifier_1)
# unique(gpdbsb2018$Observation)
# nms=strsplit(gpdbsb2018$Observation, split="_", fixed=F)
# nms2=lapply(nms, `[[`, 1, drop = FALSE)
# dts=lapply(nms, `[[`, 2, drop = FALSE)
# unique(dts)
# gpdbsb2018$Month=NA
# gpdbsb2018$Month[nms2=="GP"]=

gpd.sum=gpdbsb2018 %>% 
  group_by(Month) %>% 
  summarise(mn=mean(Modifier_1,na.rm=T), md=median(Modifier_1, na.rm=T), mx=max(Modifier_1, na.rm=T))



with(dens, table(Farm_Reef, SITE))

dens[dens$Total_Length_mm>57,] %>% select(Farm_Reef, SITE, Energy_Density_kJ_g) %>%
  # ggplot(aes(y=Energy_Density_kJ_g, x=Farm_Reef)) +
  # geom_boxplot(color = "black", notch=T, fill="gray") +
  ggplot(aes(y=Energy_Density_kJ_g, x=SITE,fill=Farm_Reef)) +
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Energy Density (kJ/g)') +
  coord_cartesian(ylim = c(4, 6))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))

dens.wgt[dens.wgt$TL_mm>57,] %>% select(Farm_Reef, Site, Energy_Density_kJ_g) %>%
  ggplot(aes(y=Energy_Density_kJ_g, x=Site, fill=Farm_Reef)) +
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Energy density (kJ/g)') +
  coord_cartesian(ylim = c(4, 6))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))

dens.wgt[dens.wgt$TL_mm>57,] %>% select(Farm_Reef, Site, Kn) %>%
  ggplot(aes(y=Kn, x=Site,fill=Farm_Reef)) +
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Condition factor Kn') +
  coord_cartesian(ylim = c(0.5, 1.5))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))
dens.wgt[dens.wgt$TL_mm>57,] %>% select(Farm_Reef, Kn) %>%
  ggplot(aes(y=Kn, x=Farm_Reef)) +
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Condition factor Kn') +
  coord_cartesian(ylim = c(0.5, 1.5))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))
dens.wgt[dens.wgt$TL_mm>57,] %>% select(Site, Kn) %>%
  ggplot(aes(y=Kn, x=Site)) +
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Condition factor Kn') +
  coord_cartesian(ylim = c(0.5, 1.5))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20))
# dens.wgt[dens.wgt$TL_mm>57,] %>% select(Farm_Reef, Site, Energy_Density_kJ_g) %>%
#   ggplot(aes(y=Energy_Density_kJ_g, x=Site,fill=Farm_Reef)) +
#   geom_boxplot(color = "black", notch=T) +
#   theme_classic()+
#   labs(x='', y = 'Energy Density (kJ/g)') +
#   coord_cartesian(ylim = c(4, 6))+
#   stat_summary(fun.y=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
#   theme(strip.background = element_blank(),
#         strip.text.y = element_blank())+
#   theme(legend.position = "top") +
#   theme(text = element_text(size = 20)) +
#   theme(legend.text = element_text(size = 20))

my_comparisons=list( c("F", "R"))
dens %>% select(Farm_Reef, SITE, Energy_Density_kJ_g) %>% 
  filter(complete.cases(Energy_Density_kJ_g)) %>%
  ggboxplot(y='Energy_Density_kJ_g', x='SITE', fill='Farm_Reef', ylab = 'Energy Density (kJ/g)', xlab='', ylim=c(4,6)) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "anova", label.y = c(5.5),label= "p.signif") #+
# my_comparisons=list( c("F", "R"))
dens.wgt %>% select(Farm_Reef, Energy_Density_kJ_g) %>%
  filter(complete.cases(Energy_Density_kJ_g))%>%
  ggboxplot(y='Energy_Density_kJ_g', x='Farm_Reef', ylab = 'Energy Density (kJ/g)', xlab='', ylim=c(4,6)) +
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label.y = c(5.5),label= "p.signif") #+
my_comparisons=list( c("C", "M"))
dens.wgt %>% select(Site, Energy_Density_kJ_g) %>%
  filter(complete.cases(Energy_Density_kJ_g))%>%
  ggboxplot(y='Energy_Density_kJ_g', x='Site', ylab = 'Energy Density (kJ/g)', xlab='', ylim=c(4,6)) +
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label.y = c(5.5),label= "p.signif") #+

## check normality
shapiro.test(subset(dens.wgt, Site == "C")$Energy_Density_kJ_g)
shapiro.test(subset(dens.wgt, Site == "M")$Energy_Density_kJ_g)
shapiro.test(subset(dens.wgt, Site == "C")$Kn)
shapiro.test(subset(dens.wgt, Site == "M")$Kn)
hist(subset(dens.wgt, Site == "C")$Energy_Density_kJ_g,
     main = "Clinton",
     xlab = "Energy Dens"
)
hist(subset(dens.wgt, Site == "M")$Energy_Density_kJ_g,
     main = "Milford",
     xlab = "Energy Dens"
)
hist(subset(dens.wgt, Site == "C")$Kn,
     main = "Clinton",
     xlab = "Kn"
)
hist(subset(dens.wgt, Site == "M")$Kn,
     main = "Milford",
     xlab = "Kn"
)

shapiro.test(subset(dens.wgt, Farm_Reef == "F")$Energy_Density_kJ_g)
shapiro.test(subset(dens.wgt, Farm_Reef == "R")$Energy_Density_kJ_g)
shapiro.test(subset(dens.wgt, Farm_Reef == "F")$Kn)
shapiro.test(subset(dens.wgt, Farm_Reef == "R")$Kn)
hist(subset(dens.wgt, Farm_Reef == "F")$Energy_Density_kJ_g,
     main = "Farm",
     xlab = "Energy Dens"
)
hist(subset(dens.wgt, Farm_Reef == "R")$Energy_Density_kJ_g,
     main = "Reef",
     xlab = "Energy Dens"
)
hist(subset(dens.wgt, Farm_Reef == "F")$Kn,
     main = "Farm",
     xlab = "Kn"
)
hist(subset(dens.wgt, Farm_Reef == "R")$Kn,
     main = "Reef",
     xlab = "Kn"
)

# use non-parametric tests -> Wilcoxon rank sum test
compare_means(Energy_Density_kJ_g ~ Site,  data = dens.wgt)
compare_means(Energy_Density_kJ_g ~ Farm_Reef,  data = dens.wgt)
compare_means(Kn ~ Site,  data = dens.wgt)
compare_means(Kn ~ Farm_Reef,  data = dens.wgt)


df1=dens.wgt %>%
  select(Farm_Reef, Site, Energy_Density_kJ_g) %>%
  group_by(Farm_Reef, Site) %>%
  summarize(mean=mean(Energy_Density_kJ_g, na.rm=T), sd=sd(Energy_Density_kJ_g, na.rm=T), med=median(Energy_Density_kJ_g, na.rm=T), num=n()) #%>%
  # group_map(~ t.test(Energy_Density_kJ_g ~ Site, .x, paired = TRUE))
df2=dens.wgt %>%
  select(Farm_Reef, Site, Energy_Density_kJ_g) %>%
  group_by(Farm_Reef) %>%
  summarize(mean=mean(Energy_Density_kJ_g, na.rm=T), sd=sd(Energy_Density_kJ_g, na.rm=T), med=median(Energy_Density_kJ_g, na.rm=T), num=n()) #%>%
df3=dens.wgt %>%
  select(Farm_Reef, Site, Energy_Density_kJ_g) %>%
  group_by(Site) %>%
  summarize(mean=mean(Energy_Density_kJ_g, na.rm=T), sd=sd(Energy_Density_kJ_g, na.rm=T), med=median(Energy_Density_kJ_g, na.rm=T), num=n()) #%>%
df4=rbind(df1, df2)
df4=rbind(df4,df3)
write.csv(df4, file=paste0(wd,'BSB_energy_density_summary.csv'))

kruskal.test(Energy_Density_kJ_g ~ Farm_Reef, data =dens.wgt)
kruskal.test(Energy_Density_kJ_g ~ Site, data =dens.wgt)


df1=dens.wgt %>%
  select(Farm_Reef, Site, Kn) %>%
  group_by(Farm_Reef, Site) %>%
  summarize(mean=mean(Kn, na.rm=T), sd=sd(Kn, na.rm=T), med=median(Kn, na.rm=T), num=n()) #%>%
# group_map(~ t.test(Kn ~ Site, .x, paired = TRUE))
df2=dens.wgt %>%
  select(Farm_Reef, Site, Kn) %>%
  group_by(Farm_Reef) %>%
  summarize(mean=mean(Kn, na.rm=T), sd=sd(Kn, na.rm=T), med=median(Kn, na.rm=T), num=n()) #%>%
df3=dens.wgt %>%
  select(Farm_Reef, Site, Kn) %>%
  group_by(Site) %>%
  summarize(mean=mean(Kn, na.rm=T), sd=sd(Kn, na.rm=T), med=median(Kn, na.rm=T), num=n()) #%>%
df4=rbind(df1, df2)
df4=rbind(df4,df3)
write.csv(df4, file=paste0(wd,'BSB_Kn_summary.csv'))

kruskal.test(Kn ~ Farm_Reef, data =dens.wgt[complete.cases(dens.wgt$Kn),])
# Kruskal-Wallis rank sum test
# data:  Kn by Farm_Reef
# Kruskal-Wallis chi-squared = 1.6008, df = 1, p-value = 0.2058
kruskal.test(Kn ~ Site, data =dens.wgt[complete.cases(dens.wgt$Kn),])
# Kruskal-Wallis rank sum test
# data:  Kn by Site
# Kruskal-Wallis chi-squared = 2.7391, df = 1, p-value = 0.09792

plot(dens$Total_Length_mm/10~dens$NOAA_WW_g, type='p', pch=19)
plot(dens.wgt$Wet_wgt_g ~ dens.wgt$TL_cm, 
     type='p', 
     pch=19, 
     col=ifelse(dens$Farm_Reef=='F', 'red', 'blue'), 
     ylab='Wet weight (g)', 
     xlab='Length (cm)'
     )
legend('topleft', legend=c('Farm', "Reef"), pch=19, col=c('red', 'blue'), bty='n')

plot(log(dens$NOAA_WW_g)~log(dens$Total_Length_mm), 
     type='p', 
     pch=19, 
     col=ifelse(dens$Farm_Reef=='F', 'red', 'blue'),
     ylab='log wet weight (g)', 
     xlab='log length (cm)'
     )
legend('topleft', legend=c('Farm', "Reef"), pch=19, col=c('red', 'blue'), bty='n')
lm1=lm(log(dens$NOAA_WW_g)~log(dens$Total_Length_mm), data=dens[dens$Farm_Reef=="F",])
abline(lm1, col='red', lw=2)
lm1=lm(log(dens$NOAA_WW_g)~log(dens$Total_Length_mm), data=dens[dens$Farm_Reef=="R",])
abline(lm1, col='blue', lw=2)

plot(log(dens.wgt$Wet_wgt_g)~log(dens.wgt$TL_mm/10), 
     type='p', 
     #pch=19, 
     col=ifelse(dens$Farm_Reef=='F', 'red', 'blue'),
     ylab='log wet weight (g)', 
     xlab='log length (cm)',
     las=1
)
x=log(dens.wgt$Wet_wgt_g[dens.wgt$Farm_Reef=="F"])
y=log(dens.wgt$TL_mm[dens.wgt$Farm_Reef=="F"]/10)
lm1=lm(x~y)
text(2.2, 3.5, labels=paste("a= ", round(exp(lm1$coefficients[1]),7), sep=''))
text(2.2, 3, labels=paste("b= ", round(lm1$coefficients[2],3), sep=''))
abline(lm1, col='red', lw=2)
x=log(dens.wgt$Wet_wgt_g[dens.wgt$Farm_Reef=="R"])
y=log(dens.wgt$TL_mm[dens.wgt$Farm_Reef=="R"]/10)
lm1=lm(x~y)
abline(lm1, col='blue', lw=2)
text(2.4, 2, labels=paste("a= ", round(exp(lm1$coefficients[1]),7), sep=''))
text(2.4, 1.5, labels=paste("b= ", round(lm1$coefficients[2],3), sep=''))
legend('topleft', legend=c('Farm', "Reef"), lty=1,lwd=2, col=c('red', 'blue'), bty='n')

points(x~y, col='red', pch=19)
plot(dens$logW~dens$logL, 
     type='p', 
     col=ifelse(dens$Farm_Reef=='F', 'red', 'blue'),
     ylab='log wet weight (kg)', 
     xlab='log length (cm)'
     )
lm1=lm(logW~logL, data=dens[dens$Farm_Reef=="F",])
abline(lm1, col='red', lw=2)
text(2.2, -3, labels=paste("a= ", round(exp(lm1$coefficients[1]),7), sep=''))
text(2.2, -3.5, labels=paste("b= ", round(lm1$coefficients[2],3), sep=''))
lm1=lm(logW~logL, data=dens[dens$Farm_Reef=="R",])
abline(lm1, col='blue', lw=2)
text(2.5, -5, labels=paste("a= ", round(exp(lm1$coefficients[1]),7), sep=''))
text(2.5, -5.5, labels=paste("b= ", round(lm1$coefficients[2],3), sep=''))
legend('topleft', legend=c('Farm', "Reef"), lty=1,lwd=2, col=c('red', 'blue'), bty='n')


plot(dens.wgt$logW~dens.wgt$logL, 
     type='p', 
     col=ifelse(dens.wgt$Farm_Reef=='F', 'red', 'blue'),
     ylab='log wet weight (kg)', 
     xlab='log length (cm)'
)
lm1=lm(logW~logL, data=dens.wgt[dens.wgt$Farm_Reef=="F",])
abline(lm1, col='red', lw=2)
text(2.2, -3, labels=paste("a= ", round(exp(lm1$coefficients[1]),7), sep=''))
text(2.2, -3.5, labels=paste("b= ", round(lm1$coefficients[2],3), sep=''))
lm1=lm(logW~logL, data=dens.wgt[dens.wgt$Farm_Reef=="R",])
abline(lm1, col='blue', lw=2)
text(2.5, -5, labels=paste("a= ", round(exp(lm1$coefficients[1]),7), sep=''))
text(2.5, -5.5, labels=paste("b= ", round(lm1$coefficients[2],3), sep=''))
legend('topleft', legend=c('Farm', "Reef"), lty=1,lwd=2, col=c('red', 'blue'), bty='n')

## summary stats
gpd %>% 
  select(month, Treatment, MaxN:PercentYOY) %>% 
  group_by(month, Treatment) %>% 
  summarise(as_tibble(rbind(summary(MaxN))))

# plot (not working as it should...)
# gpd %>% 
#   select(month, Treatment, MaxN:PercentYOY) %>% 
#   filter(month>5) %>%
#   group_by(month, Treatment) %>% 
#   ggplot(aes(x=month, y=MaxN, fill = Treatment)) +
#   geom_boxplot() +
#   theme_classic()

# gpd %>% 
#   ggplot(aes(x=month, y=MaxN, color=Treatment)) +
#   geom_boxplot()


tbl=with(gpd, table(MaxN, Treatment, month))
ggplot(as.data.frame(tbl), aes(y=factor(MaxN), x=month, fill = Treatment)) +     
  geom_col(position = 'dodge')

# Line plot by month
gpd2=gpd %>% 
  select(month, Treatment, MaxN:PercentYOY) %>% 
  filter(month>5) %>%
  group_by(month, Treatment) %>% 
  summarize(mean_N=mean(MaxN), sd_N=sd(MaxN), med_N=median(MaxN))
ggplot(gpd2, aes(x=month, y=mean_N, color = Treatment)) +
  geom_point(position=position_dodge(width =0.3)) +
  geom_errorbar(aes(ymin=mean_N-sd_N, ymax=mean_N+sd_N), width=.2,
                position=position_dodge(width =0.3))+
  theme_classic()
ggplot(gpd2, aes(x=month, y=med_N, color = Treatment)) +
  geom_point(position=position_dodge(width =0.3)) +
  geom_errorbar(aes(ymin=med_N-sd_N, ymax=med_N+sd_N), width=.2,
                position=position_dodge(width =0.3))+
  theme_classic()

# line plot with data for each date
gpd2=gpd %>% 
  select(DateTime, Treatment, MaxN:PercentYOY) %>% 
    group_by(DateTime, Treatment) %>% 
  summarize(mean_N=mean(MaxN), sd_N=sd(MaxN), med_N=median(MaxN))
ggplot(gpd2, aes(x=DateTime, y=mean_N, color = Treatment)) +
  labs(x='Date', y = 'mean (MaxN)') +
  geom_point(aes(pch=Treatment),position=position_dodge(width =0.3)) +
  geom_line(aes(color=Treatment)) +
  geom_errorbar(aes(ymin=mean_N-sd_N, ymax=mean_N+sd_N), width=.2,
                position=position_dodge(width =0.3))+
  theme_classic()

## take maximum of month and treatment combination and average of life stage composition
gpd2=gpd %>%
  select(month, Treatment, MaxN:PercentYOY) %>% 
  group_by(month, Treatment) %>% 
  summarise(N=max(MaxN),YOY=mean(PercentYOY), Juv=mean(PercentJuvenile), Adt=mean(PercentAdult))
gpd2 %>% 
  ggplot(aes(x=as.character(month), y=YOY, fill = Treatment)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  labs(x='Month', y = 'Percent YOY') +
  theme_classic()
gpd2 %>% 
  ggplot(aes(x=as.character(month), y=Juv, fill = Treatment)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  labs(x='Month', y = 'Percent Juvenile') +
  theme_classic()
gpd2 %>% 
  ggplot(aes(x=as.character(month), y=Adt, fill = Treatment)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  labs(x='Month', y = 'Percent Adult') +
  theme_classic()
gpd2 %>% 
  ggplot(aes(x=as.character(month), y=N, fill = Treatment)) +
  geom_bar(stat="identity",  position=position_dodge()) +
  labs(x='Month', y = 'Maximum # (MaxN)') +
  theme_classic()

  #filter to black sea bass #141
bsb.sp=survdat.bio %>% dplyr::filter(SVSPP==141, SEASON=="SPRING")
bsb.fl=survdat.bio %>% dplyr::filter(SVSPP==141, SEASON=="FALL")
plot(LENGTH~AGE,data=bsb.sp, main="BSB Spring", ylim=c(0,80), xlim=c(0,12))
plot(LENGTH~AGE,data=bsb.fl, main="BSB Fall", ylim=c(0,80), xlim=c(0,12))
boxplot(LENGTH~AGE,data=bsb.sp, main="BSB Spring", ylim=c(0,60), xlim=c(0,12))
boxplot(LENGTH~AGE,data=bsb.fl, main="BSB Fall", ylim=c(0,60), xlim=c(0,12))

## drop missing data
bsb.both=survdat.bio %>% dplyr::filter(SVSPP==141) %>% dplyr::filter(complete.cases(AGE))
bsb.sp2=bsb.sp[complete.cases(bsb.sp$AGE),]
bsb.fl2=bsb.fl[complete.cases(bsb.fl$AGE),]

#Spring
test.s=bsb.sp %>% count(YEAR, STRATUM)
t2.s=test.s[test.s$n>=50,]
barplot(table(t2.s$STRATUM), main="Spring Strata >=50")
#Fall
test.f=bsb.fl %>% count(YEAR, STRATUM)
t2.f=test.f[test.f$n>=50,]
barplot(table(t2.f$STRATUM),main="Fall Strata >=50")
# plot weight-at-age and weght~length relationship for spring
plot(bsb.sp2$INDWT~bsb.sp2$AGE, ylab='Wet weight (kg)', xlab='Age', main='BSB Spring', col=ifelse(bsb.sp2$SEX==2, 'red', 'black'))
plot(bsb.sp2$INDWT~bsb.sp2$LENGTH, ylab='Wet weight (kg)', xlab='Length (cm)', main='BSB Spring',col=ifelse(bsb.sp2$SEX==2, 'red', 'black'))

plot((bsb.sp2$INDWT*2.205)~bsb.sp2$LENGTH, ylab='Wet weight (lbs)', xlab='Length (cm)', main='BSB Spring',col='black')
plot((bsb.fl2$INDWT*2.205)~bsb.fl2$LENGTH, ylab='Wet weight (lbs)', xlab='Length (cm)', main='BSB Fall',col='black')


# weight-Length relationship (both sexes)
bsb.sp3=bsb.sp2 %>% filter(INDWT>0)
bsb.sp3$logW=log(bsb.sp3$INDWT)
bsb.sp3$logL=log(bsb.sp3$LENGTH)
sp2=bsb.sp3[complete.cases(bsb.sp3$logW),]
sp3=sp2[complete.cases(sp2$logL),]
lm1=lm(logW~logL, data=sp3)
plot(sp3$logW~sp3$logL, type='p', pch=21, xlab="log length", ylab="log weight")
abline(lm1, col='red', lw=2)
summary(lm1)
# text(2, -7, labels=paste("a= ", exp(lm1$coefficients[1]), sep=''))
# text(2, -8, labels=paste("b= ", lm1$coefficients[2], sep=''))
text(2, 0, labels=paste("a= ", exp(lm1$coefficients[1]), sep=''))
text(2, -1, labels=paste("b= ", lm1$coefficients[2], sep=''))



bsb.fl3=bsb.fl2 %>% filter(INDWT>0)#,STRATUM==1050)
bsb.fl3$logW=log(bsb.fl3$INDWT*1000) #grams
bsb.fl3$logL=log(bsb.fl3$LENGTH) #cm
sp2=bsb.fl3[complete.cases(bsb.fl3$logW),]
sp3=sp2[complete.cases(sp2$logL),]
lm1=lm(logW~logL, data=sp3)
plot(sp3$logW~sp3$logL, type='p', pch=21, xlab="Log length (cm)", ylab="Log weight (g)", las=1)#, main="Fall Black Sea Bass")
abline(lm1, col='red', lw=2)
summary(lm1)
# text(2, -7, labels=paste("a= ", exp(lm1$coefficients[1]), sep=''))
# text(2, -8, labels=paste("b= ", lm1$coefficients[2], sep=''))
# text(2, 0, labels=paste("a= ", round(exp(lm1$coefficients[1]),6), sep=''),pos = 2)
# text(2, -1, labels=paste("b= ", round(lm1$coefficients[2],3), sep=''),pos = 2)
legend('topleft', legend = c("Fall bottom trawl survey",
                             "Black sea bass",
                             "Length-weight:",
                             paste("a= ", round(exp(lm1$coefficients[1]),6), sep=''),
                             paste("b= ", round(lm1$coefficients[2],3), sep='')),
                             bty = 'n')
## spatial view of high abundance strata
bts.s=bts %>% filter(STRATA %in% t2.s$STRATUM)
bts.f=bts %>% filter(STRATA %in% t2.f$STRATUM)
plot(bts[1:152,2], axes=T, reset = FALSE, main="Spring Strata")
plot(bts.s[2], add=T, col='red')
plot(bts.s[[8]][[1]], add=T, col='green')
plot(bts[1:152,2], axes=T, reset = FALSE, main="Fall Strata")
plot(bts.f[1:2,2], add=T, col='red')

test=bsb %>% count(YEAR, SEASON)

# age comp in one stratum 1050 nearest RI and CT and sampled by Bigelow
acsp=bsb.fl %>% filter(STRATUM==1050) %>% group_by(AGE) %>%
  summarise(mAge=mean(ABUNDANCE), sAge=sum(ABUNDANCE))
acsp$pct=(acsp$sAge/sum(acsp$sAge))*100
acsp=acsp[complete.cases(acsp$AGE),]
barplot(acsp$sAge~acsp$AGE, ylab = 'sum abundance')
barplot(acsp$mAge~acsp$AGE, ylab='mean abundance')
# acsp$pct=acsp$sAge/sum(acsp$sAge)
barplot(acsp$pct~acsp$AGE, ylab = 'percent abundance by age', xlab='Age')

## single year -- last year with both seasons
plot(LENGTH~AGE,data=bsb.sp[which(bsb.sp$YEAR==2016),], main="BSB Spring 2016", ylim=c(0,60), xlim=c(0,12))
plot(LENGTH~AGE,data=bsb.fl[which(bsb.fl$YEAR==2015),], main="BSB Fall 2015", ylim=c(0,60), xlim=c(0,12))
boxplot(LENGTH~AGE,data=bsb.sp[which(bsb.sp$YEAR==2016),], main="BSB Spring 2016", ylim=c(0,60), xlim=c(0,12))
boxplot(LENGTH~AGE,data=bsb.fl[which(bsb.fl$YEAR==2015),], main="BSB Fall 2015", ylim=c(0,60), xlim=c(0,12))


boxplot(LENGTH~AGE,data=bsb.sp, main="BSB Spring", ylim=c(0,70), xlim=c(0,10), las=1)
boxplot(LENGTH~AGE,data=bsb.fl, ylim=c(0,70), xlim=c(0,10), las=1, ylab = 'Length (cm)', xlab='Age')
legend('topleft', legend = c("Fall bottom trawl survey",
                             "Black sea bass",
                             "Length-at-age"
                             ),
       bty = 'n') 
bsb.fl %>% 
  select(AGE, LENGTH, STRATUM) %>%
  filter(STRATUM==3020 | STRATUM==1050) %>%
  filter(!is.na(AGE)) %>%
  select(AGE, LENGTH) %>% 
  ggboxplot(y='LENGTH', x='AGE', ylim=c(0,70))+
  # ggplot(aes(y=LENGTH, x=AGE)) +
  # geom_boxplot(color = "black", notch=T) +
  # theme_classic()+
  labs(x='Age (yrs)', y = 'Length (cm)') +
  # coord_cartesian(ylim = c(0, 70))+
  # theme(strip.background = element_blank(),
  #       strip.text.y = element_blank())+
  theme(text = element_text(size = 20)) 

  # plot size of age-0 and age-1
boxplot(LENGTH~AGE,data=bsb.fl[which(bsb.fl$AGE==0),], main="BSB Fall age-0", ylim=c(0,20))
boxplot(LENGTH~AGE,data=bsb.sp[which(bsb.sp$AGE==1),], main="BSB Spr age-1", ylim=c(0,20), las=1)

## filter to strata:
# 3020 - most inshore, landward of 1050, small
# 1050 - large, seaward of 3020 (spans RI-CT)
bsb.sp[which(bsb.sp$AGE==1),] %>% 
  # filter(STRATUM==3020 | STRATUM==1050) %>%
  select(YEAR, LENGTH, STRATUM) %>% 
  group_by(YEAR) %>% 
  ggboxplot(y='LENGTH', x='YEAR', ylim=c(0,20))

bsb.fl[which(bsb.fl$AGE==0),] %>% 
  filter(STRATUM==3020 | STRATUM==1050) %>%
  select(YEAR, LENGTH, STRATUM) %>% 
  group_by(YEAR) %>% 
  ggboxplot(y='LENGTH', x='YEAR', ylim=c(0,20))
bsb.fl.sum=bsb.fl[which(bsb.fl$AGE==0),] %>% 
  filter(STRATUM==3020 | STRATUM==1050) %>%
  group_by(YEAR) %>%
  summarize(
    mn=min(LENGTH, na.rm = T),
    mx=max(LENGTH, na.rm=T),
    av=mean(LENGTH, na.rm=T),
    md=median(LENGTH, na.rm=T),
    n=n()
    
  )
plot(bsb.fl.sum$av~bsb.fl.sum$YEAR, type='b', ylim=c(0,20), ylab='BSB age-0 size (cm)', xlab='', main='Strata 1050,3020')
lines(bsb.fl.sum$mx~bsb.fl.sum$YEAR, lty=2)
lines(bsb.fl.sum$mn~bsb.fl.sum$YEAR, lty=2)
lines(bsb.fl.sum$md~bsb.fl.sum$YEAR, lty=1)
lines(bsb.fl.sum$av~bsb.fl.sum$YEAR, lty=1, col='red')
points(bsb.fl$LENGTH[which(bsb.fl$AGE==0 & bsb.fl$STRATUM %in% c(1050, 3020))]~bsb.fl$YEAR[which(bsb.fl$AGE==0 & bsb.fl$STRATUM%in% c(1050, 3020))])  
points(bsb.fl$LENGTH[which(bsb.fl$AGE==0)]~bsb.fl$YEAR[which(bsb.fl$AGE==0)])  
legend('topleft', lty=c(1,1), col=c('red','black'), legend=c('mean', 'median'), bty='n')

with(bsb.fl[which(bsb.fl$AGE==0),], table(STRATUM, YEAR))
with(bsb.fl[which(bsb.fl$AGE==0),], table(STRATUM, YEAR))
with(bsb.fl[which(bsb.fl$AGE==0),], heatmap(table(STRATUM, YEAR)))
with(bsb.fl[which(bsb.fl$AGE==0),], heatmap.2(table(STRATUM, YEAR), Rowv = NA, Colv=NA,col=bluered, scale="none", tracecol="#303030" ,dendrogram="none", key=T))
colorbar(terrain.colors(256))

### Description of the sex code field. Only 0, 1, 2 are valid entries for fscs tables.
# 0=Unsexed, unknown, or sex not observed; Since the summer 1995 Gulf of Maine Trawl Survey forgot to look for American Lobster;
# 1=Male;
# 2=Female; Female Stage I for Northern Shrimp; Since the summer 1995 Gulf of Maine Trawl Survey Female (no eggs, no notch) for American Lobster;
# 3=Female Stage II for Northern Shrimp; Since the summer 1995 Gulf of Maine Trawl Survey Female, with eggs, no notch for American Lobster;
# 4=Transitional for Northern Shrimp; Since the summer 1995 Gulf of Maine Trawl Survey Female, with notch, no eggs for American Lobster;
# 5=Ovigerous for Northern Shrimp; Since the summer 1995 Gulf of Maine Trawl Survey Female, with notch and eggs for American Lobster;
# 6=Non-spawning Female for Northern Shrimp;
# 7=Female for Northern Shrimp not staged (stage I or II not determined)
agesum <- group_by(bsb.sp2,SEX) %>%
  summarize(minage=min(AGE, na.rm = T),maxage=max(AGE, na.rm = T))
agesum


agesum <- group_by(bsb.fl2,SEX) %>%
  summarize(minage=min(AGE, na.rm = T),maxage=max(AGE, na.rm = T))
agesum

## differences between spring and fall data bsb.sp2 vs bsb.fl2
vb <- vbFuns(param="Typical")
f.starts.sp <- vbStarts(LENGTH~AGE,data=bsb.fl2) 
f.fit.sp <- nls(LENGTH~vb(AGE,Linf,K,t0),data=bsb.fl2,start=f.starts.sp)
coef(f.fit.sp)
ages <- seq(0,10,by=0.5)
ages <-c(0.5, seq(1,10,by=1))
f.boot1.sp <- Boot(f.fit.sp)  # Be aware of some non-convergence
confint(f.boot1.sp)

lp=predict(f.fit.sp)
plot(lp~ages, type='b', ylab='Length (cm)', xlab="Age", las=1, lwd=2, pch=19)

predict(f.fit.sp,data.frame(AGE=1:10))
predict2 <- function(x) predict(x,data.frame(AGE=ages))
ages <- 0.5:10
predict2(f.fit.sp)  
ages <- seq(0,10,by=0.5)
f.boot2 <- Boot(f.fit.sp,f=predict2)

preds1 <- data.frame(ages,
                     predict(f.fit.sp,data.frame(AGE=ages)),
                     confint(f.boot2))
preds2 <- filter(preds1,AGE>=agesum$minage[2],AGE<=agesum$maxage[2])

### trying with WEIGHT instead of length for M estimates
bsb.fl2$INDWTG=bsb.fl2$INDWT*1000 # grams
f.starts.sp <- vbStarts(INDWTG~AGE,data=bsb.fl2[complete.cases(bsb.fl2$INDWT),]) 
f.fit.sp <- nls(INDWTG~vb(AGE,Linf,K,t0),data=bsb.fl2[complete.cases(bsb.fl2$INDWT),],start=f.starts.sp)
coef(f.fit.sp)
ages <- seq(-1,10,by=0.2)
ages <-c(0.5, seq(1,10,by=1))
f.boot1.sp <- Boot(f.fit.sp)  # Be aware of some non-convergence
confint(f.boot1.sp)

### Estimate M with input from above fits using:
## http://barefootecologist.com.au/shiny_m.html
# Bootstrap percent confidence intervals
# 
# Estimate    95% LCI    95% UCI
# Linf 54.1583626 52.6376634 55.8632301
# K     0.2775499  0.2616433  0.2944722
# t0    0.2725081  0.2410798  0.3035582
M=load("C:/Users/ryan.morse/Downloads/M_parms_values_byage_out2023-09-27 13_25_33.DMP")