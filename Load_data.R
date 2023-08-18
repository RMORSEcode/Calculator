### RM 2023
# Load all shellfish data for calculator, add columns and QA/QC data
# TO DO: organize files into single location; QA/QC data removal -> outliers for size, N content, weight

library(readxl)
library(lubridate)
library(tidyverse)
wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"


### Cornwell CB data ###
# CB.2023=readxl::read_xlsx(paste(wd,"Oyster morphometrics/Data_compiled_MASTER_updated on11-15-17_2.xlsx",sep=''),
#                           sheet='Compiled_With_Formulas', skip=0, col_names = T)
# CB.2023$Tissue_Dry_Weight_g[which(CB.2023$Tissue_Dry_Weight_g<=0)]=NA
# CB.tissue=readxl::read_xlsx(paste(wd,"Oyster morphometrics/Data_compiled_MASTER_updated on11-15-17_2.xlsx", sep=''),
#                             sheet='Used_Tissue Analysis', skip=0, col_names = T)
# CB.shell=readxl::read_xlsx(paste(wd,"Oyster morphometrics/Data_compiled_MASTER_updated on11-15-17_2.xlsx", sep=''),
#                            sheet='Used_Shell Analysis', skip=0, col_names = T)
## original data from Cromwell 2017 BMP
# CB=readxl::read_xlsx(paste(wd,"Oyster morphometrics/CBay/CB-oyster expert panel-compiled data 9.xlsx", sep=''),
#                      sheet='All_Shell Height_Tissue_DryWT', skip=0, col_names = T)
# CBnocoast=readxl::read_xlsx(paste(wd,"Oyster morphometrics/CBay/CB-oyster expert panel-compiled data 9.xlsx", sep=''),
#                      sheet='SHeight_Tissue_DryWT_NoCoastal', skip=0, col_names = T)

## from Julie Reichert, contains data from 2022 update to BMP --> USE THIS ONE 20230807 RM/JR
CB=read_xlsx(paste(wd,"Oyster morphometrics/CBay/Tissue_Re-eval_9-6-17/Tissue_Re-eval_Gear_Ploidy_9-11-17.xlsx", sep=''), 
                    sheet='Tissue_Re-eval')


### Poach CB data ###
# poach=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited_Nov2020.xlsx', sep=''), sheet='Compiled Data')
# poach2=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='compiled')
# create better version with all vars
PCBVA=readxl::read_xlsx(paste(wd,'Nitrogen data/CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='Original Data Virginia RM')
PCBVA=PCBVA[-1,]
PCBMD=readxl::read_xlsx(paste(wd,'Nitrogen data/CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='Original Data Maryland RM')
PCBMD=PCBMD[-1,]
colnames(PCBVA)[!(colnames(PCBVA) %in% colnames(PCBMD))]
PCBVA=subset(PCBVA, select=-c(`bad_Shell_TC_g_C_per_ g_dw`, `bad_Shell_TN_g_N_per_g_dw`))
PCB=rbind(PCBVA, PCBMD)
PCB=subset(PCB, select=-c(`Shell_TP_g_P_per_g_dw`, `Shell_TP_Percent`))
## add shell P, dates are from first sampling date only, merge on sampled ID 
pcbshellPO4=readxl::read_xlsx(paste(wd,"Nitrogen data/Poach/Daily Shell Phosphate Tests_Dec_2020b.xlsx",sep=''), 
                                    sheet='Data_Summary')
pcbshellPO4=pcbshellPO4[,c(1,5,6)]
colnames(pcbshellPO4)[1]="Number_ID"
pcbshellPO4$Number_ID=as.character(pcbshellPO4$Number_ID)
# pcbshellPO4$shell_percent_P=pcbshellPO4$`Shell P content mg/g`/10
PCB=left_join(PCB, pcbshellPO4, by="Number_ID", keep=F)
PCB$Date_Oysters_Removed=as.Date(as.numeric(PCB$Date_Oysters_Removed),origin = "1899-12-30")
PCB$Month_Oysters_Removed=as.numeric(PCB$Month_Oysters_Removed)
PCB$Total_Shell_Height_Length_mm=as.numeric(PCB$Total_Shell_Height_Length_mm)
PCB$Tissue_N_Percent[which(PCB$Tissue_N_Percent==0)]=NA #fix one data point
PCB$Number_ID=as.numeric(PCB$Number_ID)
## get rid of anomalous value (>2 for shell N)
PCB$Shell_C_Percent[PCB$Number_ID==106]=NA
PCB$Shell_N_Percent[PCB$Number_ID==106]=NA

### Reitsma MA data ###
reitsma.nms=readxl::read_xlsx(paste(wd,'/', 'Reitsma Shellfish N Sample data.xlsx', sep=''), skip=0, n_max=5, col_names = F)
reitsma=readxl::read_xlsx(paste(wd,'/', 'Reitsma Shellfish N Sample data.xlsx', sep=''), skip=4)

### Grizzle data from NH
# grizzle.post=readxl::read_xlsx(paste(wd,'Nitrogen data/Grizzle_2011-data.xlsx', sep=''), sheet='post')#deployment
# grizzle.pre=readxl::read_xlsx(paste(wd,'Nitrogeen data/Grizzle_2011-data.xlsx', sep=''), sheet='pre') #initial values
grizzle=readxl::read_xlsx(paste(wd,'Nitrogen data/Grizzle_2011-data.xlsx', sep=''), sheet='all') #combined values
## correct for mislabeling of larger oysters mg->g (small oysters appear OK) No longer doing this, SMALL weights for many...
# grizzle.all$DW=NA
# grizzle.all$DW[grizzle.all$`Shell Height (mm)`>20]=grizzle.all$`Soft Tissue DW (g)`[grizzle.all$`Shell Height (mm)`>20]*10
# grizzle.all$DW[grizzle.all$`Shell Height (mm)`<20]=grizzle.all$`Soft Tissue DW (g)`[grizzle.all$`Shell Height (mm)`<20]

### Bayer CT data ###
# original file location: "paste(wd,"Nitrogen data/Copy of Greenwich FARM data 2019-2020.xlsx", sep='')
# bayer=readxl::read_xlsx(paste(wd,'/','RM.xlsx', sep=''), sheet='Bayer') file contains merged data from multiple sheets
bayer=readxl::read_xlsx(paste(wd,'Nitrogen data/Bayer_Greenwich_FARM_data_2019_2020.xlsx', sep=''), sheet='Bayer') #file contains merged data from multiple sheets
bayer2=bayer
## use Image J values for missing shell length and widths
bayer2$SH=bayer$`Length (mm)`
bayer2$SH[which(is.na(bayer$`Length (mm)`))]=bayer$`IJ Length (mm)`[which(is.na(bayer$`Length (mm)`))]
bayer2$SW=bayer$`width (mm)`
bayer2$SW[which(is.na(bayer$`width (mm)`))]=bayer$`IJ width (mm)`[which(is.na(bayer$`width (mm)`))]

### Sebastiano data from NY ###
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


### updated data from Levinton (2008-2011) and Sebastiano et al 2015.
#load Sebastiano update for Jamaica Bay E, W, C and Great South Bay E, W, C
# "C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Oyster morphometrics/Sebastiano data/JB_measurement_data.xls"
sebJBE=readxl::read_xls(paste(wd,'Oyster morphometrics/Sebastiano data/JB_measurement_data.xls', sep=''), sheet='JBE', skip = 2)
colnames(sebJBE)=c('Date', 'Cage', 'Bag', 'Oyster_ID', 'SH_mm', 'SL_mm', 'SW_mm', 'dead', 'alive', 'dead SH_mm', 'mean SH_mm')
sebJBW=readxl::read_xls(paste(wd,'Oyster morphometrics/Sebastiano data/JB_measurement_data.xls', sep=''), sheet='JBW', skip = 2)
colnames(sebJBW)=c('Date', 'Cage', 'Bag', 'Oyster_ID', 'SH_mm', 'SL_mm', 'SW_mm', 'dead', 'alive', 'dead SH_mm', 'mean SH_mm')
sebJBC=readxl::read_xls(paste(wd,'Oyster morphometrics/Sebastiano data/JB_measurement_data.xls', sep=''), sheet='JBC', skip = 2)
colnames(sebJBC)=c('Date', 'Cage', 'Bag', 'Oyster_ID', 'SH_mm', 'SL_mm', 'SW_mm', 'dead', 'alive', 'dead SH_mm', 'mean SH_mm')
sebSI=readxl::read_xls(paste(wd,'Oyster morphometrics/Sebastiano data/JB_measurement_data.xls', sep=''), sheet='SI', skip = 2)
colnames(sebSI)=c('Date', 'Cage', 'Bag', 'Oyster_ID', 'SH_mm', 'SL_mm', 'SW_mm', 'dead', 'alive', 'dead SH_mm', 'mean SH_mm')
## take mean of A and B replicates for SH, SL, SW
sebJBE=sebJBE %>% group_by(Date, Cage, Oyster_ID) %>% select(SH_mm:SW_mm) %>% summarise_all(., mean, na.rm=T)
sebJBW=sebJBW %>% group_by(Date, Cage, Oyster_ID) %>% select(SH_mm:SW_mm) %>% summarise_all(., mean, na.rm=T)
sebJBC=sebJBC %>% group_by(Date, Cage, Oyster_ID) %>% select(SH_mm:SW_mm) %>% summarise_all(., mean, na.rm=T)
sebSI=sebSI %>% group_by(Date, Cage, Oyster_ID) %>% select(SH_mm:SW_mm) %>% summarise_all(., mean, na.rm=T)
sebJBE$Site='JBE'
sebJBW$Site='JBW'
sebJBC$Site='JBC'
sebSI$Site='SI'

sebJBEw=readxl::read_xls(paste(wd,"Oyster morphometrics/Sebastiano data/JB_dry weight_data.xls",sep=''), sheet='JBE', skip = 3)
colnames(sebJBEw)=c('Date', 'Tray_wt_g', 'tray_plus_dry_tissue_g', 'x', 'Dry_soft_tissue_g', 'x2')
sebJBEw=sebJBEw %>% select(-(c(x, x2)))
table(sebJBEw$Date)
sebJBEw$Oyster_ID=rep(seq(1:40),12)
sebJBEw$Site='JBE'
sebJBWw=readxl::read_xls(paste(wd,"Oyster morphometrics/Sebastiano data/JB_dry weight_data.xls",sep=''), sheet='JBW', skip = 3)
colnames(sebJBWw)=c('Date', 'Tray_wt_g', 'tray_plus_dry_tissue_g', 'x', 'Dry_soft_tissue_g', 'x2', 'x3')
sebJBWw=sebJBWw %>% select(-(c(x, x2, x3)))
sebJBWw$Site='JBW'
table(sebJBWw$Date)
sebJBWw$Oyster_ID=rep(seq(1:40),11)
sebJBCw=readxl::read_xls(paste(wd,"Oyster morphometrics/Sebastiano data/JB_dry weight_data.xls",sep=''), sheet='JBC', skip = 3)
colnames(sebJBCw)=c('Date', 'Tray_wt_g', 'tray_plus_dry_tissue_g', 'x', 'Dry_soft_tissue_g', 'x2')
sebJBCw=sebJBCw %>% select(-(c(x, x2)))
sebJBCw$Site='JBC'
table(sebJBCw$Date)
sebJBCw$Oyster_ID=rep(seq(1:40),12)

sebSIw=readxl::read_xls(paste(wd,"Oyster morphometrics/Sebastiano data/JB_dry weight_data.xls",sep=''), sheet='SI', skip = 3)
colnames(sebSIw)=c('Date', 'Tray_wt_g', 'tray_plus_dry_tissue_g', 'x', 'Dry_soft_tissue_g', 'x2')
sebSIw=sebSIw %>% select(-(c(x, x2)))
sebSIw$Site='SI'
table(sebSIw$Date)
sebSIw$Oyster_ID=rep(seq(1:40),12)

sebJBEx=left_join(sebJBE, sebJBEw, by=c("Date", "Oyster_ID"))
sebJBWx=left_join(sebJBW, sebJBWw, by=c("Date", "Oyster_ID"))
sebJBCx=left_join(sebJBC, sebJBCw, by=c("Date", "Oyster_ID"))
sebSIx=left_join(sebSI, sebSIw, by=c("Date", "Oyster_ID"))

# now fix N and C, and left join
sebJBEcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN tissue.xlsx",sep=''), sheet='JBE')
colnames(sebJBEcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebJBEcn=sebJBEcn %>% select(1:6)
sebJBEcn$Site='JBE'
table(sebJBEcn$Date)
sebJBEcn$Oyster_ID=rep(seq(1:40),2)

sebJBWcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN tissue.xlsx",sep=''), sheet='JBW')
colnames(sebJBWcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebJBWcn=sebJBWcn %>% select(1:6)
sebJBWcn$Site='JBW'
table(sebJBWcn$Date)
sebJBWcn$Oyster_ID=rep(seq(1:40),2)

sebJBCcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN tissue.xlsx",sep=''), sheet='JBC')
colnames(sebJBCcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebJBCcn=sebJBCcn %>% select(1:6)
sebJBCcn$Site='JBC'
table(sebJBCcn$Date)
sebJBCcn$Oyster_ID=rep(seq(1:40),2)

sebJBE.f=left_join(sebJBEx, sebJBEcn, by=c("Date", "Oyster_ID"))
sebJBW.f=left_join(sebJBWx, sebJBWcn, by=c("Date", "Oyster_ID"))
sebJBC.f=left_join(sebJBCx, sebJBCcn, by=c("Date", "Oyster_ID"))


### Janine Barr DE, NJ, NY data ###
Janine_RM=paste(wd,"Oyster morphometrics/Janine_s data/Rutgers_RM.xlsx", sep='')
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
#
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
#
Barr=rbind(BarnBay, DelBay) 
Barr=rbind(Barr, RehoBay)


### Tom Kiffney ME ###
Kiff1=read.csv(paste(wd,'Oyster morphometrics/Kiffney/tissue2021.csv', sep=''))
Kiff2=read.csv(paste(wd,'Oyster morphometrics/Kiffney/shell2021.csv', sep=''))
Kiffney=left_join(Kiff1, Kiff2, by=c('Site', 'ShellHeight_mm', 'WholeWetWeight_g'))
Kiffney2=read.csv(paste(wd,'Oyster morphometrics/Kiffney/tissuePloidy2022.csv', sep=''))


## NC data from Beth Darrow
Darrow=read_xlsx(paste(wd,"Oyster morphometrics/NC/NCOysterSizeData_DarrowKInsella_forJulieRose.xlsx", sep=''), sheet='RM_morpho')

### RI data from Suzy Ayvazian
Ayvazian=read_xlsx(paste(wd,"Oyster morphometrics/RI/Ninigret oyster dry weights_height.xlsx", sep=''), sheet='Ninigret Dry weights')
