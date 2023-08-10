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
grizzle=readxl::read_xlsx(paste(wd,'Nitrogen data/Grizzle_2011-data.xlsx', sep=''), sheet='post')#deployment
grizzle2=readxl::read_xlsx(paste(wd,'Nitrogeen data/Grizzle_2011-data.xlsx', sep=''), sheet='pre') #initial values
grizzle.all=readxl::read_xlsx(paste(wd,'Nitrogen data/Grizzle_2011-data.xlsx', sep=''), sheet='all') #initial values
## correct for mislabeling of larger oysters mg->g (small oysters appear OK) No longer doing this, SMALL weights for many...
# grizzle.all$DW=NA
# grizzle.all$DW[grizzle.all$`Shell Height (mm)`>20]=grizzle.all$`Soft Tissue DW (g)`[grizzle.all$`Shell Height (mm)`>20]*10
# grizzle.all$DW[grizzle.all$`Shell Height (mm)`<20]=grizzle.all$`Soft Tissue DW (g)`[grizzle.all$`Shell Height (mm)`<20]

### Bayer CT data ###
bayer=readxl::read_xlsx(paste(wd,'/','RM.xlsx', sep=''), sheet='Bayer')
bayer2=bayer
bayer2$SH=bayer$`Length (mm)`
bayer2$SH[which(is.na(bayer$`Length (mm)`))]=bayer$`IJ Length (mm)`[which(is.na(bayer$`Length (mm)`))]

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
