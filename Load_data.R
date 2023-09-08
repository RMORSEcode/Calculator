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
# CB2016=readxl::read_xlsx(paste(wd,"Oyster morphometrics/CBay/CB-oyster expert panel-compiled data 9.xlsx", sep=''),
#                      sheet='All_Shell Height_Tissue_DryWT', skip=0, col_names = T)
# CBnocoast=readxl::read_xlsx(paste(wd,"Oyster morphometrics/CBay/CB-oyster expert panel-compiled data 9.xlsx", sep=''),
#                      sheet='SHeight_Tissue_DryWT_NoCoastal', skip=0, col_names = T)

## from Julie Reichert, contains data from 2022 update to BMP --> USE THIS ONE 20230807 RM/JR
CB=readxl::read_xlsx(paste(wd,"Oyster morphometrics/CBay/Tissue_Re-eval_9-6-17/Tissue_Re-eval_Gear_Ploidy_9-11-17.xlsx", sep=''), 
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
# RM notes - no sample 156, 310 has 2 samples (310, 310A)
# original file location: "paste(wd,"Nitrogen data/Copy of Greenwich FARM data 2019-2020.xlsx", sep='')
# bayer=readxl::read_xlsx(paste(wd,'/','RM.xlsx', sep=''), sheet='Bayer') file contains merged data from multiple sheets
bayer=readxl::read_xlsx(paste(wd,'Nitrogen data/CT/Bayer_Greenwich_FARM_data_2019_2020.xlsx', sep=''), 
                        sheet='Bayer') #file contains manually merged data from multiple sheets
bayer2=bayer
## use Image J values for missing shell length and widths
bayer2$SH=bayer$`Length (mm)`
bayer2$SH[which(is.na(bayer$`Length (mm)`))]=bayer$`IJ Length (mm)`[which(is.na(bayer$`Length (mm)`))]
bayer2$SW=bayer$`width (mm)`
bayer2$SW[which(is.na(bayer$`width (mm)`))]=bayer$`IJ width (mm)`[which(is.na(bayer$`width (mm)`))]
## read in updated shell N and C without rounding issue (all shell N was either 0.001 or 0.002)
b2shell=readxl::read_xlsx(paste(wd,'Nitrogen data/CT/Tissue and Shells Nitrogen_Carbon results.xlsx', 
                                sep=''), sheet='SHELL with 4 decimals', skip = 2)
colnames(b2shell)=c("ID", "Shell_TN_g_N_per_g_dw", "Shell_N_Percent", "Shell_TC_g_C_per_g_dw", "Shell_C_Percent")
b2shell$Shell_N_Percent=b2shell$Shell_TN_g_N_per_g_dw*100
b2shell$Shell_C_Percent=b2shell$Shell_TC_g_C_per_g_dw*100
# fix sample number issue 310, 310A
b2shell$ID[which(b2shell$ID=="310A")]=3100
b2shell$ID=as.numeric(b2shell$ID)
bayer2$ID[which(bayer2$ID=="310A")]=3100
bayer2$ID=as.numeric(bayer2$ID)
bayer2=left_join(bayer2, b2shell, by="ID")


### Sebastiano data from NY ### THIS HAS BEEN REPLACED WITH MORE EXTENSIVE DATA BELOW RM 20230822
# Sebastiano=readxl::read_xlsx(paste(wd,'/','RM.xlsx', sep=''), sheet='SebMeanSH')
# Sebastiano2=readxl::read_xlsx(paste(wd,'/','RM.xlsx', sep=''), sheet='SebDW')
# Seb.1=Sebastiano %>% group_by(Site, Date, Cage) %>%
#   mutate(MeanSH=mean(`Bag Mean (height)`)) %>% 
#   select(-Bag, -`Bag Mean (height)`, -`Site mean`) %>%
#   distinct(.)
# Seb=Seb.1 %>% left_join(Sebastiano2, by=c('Date', 'Site'))
# Sebtest=Sebastiano[complete.cases(Sebastiano$`Site mean`),]
# ## Seb=Sebtest %>% left_join(Sebastiano2, by=c('Date', 'Site'))
# Seb2=Seb[complete.cases(Seb$`Cage Mean (dry weight)`),]

### updated data from Levinton (2008-2011) and Sebastiano et al 2015.
#load Sebastiano update for Jamaica Bay E, W, C and Great South Bay E, W, C
# now fix N and C, and left join
sebJBEcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN tissue.xlsx",sep=''), sheet='JBE')
colnames(sebJBEcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebJBEcn=sebJBEcn %>% select(1:6)
# sebJBEcn$Site='JBE'
table(sebJBEcn$Date)
sebJBWcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN tissue.xlsx",sep=''), sheet='JBW')
colnames(sebJBWcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebJBWcn=sebJBWcn %>% select(1:6)
# sebJBWcn$Site='JBW'
table(sebJBWcn$Date)
sebJBCcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN tissue.xlsx",sep=''), sheet='JBC')
colnames(sebJBCcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebJBCcn=sebJBCcn %>% select(1:6)
# sebJBCcn$Site='JBC'
table(sebJBCcn$Date)
## shell N anc C (none for JBE)
sebJBEshellcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN shell.xlsx",sep=''), sheet='JBE')
colnames(sebJBEshellcn)=c('Height_mm', 'Length_mm', 'Width_mm', 'Weight_g')
sebJBEshellcn$Site='JBE'
sebJBWshellcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN shell.xlsx",sep=''), sheet='JBW')
colnames(sebJBWshellcn)=c('Height_mm', 'Length_mm', 'Width_mm', 'shell_weight_g', 'shell_percent_N', 'shell_percent_C')
sebJBWshellcn$Site='JBW'
sebJBCshellcn=readxl::read_xlsx(paste(wd,"Nitrogen data/JB CN shell.xlsx",sep=''), sheet='JBC')
colnames(sebJBCshellcn)=c('Height_mm', 'Length_mm', 'Width_mm', 'shell_weight_g', 'shell_percent_N', 'shell_percent_C')
sebJBCshellcn$Site='JBC'
sebJBE=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/JB_condition index_data.xls",sep=''), sheet='JBE', skip = 2)
colnames(sebJBE)=c('Date_collected', 'Date_processed', 'Cage', 'Bag', 'Oyster_ID', 'Live_whole_wt_g', 'Shell_height_mm',
                   'Shell_length_mm', 'Shell_width_mm', 'Wet_shell_wt_g', 'Tissue_tray_wt_g', 'Sex', 'Gonad_ranking', 'Tray_plus_dry_tissue_wt_g',
                   'Condition_index', 'Comments')
sebJBW=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/JB_condition index_data.xls",sep=''), sheet='JBW', skip = 2)
colnames(sebJBW)=c('Date_collected', 'Date_processed', 'Cage', 'Bag', 'Oyster_ID', 'Live_whole_wt_g', 'Shell_height_mm',
                   'Shell_length_mm', 'Shell_width_mm', 'Wet_shell_wt_g', 'Tissue_tray_wt_g', 'Sex', 'Gonad_ranking', 'Tray_plus_dry_tissue_wt_g',
                   'Condition_index', 'Comments')
sebJBC=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/JB_condition index_data.xls",sep=''), sheet='JBC', skip = 2)
colnames(sebJBC)=c('Date_collected', 'Date_processed', 'Cage', 'Bag', 'Oyster_ID', 'Live_whole_wt_g', 'Shell_height_mm',
                   'Shell_length_mm', 'Shell_width_mm', 'Wet_shell_wt_g', 'Tissue_tray_wt_g', 'Sex', 'Gonad_ranking', 'Tray_plus_dry_tissue_wt_g',
                   'Condition_index', 'Comments')
sebJBC=sebJBC %>% select(1:16)
# NOW load JB and GSB condition index files and left_join, then bind_rows, add to main file
sebJBE.f=left_join(sebJBE, sebJBEcn, by=c("Date_collected"="Date", "Cage", "Bag","Oyster_ID"="Sample_num"))
sebJBE.f$Site='JBE'
sebJBW.f=left_join(sebJBW, sebJBWcn, by=c("Date_collected"="Date", "Cage", "Bag","Oyster_ID"="Sample_num"))
sebJBW.f$Site='JBW'
sebJBC.f=left_join(sebJBC, sebJBCcn, by=c("Date_collected"="Date", "Cage", "Bag","Oyster_ID"="Sample_num"))
sebJBC.f$Site='JBC'
### Combine Jamaica Bay sites
sebJB=bind_rows(sebJBE.f, sebJBC.f)
sebJB=bind_rows(sebJB, sebJBW.f)

### Now do for Great South Bay sites
sebGSBEcn=readxl::read_xlsx(paste(wd,"Oyster morphometrics/Sebastiano data/GSB CN tissue.xlsx",sep=''), sheet='GSBE')
colnames(sebGSBEcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebGSBEcn=sebGSBEcn %>% select(1:6)
# sebGSBEcn$Site='GSBE'
table(sebGSBEcn$Date)
sebGSBWcn=readxl::read_xlsx(paste(wd,"Oyster morphometrics/Sebastiano data/GSB CN tissue.xlsx",sep=''), sheet='GSBW')
colnames(sebGSBWcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebGSBWcn=sebGSBWcn %>% select(1:6)
# sebGSBWcn$Site='GSBW'
table(sebGSBWcn$Date)
sebGSBCcn=readxl::read_xlsx(paste(wd,"Oyster morphometrics/Sebastiano data/GSB CN tissue.xlsx",sep=''), sheet='GSBC')
colnames(sebGSBCcn)=c('Date', 'Cage', 'Bag', 'Sample_num', 'percent_N', 'percent_C')
sebGSBCcn=sebGSBCcn %>% select(1:6)
# sebGSBCcn$Site='GSBC'
table(sebGSBCcn$Date)
sebGSBE=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/GSB_condition index_data.xls",sep=''), sheet='GSBE', skip = 2)
colnames(sebGSBE)=c('Date_collected', 'Date_processed', 'Cage', 'Bag', 'Oyster_ID', 'Live_whole_wt_g', 'Shell_height_mm',
                   'Shell_length_mm', 'Shell_width_mm', 'Wet_shell_wt_g', 'Tissue_tray_wt_g', 'Sex', 'Gonad_ranking', 'Tray_plus_dry_tissue_wt_g',
                   'Condition_index', 'Comments')
sebGSBE=sebGSBE %>% select(1:16)
sebGSBC=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/GSB_condition index_data.xls",sep=''), sheet='GSBC', skip = 2)
colnames(sebGSBC)=c('Date_collected', 'Date_processed', 'Cage', 'Bag', 'Oyster_ID', 'Live_whole_wt_g', 'Shell_height_mm',
                    'Shell_length_mm', 'Shell_width_mm', 'Wet_shell_wt_g', 'Tissue_tray_wt_g', 'Sex', 'Gonad_ranking', 'Tray_plus_dry_tissue_wt_g',
                    'Condition_index', 'Comments')
sebGSBC=sebGSBC %>% select(1:16)
sebGSBW=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/GSB_condition index_data.xls",sep=''), sheet='GSBW', skip = 2)
colnames(sebGSBW)=c('Date_collected', 'Date_processed', 'Cage', 'Bag', 'Oyster_ID', 'Live_whole_wt_g', 'Shell_height_mm',
                    'Shell_length_mm', 'Shell_width_mm', 'Wet_shell_wt_g', 'Tissue_tray_wt_g', 'Sex', 'Gonad_ranking', 'Tray_plus_dry_tissue_wt_g',
                    'Condition_index', 'Comments')

# NOW left_join, then bind_rows, add to main file
sebGSBE.f=left_join(sebGSBE, sebGSBEcn, by=c("Date_collected"="Date", "Cage", "Bag","Oyster_ID"="Sample_num"))
sebGSBE.f$Site='GSBE'
sebGSBW.f=left_join(sebGSBW, sebGSBWcn, by=c("Date_collected"="Date", "Cage", "Bag","Oyster_ID"="Sample_num"))
sebGSBW.f$Site='GSBW'
sebGSBC.f=left_join(sebGSBC, sebGSBCcn, by=c("Date_collected"="Date", "Cage", "Bag","Oyster_ID"="Sample_num"))
sebGSBC.f$Site='GSBC'
sebGSB=bind_rows(sebGSBE.f, sebGSBC.f)
sebGSB=bind_rows(sebGSB, sebGSBW.f)
## shell N anc C (none for JBE)
sebGSBEshellcn=readxl::read_xlsx(paste(wd,"Nitrogen data/GSB CN shell.xlsx",sep=''), sheet='GSBE')
colnames(sebGSBEshellcn)=c('Height_mm', 'Length_mm', 'Width_mm', 'Weight_g')
sebGSBEshellcn$Site='GSBE'
sebGSBWshellcn=readxl::read_xlsx(paste(wd,"Nitrogen data/GSB CN shell.xlsx",sep=''), sheet='GSBW')
colnames(sebGSBWshellcn)=c('Height_mm', 'Length_mm', 'Width_mm', 'shell_weight_g', 'shell_percent_N', 'shell_percent_C')
sebGSBWshellcn$Site='GSBW'
sebGSBCshellcn=readxl::read_xlsx(paste(wd,"Nitrogen data/GSB CN shell.xlsx",sep=''), sheet='GSBC')
colnames(sebGSBCshellcn)=c('Height_mm', 'Length_mm', 'Width_mm', 'shell_weight_g', 'shell_percent_N', 'shell_percent_C')
sebGSBCshellcn$Site='GSBC'
### combine sites and calculate dry weight for final data to use in SH_DW.R, build_main_file.R ###
Seb.f=bind_rows(sebJB, sebGSB)
Seb.f$Dry_tissue_wt_g=Seb.f$Tray_plus_dry_tissue_wt_g-Seb.f$Tissue_tray_wt_g

### Levinton data from NY (new source)
levP40=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Pier 40 (P40)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levP40)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
levJB=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Jamaica Bay (JB)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levJB)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
levSI=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Shelter Island (SI)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levSI)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
levRB=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Raritan Bay (RB)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levRB)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
levI=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Irvington (I)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levI)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
levPT=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Piermont (PT)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levPT)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
levWI=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Washington Irving (WI)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levWI)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
levPM=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Philipse Manor (PM)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levPM)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
levWE=readxl::read_xls(paste(wd,"Oyster morphometrics/Levinton NY/oyster_Condition Index_data.xls",sep=''), 
                        sheet='Westerly (WE)', skip = 3,
                        col_types = c("text","numeric","text","numeric","date","date",
                                      "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                      "text","numeric","numeric","numeric","text","skip"))
colnames(levWE)=c("Site", "Cage", "Bag","Oyster_ID", "Date_collected", "Date_analyzed", "Whole_weight_g","Shell_height_mm",
                   "Shell_length_mm", "Shell_Width_mm", "Wet_shell_weight_g", "Dry_shell_weight_g", "Tray_weight_g",
                   "Tray_plus_dry_soft_tissue_g", "Shell_tray_wt_g", "Sex", "Gonad_ranking", "Wet_shell_CI", 
                   "Dry_shell_CI", "Comments")
# Now combine sites for final data
Lev=bind_rows(levP40, levJB)
Lev=bind_rows(Lev, levSI)
Lev=bind_rows(Lev, levRB)
Lev=bind_rows(Lev, levI)
Lev=bind_rows(Lev, levPT)
Lev=bind_rows(Lev, levWI)
Lev=bind_rows(Lev, levPM)
Lev=bind_rows(Lev, levWE)
Lev$Dry_tissue_wt_g=Lev$Tray_plus_dry_soft_tissue_g - Lev$Tray_weight_g

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
Darrow=read_xlsx(paste(wd,"Oyster morphometrics/NC/NCOysterSizeData_DarrowKInsella_forJulieRose.xlsx", sep=''), sheet='Sheet1')
Darrow_N=read_xlsx(paste(wd,"Oyster morphometrics/NC/NCOysterSizeData_DarrowKInsella_forJulieRose.xlsx", sep=''), sheet='Oyster %N')
Darr=left_join(Darrow, Darrow_N, by=c("Site", "Bag #"="Bag", "Date Collected"="SampleDate")) # note N and C samples added to all oyster from Bag on merge

### RI data from Suzy Ayvazian
Ayvazian=read_xlsx(paste(wd,"Oyster morphometrics/RI/Ninigret oyster dry weights_height.xlsx", sep=''), sheet='Ninigret Dry weights')
Ayvazian2=read_xlsx(paste(wd,"Oyster morphometrics/RI/Green Hill Length_dry wgt.xlsx", sep=''), sheet='length_dry wgt')
