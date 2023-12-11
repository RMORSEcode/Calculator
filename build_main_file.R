### RM 2023
# Script to build main file for Calculator; conforming to Chesapeake Bay Panel BMP 2023 update
# File contains data on shell morphology, tissue and shell N, C, P content, condition
# Relies < Load_data.R >
library(lubridate)
library(tidyverse)

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
names$Seb.f=NA
names$Seb.f[1:ncol(Seb.f)]=colnames(Seb.f)
names$Lev=NA
names$Lev[1:ncol(Lev)]=colnames(Lev)
date='20230822'
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
# Near_Waterbody_General_Location, Waterbody_Type,.,Ross_Project:Quantile_Grouping))
# to match PCB
newdf=CB %>% select(-c(Original_Order, Analysis_ID, Yates_Bar_Name, .,Ross_Project:Quantile_Grouping))
colnames(newdf)[5]="Location_Index"
# colnames(newdf)[15]="Hatchery_produced_or_Wild" # this is Oyster_Stock
colnames(newdf)[16]="Hatchery_produced_or_Wild"
newdf$Number_ID=as.numeric(newdf$Number_ID)
## fix names that cause issues
colnames(newdf)[38]="Shell_TC_g_C_per_g_dw" #"Shell_TC_g_C_per_ g_dw" 
colnames(newdf)[8]="Near_Waterbody_General_Location" #"Near_Waterbody__General_Location"
colnames(newdf)[25]="Shell_and_Tissue_Total_Wet_Weight_g" # Shell&Tissue_Total_Wet_Weight_g
colnames(newdf)[31]="Tissue_TC_g_C_per_g_dw" #"Tissue_TC_g_C_per_ g_dw"


for(i in 1:ncol(newdf)){
  if(colnames(newdf)[i] %in% colnames(newdf)[xx]){
    print(paste(i, 'of', ncol(newdf), 'is numeric', sep=' '))
    next
  }
  print(paste(i, 'of', ncol(newdf), sep=' '))
  print(sapply(newdf[,i], class))
  print(unique(newdf[,i]))
}

### columns to add ###
# xxx$Raw_Data_File
# xxx$Representative_Aquaculture_Oyster_Practice
# xxx$Data_Source
# xxx$Location_Index
# xxx$State
# xxx$Near_Waterbody_General_Location
# xxx$Waterbody_Name
# xxx$Site
# xxx$Site_within_Study
# xxx$Oyster_Growth_Location_Type
# xxx$Subtidal_Intertidal_WaterColumn_Other
# xxx$Ploidy
# xxx$Oyster_Stock
# xxx$Hatchery_produced_or_Wild
# xxx$Date_Oysters_Deployed
# xxx$Date_Oysters_Removed
# xxx$Month_Oysters_Removed
# xxx$Year_Oysters_Removed
# xxx$Season_Oysters_Removed
# xxx$Total_Shell_Height_Length_Inches
# xxx$Oyster_Size_Class



### add Poach data (2023 in prep) - PCB
colnames(newdf)[!(colnames(newdf) %in% colnames(PCB))]
colnames(PCB)[!(colnames(PCB) %in% colnames(newdf))]
colnames(PCB)[1]="Representative_Aquaculture_Oyster_Practice"
colnames(PCB)[28]="Shell_TC_g_C_per_g_dw" #"Shell_TC_g_C_per_ g_dw" 
colnames(PCB)[6]="Near_Waterbody_General_Location" #"Near_Waterbody__General_Location"
colnames(PCB)[12]="Hatchery_produced_or_Wild" #Hatchery-produced_or_Wild
colnames(PCB)[22]="Tissue_TC_g_C_per_g_dw" #"Tissue_TC_g_C_per_ g_dw"
PCB2=select(PCB, -is.outlier)
### add missing cols
PCB2$Raw_Data_File='CB_oyster_nutrient_data_edited.xlsx'
# PCB2$Representative_Aquaculture_Oyster_Practice
PCB2$Data_Source="Poach et al. in prep 2023"
# PCB2$Location_Index
# PCB2$State
# PCB2$Waterbody_Name
# PCB2$Site
PCB2$Site_within_Study=NA
# PCB2$Oyster_Growth_Location_Type
PCB2$Subtidal_Intertidal_WaterColumn_Other="Subtidal"
# PCB2$Ploidy
PCB2$Oyster_Stock=NA
PCB2$Oyster_Stock[PCB2$State=='Virginia']='Rappahannock Oyster Co.'
PCB2$Oyster_Stock[PCB2$State=='Maryland']='Orchard Point Oyster Co. LLC'
# PCB2$Hatchery_produced_or_Wild
# PCB2$Date_Oysters_Deployed
# PCB2$Date_Oysters_Removed
# PCB2$Month_Oysters_Removed
# PCB2$Year_Oysters_Removed
# PCB2$Season_Oysters_Removed
PCB2$Total_Shell_Height_Length_Inches=PCB2$Total_Shell_Height_Length_mm*0.0393701
PCB2$Oyster_Size_Class=NA
PCB2$Oyster_Size_Class=ifelse(PCB2$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
PCB2$Oyster_Size_Class[which(PCB2$Total_Shell_Height_Length_Inches>=2.0 & PCB2$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
PCB2$Oyster_Size_Class[which(PCB2$Total_Shell_Height_Length_Inches>2.49 & PCB2$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
PCB2$Oyster_Size_Class[which(PCB2$Total_Shell_Height_Length_Inches>3.49 & PCB2$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
PCB2$Oyster_Size_Class[which(PCB2$Total_Shell_Height_Length_Inches>4.49 & PCB2$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
PCB2$Oyster_Size_Class[which(PCB2$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
newdf2=bind_rows(newdf, PCB2)


## now fix names and add Reitsma (C virginica only)
# drop derived columns
# t=reitsma$GroupID
r2=reitsma %>% select(-c(Analysis, GroupID, Species, OysterGrp,`off/on bottom`, ))
# colnames(newdf)[!(colnames(newdf) %in% colnames(r2))]
#fix column names
colnames(r2)[1]="Number_ID"
colnames(r2)[2]="Season_Oysters_Removed"
colnames(r2)[3]="Date_Oysters_Removed"
colnames(r2)[4]="Near_Waterbody_General_Location"
colnames(r2)[5]="Waterbody_Name"
# colnames(r2)[6]="Site"
colnames(r2)[7]="Hatchery_produced_or_Wild"
colnames(r2)[8]="Volume_ml" #"Volume (ml)" 
colnames(r2)[9]="Total_Shell_Height_Length_mm" #"Shell Length (mm)"
colnames(r2)[10]="Total_Shell_Width_mm" #"Shell Width (mm)"
colnames(r2)[11]="Total_Shell_Depth_mm" #[11] "Shell Height (mm)"
colnames(r2)[12]="Shell_and_Tissue_Total_Wet_Weight_g" #"Whole Weight (g)"
colnames(r2)[13]="Shell_Dry_Weight_g" #Dry Shell Mass (g)"
colnames(r2)[14]="Tissue_Dry_Weight_g" #"Dry Tiss Mass (g)"
colnames(r2)[15]="Condition_Index" #"Condition Index"    
colnames(r2)[18]="Tissue_N_Percent" #Meat %N"
colnames(r2)[19]="Tissue_C_Percent"#"Meat % C"
colnames(r2)[22]="Shell_N_Percent"#"Shell %N" 
colnames(r2)[23]="Shell_C_Percent"#"Shell % C" 
# colnames(r2)[!(colnames(r2) %in% colnames(newdf))]
r3=r2 %>% select(-c(`Shell/Length (g/mm)`, `DryT/Length (g/mm)`,`Tissue N (g)`,
                    `Tissue C (g)`,`Shell N (g)`:`%C/animal`))#, `Shell C (g)`))
# colnames(r3)[!(colnames(r3) %in% colnames(newdf))]
### now add in missing cols that will not be NA:
### columns to add ###
r3$Raw_Data_File="Reitsma Shellfish N Sample data.xlsx"
r3$Representative_Aquaculture_Oyster_Practice=NA
r3$Representative_Aquaculture_Oyster_Practice[reitsma$OysterGrp=="W"]="On-Bottom without Gear" 
r3$Representative_Aquaculture_Oyster_Practice[reitsma$OysterGrp=="Con"]="On-Bottom without Gear"
r3$Representative_Aquaculture_Oyster_Practice[reitsma$OysterGrp=="Coff"]="Off-Bottom with Gear"
r3$Representative_Aquaculture_Oyster_Practice[reitsma$OysterGrp=="Coff-T"]="Off-Bottom with Gear"
r3$Data_Source='Reitsma et al. 2017'
r3$Location_Index=NA
r3$Location_Index[r3$Waterbody_Name=='Duxbury Bay']="Cape Cod Bay"
r3$Location_Index[r3$Waterbody_Name=='Barnstable Harbor']="Cape Cod Bay"
r3$Location_Index[r3$Waterbody_Name=='Wellfleet Harbor']="Cape Cod Bay"
r3$Location_Index[r3$Waterbody_Name=='Pleasant Bay']="Atlantic Ocean"
r3$Location_Index[r3$Waterbody_Name=='Oyster Pond']="Nantucket Sound"
r3$Location_Index[r3$Waterbody_Name=='Swan River']="Nantucket Sound"
r3$Location_Index[r3$Waterbody_Name=='Popponesset Bay']="Nantucket Sound"
r3$Location_Index[r3$Waterbody_Name=='Buzzards Bay']="Buzzards Bay"
r3$Location_Index[r3$Waterbody_Name=='Little Bay']="Buzzards Bay"
r3$Location_Index[r3$Waterbody_Name=='Pocasset Harbor']="Buzzards Bay"
r3$Location_Index[r3$Waterbody_Name=='Pocasset River']="Buzzards Bay"
r3$Location_Index[r3$Waterbody_Name=='Mashpee River']="Nantucket Sound"
r3$Location_Index[r3$Waterbody_Name=='Phinneys Harbor']="Buzzards Bay"
r3$State="Massachusetts"
# r3$Subtidal_Intertidal_WaterColumn_Other
# r3$Oyster_Stock
# r3$Date_Oysters_Deployed
r3$Month_Oysters_Removed=month(r3$Date_Oysters_Removed)
r3$Year_Oysters_Removed=year(r3$Date_Oysters_Removed)
# colnames(newdf)[!(colnames(newdf) %in% colnames(r3))]
r3$Year_Oysters_Removed=year(r3$Date_Oysters_Removed)
r3$Month_Oysters_Removed=month(r3$Date_Oysters_Removed)
r3$Total_Shell_Height_Length_Inches=r3$Total_Shell_Height_Length_mm*0.0393701
r3$Oyster_Size_Class=NA
r3$Oyster_Size_Class=ifelse(r3$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
r3$Oyster_Size_Class[which(r3$Total_Shell_Height_Length_Inches>=2.0 & r3$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
r3$Oyster_Size_Class[which(r3$Total_Shell_Height_Length_Inches>2.49 & r3$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
r3$Oyster_Size_Class[which(r3$Total_Shell_Height_Length_Inches>3.49 & r3$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
r3$Oyster_Size_Class[which(r3$Total_Shell_Height_Length_Inches>4.49 & r3$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
r3$Oyster_Size_Class[which(r3$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
# table(r3$Oyster_Size_Class)
r3$Ploidy="Diploid"
r3$Ploidy[reitsma$`off/on bottom`=='off-triploid']="Triploid"
r3$Oyster_Growth_Location_Type=NA
r3$Oyster_Growth_Location_Type[reitsma$OysterGrp=="Coff"]='Near-bottom cages'
r3$Oyster_Growth_Location_Type[reitsma$OysterGrp=="Coff-T"]='Near-bottom cages'
r3$Oyster_Growth_Location_Type[reitsma$OysterGrp=="W"]='Reef'
r3$Oyster_Growth_Location_Type[reitsma$OysterGrp=="Con"]='Reef'
## Fix issue with total C (fall) and organic C (spring)
r3$Shell_Organic_C_Percent=NA
r3$Shell_Organic_C_Percent[r3$Shell_C_Percent<4 & !is.na(r3$Shell_C_Percent)]=r3$Shell_C_Percent[r3$Shell_C_Percent<4 & !is.na(r3$Shell_C_Percent)]
r3$Shell_C_Percent[r3$Shell_C_Percent<4 & !is.na(r3$Shell_C_Percent)]=NA

newdf3=bind_rows(newdf2, r3)



### Fix names for Grizzle and Ward 2011
# colnames(grizzle)
grz=grizzle %>% select(-c(Replicate, N, `Size Class`))
# colnames(grz)
#"Study Site"         
#"Shell Height (mm)" 
#"Soft Tissue DW (g)" 
#"%N" 
#"%C" 
colnames(grz)=c('Site', 'Total_Shell_Height_Length_mm', 'Tissue_Dry_Weight_g','Tissue_N_Percent', 'Tissue_C_Percent')
grz$Oyster_Size_Class=ifelse(grz$`Total_Shell_Height_Length_mm`*0.0393701 < 2.0, "< 2.0", NA)
grz$Oyster_Size_Class[which(grz$`Total_Shell_Height_Length_mm`*0.0393701>=2.0 & grz$`Total_Shell_Height_Length_mm`*0.0393701<=2.49)]="2.0 - 2.49"
grz$Oyster_Size_Class[which(grz$`Total_Shell_Height_Length_mm`*0.0393701>2.49 & grz$`Total_Shell_Height_Length_mm`*0.0393701<=3.49)]="2.5 - 3.49"
# table(grz$Oyster_Size_Class)
grz$Raw_Data_File="Grizzle_2011-data.xlsx"
grz$Data_Source='Grizzle and Ward 2011'
grz$State="New Hampshire"
grz$Representative_Aquaculture_Oyster_Practice='Off-Bottom with Gear'
# grz$Gear_Class="Bottom"
# grz$Gear_type="off_bottom_cage"
grz$Oyster_Growth_Location_Type='Near-bottom cages'
grz$Date_Oysters_Deployed="08/09/10"#as.Date("08/09/10",format = "%m/%d/%y")
grz$Date_Oysters_Removed=as.Date("11/04/10",format = "%m/%d/%y")
grz$Month_Oysters_Removed=month(grz$Date_Oysters_Removed)
grz$Year_Oysters_Removed=year(grz$Date_Oysters_Removed)
grz$Waterbody_Name=NA
grz$Waterbody_Name[grz$Site=='SQ']='Great Bay'
grz$Waterbody_Name[grz$Site=='NI']='Great Bay'
grz$Waterbody_Name[grz$Site=='LBO']='Little Bay'
grz$Waterbody_Name[grz$Site=='GSS']='Little Bay'
grz$Waterbody_Name[grz$Site=='BMY']='Little Bay'
grz$Waterbody_Name[grz$Site=='AP']='Little Bay'
grz$Location_Index=NA
grz$Location_Index[grz$Site=='SQ']='Squamscott River'
grz$Location_Index[grz$Site=='NI']='Nannie Island'
grz$Location_Index[grz$Site=='LBO']='Little Bay Oyster Company Fox Point'
grz$Location_Index[grz$Site=='GSS']='Granite State Shellfish Oyster River'
grz$Location_Index[grz$Site=='BMY']='Bellamy River'
grz$Location_Index[grz$Site=='AP']='Adams Point'
grz$Near_Waterbody_General_Location="Portsmouth"
grz$Ploidy="Diploid"
grz$Total_Shell_Height_Length_Inches=grz$Total_Shell_Height_Length_mm*0.0393701
grz$Hatchery_produced_or_Wild="Hatchery-produced"
grz$Oyster_Stock='Little Bay Oyster Company'
grz$Location_Index[which(is.na(grz$Location_Index))]="Little Bay Oyster Company Fox Point" # add for initial deployment
grz$Waterbody_Name[is.na(grz$Waterbody_Name)]="Little Bay" #initial deployment
newdf4=bind_rows(newdf3, grz)

### Now add bayer (CT)
# colnames(bayer2)
b2=bayer2 %>% select(-(c(`Length (mm)`,`width (mm)`, `Image J`:`dry tissue + boat weight (g)`, `Comments`, `Shell N (mg/mg)`, `Shell %N`, `Shell C(mg/mg)`)))
colnames(b2)[1]="Number_ID" #"ID"
colnames(b2)[2]="Tissue_TN_g_N_per_g_dw" #"Tissue N (mg/mg)" 
colnames(b2)[3]="Tissue_N_Percent" #"Tissue %N"
colnames(b2)[4]="Tissue_TC_g_C_per_g_dw" #"Tissue C(mg/mg)"
colnames(b2)[5]="Tissue_CN_molar" #"Tissue C:N"
colnames(b2)[6]="Near_Waterbody_General_Location" #City
colnames(b2)[7]="State" #"State"
colnames(b2)[8]="Date_Oysters_Removed" #"Date"
# colnames(b2)[9]="Shell_TN_g_N_per_g_dw" #"Shell N (mg/mg)"
# colnames(b2)[10]="Shell_N_Percent" #"Shell %N"
# colnames(b2)[11]="Shell_TC_g_C_per_g_dw" # "Shell C(mg/mg)"
colnames(b2)[12]="Month_Oysters_Removed"#"Month"
colnames(b2)[13]="Shell_CN_molar" #"Shell C:N"
#[14]="ID #"
colnames(b2)[15]="Total_Shell_Depth_mm" #"depth (mm)"
colnames(b2)[16]="Tissue_Dry_Weight_g" #"dry tissue weight (g)"
colnames(b2)[17]="Shell_Dry_Weight_g" #"dry shell weight (g)"
colnames(b2)[18]="Total_Shell_Height_Length_mm" #"SH"
colnames(b2)[19]="Total_Shell_Width_mm" #"SW"
b2=b2 %>% select(-`ID #`)
b2$Raw_Data_File="Bayer_Greenwich_FARM_data_2019_2020.xlsx"
b2$Representative_Aquaculture_Oyster_Practice="On-Bottom without Gear"
b2$Data_Source="Bayer et al. in prep. 2023"
b2$Location_Index="Greenwich Bay"
b2$Waterbody_Name="Greenwich Bay"
b2$Site="Elias Point"
# b2$Site_within_Study
b2$Oyster_Growth_Location_Type="Subtidal Bottom (Not a Discrete Patch)"# "oyster reef (public grounds)"
b2$Subtidal_Intertidal_WaterColumn_Other="Subtidal"
b2$Ploidy="Diploid"
b2$Oyster_Stock="Stella Mar Oyster Co."
b2$Hatchery_produced_or_Wild="Hatchery-produced"
# b2$Date_Oysters_Deployed
b2$Month_Oysters_Removed=month(b2$Date_Oysters_Removed)
b2$Year_Oysters_Removed=year(b2$Date_Oysters_Removed)
# b2$Season_Oysters_Removed
b2$Total_Shell_Height_Length_Inches=b2$Total_Shell_Height_Length_mm*0.0393701
b2$Oyster_Size_Class=NA
b2$Oyster_Size_Class=ifelse(b2$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
b2$Oyster_Size_Class[which(b2$Total_Shell_Height_Length_Inches>=2.0 & b2$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
b2$Oyster_Size_Class[which(b2$Total_Shell_Height_Length_Inches>2.49 & b2$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
b2$Oyster_Size_Class[which(b2$Total_Shell_Height_Length_Inches>3.49 & b2$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
b2$Oyster_Size_Class[which(b2$Total_Shell_Height_Length_Inches>4.49 & b2$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
b2$Oyster_Size_Class[which(b2$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
# fix sample number issue 310, 310A
b2$Number_ID[which(b2$Number_ID=="310A")]=3100
b2$Number_ID=as.numeric(b2$Number_ID)
b2$Tissue_C_Percent=b2$Tissue_TC_g_C_per_g_dw*100
newdf5=bind_rows(newdf4, b2)



### Now add Sebastiano (NY)
# colnames(Seb.f)
# [1] "Date_collected"            "Date_processed"            "Cage"                      "Bag"                      
# [5] "Oyster_ID"                 "Live_whole_wt_g"           "Shell_height_mm"           "Shell_length_mm"          
# [9] "Shell_width_mm"            "Wet_shell_wt_g"            "Tissue_tray_wt_g"          "Sex"                      
# [13] "Gonad_ranking"             "Tray_plus_dry_tissue_wt_g" "Condition_index"           "Comments"                 
# [17] "percent_N"                 "percent_C"                 "Site"                      "Dry_tissue_wt_g" 
sbs=Seb.f %>% select(-c(Date_processed, Cage, Bag, Wet_shell_wt_g, Tissue_tray_wt_g, Tray_plus_dry_tissue_wt_g, Comments))
colnames(sbs)
# [1] "Date_collected"  "Oyster_ID"       "Live_whole_wt_g" "Shell_height_mm" "Shell_length_mm" "Shell_width_mm" 
# [7] "percent_N"       "percent_C"       "Site"            "Dry_tissue_wt_g"
colnames(sbs)[1]="Date_Oysters_Removed"
colnames(sbs)[2]="Number_ID"
colnames(sbs)[3]="Shell_and_Tissue_Total_Wet_Weight_g"
colnames(sbs)[4]="Total_Shell_Height_Length_mm"
colnames(sbs)[5]="Total_Shell_Width_mm"
colnames(sbs)[6]="Total_Shell_Depth_mm"
#"Sex"
#"Gonad_ranking"
colnames(sbs)[9]="Condition_Index"
colnames(sbs)[10]="Tissue_N_Percent"
colnames(sbs)[11]="Tissue_C_Percent"
colnames(sbs)[12]="Site"
colnames(sbs)[13]="Tissue_Dry_Weight_g"
### columns to add ###
sbs$Raw_Data_File="JB_or_GSB_condition index_data.xls"
sbs$Representative_Aquaculture_Oyster_Practice="Off-Bottom with Gear"
sbs$Data_Source="Sebastiano et al 2015"
sbs$Location_Index=NA
sbs$Location_Index[sbs$Site == "JBE" | sbs$Site == "JBC"| sbs$Site == "JBW"]="Jamaica Bay"
sbs$Location_Index[sbs$Site == "GSBE" | sbs$Site == "GSBC"| sbs$Site == "GSBW"]="Great South Bay"
sbs$State="New York"
sbs$Near_Waterbody_General_Location="Long Island"
sbs$Waterbody_Name="NY Coastal Bays"
sbs$Site_within_Study=sbs$Site
sbs$Oyster_Growth_Location_Type="Near-bottom cages"
sbs$Subtidal_Intertidal_WaterColumn_Other="Subtidal"
sbs$Ploidy="Diploid"
sbs$Oyster_Stock="Fishers Island Oyster Farm"
sbs$Hatchery_produced_or_Wild="Hatchery-produced"
sbs$Date_Oysters_Deployed=NA
sbs$Month_Oysters_Removed=month(sbs$Date_Oysters_Removed)
sbs$Year_Oysters_Removed=year(sbs$Date_Oysters_Removed)
sbs$Season_Oysters_Removed=NA
sbs$Total_Shell_Height_Length_Inches=sbs$Total_Shell_Height_Length_mm*0.0393701
sbs$Oyster_Size_Class=NA
sbs$Oyster_Size_Class=ifelse(sbs$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
sbs$Oyster_Size_Class[which(sbs$Total_Shell_Height_Length_Inches>=2.0 & sbs$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
sbs$Oyster_Size_Class[which(sbs$Total_Shell_Height_Length_Inches>2.49 & sbs$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
sbs$Oyster_Size_Class[which(sbs$Total_Shell_Height_Length_Inches>3.49 & sbs$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
sbs$Oyster_Size_Class[which(sbs$Total_Shell_Height_Length_Inches>4.49 & sbs$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
sbs$Oyster_Size_Class[which(sbs$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
newdf6=bind_rows(newdf5, sbs)


### Now add Levinton (NY, NJ)
Lv=Lev %>% select(-c(Date_analyzed, Cage, Bag, Wet_shell_weight_g, Tray_weight_g, Tray_plus_dry_soft_tissue_g, 
                     Shell_tray_wt_g, Comments))
colnames(Lv)
# [1] "Site"               "Oyster_ID"          "Date_collected"     "Whole_weight_g"     "Shell_height_mm"   
# [6] "Shell_length_mm"    "Shell_Width_mm"     "Dry_shell_weight_g" "Sex"                "Gonad_ranking"     
# [11] "Wet_shell_CI"       "Dry_shell_CI"       "Dry_tissue_wt_g"   
colnames(Lv)[2]="Number_ID"
colnames(Lv)[3]="Date_Oysters_Removed"
colnames(Lv)[4]="Shell_and_Tissue_Total_Wet_Weight_g"
colnames(Lv)[5]="Total_Shell_Height_Length_mm"
colnames(Lv)[6]="Total_Shell_Width_mm"
colnames(Lv)[7]="Total_Shell_Depth_mm"
colnames(Lv)[8]="Shell_Dry_Weight_g"
# colnames(Lv)[9]="Sex"
# colnames(Lv)[10]="Gonad_ranking"
colnames(Lv)[11]="Condition_Index_wet_shell"
colnames(Lv)[12]="Condition_Index_dry_shell"
colnames(Lv)[13]="Tissue_Dry_Weight_g"
# Original_Order
Lv$Raw_Data_File="oyster_Condition Index_data.xls"
Lv$Representative_Aquaculture_Oyster_Practice="Off-Bottom with Gear"
Lv$Data_Source="Levinton J, et al. 2011 PLoS ONE 6(4)"
# Lv$Analysis_ID
Lv$Location_Index=Lv$Site
Lv$State="New York"
Lv$State[Lv$Site==RB]="New Jersey"
# Lv$Waterbody_Type
Lv$Near_Waterbody_General_Location="NY and NJ Coastal Bays"
Lv$Waterbody_Name=NA
Lv$Waterbody_Name[Lv$Site=="RB"]="Raritan Bay"
Lv$Waterbody_Name[Lv$Site=="PF"]="Hudson River"
Lv$Waterbody_Name[Lv$Site=="JB"]="Jamaica Bay"
Lv$Waterbody_Name[Lv$Site=="SI"]="Shelter Island Sound"
Lv$Waterbody_Name[Lv$Site=="I"]="Tappan Zee-Haverstraw Bay"
Lv$Waterbody_Name[Lv$Site=="PT"]="Tappan Zee-Haverstraw Bay"
Lv$Waterbody_Name[Lv$Site=="WI"]="Tappan Zee-Haverstraw Bay"
Lv$Waterbody_Name[Lv$Site=="PM"]="Tappan Zee-Haverstraw Bay"
Lv$Waterbody_Name[Lv$Site=="WE"]="Tappan Zee-Haverstraw Bay"
Lv$Site_within_Study=NA
Lv$Site_within_Study[Lv$Site=="PF"]="Pier 40"
Lv$Site_within_Study[Lv$Site=="JB"]="Jamaica Bay"
Lv$Site_within_Study[Lv$Site=="SI"]="Shelter Island"
Lv$Site_within_Study[Lv$Site=="RB"]="Raritan Bay"
Lv$Site_within_Study[Lv$Site=="I"]="Irvington"
Lv$Site_within_Study[Lv$Site=="PT"]="Piermont"
Lv$Site_within_Study[Lv$Site=="WI"]="Washington Irving Boat Club"
Lv$Site_within_Study[Lv$Site=="PM"]="Philips Manor"
Lv$Site_within_Study[Lv$Site=="WE"]="Westerly Marina Ossining"
Lv$Oyster_Growth_Location_Type="Near-bottom cages"
Lv$Subtidal_Intertidal_WaterColumn_Other="Subtidal"
Lv$Ploidy="Diploid"
Lv$Oyster_Stock="Fishers Island Oyster Farm"
Lv$Hatchery_produced_or_Wild="Hatchery-produced"
Lv$Date_Oysters_Deployed=NA
Lv$Month_Oysters_Removed=month(Lv$Date_Oysters_Removed)
Lv$Year_Oysters_Removed=year(Lv$Date_Oysters_Removed)
Lv$Season_Oysters_Removed=NA
Lv$Total_Shell_Height_Length_Inches=Lv$Total_Shell_Height_Length_mm*0.0393701
Lv$Oyster_Size_Class=NA
Lv$Oyster_Size_Class=ifelse(Lv$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
Lv$Oyster_Size_Class[which(Lv$Total_Shell_Height_Length_Inches>=2.0 & Lv$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
Lv$Oyster_Size_Class[which(Lv$Total_Shell_Height_Length_Inches>2.49 & Lv$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
Lv$Oyster_Size_Class[which(Lv$Total_Shell_Height_Length_Inches>3.49 & Lv$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
Lv$Oyster_Size_Class[which(Lv$Total_Shell_Height_Length_Inches>4.49 & Lv$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
Lv$Oyster_Size_Class[which(Lv$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
newdf7=bind_rows(newdf6, Lv)

### Janine Barr data from NY, NJ
colnames(Barr)
# [1] "Oyster"                   "Shell Height (mm)"        "Dry Tissue Weight (g)"    "Clearance Rate (L/h/g)"  
# [5] "Filtration Rate (mg/h/g)" "Clearance Rate (L/h)"     "Filtration Rate (mg/h)"   "Sex"                     
# [9] "date"      "Location"                 "Sublocation"              "State" 
B2=Barr
colnames(B2)[1]="Number_ID"
B2$Number_ID=as.numeric(B2$Number_ID)
colnames(B2)[2]="Total_Shell_Height_Length_mm"
B2$Total_Shell_Height_Length_mm=as.numeric(B2$Total_Shell_Height_Length_mm)
colnames(B2)[3]="Tissue_Dry_Weight_g"
B2$Tissue_Dry_Weight_g=as.numeric(B2$Tissue_Dry_Weight_g)
colnames(B2)[4]="Clearance_Rate_L_h_g"
colnames(B2)[5]="Filtration_Rate_mg_h_g"
colnames(B2)[6]="Clearance_Rate_L_h"
colnames(B2)[7]="Filtration_Rate_mg_h"
#[8]Sex
colnames(B2)[9]="Date_Oysters_Removed"
B2$Date_Oysters_Removed=as.Date(Barr$date,format = "%m/%d/%Y")
colnames(B2)[10]="Near_Waterbody_General_Location"
colnames(B2)[11]="Location_Index"
## add data
# Original_Order
B2$Raw_Data_File="Rutgers_RM.xlsx"
B2$Data_Source="Barr et al. in press 2023"
# B2$Number_ID
# B2$Analysis_ID
# B2$Location_Index
B2$State[B2$State=="NJ"]="New Jersey"
B2$State[B2$State=="DE"]="Delaware"
# B2$Waterbody_Type
B2$Waterbody_Name=B2$Near_Waterbody_General_Location
B2$Site=B2$Location_Index
B2$Waterbody_Name[B2$Waterbody_Name=="Rehobath Bay"]="Rehoboth Bay"
B2$Representative_Aquaculture_Oyster_Practice=NA
B2$Representative_Aquaculture_Oyster_Practice[B2$Near_Waterbody_General_Location=='Barnegat Bay']="Off-Bottom with Gear"
B2$Representative_Aquaculture_Oyster_Practice[B2$Near_Waterbody_General_Location=='Delaware Bay']="Off-Bottom with Gear"
B2$Representative_Aquaculture_Oyster_Practice[B2$Near_Waterbody_General_Location=='Rehoboth Bay']="Off-Bottom with Gear"
# B2$Site_within_Study
B2$Oyster_Growth_Location_Type=NA
B2$Oyster_Growth_Location_Type[B2$Near_Waterbody_General_Location=='Barnegat Bay']="Floating Rafts"
B2$Oyster_Growth_Location_Type[B2$Near_Waterbody_General_Location=='Delaware Bay']="Near-bottom cages"
B2$Oyster_Growth_Location_Type[B2$Near_Waterbody_General_Location=='Rehoboth Bay']="Floats in water column"
B2$Subtidal_Intertidal_WaterColumn_Other="Subtidal"
B2$Ploidy="Diploid"
# B2$Oyster_Stock
B2$Hatchery_produced_or_Wild="Hatchery produced"
# B2$Date_Oysters_Deployed
# B2$Date_Oysters_Removed
B2$Month_Oysters_Removed=month(B2$Date_Oysters_Removed)
B2$Year_Oysters_Removed=year(B2$Date_Oysters_Removed)
# B2$Season_Oysters_Removed
# B2$Total_Shell_Height_Length_mm
# B2$Total_Shell_Width_mm
# B2$Total_Shell_Depth_mm
# B2$Shell_and_Tissue_Total_Wet_Weight_g
# B2$Shell_Dry_Weight_g
# B2$Tissue_Dry_Weight_g
# B2$Tissue_AFDW_g
# B2$Tissue_CN_molar
# B2$Tissue_C_Percent
# B2$Tissue_TC_g_C_per_g_dw
# B2$Tissue_N_Percent
# B2$Tissue_TN_g_N_per_g_dw
# Tissue_TP_Percent
# Tissue_TP_g_P_per_g_dw
# Shell_CN_molar
# Shell_C_Percent
# Shell_TC_g_C_per_g_dw
# Shell_N_Percent
# Shell_TN_g_N_per_g_dw
# Shell_TP_Percent
# Shell_TP_g_P_per_g_dw
B2$Total_Shell_Height_Length_Inches=B2$Total_Shell_Height_Length_mm*0.0393701
B2$Oyster_Size_Class=NA
B2$Oyster_Size_Class=ifelse(B2$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
B2$Oyster_Size_Class[which(B2$Total_Shell_Height_Length_Inches>=2.0 & B2$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
B2$Oyster_Size_Class[which(B2$Total_Shell_Height_Length_Inches>2.49 & B2$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
B2$Oyster_Size_Class[which(B2$Total_Shell_Height_Length_Inches>3.49 & B2$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
B2$Oyster_Size_Class[which(B2$Total_Shell_Height_Length_Inches>4.49 & B2$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
B2$Oyster_Size_Class[which(B2$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
B2$Clearance_Rate_L_h=as.numeric(B2$Clearance_Rate_L_h)
B2$Filtration_Rate_mg_h_g =as.numeric(B2$Filtration_Rate_mg_h_g )
B2$Clearance_Rate_L_h_g=as.numeric(B2$Clearance_Rate_L_h_g)
B2$Filtration_Rate_mg_h=as.numeric(B2$Filtration_Rate_mg_h)

B2$Location_Index[is.na(B2$Location_Index)]="Rutgers Cape Shore Lab"
B2$Waterbody_Name[B2$Waterbody_Name=="Rehobath Bay"]="Rehoboth Bay"
## Barr
# Delaware Bay NJ, 39.071222, -74.913194
# Barnegat Bay NJ, Rose Cove 39.603000, -74.302000
# Rehoboth Bay DE, Sally Cove 38.650929, -75.126330
newdf8=bind_rows(newdf7, B2)

# colnames(Kiffney)
K1=Kiffney %>% select(-c(Type.x, Type.y))
# [1] "Site"              "ShellHeight_mm"    "WholeWetWeight_g"  "WetTissueWeight_g" "DryTissueWeight_g"
# [6] "WetShellWeight_g"  "DryShellWeight_g" 
colnames(K1)[1]="Location_Index"
K1$Site=NA
K1$Site[K1$Location_Index=="HIF"]="Hurricane Island Foundation"
K1$Waterbody_Name[K1$Location_Index=="HIF"]="Penobscot Bay"
K1$Site[K1$Location_Index=="SSF"]="Spartan Sea Farms"
K1$Waterbody_Name[K1$Location_Index=="SSF"]="Casco Bay"
K1$Site[K1$Location_Index=="DMC"]="Darling Marine Center"
K1$Waterbody_Name[K1$Location_Index=="DMC"]="Damariscotta River"
K1$Site[K1$Location_Index=="POC"]="Pemaquid Oyster Company"
K1$Waterbody_Name[K1$Location_Index=="POC"]="Damariscotta River"
K1Near_Waterbody_General_Location="Coastal Maine"
colnames(K1)[2]="Total_Shell_Height_Length_mm"
colnames(K1)[3]="Shell_and_Tissue_Total_Wet_Weight_g"
colnames(K1)[4]="Tissue_Wet_Weight_g"
colnames(K1)[5]="Tissue_Dry_Weight_g"
colnames(K1)[6]="Shell_Wet_Weight_g"
colnames(K1)[7]="Shell_Dry_Weight_g"
K1$Raw_Data_File="tissue2021.csv + shell2021.csv"
K1$Representative_Aquaculture_Oyster_Practice="Off-Bottom with Gear"
K1$Data_Source="Tom Kiffney unpublished, in prep"
K1$State="Maine"
# K1$Site_within_Study
K1$Oyster_Growth_Location_Type="Floating Rafts"
K1$Subtidal_Intertidal_WaterColumn_Other="Other"
K1$Ploidy="Diploid"
# K1$Oyster_Stock
K1$Hatchery_produced_or_Wild="Hatchery produced"
# K1$Date_Oysters_Deployed=
# K1$Date_Oysters_Removed
# K1$Month_Oysters_Removed
K1$Year_Oysters_Removed=2021
# K1$Season_Oysters_Removed
K1$Total_Shell_Height_Length_Inches=K1$Total_Shell_Height_Length_mm*0.0393701
K1$Oyster_Size_Class=NA
K1$Oyster_Size_Class=ifelse(K1$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
K1$Oyster_Size_Class[which(K1$Total_Shell_Height_Length_Inches>=2.0 & K1$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
K1$Oyster_Size_Class[which(K1$Total_Shell_Height_Length_Inches>2.49 & K1$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
K1$Oyster_Size_Class[which(K1$Total_Shell_Height_Length_Inches>3.49 & K1$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
K1$Oyster_Size_Class[which(K1$Total_Shell_Height_Length_Inches>4.49 & K1$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
K1$Oyster_Size_Class[which(K1$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
newdf9=bind_rows(newdf8, K1)

K2=Kiffney2
colnames(K2)[1]="Location_Index"
# colnames(K2)[2]="Ploidy"
colnames(K2)[3]="Total_Shell_Height_Length_mm"
K2$"Total_Shell_Height_Length_mm"=K2$"Total_Shell_Height_Length_mm"/1000
colnames(K2)[4]="Tissue_Dry_Weight_g"
K2$Raw_Data_File="tissuePloidy2022.csv"
K2$Representative_Aquaculture_Oyster_Practice="Off-Bottom with Gear"
K2$Data_Source="Tom Kiffney unpublished, in prep"
K2$State="Maine"
K2$Site=NA
K2$Waterbody_Name=NA
K2$Site[K2$Location_Index=="DMC"]="Darling Marine Center"
K2$Waterbody_Name[K2$Location_Index=="DMC"]="Damariscotta River"
K2$Site[K2$Location_Index=="POC"]="Pemaquid Oyster Company"
K2$Waterbody_Name[K2$Location_Index=="POC"]="Damariscotta River"
K2Near_Waterbody_General_Location="Coastal Maine"
# K2$Site_within_Study=
K2$Oyster_Growth_Location_Type="Floating Rafts"
K2$Subtidal_Intertidal_WaterColumn_Other="Other"
K2$ploidy="Diploid"
K2$ploidy[K2$Ploidy=="3N"]="Triploid"
K2$Ploidy=K2$ploidy; K2=K2 %>% select(-ploidy)
# K2$Oyster_Stock
K2$Hatchery_produced_or_Wild="Hatchery-produced"
# K2$Date_Oysters_Deployed
# K2$Date_Oysters_Removed
# K2$Month_Oysters_Removed
K2$Year_Oysters_Removed=2021
# K2$Season_Oysters_Removed
K2$Total_Shell_Height_Length_Inches=K2$Total_Shell_Height_Length_mm*0.0393701
K2$Oyster_Size_Class=NA
K2$Oyster_Size_Class=ifelse(K2$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
K2$Oyster_Size_Class[which(K2$Total_Shell_Height_Length_Inches>=2.0 & K2$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
K2$Oyster_Size_Class[which(K2$Total_Shell_Height_Length_Inches>2.49 & K2$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
K2$Oyster_Size_Class[which(K2$Total_Shell_Height_Length_Inches>3.49 & K2$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
K2$Oyster_Size_Class[which(K2$Total_Shell_Height_Length_Inches>4.49 & K2$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
K2$Oyster_Size_Class[which(K2$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
newdf10=bind_rows(newdf9, K2)



colnames(Darr)
# [1] "Date Collected"        "Site"                  "Bag #"                 "Oyster #"
# [5] "Pan #"                 "Comments"              "Height (mm)"           "Length (mm)"          
# [9] "Width (mm)"            "Total Wet Wt (g)"      "Empty Shell Wt (g)"    "Tissue Wet Wt (g)"    
# [13] "Tissue Dry Wt (g)"     "Tissue AFDW (g)"       "Sample Mass (mg)"      "Carbon Content (mg)"  
# [17] "Carbon Content (%)"    "Corrected δ13C"        "Nitrogen Content (mg)" "Nitrogen Content (%)" 
# [21] "Corrected δ15N"        "C/N Ratio" 
dar=Darr %>% select(-c(`Bag #`, Comments,`Pan #`, `Sample Mass (mg)`, `Corrected δ13C`,  `Corrected δ15N`))
# "Shell_and_Tissue_Total_Wet_Weight_g", "Shell_Dry_Weight_g", "Tissue_Wet_Weight_g",
# "Tissue_Dry_Weight_g","Tissue_AFDW_g")
colnames(dar)[1]="Date_Oysters_Removed"
colnames(dar)[2]="Location_Index"
colnames(dar)[3]="Number_ID"
colnames(dar)[4]="Total_Shell_Height_Length_mm"
colnames(dar)[5]="Total_Shell_Width_mm"
colnames(dar)[6]="Total_Shell_Depth_mm"
colnames(dar)[7]="Shell_and_Tissue_Total_Wet_Weight_g"
colnames(dar)[8]="Shell_Dry_Weight_g"
colnames(dar)[9]="Tissue_Wet_Weight_g"
colnames(dar)[10]="Tissue_Dry_Weight_g"
colnames(dar)[11]="Tissue_AFDW_g"
colnames(dar)[12]="Tissue_TC_g_C_per_g_dw"
dar$Tissue_TC_g_C_per_g_dw=Darr$`Carbon Content (mg)`/Darr$`Sample Mass (mg)`
colnames(dar)[13]="Tissue_C_Percent"
colnames(dar)[14]="Tissue_TN_g_N_per_g_dw"
dar$Tissue_TN_g_C_per_g_dw=Darr$`Nitrogen Content (mg)`/Darr$`Sample Mass (mg)`
colnames(dar)[15]="Tissue_N_Percent"
colnames(dar)[16]="Tissue_CN_molar"
dar$Raw_Data_File="NCOysterSizeData_DarrowKInsella_forJulieRose.xlsx"
dar$Data_Source="Darrow ES & Kinsella JD, unpublished"
dar$State="North Carolina"
dar$Near_Waterbody_General_Location=NA
dar$Near_Waterbody_General_Location[dar$Location_Index=="F1"]="Masonboro Island"
dar$Near_Waterbody_General_Location[dar$Location_Index=="F2"]="Masonboro Island"
dar$Near_Waterbody_General_Location[dar$Location_Index=="F3"]="New River"
dar$Waterbody_Name="Masonboro Island"
dar$Waterbody_Name[dar$Location_Index=="F3"]="New River"
dar$Site=NA
dar$Site[dar$Location_Index=="F1"]="Farm 1"
dar$Site[dar$Location_Index=="F2"]="Farm 2"
dar$Site[dar$Location_Index=="F3"]="Farm 3"
# dar$Site_within_Study
dar$Oyster_Growth_Location_Type=NA
dar$Oyster_Growth_Location_Type[dar$Location_Index=="F1"]="Off-Bottom with Gear"
dar$Oyster_Growth_Location_Type[dar$Location_Index=="F2"]="On-Bottom without Gear"
dar$Oyster_Growth_Location_Type[dar$Location_Index=="F3"]="Off-Bottom with Gear"
dar$Representative_Aquaculture_Oyster_Practice=NA
dar$Representative_Aquaculture_Oyster_Practice[dar$Location_Index=="F1"]="Floating Rafts"
dar$Representative_Aquaculture_Oyster_Practice[dar$Location_Index=="F2"]="bottom cage design"
dar$Representative_Aquaculture_Oyster_Practice[dar$Location_Index=="F3"]="Near-bottom cages"

dar$Subtidal_Intertidal_WaterColumn_Other="Unknown"
dar$Ploidy="Diploid"
# dar$Oyster_Stock
# dar$Hatchery_produced_or_Wild
# dar$Date_Oysters_Deployed
# dar$Date_Oysters_Removed
dar$Month_Oysters_Removed=month(dar$Date_Oysters_Removed)
dar$Year_Oysters_Removed=year(dar$Date_Oysters_Removed)
dar$Total_Shell_Height_Length_Inches=as.numeric(dar$Total_Shell_Height_Length_mm)*0.0393701
dar$Oyster_Size_Class=NA
dar$Oyster_Size_Class=ifelse(dar$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
dar$Oyster_Size_Class[which(dar$Total_Shell_Height_Length_Inches>=2.0 & dar$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
dar$Oyster_Size_Class[which(dar$Total_Shell_Height_Length_Inches>2.49 & dar$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
dar$Oyster_Size_Class[which(dar$Total_Shell_Height_Length_Inches>3.49 & dar$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
dar$Oyster_Size_Class[which(dar$Total_Shell_Height_Length_Inches>4.49 & dar$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
dar$Oyster_Size_Class[which(dar$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
dar$Total_Shell_Height_Length_mm=as.numeric(dar$Total_Shell_Height_Length_mm)
dar$Total_Shell_Width_mm=as.numeric(dar$Total_Shell_Width_mm)
dar$Total_Shell_Depth_mm=as.numeric(dar$Total_Shell_Depth_mm)
dar$Shell_and_Tissue_Total_Wet_Weight_g=as.numeric(dar$Shell_and_Tissue_Total_Wet_Weight_g)
dar$Shell_Dry_Weight_g=as.numeric(dar$Shell_Dry_Weight_g)
dar$Tissue_Wet_Weight_g=as.numeric(dar$Tissue_Wet_Weight_g)
dar$Tissue_Dry_Weight_g=as.numeric(dar$Tissue_Dry_Weight_g)
dar$Tissue_AFDW_g=as.numeric(dar$Tissue_AFDW_g)
newdf11=bind_rows(newdf10, dar)


colnames(Ayvazian)
avz=Ayvazian
colnames(avz)[1]="Year_Oysters_Removed"
colnames(avz)[2]="Season_Oysters_Removed"
colnames(avz)[4]="Total_Shell_Height_Length_mm"
colnames(avz)[5]="Shell_Dry_Weight_g"
colnames(avz)[6]="Tissue_Wet_Weight_g"
colnames(avz)[7]="Tissue_Dry_Weight_g"
avz$Raw_Data_File="Ninigret oyster dry weights_height.xlsx"
avz$Representative_Aquaculture_Oyster_Practice="On-Bottom without Gear"
avz$Data_Source="Ayvazian et al. TNC unpublished"
avz$Location_Index="Ninigret Pond"
avz$State="Rhode Island"
avz$Near_Waterbody_General_Location="Charlestown"
avz$Waterbody_Name="Ninigret Pond"
# avz$Site_within_Study
avz$Oyster_Growth_Location_Type="Subtidal Bottom (Not a Discrete Patch)"
avz$Subtidal_Intertidal_WaterColumn_Other="subtidal"
avz$Ploidy="Diploid"
# avz$Oyster_Stock
avz$Hatchery_produced_or_Wild="Wild"
# avz$Date_Oysters_Deployed
# avz$Date_Oysters_Removed
# avz$Month_Oysters_Removed
avz$Total_Shell_Height_Length_Inches=as.numeric(avz$Total_Shell_Height_Length_mm)*0.0393701
avz$Oyster_Size_Class=NA
avz$Oyster_Size_Class=ifelse(avz$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
avz$Oyster_Size_Class[which(avz$Total_Shell_Height_Length_Inches>=2.0 & avz$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
avz$Oyster_Size_Class[which(avz$Total_Shell_Height_Length_Inches>2.49 & avz$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
avz$Oyster_Size_Class[which(avz$Total_Shell_Height_Length_Inches>3.49 & avz$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
avz$Oyster_Size_Class[which(avz$Total_Shell_Height_Length_Inches>4.49 & avz$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
avz$Oyster_Size_Class[which(avz$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
newdf12=bind_rows(newdf11, avz)

colnames(Ayvazian2)
avz2=Ayvazian2 %>% select(-Tissue)
colnames(avz2)[1]="Number_ID"
colnames(avz2)[2]="Month_Oysters_Removed"
avz2$Month_Oysters_Removed=match(avz2$Month_Oysters_Removed, month.name)
colnames(avz2)[3]="Site_within_Study"
colnames(avz2)[4]="Total_Shell_Height_Length_mm"
colnames(avz2)[5]="Tissue_Dry_Weight_g"
avz2$Raw_Data_File="Green Hill Length_dry wgt.xlsx"
avz2$Representative_Aquaculture_Oyster_Practice="On-Bottom without Gear"
avz2$Data_Source="Ayvazian et al. TNC unpublished"
avz2$Location_Index="Green Hill Pond"
avz2$State="Rhode Island"
avz2$Near_Waterbody_General_Location="Charlestown"
avz2$Waterbody_Name="Green Hill Pond"
avz2$Site="Green Hill Pond"
avz2$Oyster_Growth_Location_Type="Subtidal Bottom (Not a Discrete Patch)"
avz2$Subtidal_Intertidal_WaterColumn_Other="subtidal"
avz2$Ploidy="Diploid"
# avz2$Oyster_Stock
avz2$Hatchery_produced_or_Wild="Wild"
# avz2$Date_Oysters_Deployed
avz2$Year_Oysters_Removed=2015
# avz2$Month_Oysters_Removed
avz2$Total_Shell_Height_Length_Inches=as.numeric(avz2$Total_Shell_Height_Length_mm)*0.0393701
avz2$Oyster_Size_Class=NA
avz2$Oyster_Size_Class=ifelse(avz2$Total_Shell_Height_Length_Inches < 2.0, "< 2.0", NA)
avz2$Oyster_Size_Class[which(avz2$Total_Shell_Height_Length_Inches>=2.0 & avz2$Total_Shell_Height_Length_Inches<=2.49)]="2.0 - 2.49"
avz2$Oyster_Size_Class[which(avz2$Total_Shell_Height_Length_Inches>2.49 & avz2$Total_Shell_Height_Length_Inches<=3.49)]="2.5 - 3.49"
avz2$Oyster_Size_Class[which(avz2$Total_Shell_Height_Length_Inches>3.49 & avz2$Total_Shell_Height_Length_Inches<=4.49)]="3.5 - 4.49"
avz2$Oyster_Size_Class[which(avz2$Total_Shell_Height_Length_Inches>4.49 & avz2$Total_Shell_Height_Length_Inches<=5.49)]="4.5 - 5.49"
avz2$Oyster_Size_Class[which(avz2$Total_Shell_Height_Length_Inches>5.49)]="≥ 5.5"
newdf13=bind_rows(newdf12, avz2)

Main=newdf13
# 20230823
# Lisa Kellogg is fine with us using the "Kellogg-Choptank" data set. It's about 300 points.
# She's writing up the "Kellogg-Hillcrest" and "Kellogg-Onancock"
# so if you can pull those out of the CB-C data then we should be good to go!

# > unique(CB$Data_Source)
# [1] "Kellogg-Onancock"  "Higgins-2011"      "Parker-unpubl"     "Kingsley-Smith"    "Higgins-Choptank" 
# [6] "Higgins-Lynnhaven" "Ross-LA"           "Ross-Monitor"      "Kellogg-Harris"    "Kellogg-Choptank" 
# [11] "Liddel-2008"       "Powell_Mann"      
# > unique(CB$Raw_Data_File)
# [1] "HillcrestCv Tissue L to Biomass"  "Higgins_Oyster Biomass"           "Parker_Master File_beforeNov2016"
# [4] "Parker_Master File_Nov2016"       "Parker_Jan-Mar2017_data"          "Parker_Apr-Jul2017_data"         
# [7] "Kingsley_Data_CV_Only"            "Parker_Dec2016"                   "Ross_Data"                       
# [10] "HarrisOyster L-Biom Regressions"  "ChoptankOysters-OutliersRemoved"  "Liddel_Condition Index"          
# [13] "Powell_Mann" 

### Kellogg-Onancock removed
Main=Main[Main$Data_Source != "Kellogg-Onancock",]
Main=Main[Main$Data_Source != "Kellogg-Harris",]

### FIXES these can be removed as all changes have been made to Main file and to above code ###
### fix Kiffney2 mg -> g (added to Main file, added fix to build for K2)
# Main$Tissue_Dry_Weight_g[Main$Raw_Data_File=="tissuePloidy2022.csv"]=Main$Tissue_Dry_Weight_g[Main$Raw_Data_File=="tissuePloidy2022.csv"]/1000
## fix states for Barr (done above now)
# Main$State[Main$State=="NJ"]="New Jersey"
# Main$State[Main$State=="DE"]="Delaware"

### fix Reitsma shell C percent for low values (Organic C vs total C) (added to r3 above)
# Main$Shell_Organic_C_Percent=NA
# Main$Shell_Organic_C_Percent[Main$Shell_C_Percent<4 & !is.na(Main$Shell_C_Percent)]=Main$Shell_C_Percent[Main$Shell_C_Percent<4 & !is.na(Main$Shell_C_Percent)]
# Main$Shell_C_Percent[Main$Shell_C_Percent<4 & !is.na(Main$Shell_C_Percent)]=NA

### fix Barr "Near bottom cages" -> "Near-bottom cages", (added above)
# Main$Oyster_Growth_Location_Type[Main$Oyster_Growth_Location_Type=="Near bottom cages"]="Near-bottom cages"

### fix Darrow growth practices with updated information (added above)
# table(Main$Oyster_Growth_Location_Type[Main$Data_Source=="Darrow ES & Kinsella JD, unpublished"])
# Main$Oyster_Growth_Location_Type[which(Main$Data_Source=="Darrow ES & Kinsella JD, unpublished" & Main$Location_Index=="F1")]="Off-Bottom with Gear"
# Main$Oyster_Growth_Location_Type[which(Main$Data_Source=="Darrow ES & Kinsella JD, unpublished" & Main$Location_Index=="F2")]="On-Bottom without Gear"
# Main$Oyster_Growth_Location_Type[which(Main$Data_Source=="Darrow ES & Kinsella JD, unpublished" & Main$Location_Index=="F3")]="Off-Bottom with Gear"
# Main$Representative_Aquaculture_Oyster_Practice[which(Main$Data_Source=="Darrow ES & Kinsella JD, unpublished" & Main$Location_Index=="F1")]="Floating Rafts"
# Main$Representative_Aquaculture_Oyster_Practice[which(Main$Data_Source=="Darrow ES & Kinsella JD, unpublished" & Main$Location_Index=="F2")]="bottom cage design"
# Main$Representative_Aquaculture_Oyster_Practice[which(Main$Data_Source=="Darrow ES & Kinsella JD, unpublished" & Main$Location_Index=="F1")]="Near-bottom cages"

### issues with colnames(Main) 20230908, fixed and added to above
#[7] "Waterbody_Type"                             "Near_Waterbody__General_Location"  
#[15] "Hatchery_produced_or_Wild"                  "Hatchery-produced_or_Wild" 
#[25] "Shell&Tissue_Total_Wet_Weight_g"            "Shell_Dry_Weight_g"   
#[31] "Tissue_TC_g_C_per_ g_dw"                    "Tissue_N_Percent"  
#[37] "Shell_C_Percent"                            "Shell_TC_g_C_per_ g_dw"
#[49] "Tissue_TC_g_C_per_g_dw"                     "Sex"     
### combine 2 Tissue_TC
# test1=is.na(Main$`Tissue_TC_g_C_per_ g_dw`)
# test2=is.na(Main$`Tissue_TC_g_C_per_g_dw`)
# test=Main[which(test1==F & test2==F),]
# test=Main %>% mutate(mycol = coalesce(`Tissue_TC_g_C_per_ g_dw`,`Tissue_TC_g_C_per_g_dw`)) %>%
#   select( mycol)
# Main$Tissue_TC_g_C_per_g_dw=test
# Main=Main %>% select(-`Tissue_TC_g_C_per_ g_dw`)
# colnames(Main)[37]="Shell_TC_g_C_per_g_dw" #"Shell_TC_g_C_per_ g_dw" 
# colnames(Main)[8]="Near_Waterbody_General_Location" #"Near_Waterbody__General_Location"
# test1=is.na(Main$Hatchery_produced_or_Wild)
# test2=is.na(Main$`Hatchery-produced_or_Wild`)
# test=Main[which(test1==F & test2==F),]
# colnames(Main)
# colnames(Main)[15]="Oyster_Stock" # carryover from mistake in newdf, fixed above
# colnames(Main)[16]="Hatchery_produced_or_Wild"
# colnames(Main)[25]="Shell_and_Tissue_Total_Wet_Weight_g"  #"Shell&Tissue_Total_Wet_Weight_g"

### update Bayer shell N and C (values were rounded), fixed above
# test=as.data.frame(Main$Shell_N_Percent[which(Main$Data_Source=="Bayer et al. in prep. 2023")])
# colnames(test)='og'
# test$new=NA
# test$new=b2shell$Shell_N_Percent
# Main$Shell_TN_g_N_per_g_dw[which(Main$Data_Source=="Bayer et al. in prep. 2023")]=b2shell$Shell_TN_g_N_per_g_dw
# Main$Shell_N_Percent[which(Main$Data_Source=="Bayer et al. in prep. 2023")]=b2shell$Shell_N_Percent
# Main$Shell_TC_g_C_per_g_dw[which(Main$Data_Source=="Bayer et al. in prep. 2023")]=b2shell$Shell_TC_g_C_per_g_dw
# Main$Shell_C_Percent[which(Main$Data_Source=="Bayer et al. in prep. 2023")]=b2shell$Shell_C_Percent

# Add label to search on data included in CB BMP report
s1=match("Poach et al. in prep 2023", Main$Data_Source) # start of Poach data (after end of CB)
Main$Panel=F
Main$Panel[1:s1-1]=T

##fix duplicated Oyster_Stock; fixed 20230912 added to above
# duplicated(names(Main))
# unique(Main[,15])
# unique(Main[,45])
# OS=coalesce(Main[,15], Main[,45])
# sum(is.na(OS))
# sum(is.na(Main[,15]))
# sum(is.na(Main[,45]))
# test15=is.na(Main[,15])
# test45=is.na(Main[,45])
# test=Main[which(test15==F & test45==F),]
# unique(test$Data_Source)
# # [1] "Grizzle and Ward 2011"                 "Levinton J, et al. 2011 PLoS ONE 6(4)"
# test2=test[,c(15,45)]
# table(test[which(test$Data_Source=="Grizzle and Ward 2011"),15])
# table(test[which(test$Data_Source=="Levinton J, et al. 2011 PLoS ONE 6(4)"),15])
# table(Main[which(Main$Data_Source=="Grizzle and Ward 2011"),15])
# table(Main[which(Main$Data_Source=="Levinton J, et al. 2011 PLoS ONE 6(4)"),15])
# Main[which(Main$Data_Source=="Levinton J, et al. 2011 PLoS ONE 6(4)"),15]=NA
# Main[which(Main$Data_Source=="Grizzle and Ward 2011"),15]=NA
# OS=coalesce(Main[,15], Main[,45])
# unique(Main[,15])
# unique(Main[,45])
# unique(OS)
# Main=Main[,-15]
# Main$Oyster_Stock=OS
# test=Main[which(Main$Oyster_Stock=="Hatchery-produced"),]
# unique(test$Data_Source)
# # [1] "Bayer et al. in prep. 2023" "Sebastiano et al 2015"
# test=Main[which(Main$Data_Source=="Bayer et al. in prep. 2023"),]
# unique(test$Oyster_Stock)
# # 1 Hatchery-produced
# test=Main[which(Main$Data_Source=="Sebastiano et al 2015"),]
# unique(test$Oyster_Stock)
# # 1 Hatchery-produced
# OS2=data.frame(OS)
# Main$Oyster_Stock=OS2$Oyster_Stock
# Main$Oyster_Stock[which(Main$Data_Source=="Sebastiano et al 2015")]="Fishers Island Oyster Farm"
# Main$Oyster_Stock[Main$Data_Source=="Bayer et al. in prep. 2023"]="Stella Mar Oyster Co."
# Main$Hatchery_produced_or_Wild[Main$Data_Source=="Bayer et al. in prep. 2023"]="Wild"

###Fix error in Darrow data set F3 was set to F1 for representative_aqua... corrected above
# Main$Oyster_Growth_Location_Type[Main$Location_Index=="F1"]="Off-Bottom with Gear"
# Main$Oyster_Growth_Location_Type[Main$Location_Index=="F2"]="On-Bottom without Gear"
# Main$Oyster_Growth_Location_Type[Main$Location_Index=="F3"]="Off-Bottom with Gear"
# Main$Representative_Aquaculture_Oyster_Practice[Main$Location_Index=="F1"]="Floating Rafts"
# Main$Representative_Aquaculture_Oyster_Practice[Main$Location_Index=="F2"]="bottom cage design"
# Main$Representative_Aquaculture_Oyster_Practice[Main$Location_Index=="F3"]="Near-bottom cages"

#Fix Grizzle and Ward pre-deployment oyster location
# Main$Waterbody_Name[is.na(Main$Waterbody_Name)]="Little Bay"
# Main$Location_Index[which(is.na(Main$Location_Index)&(Main$Data_Source=="Grizzle and Ward 2011"))]="Little Bay Oyster Company Fox Point"
#Add location index to Barr Delaware Bay
# Main$Location_Index[which(is.na(Main$Location_Index)&(Main$Data_Source=="Barr et al. submitted 2022"))]="Rutgers Cape Shore Lab"
# Main$Waterbody_Name[Main$Waterbody_Name=="Rehobath Bay"]="Rehoboth Bay"

# Main$State[Main$Waterbody_Name=="Raritan Bay"]="New Jersey"

unique(Main$State)
Main$st_abrv=NA
# [1] "Virginia"       "Maryland"       "Massachusetts"  "New Hampshire"  "Connecticut"    "New York"       "New Jersey"    
# [8] "Delaware"       "Maine"          "North Carolina" "Rhode Island"  
Main$st_abrv[Main$State=="Virginia"]="VA"
Main$st_abrv[Main$State=="Maryland"]="MD"
Main$st_abrv[Main$State=="Massachusetts"]="MA"
Main$st_abrv[Main$State=="New Hampshire"]="NH"
Main$st_abrv[Main$State=="Connecticut"]="CT"
Main$st_abrv[Main$State=="New York"]="NY"
Main$st_abrv[Main$State=="New Jersey"]="NJ"
Main$st_abrv[Main$State=="Delaware"]="DE"
Main$st_abrv[Main$State=="Maine"]="ME"
Main$st_abrv[Main$State=="North Carolina"]="NC"
Main$st_abrv[Main$State=="Rhode Island"]="RI"
table(Main$st_abrv) 

### Fix missing C tissue percent for Bayer
# test=Main$Tissue_TC_g_C_per_g_dw[which(Main$Data_Source=="Bayer et al. in prep. 2023")]
# test=Main[Main$Data_Source=="Bayer et al. in prep. 2023",]
# test$Tissue_C_Percent=test$Tissue_TC_g_C_per_g_dw*100
# > t2=as.matrix(test$Tissue_C_Percent)
# > t3=as.data.frame(t2)
# > colnames(t3)=NULL
# > save(Main, file=paste(wd, dat, "_Calculator_Main_oyster_data.rdata", sep=''))
# > wd
# [1] "C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
# > badC=Main
# > load("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/20230920_Calculator_Main_oyster_data.rdata")
# > badC2=badC %>% select(-Tissue_C_Percent)
# > badC2$Tissue_C_Percent=Main$Tissue_C_Percent
# Main=badC2
# Main$Tissue_C_Percent[which(Main$Data_Source=="Bayer et al. in prep. 2023")]=t2
# class(Main$Tissue_C_Percent)
# class(Main$Tissue_TC_g_C_per_g_dw)

#update status of Barr paper
# Main$Data_Source[Main$Data_Source=="Barr et al. submitted 2022"]="Barr et al. in press 2023"

#Change Bayer bottom type (fixed above)
# Main$Oyster_Growth_Location_Type[Main$Data_Source=="Bayer et al. in prep. 2023"]="Subtidal Bottom (Not a Discrete Patch)"

# this is causing unknown NAs not sure why
test=Main[Main$Oyster_Stock=="Hatchery produced",]
unique(test$Data_Source)
test2=test[is.na(test$Data_Source),]

## add new column
Main$Gear_Class=NA
Main$Gear_Class[which(Main$Data_Source=="Levinton J, et al. 2011 PLoS ONE 6(4)")]="Floating"
Main$Gear_Class[which(Main$Data_Source=="Sebastiano et al 2015")]="Floating"
Main$Gear_Class[which(Main$Data_Source=="Ayvazian et al. TNC unpublished")]="No Gear"
Main$Gear_Class[which((Main$Data_Source=="Darrow ES & Kinsella JD, unpublished") & (Main$Location_Index=='F1'))]="Floating"
Main$Gear_Class[which((Main$Data_Source=="Darrow ES & Kinsella JD, unpublished") & (Main$Location_Index=='F2'))]="No Gear"
Main$Gear_Class[which((Main$Data_Source=="Darrow ES & Kinsella JD, unpublished") & (Main$Location_Index=='F3'))]="Bottom"
Main$Gear_Class[which(Main$Data_Source=="Grizzle and Ward 2011")]="Bottom"
Main$Gear_Class[which(Main$Data_Source=="Poach et al. in prep 2023")]="Bottom"
Main$Gear_Class[which(Main$Data_Source=="Bayer et al. in prep. 2023")]="No Gear"
Main$Gear_Class[which(Main$Data_Source=="Tom Kiffney unpublished, in prep")]="Floating"
Main$Gear_Class[which((Main$Data_Source=="Barr et al. in press 2023")&(Main$Oyster_Growth_Location_Type=="Floating Rafts"))]="Floating"
Main$Gear_Class[which((Main$Data_Source=="Barr et al. in press 2023")&(Main$Oyster_Growth_Location_Type=="Floats in water column"))]="Floating"
Main$Gear_Class[which((Main$Data_Source=="Barr et al. in press 2023")&(Main$Oyster_Growth_Location_Type=="Near-bottom cages"))]="Bottom"
Main$Gear_Class[which((Main$Data_Source=="Reitsma et al. 2017") & (Main$Oyster_Growth_Location_Type=="Near-bottom cages"))]="Bottom"
Main$Gear_Class[which((Main$Data_Source=="Reitsma et al. 2017") & (Main$Oyster_Growth_Location_Type=="Reef"))]="No Gear"

## add new column
Main$Waterbody_Region=NA
Main$Waterbody_Region[which((Main$st_abrv=="NY") & (Main$Waterbody_Name=="NY Coastal Bays"))]="South Shore Bay"
Main$Waterbody_Region[which((Main$st_abrv=="NY") & (Main$Waterbody_Name=="Hudson River"))]="Hudson River"
Main$Waterbody_Region[which((Main$st_abrv=="NY") & (Main$Waterbody_Name=="Jamaica Bay"))]="Jamaica Bay"
Main$Waterbody_Region[which((Main$st_abrv=="NY") & (Main$Location_Index=="Jamaica Bay"))]="Jamaica Bay" # Sebastiano
Main$Waterbody_Region[which((Main$st_abrv=="NY") & (Main$Location_Index=="Great South Bay"))]="South Shore Bay" # Sebastiano
Main$Waterbody_Region[which((Main$st_abrv=="NY") & (Main$Waterbody_Name=="Shelter Island Sound"))]="Peconic Bay"
Main$Waterbody_Region[which((Main$st_abrv=="NY") & (Main$Waterbody_Name=="Tappan Zee-Haverstraw Bay"))]="Hudson River"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Wellfleet Harbor"))]="Cape Cod Bay"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Pocasset River"))]="Buzzards Bay"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Little Bay"))]="Buzzards Bay"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Pleasant Bay"))]="Atlantic Ocean"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Barnstable Harbor"))]="Cape Cod Bay"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Oyster Pond"))]="Nantucket Sound"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Buzzards Bay"))]="Buzzards Bay"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Popponesset Bay"))]="Nantucket Sound"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Mashpee River"))]="Nantucket Sound"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Swan River"))]="Nantucket Sound"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Duxbury Bay"))]="Cape Cod Bay"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Phinneys Harbor"))]="Buzzards Bay"
Main$Waterbody_Region[which((Main$st_abrv=="MA") & (Main$Waterbody_Name=="Pocasset Harbor"))]="Buzzards Bay"
Main$Waterbody_Region[which(Main$st_abrv=="VA")]="Chesapeake Bay"
Main$Waterbody_Region[which(Main$st_abrv=="MD")]="Chesapeake Bay"
Main$Waterbody_Region[which(Main$st_abrv=="NH")]="Great Bay"
Main$Waterbody_Region[which(Main$st_abrv=="CT")]="Long Island Sound"
Main$Waterbody_Region[which(Main$st_abrv=="NJ")]=Main$Waterbody_Name[Main$st_abrv=="NJ"]
Main$Waterbody_Region[which(Main$st_abrv=="DE")]=Main$Waterbody_Name[Main$st_abrv=="DE"]
Main$Waterbody_Region[which(Main$st_abrv=="ME")]=Main$Waterbody_Name[Main$st_abrv=="ME"]
Main$Waterbody_Region[which(Main$st_abrv=="NC")]=Main$Waterbody_Name[Main$st_abrv=="NC"]
Main$Waterbody_Region[which(Main$st_abrv=="RI")]="RI Coastal Ponds"




# Add estuary information
unique(Main$Waterbody_Name[Main$st_abrv=="NY"])

dt=now()
dat=as.character(dt, format="%Y%m%d")
save(Main, file=paste(wd, dat, "_Calculator_Main_oyster_data.rdata", sep=''))
