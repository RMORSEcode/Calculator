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

### columns to add ###
# xxx$Raw_Data_File
# xxx$Representative_Aquaculture_Oyster_Practice
# xxx$Data_Source
# xxx$Location_Index
# xxx$State
# xxx$Near_Waterbody__General_Location
# xxx$Waterbody_Name
# xxx$Site
# xxx$Site_within_Study
# xxx$Oyster_Growth_Location_Type
# xxx$Subtidal_Intertidal_WaterColumn_Other
# xxx$Ploidy
# xxx$Oyster_Stock
# xxx$Hatchery-produced_or_Wild
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
PCB2=select(PCB, -is.outlier)
### add missing cols
PCB2$Raw_Data_File='CB_oyster_nutrient_data_edited.xlsx'
# PCB2$Representative_Aquaculture_Oyster_Practice
PCB2$Data_Source="Poach et al. in prep 2023"
# PCB2$Location_Index
# PCB2$State
# PCB2$Near_Waterbody__General_Location
# PCB2$Waterbody_Name
# PCB2$Site
PCB2$Site_within_Study
# PCB2$Oyster_Growth_Location_Type
PCB2$Subtidal_Intertidal_WaterColumn_Other
# PCB2$Ploidy
PCB2$Oyster_Stock=NA
PCB2$Oyster_Stock[PCB2$State=='Virginia']='Rappahannock Oyster Co.'
PCB2$Oyster_Stock[PCB2$State=='Maryland']='Orchard Point Oyster Co. LLC'
# PCB2$Hatchery-produced_or_Wild
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
colnames(r2)[4]="Near_Waterbody__General_Location"
colnames(r2)[5]="Waterbody_Name"
# colnames(r2)[6]="Site"
colnames(r2)[7]="Hatchery-produced_or_Wild"
colnames(r2)[8]="Volume_ml" #"Volume (ml)" 
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
grz$Near_Waterbody__General_Location="Portsmouth"
grz$Ploidy="Diploid"
grz$Total_Shell_Height_Length_Inches=grz$Total_Shell_Height_Length_mm*0.0393701
grz$`Hatchery-produced_or_Wild`="?"
grz$Oyster_Stock='Little Bay Oyster Company'
newdf4=bind_rows(newdf3, grz)

### Now add bayer (CT)
# colnames(bayer2)
b2=bayer2 %>% select(-(c(`Length (mm)`,`width (mm)`, `Image J`:`dry tissue + boat weight (g)`, `Comments`)))
colnames(b2)[1]="Number_ID" #"ID"
colnames(b2)[2]="Tissue_TN_g_N_per_g_dw" #"Tissue N (mg/mg)" 
colnames(b2)[3]="Tissue_N_Percent" #"Tissue %N"
colnames(b2)[4]="Tissue_TC_g_C_per_g_dw" #"Tissue C(mg/mg)"
colnames(b2)[5]="Tissue_CN_molar" #"Tissue C:N"
colnames(b2)[6]="Near_Waterbody__General_Location" #City
colnames(b2)[7]="State" #"State"
colnames(b2)[8]="Date_Oysters_Removed" #"Date"
colnames(b2)[9]="Shell_TN_g_N_per_g_dw" #"Shell N (mg/mg)"
colnames(b2)[10]="Shell_N_Percent" #"Shell %N"
colnames(b2)[11]="Shell_TC_g_C_per_ g_dw" # "Shell C(mg/mg)"
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
b2$Oyster_Growth_Location_Type="oyster reef (public grounds)"
b2$Subtidal_Intertidal_WaterColumn_Other="Subtidal"
b2$Ploidy="Diploid"
# b2$Oyster_Stock
b2$`Hatchery-produced_or_Wild`="Hatchery-produced"
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
newdf5=bind_rows(newdf4, b2)



### Now add Sebastiano (NY)
colnames(Seb2)

[1] "Site"
"Date"
"Cage.x"
"MeanSH"                
[5] "Cage.y"
"Cage Mean (dry weight)"

### columns to add ###
# xxx$Raw_Data_File
# xxx$Representative_Aquaculture_Oyster_Practice
# xxx$Data_Source
# xxx$Location_Index
# xxx$State
# xxx$Near_Waterbody__General_Location
# xxx$Waterbody_Name
# xxx$Site
# xxx$Site_within_Study
# xxx$Oyster_Growth_Location_Type
# xxx$Subtidal_Intertidal_WaterColumn_Other
# xxx$Ploidy
# xxx$Oyster_Stock
# xxx$Hatchery-produced_or_Wild
# xxx$Date_Oysters_Deployed
# xxx$Date_Oysters_Removed
# xxx$Month_Oysters_Removed
# xxx$Year_Oysters_Removed
# xxx$Season_Oysters_Removed
# xxx$Total_Shell_Height_Length_Inches
# xxx$Oyster_Size_Class






