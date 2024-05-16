### files associated with this Zenodo pulblication are in "C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Zenodo/CBay/"
## 20240516 this file used to process and clean data file submitted to Zenodo 5/16/2024
## DOI 10.5281/zenodo.11205266

library(tidyverse)
library(readxl)
wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
s1=match("Poach et al. in prep 2023", Main$Data_Source) # start of Poach data (after end of CB)
CB2=Main[1:s1-1,]
CB2=CB2 %>% filter(Data_Source!="Powell_Mann") # drop Powell_Mann ( Mann, Southworth, and Wesson unpubl.)
# test=as.matrix(CB2$Tissue_TC_g_C_per_g_dw)
# colnames(test)=NULL
# CB2=CB2 %>% dplyr::select(-Tissue_TC_g_C_per_g_dw)
# CB2$Tissue_TC_g_C_per_g_dw=test[,1]
### fix entries
CB2$Site[which(CB2$Site=="budder")]="budden"
CB2$Subtidal_Intertidal_WaterColumn_Other[which(CB2$Subtidal_Intertidal_WaterColumn_Other=="Other")]=NA
CB2$Hatchery_produced_or_Wild[which(CB2$Hatchery_produced_or_Wild=="?")]=NA
CB2$Season_Oysters_Removed[which(CB2$Season_Oysters_Removed=="?")]=NA
CB2$Oyster_Stock[which(CB2$Oyster_Stock=="?")]=NA

#add lat/long
JRstat=read_xlsx(paste(wd,"Geographic Information_Full_updated on 7-10-17.xlsx",sep=''), sheet = "Chesapeake Bay")
JR=JRstat %>% filter(!(Raw_Data_File %in% c("Powell_Mann", "Kellogg-Harris", "Kellogg-Onancock"))) # drop data not included in zenodo 20240508
JR2=JR %>% select(Data_Source, Waterbody_Name, Site, `Latitude_Decimal Degree`, `Longitude_Decimal Degree`, `Salinity classification`)
colnames(JR2)[4]="Latitude"
colnames(JR2)[5]="Longitude"
test=left_join(CB2, JR2, by=c("Data_Source", "Waterbody_Name", "Site"))

t2=test[!(complete.cases(test$Latitude)),]
table(t2$Data_Source)
with(t2, table(Data_Source, Waterbody_Name, Site))
with(t2, table(Data_Source, Waterbody_Name))

x=unique(t2$Data_Source)
i=5
x[i]
with(t2[which(t2$Data_Source==x[i]),], table(Waterbody_Name, Site))


outfile=test %>% select(-(c(Near_Waterbody_General_Location, Shell_Organic_C_Percent, Raw_Data_File, Number_ID,Location_Index,Site_within_Study,Month_Oysters_Removed,Year_Oysters_Removed,
                           Total_Shell_Height_Length_Inches , Waterbody_Region,Volume_ml:Gear_Class, `Salinity classification`)))
outfile=outfile[,c(1:7,39,40,8:11,36:38,12:24,26,25,28,27,29,31,30,33,32,35,34)]
colnames(outfile)
colnames(outfile)[11]
colnames(outfile)[11]="Subtidal_Intertidal_WaterColumn"
outfile$Data_Source[which(outfile$Data_Source=="Parker-unpubl")]="Parker-Bricker"
colnames(outfile)[20]="Total_Shell_Height_mm"
sum(is.na(outfile$Latitude))
outfile$Latitude[which(outfile$Site=="budden")]=outfile$Latitude[which(outfile$Site=="Orchard Point")][1]
outfile$Latitude[which(outfile$Site=="orchard point")]=outfile$Latitude[which(outfile$Site=="Orchard Point")][1]
outfile$Longitude[which(outfile$Site=="budden")]=outfile$Longitude[which(outfile$Site=="Orchard Point")][1]
outfile$Longitude[which(outfile$Site=="orchard point")]=outfile$Longitude[which(outfile$Site=="Orchard Point")][1]
write.csv(outfile, file=paste(wd,'CB2023oysterBMP.csv', sep=''),row.names=FALSE)
