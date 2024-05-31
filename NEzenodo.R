library(tidyverse)
library(readxl)
library(lubridate)

wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
files=list.files(wd, patter="_Calculator_Main_oyster_data.rdata")
load(files[length(files)]) # load last file, should be latest version

s1=match("Poach et al. in prep 2023", Main$Data_Source) # start of Poach data (after end of CB)
MainNoCB=Main[s1:dim(Main)[1],]
RegionFarm=MainNoCB %>% filter(!(Waterbody_Region %in% c("Jamaica Bay", "Hudson River", "Raritan Bay")))
## update Data Source names
RegionFarm$Data_Source[which(RegionFarm$Data_Source=="Ayvazian et al. TNC unpublished")]="Ayvazian et al. unpublished"
RegionFarm$Data_Source[which(RegionFarm$Data_Source=="Barr et al. in press 2023")]="Barr et al. 2023"
RegionFarm$Data_Source[which(RegionFarm$Data_Source=="Bayer et al. in prep. 2023")]="Bayer et al. 2024"
RegionFarm$Data_Source[which(RegionFarm$Data_Source=="Levinton J, et al. 2011 PLoS ONE 6(4)")]="Levinton et al. 2011"
RegionFarm$Data_Source[which(RegionFarm$Data_Source=="Poach et al. in prep 2023")]="Poach et al. 2024"
RegionFarm$Data_Source[which(RegionFarm$Data_Source=="Tom Kiffney unpublished, in prep")]="Kiffney et al. unpublished"
RegionFarm$Oyster_Stock[which(RegionFarm$Data_Source=="Reitsma et al. 2017" & RegionFarm$Hatchery_produced_or_Wild=="wild")]="Wild"

# Bayer, S.R., Cubillo, A.M., Rose, J.M., Ferreira, J.G., Dixon, M., Alvarado, A., Barr, J., Bernatchez, G., Meseck, S., Poach, M., Pousse, E., Wikfors, G.H., Bricker, S., 2024. Refining the Farm Aquaculture Resource Management Model for Shellfish Nitrogen Removal at the Local Scale. Estuaries and Coasts. doi 10.1007/s12237-024-01354-7
# 
# Barr, J.M., Munroe, D., Rose, J.M., Calvo, L., Cheng, K.M., Bayer, S., Kreeger, D., 2023. Seasonal Feeding Behavior of Aquaculture Eastern Oysters (Crassostrea virginica) in the Mid-Atlantic. Estuaries and Coasts. doi 10.1007/s12237-023-01293-9
# 
# Poach, M., Morse, R., Meseck, S.L., Alvarado, A., Reichert-Nguyen, J., McFarland, K., Elliott, H., Kellogg, M.L., Luckenbach, M.W., Rose, J.M., 2024. Nutrient reduction by eastern oysters exhibits low variability associated with reproduction, ploidy, and farm location. Marine Pollution Bulletin 202, 116286. doi 10.1016/j.marpolbul.2024.116286
outfile=RegionFarm %>% select(-c(Raw_Data_File, Representative_Aquaculture_Oyster_Practice, Number_ID, 
                                 Location_Index, Waterbody_Type, Site_within_Study, Oyster_Growth_Location_Type,
                                 Subtidal_Intertidal_WaterColumn_Other, Hatchery_produced_or_Wild, Date_Oysters_Deployed,
                                 Month_Oysters_Removed, Year_Oysters_Removed, Season_Oysters_Removed,
                                 Tissue_TP_Percent, Tissue_TP_g_P_per_g_dw, Shell_TP_Percent:Habitat_Group,
                                 Volume_ml:Panel))
colnames(outfile)[10]="Total_Shell_Height_mm"
outfile=outfile[,c(1:8,29,28, 9:19,21,20,22,25,23,24,27,26)]

## add lat long
stations=readxl::read_xlsx(paste(wd, "Location_data.xlsx", sep=''),sheet='final2')
sta=stations %>% select(Waterbody_Name, st_abrv, Latitude, Longitude)
sta2=sta[!duplicated(sta$Waterbody_Name),]
## remove repeats, merge on first 2 columns, then fix repeats for NY and ME, NH
test=left_join(outfile, sta2, by=c("Waterbody_Name", "st_abrv"))
sum(is.na(test$Latitude))
test$Latitude[which(test$st_abrv=="NY" & test$Site=="GSBW")]=40.660075
test$Latitude[which(test$st_abrv=="NY" & test$Site=="GSBC")]=40.717588
test$Latitude[which(test$st_abrv=="NY" & test$Site=="GSBE")]=40.742127
test$Longitude[which(test$st_abrv=="NY" & test$Site=="GSBW")]=-73.278951
test$Longitude[which(test$st_abrv=="NY" & test$Site=="GSBC")]=-73.104576
test$Longitude[which(test$st_abrv=="NY" & test$Site=="GSBE")]=-72.953976
test$Latitude[which(test$st_abrv=="ME" & test$Site=="Pemaquid Oyster Company")]=43.95824
test$Latitude[which(test$st_abrv=="ME" & test$Site=="Darling Marine Center")]=43.935007
test$Longitude[which(test$st_abrv=="ME" & test$Site=="Pemaquid Oyster Company")]=-69.570455
test$Longitude[which(test$st_abrv=="ME" & test$Site=="Darling Marine Center")]=-69.5812
test$Latitude[which(test$st_abrv=="NH" & test$Waterbody_Name=="Little Bay")]=43.120057
test$Longitude[which(test$st_abrv=="NH" & test$Waterbody_Name=="Little Bay")]=-70.859828
sum(is.na(test$Latitude))
outfile$Latitude=test$Latitude
outfile$Longitude=test$Longitude
outfile=outfile[,c(1:7,30,31,8:29)]
colnames(outfile)

# fix Oyster_Stock issue -> Source of oysters instead
colnames(outfile)[12]="Oyster_Source"
# outfile$Oyster_Source[which(RegionFarm$Data_Source=="Reitsma et al. 2017" & RegionFarm$Hatchery_produced_or_Wild=="wild")]="Wild"


dt=lubridate::now()
dat=format(dt,"%Y%m%d")
write.csv(outfile, file=paste(wd,dat,'RegionFarm.csv', sep=''),row.names=FALSE)
