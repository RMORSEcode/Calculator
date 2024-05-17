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
outfile=outfile[,c(1:8,29,28, 9:27)]

## add lat long
stations=readxl::read_xlsx(paste(wd, "Location_data.xlsx", sep=''),sheet='final2')
JR=JRstat %>% filter(!(Raw_Data_File %in% c("Powell_Mann", "Kellogg-Harris", "Kellogg-Onancock"))) # drop data not included in zenodo 20240508
JR2=JR %>% select(Data_Source, Waterbody_Name, Site, `Latitude_Decimal Degree`, `Longitude_Decimal Degree`, `Salinity classification`)
colnames(JR2)[4]="Latitude"
colnames(JR2)[5]="Longitude"
test=left_join(CB2, JR2, by=c("Data_Source", "Waterbody_Name", "Site"))

colnames(outfile)
dt=lubridate::now()
dat=format(dt,"%Y%m%d")
write.csv(outfile, file=paste(wd,dat,'RegionFarm.csv', sep=''),row.names=FALSE)
