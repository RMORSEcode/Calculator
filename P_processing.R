# RM 20250711 process ANRC RegionFarm data to analyze P for manuscript and version 2.0 update
# see also 'load_P_samples.R'

library(tidyverse)
library(ggExtra)
library(ggpubr)
library(quantreg)
library(patchwork)
library(maps)
### Figure 1 ### Map of region and samples
data(stateMapEnv)
wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
stations=readxl::read_xlsx(paste(wd, "Location_data.xlsx", sep=''),sheet='final2')
par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
map("worldHires", xlim=c(-79,-68),ylim=c(33,45), fill=T,border=0,col="gray70", xlab="Lon")
map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')
map.axes(las=1)
mtext(c("Longitude", "Latitude"), side=c(1,2), line = 2.5)
map('state', fill = F, add=T) # add state lines
# plot stations with N in different color
stations2=stations %>% filter(Waterbody_Name %in% RegionFarm$Waterbody_Name[complete.cases(RegionFarm$Tissue_N_Percent)]) # N plus morpho
stations3=stations %>% filter(!(Waterbody_Name %in% RegionFarm$Waterbody_Name[complete.cases(RegionFarm$Tissue_N_Percent)])) # morpho
stations4=stations %>% filter((Waterbody_Name %in% RegionFarm$Waterbody_Name[complete.cases(RegionFarm$Tissue_TP_Percent)])) # P
## plot
tiff(paste0(wd,"Fig1map_Rose_et_al_12_17.tiff"), height = 12, width = 17, units = 'cm', 
     compression = "lzw", res = 300)
par(mar = c(2,2,0,0)) #(b,l,t,r)
par(oma = c(2,2,0,0))
par(family = "Arial")
map("worldHires", xlim=c(-79,-68),ylim=c(33,45), fill=T,border=0,col="gray90")
map('lakes', add=TRUE, fill=TRUE, col='white', boundary='black')
map.axes(las=1)
map('state', fill = F, add=T) # add state lines
mtext(c("Longitude", "Latitude"), side=c(1,2), line = 2.5)
points(stations3$Longitude, stations3$Latitude, pch=23, col='black', bg='orange', cex=1.15)
points(stations2$Longitude, stations2$Latitude, pch=21, col='black', bg='green', cex=1.15)
points(stations4$Longitude, stations4$Latitude, pch=23, col='black', bg='red', cex=1.15)
legend(-76,36, pch=c(23,21), col='black', pt.bg=c('orange', 'green'),legend=c('Morphometric data', 'Morphometric + N'), bty='n', )
legend(-76,36, pch=c(23), col='black', pt.bg=c('red'),legend=c('Phosphorus data'), bty='n', )
dev.off()

wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"

testT = RegionFarm %>% 
  group_by(st_abrv)%>%
  summarise(across(c(Tissue_TP_Percent, Tissue_C_Percent, Tissue_N_Percent),
                   list(Mean = ~mean(., na.rm=T), Med= ~median(., na.rm=T), SD = ~sd(.,na.rm=T), Num = ~sum(!is.na(.)))
  ))
write.csv(testT, file=paste0(wd,'manuscript/Phosphorus/Tissue_states.csv'))
# write.csv(testT, file=paste0(wd,'manuscript/Phosphorus/Tissue.csv'))

testS = RegionFarm %>% 
  group_by(st_abrv)%>%
  summarise(across(c(Shell_TP_Percent, Shell_C_Percent, Shell_N_Percent),
                   list(Mean = ~mean(., na.rm=T), Med= ~median(., na.rm=T), SD = ~sd(.,na.rm=T), Num = ~sum(!is.na(.)))
  ))
write.csv(testS, file=paste0(wd,'manuscript/Phosphorus/Shell_states.csv'))
# write.csv(testS, file=paste0(wd,'manuscript/Phosphorus/Shell.csv'))

## monthly mean values for CT
testT = RegionFarm[which(RegionFarm$st_abrv=='CT'),] %>% 
  group_by(Month_Oysters_Removed) %>%
  summarise(across(c(Tissue_TP_Percent, Tissue_C_Percent, Tissue_N_Percent),
                   list(Mean = ~mean(., na.rm=T), diff= ~(mean(., na.rm=T)-0.83)/0.83, Med= ~median(., na.rm=T), SD = ~sd(.,na.rm=T), Num = ~sum(!is.na(.)))
  ))
write.csv(testT, file=paste0(wd,'manuscript/Phosphorus/Tissue_CT_month.csv'))
plot(testT$Tissue_TP_Percent_Mean~testT$Month_Oysters_Removed, type='b', ylim=c(0, 1.25), ylab='Mean Tissue P')
abline(h=0.83, lty=3)
arrows(testT$Month_Oysters_Removed, 
       testT$Tissue_TP_Percent_Mean+testT$Tissue_TP_Percent_SD, 
       testT$Month_Oysters_Removed, 
       testT$Tissue_TP_Percent_Mean-testT$Tissue_TP_Percent_SD,
       length=0.05, angle=90, code=3)

plot(testT$Tissue_TP_Percent_diff~testT$Month_Oysters_Removed, type='b', ylim=c(-0.5, 0.5), ylab='Tissue P difference from mean')
abline(h=0, lty=3)
arrows(testT$Month_Oysters_Removed, 
       testT$Tissue_TP_Percent_diff+testT$Tissue_TP_Percent_SD, 
       testT$Month_Oysters_Removed, 
       testT$Tissue_TP_Percent_diff-testT$Tissue_TP_Percent_SD,
       length=0.05, angle=90, code=3)

### tests for difference between state means
my_comparisons <- list( c("MA","CT"), c("MD","CT"),c("ME","CT"),c("NH","CT"),
                        c("RI","CT"),c("VA","CT"),c("MD","MA"),c("ME","MA"),
                        c("NH","MA"),c("RI","MA"),c("VA","MA"),c("ME","MD" ),
                        c("NH","MD"),c("RI","MD" ),c("VA","MD"),c("NH","ME"),
                        c("RI","ME"),c("VA","ME"),c("RI","NH" ),c("VA","NH"),
                        c("VA","RI"))
RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>% select(State, st_abrv, Ploidy, Tissue_TP_Percent) %>% 
  ggboxplot(y='Tissue_TP_Percent', x='st_abrv', ylab = 'Tissue P Percent', xlab='', ylim=c(0,5)) +
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T), lty=2)+
  geom_point(aes(color=State), position=position_jitterdodge(0.2)) +
  stat_compare_means(comparisons=my_comparisons, label = "p.signif")
RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>% select(State, st_abrv, Ploidy, Tissue_TP_Percent) %>% 
  ggboxplot(y='Tissue_TP_Percent', x='st_abrv', ylab = 'Tissue P Percent', xlab='', ylim=c(0,2)) +
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T), lty=2) +
  geom_point(aes(color=State), position=position_jitterdodge(0.2))

RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),] %>% select(State, st_abrv, Ploidy, Shell_TP_Percent) %>% 
  ggboxplot(y='Shell_TP_Percent', x='st_abrv', ylab = 'Shell P Percent', xlab='', ylim=c(0,0.2)) +
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T), lty=2)+
  geom_point(aes(color=State), position=position_jitterdodge(0.1)) +
  stat_compare_means(comparisons=my_comparisons, label = "p.signif")



# Fit the ANOVA model
Tmodel <- aov(Tissue_TP_Percent ~ st_abrv, data = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),])
summary(Tmodel)
# Perform Tukey's HSD test to see which combinations are different
TukeyHSD(Tmodel)
RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),] %>% select(State, st_abrv, Ploidy, Shell_TP_Percent) %>% 
  ggboxplot(y='Shell_TP_Percent', x='st_abrv', ylab = 'Shell P Percent', xlab='', ylim=c(0,0.15)) +
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T), lty=2)+
  geom_point(aes(color=State), position=position_jitterdodge(0.2)) +
  stat_compare_means(comparisons=my_comparisons, label = "p.signif")
RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),] %>% select(State, st_abrv, Ploidy, Shell_TP_Percent) %>% 
  ggboxplot(y='Shell_TP_Percent', x='st_abrv', ylab = 'Shell P Percent', xlab='', ylim=c(0,0.1)) +
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T), lty=2) +
  geom_point(aes(color=State), position=position_jitterdodge(0.2))
Smodel <- aov(Shell_TP_Percent ~ st_abrv, data = RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),])
summary(Smodel)
# Perform Tukey's HSD test to see which combinations are different
TukeyHSD(Smodel)

### plot tissue P by ploidy and state
ggplot(RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),], aes(x = Ploidy, y = Tissue_TP_Percent)) +
  geom_boxplot(aes(fill=st_abrv)) +
  geom_jitter(colour = 4, alpha = .25) +
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T), lty=2, lwd=2) +
  ylab('Tissue P (%)')
### plot tissue P by ploidy and state
ggplot(RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),], aes(x = Ploidy, y = Shell_TP_Percent)) +
  geom_boxplot(aes(fill=st_abrv)) +
  geom_jitter(colour = 4, alpha = .25) +
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T), lty=2, lwd=2) +
  ylab('Shell P (%)')
## ploidy
Smodel <- aov(Shell_TP_Percent ~ Ploidy, data = RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),])
summary(Smodel)
TukeyHSD(Smodel)
Tmodel <- aov(Tissue_TP_Percent ~ Ploidy, data = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),])
summary(Tmodel)
TukeyHSD(Tmodel)

library(car)
leveneTest(Tissue_TP_Percent ~ Ploidy, data = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),])
oneway.test(Tissue_TP_Percent ~ Ploidy, data = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),])
plot(Smodel)
# Extract the residuals
aov_residuals <- residuals(object = Smodel )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
## above assumptions not met use this instead:
kruskal.test(Tissue_TP_Percent ~ Ploidy, data = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),])

library(WRS2)
t1way(Tissue_TP_Percent ~ Ploidy, data = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),], tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Tissue_TP_Percent ~ Ploidy, data = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),], tr = 0.2, alpha = 0.05, method = "hochberg")


### Test whether each group differs from a value (the overall mean of all groups)
# https://stackoverflow.com/questions/55213124/use-stat-compare-means-to-test-whether-multiple-groups-are-significantly-differe
myt_tests = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>%
  group_by(st_abrv) %>%
  summarise(P = t.test(Tissue_TP_Percent, mu =mean(RegionFarm$Tissue_TP_Percent[complete.cases(RegionFarm$Tissue_TP_Percent)], na.rm=T))$p.value,
            Sig = ifelse(P < 0.05, "*", "ns"),
            Mean = mean(Tissue_TP_Percent),
            SD = sd(Tissue_TP_Percent),
            Range = max(Tissue_TP_Percent)-min(Tissue_TP_Percent),
            Diff=0.83-mean(Tissue_TP_Percent))
ggplot(RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),], aes(x = st_abrv, y = Tissue_TP_Percent)) +
  geom_boxplot() +
  # geom_violin(trim = FALSE)+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T), lty=2, lwd=2)+
  # Use the prepared table of test results as data for the geom
  geom_text(aes(label = Sig, y = 1.53, col='red'), size = 6, data = myt_tests) +
  guides(colour='none')
## Shell
myt_tests = RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),] %>%
  group_by(st_abrv) %>%
  summarise(P = t.test(Shell_TP_Percent, mu = mean(RegionFarm$Shell_TP_Percent[complete.cases(RegionFarm$Shell_TP_Percent)], na.rm=T)) $p.value,
            Sig = ifelse(P < 0.05, "*", "ns"),
            Mean = mean(Shell_TP_Percent),
            SD =sd(Shell_TP_Percent),
            Range = max(Shell_TP_Percent)-min(Shell_TP_Percent),
            Diff=0.043-mean(Shell_TP_Percent))
ggplot(RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),], aes(x = st_abrv, y = Shell_TP_Percent)) +
  ylim(0,0.1)+
  geom_boxplot() +
  # geom_violin(trim = FALSE)+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T), lty=2, lwd=2)+
  # Use the prepared table of test results as data for the geom
  geom_text(aes(label = Sig, y = 0.075, col='red'), size = 6,data = myt_tests) + 
  guides(colour='none')

### Line plot with SD segments as in the ANRC manuscript for CT Greenwich Cove time series
test = RegionFarm %>% 
  filter(st_abrv=='CT') %>%
  group_by(Month_Oysters_Removed)%>%
  summarise(mnN=mean(Tissue_N_Percent, na.rm=T), mnP=mean(Tissue_TP_Percent, na.rm=T),
            mnC=mean(Tissue_C_Percent, na.rm=T), sdC=sd(Tissue_C_Percent, na.rm=T),
            sdN=sd(Tissue_N_Percent, na.rm=T),sdP=sd(Tissue_TP_Percent, na.rm=T))
plot(test$mnP~test$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,1.5),
     las=1, ylab='Tissue Percent P', xlab='Month', axes=F, cex.lab=1.5,
     cex.axis=1.5, cex=1.5)
box(bty='l')
axis(1,cex.axis=1.5)
axis(2, las=1, cex.axis=1.5)
segments(test$Month_Oysters_Removed, test$mnP-test$sdP, test$Month_Oysters_Removed,test$mnP+test$sdP, col='black')
#
test = RegionFarm %>% 
  filter(st_abrv=='CT') %>%
  group_by(Month_Oysters_Removed)%>%
  summarise(mnN=mean(Shell_N_Percent, na.rm=T), mnP=mean(Shell_TP_Percent, na.rm=T),
            mnC=mean(Shell_C_Percent, na.rm=T), sdC=sd(Shell_C_Percent, na.rm=T),
            sdN=sd(Shell_N_Percent, na.rm=T),sdP=sd(Shell_TP_Percent, na.rm=T))
plot(test$mnP~test$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,0.07),
     las=1, ylab='Shell Percent P', xlab='Month', axes=F, cex.lab=1.5,
     cex.axis=1.5, cex=1.5)
box(bty='l')
axis(1,cex.axis=1.5)
axis(2, las=1, cex.axis=1.5)
segments(test$Month_Oysters_Removed, test$mnP-test$sdP, test$Month_Oysters_Removed,test$mnP+test$sdP, col='black')

# adding N:P ratio
test = RegionFarm %>% 
  mutate(Tissue_NP=Tissue_N_Percent/Tissue_TP_Percent) %>%
  filter(st_abrv=='VA') %>%
  group_by(Month_Oysters_Removed)%>%
  summarise(mnNP=mean(Tissue_NP, na.rm=T) , mnN=mean(Tissue_N_Percent, na.rm=T), mnP=mean(Tissue_TP_Percent, na.rm=T),
            mnC=mean(Tissue_C_Percent, na.rm=T), sdC=sd(Tissue_C_Percent, na.rm=T),
            sdNP=sd(Tissue_NP, na.rm=T), sdN=sd(Tissue_N_Percent, na.rm=T),sdP=sd(Tissue_TP_Percent, na.rm=T))
plot(test$mnNP~test$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(6,11),
     las=1, ylab='Tissue N:P', xlab='Month', axes=F, cex.lab=1.5,
     cex.axis=1.5, cex=1.5)
box(bty='l')
axis(1,cex.axis=1.5)
axis(2, las=1, cex.axis=1.5)
segments(test$Month_Oysters_Removed, test$mnNP-test$sdNP, test$Month_Oysters_Removed,test$mnNP+test$sdNP, col='black')

RegionFarm %>% 
  mutate(Tissue_NP=Tissue_N_Percent/Tissue_TP_Percent) %>%
  filter(st_abrv=='CT') %>%
  ggplot(aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Tissue_NP)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  # geom_jitter(colour = 2)+
  coord_cartesian(ylim = c(5,13))+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  # geom_hline(yintercept=mean(Tissue_NP, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'CT Tissue N:P') +
  # scale_x_discrete(limits = 1:12, labels = month.abb)+
  scale_x_discrete(limits = month.abb)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) 
RegionFarm %>% 
  mutate(Tissue_NP=Tissue_N_Percent/Tissue_TP_Percent) %>%
  filter(st_abrv=='VA') %>%
  ggplot(aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Tissue_NP)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  # geom_jitter(colour = 2)+
  coord_cartesian(ylim = c(5,13))+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  # geom_hline(yintercept=mean(Tissue_NP, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'VA Tissue N:P') +
  # scale_x_discrete(limits = 1:12, labels = month.abb)+
  scale_x_discrete(limits = month.abb)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))  

RegionFarm %>% 
  mutate(Tissue_NP=Tissue_N_Percent/Tissue_TP_Percent) %>%
  filter(st_abrv=='MD') %>%
  ggplot(aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Tissue_NP)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  # geom_jitter(colour = 2)+
  coord_cartesian(ylim = c(5,13))+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  # geom_hline(yintercept=mean(Tissue_NP, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'MD Tissue N:P') +
  # scale_x_discrete(limits = 1:12, labels = month.abb)+
  scale_x_discrete(limits = month.abb)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))  
### Figure 2 ###
## All States Tissue P (all time)
level_order <- c('VA', 'MD', 'CT', 'RI', 'MA', 'NH', 'ME') 
p1=RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>% 
  ggplot(aes(y=Tissue_TP_Percent, x=factor(st_abrv, level = level_order))) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)') +
  coord_cartesian(ylim = c(0, 2))+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
## All States Shell P (all time)
p2=RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),] %>% 
  ggplot(aes(y=Shell_TP_Percent, x=factor(st_abrv, level=level_order))) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='Location', y = 'Shell P (%)') +
  coord_cartesian(ylim = c(0, 0.1))+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
## CT Seasonal Tissue P *** OR ALL monthly ***
test=RegionFarm
test$Month_Oysters_Removed[which(test$Date_Oysters_Removed=='2019-05-02')]=4 # set 5/2 to April 5/30 is May
# p3=
    test %>%
    filter(st_abrv %in% c('CT','MD','VA'), Ploidy=="Diploid") %>%
    ggplot(aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Tissue_TP_Percent)) +
  # p3=ggplot(data = RegionFarm, aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Tissue_TP_Percent)) +
# p3=ggplot(data = RegionFarm[which(RegionFarm$st_abrv=='CT'),],
          # aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Tissue_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  coord_cartesian(ylim = c(0, 2))+
  geom_jitter(aes(color=st_abrv), shape = 19, position = position_jitter(0.2))+
  # geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)', color='') +
  scale_x_discrete(limits = month.abb)+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
## CT Seasonal Shell P *** OR ALL monthly ****
p4=
  test %>%
  filter(st_abrv %in% c('CT','MD','VA'), Ploidy=="Diploid") %>%
  ggplot(aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Shell_TP_Percent)) +
    # ggplot(data = RegionFarm, aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Shell_TP_Percent)) +
# p4=ggplot(data = RegionFarm[which(RegionFarm$st_abrv=='CT'),],
#           aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Shell_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  coord_cartesian(ylim = c(0, 0.1))+
  geom_jitter(aes(color=st_abrv), shape = 19, position = position_jitter(0.2))+
  # geom_jitter(shape = 19, color = "#0085CA", alpha = 0.5, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='Month', y = 'Shell P (%)', color='') +
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  scale_x_discrete(limits = month.abb)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# tiff(paste0(wd,"/manuscript/Phosphorus/Fig1b_Morse_et_al_15_10.tiff"), height = 15, width = 12, units = 'cm', 
#      compression = "lzw", res = 300)
# p1 / p2  + plot_annotation(tag_levels = 'A')
# dev.off()
# tiff(paste0(wd,"/manuscript/Phosphorus/Fig2AB2_Morse_et_al_15_10.tiff"), height = 15, width = 15, units = 'cm', 
#      compression = "lzw", res = 300)
# p3 / p4  + plot_annotation(tag_levels = 'A')
# dev.off()

#### add CT and VA to temporal analysis, but filter to only diploid for manuscript (triploid vs diploid in supplemental material)
test=RegionFarm
test$Month_Oysters_Removed[which(test$Date_Oysters_Removed=='2019-05-02')]=4 # set 5/2 to April 5/30 is May
test$day_of_year = yday(test$Date_Oysters_Removed)
test$day_factor = as.factor(test$day_of_year)
p3=
  test %>%
  filter(st_abrv %in% c('CT','MD','VA'), Ploidy=="Diploid") %>%
  ggplot(aes(group=day_factor, x=day_factor, y = Tissue_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  coord_cartesian(ylim = c(0, 2))+
  geom_jitter(aes(color=st_abrv), shape=19, position = position_jitter(0.2))+
  scale_color_manual(values = c("CT" = "#440154FF", 
                                "MD" = "#21908CFF", 
                                "VA" = "#FDE725FF"))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)', color='') +
  scale_x_discrete(
    labels = ~ format(as.Date(paste("2023", pmax(., 1)), format = "%Y %j"), format = "%m-%d")
  )+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
## shell
p4=
  test %>%
  filter(st_abrv %in% c('CT','MD','VA'), Ploidy=="Diploid") %>%
  ggplot(aes(group=day_factor, x=day_factor, y = Shell_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  coord_cartesian(ylim = c(0, 0.1))+
  geom_jitter(aes(color=st_abrv), shape=19, position = position_jitter(0.2))+
  scale_color_manual(values = c("CT" = "#440154FF", 
                                "MD" = "#21908CFF", 
                                "VA" = "#FDE725FF"))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Shell P (%)', color='') +
  scale_x_discrete(
    labels = ~ format(as.Date(paste("2023", pmax(., 1)), format = "%Y %j"), format = "%m-%d")
  )+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

tiff(paste0(wd,"/manuscript/Phosphorus/Fig2ABCD_Morse_et_al.tif"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
(p1 | p3) / (p2 | p4)  + plot_annotation(tag_levels = 'A')
dev.off()

## change diploid to triploid for p3t and p4t
tiff(paste0(wd,"/manuscript/Phosphorus/location_ploidy.tif"), height = 15, width = 20, units = 'cm', compression = "lzw", res = 300)
(p3 | p3t) / (p4 | p4t)  + plot_annotation(tag_levels = 'A')
dev.off()

## change NAs to diploid
# testd=RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),]
# testd=RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),]
# testd$Ploidy[is.na(testd$Ploidy)]="Diploid"
# testd$Ploidy=factor(testd$Ploidy, levels=c("Diploid", "Triploid"))
# A - Tissue P by Ploidy
p5=RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>% 
  select(Ploidy, Tissue_TP_Percent) %>% 
  ggplot(aes(y=Tissue_TP_Percent, x=`Ploidy`)) +
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)') +
  coord_cartesian(ylim = c(0, 2))+
  stat_summary(fun=mean, geom="point", shape=18, size=4)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))
# B - Shell P
p6=RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),] %>% 
  select(`Ploidy`, Shell_TP_Percent) %>% 
  ggplot(aes(y=Shell_TP_Percent, x=`Ploidy`)) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='Ploidy', y = 'Shell P (%)') +
  coord_cartesian(ylim = c(0, 0.1))+
  stat_summary(fun=mean, geom="point", shape=18, size=4)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))

tiff(paste0(wd,"/manuscript/Phosphorus/Fig2ABCDEF_diploids.tif"), height = 15, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
(p1 | p3 |p5) / (p2 | p4 | p6)  + plot_annotation(tag_levels = 'A')
dev.off()

tiff(paste0(wd,"/manuscript/Phosphorus/Fig2ABCDEFvert_Morse_et_al_15_10.tiff"), height = 30, width = 15, units = 'cm', compression = "lzw", res = 300)
(p1 | p2) / (p3 | p4) / (p5 | p6)  + plot_annotation(tag_levels = 'A')
dev.off()

### Figure 3 ###
tiff(paste0(wd,"/manuscript/Phosphorus/Fig3x_Morse_et_al_15_10.tiff"), height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
par(oma=c(2,2,0,0)) 
par(mar=c(4,4,2,2) + 0.1)
par(mfrow = c(2, 1))
par(family = "Arial")
plot(RegionFarm2$Tissue_Dry_Weight_g ~ RegionFarm2$Total_Shell_Height_Length_mm, type='n', ylim=c(0,8), xlim=c(0,150), ylab="Tissue dry weight (g)", xlab="", las=1)
points(RegionFarm2$Tissue_Dry_Weight_g ~RegionFarm2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 190/255, green = 190/255, blue = 190/255, alpha = 0.2))
points(NewP$Tissue_Dry_Weight_g~NewP$Total_Shell_Height_Length_mm, pch=17, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.4))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='red', lwd=2, lty=1)
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm2, start = list(a = 0.00037, b = 1.83359), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='black', lwd=2, lty=2)
# a2=round(a,6)
# b2=round(b,3)
# text(0,7, labels=bquote(Y==.(a2)*X^.(b2)), cex=1, pos = 4)
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,8, labels="A", cex=1)
legend('topleft', 
       col=c(rgb(red = 190/255, green = 190/255, blue = 190/255, alpha = 0.2),rgb(red = 0, green = 0, blue = 1, alpha = 0.4),'black','red'),
       lty=c(NA,NA, 2,1), 
       lwd=c(NA,NA,2,2), 
       pch=c(19,17,NA,NA),
       legend=c('ANRC', 'This study', 'ANRC','All data'), 
       bty='n'
)
# B - Shell
plot(RegionFarm$Shell_Dry_Weight_g ~ RegionFarm$Total_Shell_Height_Length_mm, type='n', ylim=c(0,150), xlim=c(0,150), ylab="Shell dry weight (g)", xlab='Shell height (mm)', las=1)
points(RegionFarm2$Shell_Dry_Weight_g ~RegionFarm2$Total_Shell_Height_Length_mm, pch=19, col=rgb(red = 190/255, green = 190/255, blue = 190/255, alpha = 0.2))
points(NewP$Shell_Dry_Weight_g ~NewP$Total_Shell_Height_Length_mm, pch=17, col=rgb(red = 0, green = 0, blue = 1, alpha = 0.4))
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm, start = list(a = 0.0007, b = 2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='red', lwd=2, lty=1)
qrx=nlrq(Shell_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = RegionFarm2, start = list(a = 0.0007, b = 2), tau=0.5)
a=summary(qrx)$coefficients[1,1]
b=summary(qrx)$coefficients[2,1]
x=seq(0, 150, length = 250)
yval=(a*(x^b))
lines(x, yval, col='black', lwd=2, lty=2)
legend('topleft', 
       col=c(rgb(red = 190/255, green = 190/255, blue = 190/255, alpha = 0.2),rgb(red = 0, green = 0, blue = 1, alpha = 0.4),'black','red'),
       lty=c(NA,NA, 2,1), 
       lwd=c(NA,NA,2,2), 
       pch=c(19,17,NA,NA),
       legend=c('ANRC', 'This study', 'ANRC','All data'), 
       bty='n'
)
# a2=round(a,6)
# b2=round(b,3)
# text(0,140, labels=bquote(Y==.(a2)*X^.(b2)), cex=1, pos = 4)
abline(v=63.5, lty=2)
abline(v=88.9, lty=2)
text(140,150, labels="B", cex=1)
dev.off()


p1=RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>% 
  ggplot(aes(y=Tissue_TP_Percent, x=st_abrv)) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  # geom_jitter(colour = 2)+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)') +
  coord_cartesian(ylim = c(0, 2))+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
# A - Tissue P by Ploidy
p1=RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>% 
  select(Ploidy, Tissue_TP_Percent) %>% 
  ggplot(aes(y=Tissue_TP_Percent, x=`Ploidy`)) + 
  geom_boxplot(color = "black", notch=F, fill="gray", show.legend = FALSE) +
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T), lty=2)+
  geom_point(aes(color=`Ploidy`),show.legend = FALSE, position=position_jitterdodge(0.2)) +
  # stat_compare_means(comparisons=my_comparisons, label = "p.signif")
  theme_classic()+
  labs(x='', y = 'Tissue P Percent') +
  coord_cartesian(ylim = c(0, 2))+
  stat_summary(fun=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 12, family="Arial")) +
  theme(legend.text = element_text(size = 12, family="Arial")) 
# B - Shell P
p2=RegionFarm[complete.cases(TXall$Shell_TP_Percent),] %>% 
  filter(is.na(Notes)) %>%
  select(`Ploidy`, Shell_TP_Percent) %>% 
  ggplot(aes(y=Shell_TP_Percent, x=`Ploidy`)) + 
  geom_boxplot(color = "black", notch=F, fill="gray", show.legend = FALSE) +
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T), lty=2)+
  geom_point(aes(color=`Ploidy`),show.legend = FALSE, position=position_jitterdodge(0.2)) +
  theme_classic()+
  labs(x='', y = 'Shell P Percent') +
  coord_cartesian(ylim = c(0, 0.075))+
  stat_summary(fun=mean, geom="point", shape=18, size=4, position = position_dodge(width = .75))+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  theme(text = element_text(size = 12, family="Arial")) +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1))
tiff(paste0(wd1,"Fig_TX_P_ploidy_15_10.tiff"), height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
p1 / p2 + plot_annotation(tag_levels = 'A')
dev.off()


### 20261008 updated figure 2 and gear comparison for revision
#
#### Tissue #### 
test=RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),]
## Gear Class
test2=test %>% filter(Month_Oysters_Removed %in% c(1,2,3)) # limit to Jan-Mar for most even comparison without resample
table(test2$Gear_Class)
p7=ggplot(data = test2, aes(group=Gear_Class, x=factor(Gear_Class), y = Tissue_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  coord_cartesian(ylim = c(0, 2))+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1)) +
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)') +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) #+
# theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
with(test2, table(st_abrv, Gear_Class))
t1way(Tissue_TP_Percent ~ Gear_Class, data = test2, tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Tissue_TP_Percent ~ Gear_Class, data = test2, tr = 0.2, alpha = 0.05, method = "hochberg")
t.sum.gear=RegionFarm %>%
  group_by(Gear_Class) %>%
  summarize(mn=mean(Tissue_TP_Percent, na.rm=T), sd=sd(Tissue_TP_Percent, na.rm=T), med=median(Tissue_TP_Percent, na.rm=T),cnt=n())
## Location
test2=test %>% filter(Month_Oysters_Removed %in% c(1,2)) # limit to Jan-Feb
p1=test2 %>%
  ggplot(aes(y=Tissue_TP_Percent, x=factor(st_abrv, level = level_order))) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)') +
  coord_cartesian(ylim = c(0, 2))+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
t1way(Tissue_TP_Percent ~ st_abrv, data =test2, tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Tissue_TP_Percent ~ st_abrv, data = test2, tr = 0.2, alpha = 0.05, method = "hochberg")
t.sum.loc=test2 %>%
  group_by(st_abrv) %>%
  summarize(mn=mean(Tissue_TP_Percent, na.rm=T), sd=sd(Tissue_TP_Percent, na.rm=T), med=median(Tissue_TP_Percent, na.rm=T), cnt=n())
## Time
test=RegionFarm
test$Month_Oysters_Removed[which(test$Date_Oysters_Removed=='2019-05-02')]=4 # set 5/2 to April 5/30 is May
test$day_of_year = yday(test$Date_Oysters_Removed)
test$day_factor = as.factor(test$day_of_year)
p3=
  test %>%
  filter(st_abrv %in% c('CT','MD','VA'), Ploidy=="Diploid") %>%
  ggplot(aes(group=day_factor, x=day_factor, y = Tissue_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  coord_cartesian(ylim = c(0, 2))+
  geom_jitter(aes(color=st_abrv), shape=19, position = position_jitter(0.2))+
  scale_color_manual(values = c("CT" = "#440154FF", 
                                "MD" = "#21908CFF", 
                                "VA" = "#FDE725FF"))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)', color='') +
  scale_x_discrete(
    labels = ~ format(as.Date(paste("2023", pmax(., 1)), format = "%Y %j"), format = "%m-%d")
  )+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
test2=test %>%
  filter(st_abrv %in% c('CT','MD','VA'), Ploidy=="Diploid")
t1way(Tissue_TP_Percent ~ Month_Oysters_Removed, data =test2, tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Tissue_TP_Percent ~ Month_Oysters_Removed, data = test2, tr = 0.2, alpha = 0.05, method = "hochberg")
## Ploidy
p5=test %>%
  ggplot(aes(y=Tissue_TP_Percent, x=Ploidy)) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue P (%)') +
  coord_cartesian(ylim = c(0, 2))+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))#+
# theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
t1way(Tissue_TP_Percent ~ Ploidy, data =test, tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Tissue_TP_Percent ~ Ploidy, data =test, tr = 0.2, alpha = 0.05, method = "hochberg")



#### Shell #### 
test=RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),]
## Gear Class
test2=test %>% filter(Month_Oysters_Removed %in% c(1,2,3)) 
table(test2$Gear_Class)
p8=ggplot(data = test2, aes(group=Gear_Class, x=factor(Gear_Class), y = Shell_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  coord_cartesian(ylim = c(0, 0.1))+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1)) +
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Shell P (%)') +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) #+
# theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
with(test2, table(st_abrv, Gear_Class))
t1way(Shell_TP_Percent ~ Gear_Class, data = test2, tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Shell_TP_Percent ~ Gear_Class, data = test2, tr = 0.2, alpha = 0.05, method = "hochberg")
## Time
test=RegionFarm
test$Month_Oysters_Removed[which(test$Date_Oysters_Removed=='2019-05-02')]=4 # set 5/2 to April 5/30 is May
test$day_of_year = yday(test$Date_Oysters_Removed)
test$day_factor = as.factor(test$day_of_year)
p4=test %>%
  filter(st_abrv %in% c('CT','MD','VA'), Ploidy=="Diploid") %>%
  ggplot(aes(group=day_factor, x=day_factor, y = Shell_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  coord_cartesian(ylim = c(0, 0.1))+
  geom_jitter(aes(color=st_abrv), shape=19, position = position_jitter(0.2))+
  scale_color_manual(values = c("CT" = "#440154FF", 
                                "MD" = "#21908CFF", 
                                "VA" = "#FDE725FF"))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Shell P (%)', color='') +
  scale_x_discrete(
    labels = ~ format(as.Date(paste("2023", pmax(., 1)), format = "%Y %j"), format = "%m-%d")
  )+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "top") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
test2=test %>%
  filter(st_abrv %in% c('CT','MD','VA'), Ploidy=="Diploid")
t1way(Shell_TP_Percent ~ Month_Oysters_Removed, data =test2, tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Shell_TP_Percent ~ Month_Oysters_Removed, data = test2, tr = 0.2, alpha = 0.05, method = "hochberg")
## Ploidy
p6=test %>%
  ggplot(aes(y=Shell_TP_Percent, x=Ploidy)) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Shell P (%)') +
  coord_cartesian(ylim = c(0, 0.1))+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))#+
# theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
t1way(Shell_TP_Percent ~ Ploidy, data =test, tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Shell_TP_Percent ~ Ploidy, data =test, tr = 0.2, alpha = 0.05, method = "hochberg")
## Location
test2=test %>% filter(Month_Oysters_Removed %in% c(1,2)) # limit to Jan-Feb
p2=test2 %>%
  ggplot(aes(y=Shell_TP_Percent, x=factor(st_abrv, level = level_order))) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Shell P (%)') +
  coord_cartesian(ylim = c(0, 0.1))+
  stat_summary(fun=mean, geom="point", shape=18, size=2)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
t1way(Shell_TP_Percent ~ st_abrv, data =test2, tr = 0.2, alpha = 0.05, nboot = 100)
lincon(Shell_TP_Percent ~ st_abrv, data = test2, tr = 0.2, alpha = 0.05, method = "hochberg")
t.sum.loc=test2 %>%
  group_by(st_abrv) %>%
  summarize(mn=mean(Shell_TP_Percent, na.rm=T), sd=sd(Shell_TP_Percent, na.rm=T), med=median(Shell_TP_Percent, na.rm=T), cnt=n())
with(test2, table(st_abrv))


tiff(paste0(wd,"/manuscript/Phosphorus/Fig2_20261008.tif"), height = 15, width = 30, units = 'cm', 
     compression = "lzw", res = 300)
(p1 | p3 |p5) / (p2 | p4 | p6)  + plot_annotation(tag_levels = 'A')
(p1 | p3 | p5 | p7) / (p2 | p4 | p6 | p8)  + plot_annotation(tag_levels = 'A')

dev.off()

tiff(paste0(wd,"/manuscript/Phosphorus/Figx_20261008_gear.tif"), height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
p7 / p8 + plot_annotation(tag_levels = 'A')

dev.off()
