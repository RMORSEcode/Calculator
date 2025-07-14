# RM 20250711 process ANRC RegionFarm data to analyze P for manuscript and version 2.0 update
# see also 'load_P_samples.R'

library(tidyverse)
library(ggExtra)
library(ggpubr)
library(quantreg)

wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"

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
Tmodel <- aov(Shell_TP_Percent ~ st_abrv, data = RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),])
summary(Tmodel)
# Perform Tukey's HSD test to see which combinations are different
TukeyHSD(Tmodel)

### Test whether each group differs from a value (the overall mean of all groups)
# https://stackoverflow.com/questions/55213124/use-stat-compare-means-to-test-whether-multiple-groups-are-significantly-differe
myt_tests = RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>%
  group_by(st_abrv) %>%
  summarise(P = t.test(Tissue_TP_Percent, mu = 0.83)$p.value,
            Sig = ifelse(P < 0.05, "*", "ns"),
            MaxWidth = max(Tissue_TP_Percent))
ggplot(RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),], aes(x = st_abrv, y = Tissue_TP_Percent)) +
  # geom_boxplot() +
  geom_violin(trim = FALSE)+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T), lty=2)+
  # Use the prepared table of test results as data for the geom
  geom_text(aes(label = Sig, y = MaxWidth + 0.2), size = 6,
            data = myt_tests)
## Shell
myt_tests = RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),] %>%
  group_by(st_abrv) %>%
  summarise(P = t.test(Shell_TP_Percent, mu = mean(RegionFarm$Shell_TP_Percent[complete.cases(RegionFarm$Shell_TP_Percent)], na.rm=T)) $p.value,
            Sig = ifelse(P < 0.05, "*", "ns"),
            MaxWidth = max(Shell_TP_Percent))
ggplot(RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),], aes(x = st_abrv, y = Shell_TP_Percent)) +
  # geom_boxplot() +
  geom_violin(trim = FALSE)+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T), lty=2)+
  # Use the prepared table of test results as data for the geom
  geom_text(aes(label = Sig, y = MaxWidth + 0.2), size = 6,
            data = myt_tests)

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

### Figure 1


## All States Tissue P (all time)
p1=RegionFarm[complete.cases(RegionFarm$Tissue_TP_Percent),] %>% 
  ggplot(aes(y=Tissue_TP_Percent, x=st_abrv)) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  # geom_jitter(colour = 2)+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue Phosphorus (%)') +
  coord_cartesian(ylim = c(0, 1.5))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))
# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank())
## All States Shell P (all time)
p2=RegionFarm[complete.cases(RegionFarm$Shell_TP_Percent),] %>% 
  ggplot(aes(y=Shell_TP_Percent, x=st_abrv)) + 
  geom_boxplot(color = "black", notch=F, fill='gray90', outlier.shape = NA) +
  # geom_jitter(colour = 2)+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='State', y = 'Shell Phosphorus (%)') +
  coord_cartesian(ylim = c(0.02, 0.07))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) 
tiff(paste0(wd,"/manuscript/Phosphorus/Fig1_Morse_et_al_15_10.tiff"), height = 15, width = 10, units = 'cm', 
     compression = "lzw", res = 300)
p1 / p2  + plot_annotation(tag_levels = 'A')
dev.off()

### Figure 2 ###
## CT Seasonal Tissue P
p1=ggplot(data = RegionFarm[which(RegionFarm$st_abrv=='CT'),],
          aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Tissue_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  # geom_jitter(colour = 2)+
  coord_cartesian(ylim = c(min(RegionFarm$Tissue_TP_Percent[complete.cases(RegionFarm$Tissue_TP_Percent)]), 
                           max(RegionFarm$Tissue_TP_Percent[complete.cases(RegionFarm$Tissue_TP_Percent)])))+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.25, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Tissue_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Tissue Phosphorus (%)') +
  # scale_x_discrete(limits = 1:12, labels = month.abb)+
  scale_x_discrete(limits = month.abb)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial"))  
## CT Seasonal Shell P
p2=ggplot(data = RegionFarm[which(RegionFarm$st_abrv=='CT'),],
          aes(group=Month_Oysters_Removed, x=month.abb[Month_Oysters_Removed], y = Shell_TP_Percent)) +
  geom_boxplot(outlier.shape = NA, fill='gray90') +
  # geom_jitter(colour = 2)+
  coord_cartesian(ylim = c(min(RegionFarm$Shell_TP_Percent[complete.cases(RegionFarm$Shell_TP_Percent)]), 
                           max(RegionFarm$Shell_TP_Percent[complete.cases(RegionFarm$Shell_TP_Percent)])))+
  geom_jitter(shape = 19, color = "#0085CA", alpha = 0.5, position = position_jitter(0.1))+
  geom_hline(yintercept=mean(RegionFarm$Shell_TP_Percent, na.rm=T),linetype="dashed") +
  theme_classic()+
  labs(x='', y = 'Shell Phosphorus (%)') +
  # scale_x_discrete(limits = 1:12, labels = month.abb)+
  scale_x_discrete(limits = month.abb)+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14, family="Arial")) +
  theme(legend.text = element_text(size = 14, ))+
  theme(axis.text = element_text(size = 14, family="Arial")) 
tiff(paste0(wd,"/manuscript/Phosphorus/Fig2AB_Morse_et_al_15_10.tiff"), height = 15, width = 15, units = 'cm', 
     compression = "lzw", res = 300)
p1 / p2  + plot_annotation(tag_levels = 'A')
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
