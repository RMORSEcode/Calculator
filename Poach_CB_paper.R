library(tidyverse)
library(lubridate)
library(rstatix)
library(ggpubr)
library(moments)
library(quantreg)
library(ggbreak)

wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Nitrogen data"
files=list.files(wd, pattern='.xls')

# poach=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited_Nov2020.xlsx', sep=''), sheet='Compiled Data')
# poach2=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='compiled')
# create better version with all vars
PCBVA=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='Original Data Virginia RM')
PCBVA=PCBVA[-1,]
PCBMD=readxl::read_xlsx(paste(wd,'/', 'CB_oyster_nutrient_data_edited.xlsx', sep=''), sheet='Original Data Maryland RM')
PCBMD=PCBMD[-1,]
colnames(PCBVA)[!(colnames(PCBVA) %in% colnames(PCBMD))]
PCBVA=subset(PCBVA, select=-c(`bad_Shell_TC_g_C_per_ g_dw`, `bad_Shell_TN_g_N_per_g_dw`))
PCB=rbind(PCBVA, PCBMD)
PCB=subset(PCB, select=-c(`Shell_TP_g_P_per_g_dw`, `Shell_TP_Percent`))
## add shell P, dates are from first sampling date only, merge on sampled ID 
pcbshellPO4=readxl::read_xlsx("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Nitrogen data/Poach/Daily Shell Phosphate Tests_Dec_2020b.xlsx", sheet='Data_Summary')
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


### normality checks
# remove rows with extreme outliers from
test=identify_outliers(PCB, variabe=Tissue_N_Percent)
# t2=PCB$Number_ID %in% test$Number_ID 
# PCB2=PCB[-c(which(t2==T)),]
test=identify_outliers(PCB, variabe=Total_Shell_Height_Length_mm)
# t2=PCB2$Number_ID %in% test$Number_ID 
# PCB2=PCB2[-c(which(t2==T)),]
test=identify_outliers(PCB, variabe=Tissue_TP_Percent)
test=identify_outliers(PCB, variabe=Tissue_N_Percent)

# Now check for noramailty by state
PCB2=PCB
PCB2[PCB2$State=="Virginia",] %>% shapiro_test(`Tissue_N_Percent`, `Total_Shell_Height_Length_mm`)
PCB2[PCB2$State=="Maryland",] %>% shapiro_test(`Tissue_N_Percent`, `Total_Shell_Height_Length_mm`)
ggqqplot(PCB2$Tissue_N_Percent)
ggqqplot(log(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Virginia"]))
ggqqplot(log(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Maryland"]))
barplot(table(round(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Virginia"],-1)))
barplot(table(round(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Maryland"],-1)))

skewness(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Virginia"], na.rm = TRUE)
skewness(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Maryland"], na.rm = TRUE)

skewness(log(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Virginia"]), na.rm = TRUE)
skewness(log(PCB2$Total_Shell_Height_Length_mm[PCB2$State=="Maryland"]), na.rm = TRUE)

### grouped
### test for outlier
x=PCB %>% select(Tissue_N_Percent, Month_Oysters_Removed, State, Ploidy) %>%
  group_by(State, Month_Oysters_Removed) %>%
  identify_outliers(Tissue_N_Percent)

### test for normal distribution
x=PCB %>% select(Tissue_N_Percent, Month_Oysters_Removed, State, Ploidy, Total_Shell_Height_Length_mm) %>%
  group_by(State, Month_Oysters_Removed, Ploidy) %>%
  shapiro_test(Tissue_N_Percent, Total_Shell_Height_Length_mm)
x2=x[which(x$p<0.05),]

x=PCB %>% select(Tissue_N_Percent, Month_Oysters_Removed, State, Ploidy, Total_Shell_Height_Length_mm, Tissue_TP_Percent, 
                 Shell_N_Percent, Shell_TP_Percent) %>%
  group_by(State, Month_Oysters_Removed, Ploidy) %>%
  shapiro_test(Tissue_N_Percent, Total_Shell_Height_Length_mm, Tissue_TP_Percent, 
               Shell_N_Percent, Shell_TP_Percent)
x2=x[which(x$p<0.05),]
ggqqplot(VA, "Tissue_N_Percent", color='Ploidy', facet.by = "Month_Oysters_Removed", title = "VA %N")
ggqqplot(MD, "Tissue_N_Percent", color='Ploidy', facet.by = "Month_Oysters_Removed", title = "MD %N")
ggqqplot(VA, "Total_Shell_Height_Length_mm", color='Ploidy', facet.by = "Month_Oysters_Removed", title = "VA SH")
ggqqplot(MD, "Total_Shell_Height_Length_mm", color='Ploidy', facet.by = "Month_Oysters_Removed", title = "MD SH")
ggqqplot(VA, "Tissue_TP_Percent", color='Ploidy', facet.by = "Month_Oysters_Removed", title = "VA T %P")
ggqqplot(MD, "Tissue_TP_Percent", color='Ploidy', facet.by = "Month_Oysters_Removed", title = "MD T %P")

ggboxplot(VA[which(VA$Ploidy=="Diploid"),], x='Month_Oysters_Removed', y='Tissue_N_Percent', add='point' )
ggboxplot(VA[which(VA$Ploidy=="Triploid"),], x='Month_Oysters_Removed', y='Tissue_N_Percent', add='point' )
ggboxplot(MD[which(MD$Ploidy=="Diploid"),], x='Month_Oysters_Removed', y='Tissue_N_Percent', add='point' )
ggboxplot(MD[which(MD$Ploidy=="Triploid"),], x='Month_Oysters_Removed', y='Tissue_N_Percent', add='point' )

ggboxplot(PCB[which(PCB$Ploidy=="Diploid"),], x='Month_Oysters_Removed', y='Tissue_N_Percent', add='point', color='State' )
ggboxplot(PCB, x='Month_Oysters_Removed', y='Tissue_N_Percent', add='point', color='State', facet.by='Ploidy' )

ggboxplot(PCB, x='Month_Oysters_Removed', y='Tissue_N_Percent', add='point', color='Ploidy', facet.by='State' )
ggboxplot(PCB, x='Month_Oysters_Removed', y='Tissue_TP_Percent', add='point', color='Ploidy', facet.by='State' )
ggboxplot(PCB, x='Month_Oysters_Removed', y='Tissue_C_Percent', add='point', color='Ploidy', facet.by='State' )
ggboxplot(PCB, x='Month_Oysters_Removed', y='Shell_N_Percent', add='point', color='Ploidy', facet.by='State' )
ggboxplot(PCB, x='Month_Oysters_Removed', y='Shell_TP_Percent', add='point', color='Ploidy', facet.by='State' )
ggboxplot(PCB, x='Month_Oysters_Removed', y='Shell_C_Percent', add='point', color='Ploidy', facet.by='State' )

## by season
ggboxplot(PCB, x='Season_Oysters_Removed', y='Tissue_N_Percent', add='point', color='Ploidy', facet.by='State' )
ggboxplot(PCB, x='Season_Oysters_Removed', y='Tissue_TP_Percent', add='point', color='Ploidy', facet.by='State' )
ggboxplot(PCB, x='Season_Oysters_Removed', y='Tissue_C_Percent', add='point', color='Ploidy', facet.by='State' )

ggboxplot(PCB, y=c('Tissue_N_Percent', 'Tissue_TP_Percent'), add='point', color='Ploidy', facet.by = 'State' )

PCB %>% select(State, Ploidy, Tissue_N_Percent, Tissue_TP_Percent) %>% 
  pivot_longer(cols=c(Tissue_TP_Percent, Tissue_N_Percent), names_to = 'Nutrients', values_to = 'Percent')%>%
  ggboxplot(y='Percent', x='Nutrients', add='point', color='Ploidy', facet.by = 'State')

PCB %>% select(State, Ploidy, Tissue_N_Percent, Tissue_TP_Percent) %>% 
  pivot_longer(cols=c(Tissue_TP_Percent, Tissue_N_Percent), names_to = 'Nutrients', values_to = 'Percent')%>%
  ggboxplot(y='Percent', x='State', add='point', color='Ploidy', facet.by = 'Nutrients') 


PCB %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggboxplot(y='Tissue_N_Percent', x='State', fill='Ploidy', ylab = 'Tissue N Percent', xlab='', ylim=c(0,15),
            color = "black", palette = c("white", "gray"), notch=T) +
  theme(text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15))  
PCB %>% select(State, Ploidy, Tissue_TP_Percent) %>% 
  ggboxplot(y='Tissue_TP_Percent', x='State', fill='Ploidy',ylab = 'Tissue P Percent', xlab='', ylim=c(0,2),
            color = "black", palette = c("white", "gray"), notch=T)  +
  theme(text = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15))  

### USED FOR FIGURE 2 ###
## try with base ggplot for N
PCB %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggplot(aes(y=Tissue_N_Percent, x=Ploidy, fill=Ploidy)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Tissue N Percent') +
  coord_cartesian(ylim = c(0, 12))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  facet_wrap(~State) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 
## try with base ggplot for P
PCB %>% select(State, Ploidy, Tissue_TP_Percent) %>% 
  ggplot(aes(y=Tissue_TP_Percent, x=Ploidy, fill=Ploidy)) + 
  geom_boxplot(color = "black", notch=T) +
  theme_classic()+
  labs(x='', y = 'Tissue P Percent') +
  coord_cartesian(ylim = c(0, 2))+
  scale_fill_manual(values=c("white", "gray")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+ 
  facet_wrap(~State) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  theme(legend.position = "none") +
  theme(text = element_text(size = 20)) +
  theme(legend.text = element_text(size = 20)) 

## add stats to plots ###
my_comparisons=list( c("Virginia", "Maryland"))

PCB %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggboxplot(y='Tissue_N_Percent', x='State', fill='Ploidy', ylab = 'Tissue N Percent', xlab='', ylim=c(5,12), 
            color = "black", palette = c("white", "gray"), notch=T) + 
  # stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", fill="red") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red",
               position = position_dodge2(width = 0.75, preserve = "single"))+
stat_compare_means(label = "p.signif", method = "wilcox.test", label.y = c(11), ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label.y = c(11.5),label= "p.signif") 

PCB %>% select(State, Ploidy, Tissue_TP_Percent) %>% 
  ggboxplot(y='Tissue_TP_Percent', x='State', fill='Ploidy',ylab = 'Tissue P Percent', xlab='', ylim=c(0.5,1.8),
            color = "black", palette = c("white", "gray"), notch=T) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red",
               position = position_dodge2(width = 0.75, preserve = "single"))+
  stat_compare_means(label = "p.signif", method = "wilcox.test", label.y = c(1.55), ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label.y = c(1.6), label= "p.signif")

PCB %>% select(State, Ploidy, Tissue_C_Percent) %>% 
  ggboxplot(y='Tissue_C_Percent', x='State', fill='Ploidy', ylab = 'Tissue C Percent', xlab='', ylim=c(30,60),
            color = "black", palette = c("white", "gray"), notch=T) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", label.y = c(56), ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label.y = c(58), label= "p.signif") 

PCB %>% select(State, Ploidy, Shell_N_Percent) %>% 
  ggboxplot(y='Shell_N_Percent', x='State', fill='Ploidy', ylab = 'Shell N Percent', xlab='', ylim=c(0,0.75),
            color = "black", palette = c("white", "gray"), notch=T) + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red",
               position = position_dodge2(width = 0.75, preserve = "single"))+
  stat_compare_means(label = "p.signif", method = "wilcox.test", label.y = c(0.5), ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label.y = c(0.5), label= "p.signif") 

PCB %>% select(State, Ploidy, Shell_TP_Percent) %>% 
  ggboxplot(y='Shell_TP_Percent', x='State', fill='Ploidy',ylab = 'Shell P Percent', xlab='', ylim=c(0.02,0.075),
            color = "black", palette = c("white", "gray"), notch=T) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red",
               position = position_dodge2(width = 0.75, preserve = "single"))+
  stat_compare_means(label = "p.signif", method = "wilcox.test", label.y = c(.065), ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label.y = c(.07), label= "p.signif")

PCB %>% select(State, Ploidy, Shell_C_Percent) %>% 
  ggboxplot(y='Shell_C_Percent', x='State', fill='Ploidy', ylab = 'Shell C Percent', xlab='', ylim=c(10,15),
            color = "black", palette = c("white", "gray"), notch=T) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", label.y = c(14), ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", label.y = c(14),label= "p.signif") 





test=PCB %>% select(State, Month_Oysters_Removed, Ploidy, Tissue_N_Percent, Tissue_TP_Percent) %>% 
  group_by(Month_Oysters_Removed, State, Ploidy) %>%
  summarise(mnN=mean(Tissue_N_Percent, na.rm=T), mnP=mean(Tissue_TP_Percent, na.rm=T),
            medN=median(Tissue_N_Percent, na.rm=T), medP=median(Tissue_TP_Percent, na.rm=T), 
            sdN=sd(Tissue_N_Percent, na.rm=T),sdP=sd(Tissue_TP_Percent, na.rm=T), 
            q1N=quantile(Tissue_N_Percent, 0.25, na.rm=T), q3N=quantile(Tissue_N_Percent, 0.75, na.rm=T),
            q1P=quantile(Tissue_TP_Percent, 0.25, na.rm=T), q3P=quantile(Tissue_TP_Percent, 0.75, na.rm=T)) #%>%
  # test2=test %>% pivot_longer(cols=c(mnN, mnP, medN, medP, ), names_to = 'Nutrients', values_to = 'Value')
## drop quantiles and medians, add C
test=PCB %>% select(State, Month_Oysters_Removed, Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent) %>% 
  group_by(Month_Oysters_Removed, State, Ploidy) %>%
  summarise(mnN=mean(Tissue_N_Percent, na.rm=T), mnP=mean(Tissue_TP_Percent, na.rm=T),
            mnC=mean(Tissue_C_Percent, na.rm=T), sdC=sd(Tissue_C_Percent, na.rm=T),
            sdN=sd(Tissue_N_Percent, na.rm=T),sdP=sd(Tissue_TP_Percent, na.rm=T))


ggplot(test, aes(y='Percent', x='Month_Oysters_Removed', group='Nutrients', facet.by = 'State')) + geom_line()

t2=test %>% filter(Nutrients=='medN', Ploidy=="Diploid", State=='Virginia')

t2=test %>% filter(Nutrients=='medN', Ploidy=="Diploid", State=='Virginia')
t3=test %>% filter(Nutrients=='medN', Ploidy=="Triploid", State=='Virginia')
plot(t2$Percent~t2$Month_Oysters_Removed, type='b', pch=19, ylim=c(5,10), 
     las=1, ylab='Percent N', xlab='Month')
lines(t3$Percent~t3$Month_Oysters_Removed, col='red')
points(t3$Percent~t3$Month_Oysters_Removed, col='red', pch=19)
t2=test %>% filter(Nutrients=='medN', Ploidy=="Diploid", State=='Maryland')
t3=test %>% filter(Nutrients=='medN', Ploidy=="Triploid", State=='Maryland')
lines(t2$Percent~t3$Month_Oysters_Removed, col='black', lty=3)
points(t2$Percent~t3$Month_Oysters_Removed, col='black', pch=17)
lines(t3$Percent~t3$Month_Oysters_Removed, col='red', lty=3)
points(t3$Percent~t3$Month_Oysters_Removed, col='red', pch=17)
legend('topleft', lty = c(1,1,3,3), pch=c(19, 19, 17, 17), col = c('black', 'red', 'black', 'red'), 
       legend=c('VA diploid', 'VA triploid', 'MD diploid', 'MD triploid'), horiz = F, bty='n')

t2=test %>% filter(Nutrients=='medP', Ploidy=="Diploid", State=='Virginia')
t3=test %>% filter(Nutrients=='medP', Ploidy=="Triploid", State=='Virginia')
plot(t2$Percent~t2$Month_Oysters_Removed, type='b', pch=19, ylim=c(0,1.5), 
     las=1, ylab='Percent P', xlab='Month')
lines(t3$Percent~t3$Month_Oysters_Removed, col='red')
points(t3$Percent~t3$Month_Oysters_Removed, col='red', pch=19)
t2=test %>% filter(Nutrients=='medP', Ploidy=="Diploid", State=='Maryland')
t3=test %>% filter(Nutrients=='medP', Ploidy=="Triploid", State=='Maryland')
lines(t2$Percent~t3$Month_Oysters_Removed, col='black', lty=3)
points(t2$Percent~t3$Month_Oysters_Removed, col='black', pch=17)
lines(t3$Percent~t3$Month_Oysters_Removed, col='red', lty=3)
points(t3$Percent~t3$Month_Oysters_Removed, col='red', pch=17)
legend('topleft', lty = c(1,1,3,3), pch=c(19, 19, 17, 17), col = c('black', 'red', 'black', 'red'), 
       legend=c('VA diploid', 'VA triploid', 'MD diploid', 'MD triploid'), horiz = F, bty='n')

### 2 line mean plots by state with sd error bars for Tissue %N
t2=test %>% filter(Ploidy=="Diploid", State=='Virginia')
t3=test %>% filter(Ploidy=="Triploid", State=='Virginia')
plot(t2$mnN~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,12), 
     las=1, ylab='Tissue Percent N', xlab='Month', axes=F, cex.lab=1.5,
     cex.axis=1.5, cex=1.5)
box(bty='l')
axis(1,cex.axis=1.5)
axis(2, las=1, cex.axis=1.5)
lines(t3$mnN~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$mnN~t3$Month_Oysters_Removed, col='red', pch=17, cex=1.5)
segments(t2$Month_Oysters_Removed, t2$mnN-t2$sdN, t2$Month_Oysters_Removed,t2$mnN+t2$sdN, col='black')
segments(t3$Month_Oysters_Removed, t3$mnN-t3$sdN, t3$Month_Oysters_Removed,t3$mnN+t3$sdN, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), cex=c(1.5, 1.5), col = c('black', 'red'), 
       legend=c('VA diploid', 'VA triploid'), horiz = F, bty='n')
t2=test %>% filter(Ploidy=="Diploid", State=='Maryland')
t3=test %>% filter(Ploidy=="Triploid", State=='Maryland')
plot(t2$mnN~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,12), 
     las=1, ylab='Tissue Percent N', xlab='Month', axes=F, cex.lab=1.5,
     cex.axis=1.5, cex=1.5)
box(bty='l')
axis(1, cex.axis=1.5)
axis(2, las=1, cex.axis=1.5)
lines(t3$mnN~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$mnN~t3$Month_Oysters_Removed, col='red', pch=17, cex=1.5)
segments(t2$Month_Oysters_Removed, t2$mnN-t2$sdN, t2$Month_Oysters_Removed,t2$mnN+t2$sdN, col='black')
segments(t3$Month_Oysters_Removed, t3$mnN-t3$sdN, t3$Month_Oysters_Removed,t3$mnN+t3$sdN, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), cex=c(1.5, 1.5), col = c('black', 'red'), 
       legend=c('MD diploid', 'MD triploid'), horiz = F, bty='n')
### 2 line means plots by state with sd error bars for Tissue %P
t2=test %>% filter(Ploidy=="Diploid", State=='Virginia')
t3=test %>% filter(Ploidy=="Triploid", State=='Virginia')
plot(t2$mnP~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,1.5), 
     las=1, ylab='Tissue Percent P', xlab='Month', axes=F, cex.lab=1.5,
     cex.axis=1.5, cex=1.5)
box(bty='l')
axis(1,cex.axis=1.5)
axis(2, las=1,cex.axis=1.5)
lines(t3$mnP~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$mnP~t3$Month_Oysters_Removed, col='red', pch=17,cex=1.5)
segments(t2$Month_Oysters_Removed, t2$mnP-t2$sdP, t2$Month_Oysters_Removed,t2$mnP+t2$sdP, col='black')
segments(t3$Month_Oysters_Removed, t3$mnP-t3$sdP, t3$Month_Oysters_Removed,t3$mnP+t3$sdP, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), col = c('black', 'red'), 
       legend=c('VA diploid', 'VA triploid'), cex=c(1.5, 1.5), horiz = F, bty='n')
t2=test %>% filter(Ploidy=="Diploid", State=='Maryland')
t3=test %>% filter(Ploidy=="Triploid", State=='Maryland')
plot(t2$mnP~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,1.5), 
     las=1, ylab='Tissue Percent P', xlab='Month', axes=F, cex.lab=1.5,
     cex.axis=1.5, cex=1.5)
box(bty='l')
axis(1,cex.axis=1.5)
axis(2, las=1,cex.axis=1.5)
lines(t3$mnP~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$mnP~t3$Month_Oysters_Removed, col='red', pch=17,cex=1.5)
segments(t2$Month_Oysters_Removed, t2$mnP-t2$sdP, t2$Month_Oysters_Removed,t2$mnP+t2$sdP, col='black')
segments(t3$Month_Oysters_Removed, t3$mnP-t3$sdP, t3$Month_Oysters_Removed,t3$mnP+t3$sdP, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), col = c('black', 'red'), 
       legend=c('MD diploid', 'MD triploid'), cex=c(1.5, 1.5), horiz = F, bty='n')

### 2 line means plots by state with sd error bars for Tissue %P
t2=test %>% filter(Ploidy=="Diploid", State=='Virginia')
t3=test %>% filter(Ploidy=="Triploid", State=='Virginia')
plot(t2$mnC~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(30,50), 
     las=1, ylab='Tissue Percent C', xlab='Month', axes=F)
box(bty='l')
axis(1)
axis(2, las=1)
lines(t3$mnC~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$mnC~t3$Month_Oysters_Removed, col='red', pch=17)
segments(t2$Month_Oysters_Removed, t2$mnC-t2$sdC, t2$Month_Oysters_Removed,t2$mnC+t2$sdC, col='black')
segments(t3$Month_Oysters_Removed, t3$mnC-t3$sdC, t3$Month_Oysters_Removed,t3$mnC+t3$sdC, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), col = c('black', 'red'), 
       legend=c('VA diploid', 'VA triploid'), horiz = F, bty='n')
t2=test %>% filter(Ploidy=="Diploid", State=='Maryland')
t3=test %>% filter(Ploidy=="Triploid", State=='Maryland')
plot(t2$mnC~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(30,50), 
     las=1, ylab='Tissue Percent C', xlab='Month', axes=F)
box(bty='l')
axis(1)
axis(2, las=1)
lines(t3$mnC~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$mnC~t3$Month_Oysters_Removed, col='red', pch=17)
segments(t2$Month_Oysters_Removed, t2$mnC-t2$sdC, t2$Month_Oysters_Removed,t2$mnC+t2$sdC, col='black')
segments(t3$Month_Oysters_Removed, t3$mnC-t3$sdC, t3$Month_Oysters_Removed,t3$mnC+t3$sdC, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), col = c('black', 'red'), 
       legend=c('MD diploid', 'MD triploid'), horiz = F, bty='n')
### 2 line plots median by state with quantile error bars for Tissue %N
t2=test %>% filter(Ploidy=="Diploid", State=='Virginia')
t3=test %>% filter(Ploidy=="Triploid", State=='Virginia')
plot(t2$medN~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,12), 
     las=1, ylab='Tissue Percent N', xlab='Month', axes=F)
box(bty='l')
axis(1)
axis(2, las=1)
lines(t3$medN~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$medN~t3$Month_Oysters_Removed, col='red', pch=17)
segments(t2$Month_Oysters_Removed, t2$q1N , t2$Month_Oysters_Removed,t2$q3N, col='black')
segments(t3$Month_Oysters_Removed, t3$q1N, t3$Month_Oysters_Removed,t3$q3N, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), col = c('black', 'red'), 
       legend=c('VA diploid', 'VA triploid'), horiz = F, bty='n')
t2=test %>% filter(Ploidy=="Diploid", State=='Maryland')
t3=test %>% filter(Ploidy=="Triploid", State=='Maryland')
plot(t2$medN~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,12), 
     las=1, ylab='Tissue Percent N', xlab='Month', axes=F)
box(bty='l')
axis(1)
axis(2, las=1)
lines(t3$medN~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$medN~t3$Month_Oysters_Removed, col='red', pch=17)
segments(t2$Month_Oysters_Removed, t2$q1N , t2$Month_Oysters_Removed,t2$q3N, col='black')
segments(t3$Month_Oysters_Removed, t3$q1N, t3$Month_Oysters_Removed,t3$q3N, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), col = c('black', 'red'), 
       legend=c('MD diploid', 'MD triploid'), horiz = F, bty='n')
### 2 line plots median by state with quantile error bars for Tissue %P
t2=test %>% filter(Ploidy=="Diploid", State=='Virginia')
t3=test %>% filter(Ploidy=="Triploid", State=='Virginia')
plot(t2$medN~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,12), 
     las=1, ylab='Tissue Percent N', xlab='Month', axes=F)
box(bty='l')
axis(1)
axis(2, las=1)
lines(t3$medN~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$medN~t3$Month_Oysters_Removed, col='red', pch=17)
segments(t2$Month_Oysters_Removed, t2$q1N , t2$Month_Oysters_Removed,t2$q3N, col='black')
segments(t3$Month_Oysters_Removed, t3$q1N, t3$Month_Oysters_Removed,t3$q3N, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), col = c('black', 'red'), 
       legend=c('VA diploid', 'VA triploid'), horiz = F, bty='n')
t2=test %>% filter(Ploidy=="Diploid", State=='Maryland')
t3=test %>% filter(Ploidy=="Triploid", State=='Maryland')
plot(t2$medN~t2$Month_Oysters_Removed, type='b', pch=19, lty=1, ylim=c(0,12), 
     las=1, ylab='Tissue Percent N', xlab='Month', axes=F)
box(bty='l')
axis(1)
axis(2, las=1)
lines(t3$medN~t3$Month_Oysters_Removed, col='red', lty=2)
points(t3$medN~t3$Month_Oysters_Removed, col='red', pch=17)
segments(t2$Month_Oysters_Removed, t2$q1N , t2$Month_Oysters_Removed,t2$q3N, col='black')
segments(t3$Month_Oysters_Removed, t3$q1N, t3$Month_Oysters_Removed,t3$q3N, col='red', lty=2)
legend('topleft', lty = c(1,2), pch=c(19, 17), col = c('black', 'red'), 
       legend=c('MD diploid', 'MD triploid'), horiz = F, bty='n')



# res.aov=anova_test(data=PCB, dv=Tissue_N_Percent, wid=State, within=Ploidy)

# 2-way anova
PCB %>% anova_test(Tissue_N_Percent ~ Ploidy * State)

PCB[complete.cases(PCB$Tissue_N_Percent),] %>% anova_test(Tissue_N_Percent ~ Ploidy * State)
VA[complete.cases(VA$Tissue_N_Percent),] %>% anova_test(Tissue_N_Percent ~ Ploidy * Month_Oysters_Removed)
MD[complete.cases(MD$Tissue_N_Percent),] %>% anova_test(Tissue_N_Percent ~ Ploidy * Month_Oysters_Removed)

# 2-way repeated measures (throws spread error)
#n throws error
# PCB[complete.cases(PCB$Tissue_N_Percent),] %>% anova_test(Tissue_N_Percent ~ Ploidy * State + Error(Month_Oysters_Removed/(Ploidy * State)))
# PCB[complete.cases(PCB$Tissue_N_Percent),] %>% anova_test(dv=Tissue_N_Percent, wid=Month_Oysters_Removed, within=c(Ploidy, State))
PCB[complete.cases(PCB$Tissue_N_Percent),] %>% 
  anova_test(dv=Tissue_N_Percent, wid=State, between=c(Month_Oysters_Removed, Ploidy))

x=PCB[complete.cases(PCB$Tissue_N_Percent),] %>% 
  anova_test(dv=Tissue_N_Percent, wid=Month_Oysters_Removed, between=c(State, Ploidy))

x=PCB[complete.cases(PCB$Tissue_N_Percent),] %>% 
  anova_test(dv=Tissue_N_Percent, wid=Month_Oysters_Removed, between=c(Season_Oysters_Removed, State, Ploidy))

#grouped 
PCB[complete.cases(PCB$Tissue_N_Percent),] %>% 
  group_by(State) %>%
  anova_test(dv=Tissue_N_Percent, wid=Ploidy, between=Month_Oysters_Removed)

PCB[complete.cases(PCB$Tissue_N_Percent),] %>% 
  group_by(State) %>%
  anova_test(dv=Tissue_N_Percent, wid=Ploidy, between=Month_Oysters_Removed)


PCB[complete.cases(PCB$Tissue_N_Percent),] %>% 
  group_by(State) %>%
  anova_test(dv=Tissue_N_Percent, wid=Month_Oysters_Removed, between=Ploidy)


## unbalanced design:
my_anova=aov(Tissue_N_Percent ~ State * Ploidy, data=PCB[complete.cases(PCB$Tissue_N_Percent),])
car::Anova(my_anova, type = "III")




###

# By state
VA=PCB %>% filter(State == 'Virginia')
MD=PCB %>% filter(State == 'Maryland')

## individual box plots
boxplot(VA$Tissue_N_Percent[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], main='VA Triploid', ylab='Tissue %N', xlab='Month', ylim=c(5,12), las=1, col='gray40')
boxplot(VA$Tissue_N_Percent[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], main='VA Diploid', ylab='Tissue %N', xlab='Month', ylim=c(5,12), las=1, col='orange')
boxplot(MD$Tissue_N_Percent[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], main='MD Triploid', ylab='Tissue %N', xlab='Month', ylim=c(5,12), las=1, col='gray')
boxplot(MD$Tissue_N_Percent[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], main='MD Diploid', ylab='Tissue %N', xlab='Month', ylim=c(5,12), las=1, col='red')
## VA Triploid N
model <- aov(VA$Tissue_N_Percent[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'])
summary(model)
pairwise.t.test(VA$Tissue_N_Percent[VA$Ploidy=='Triploid'], VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], p.adjust.method = "bonf")
## VA Diploid N
model <- aov(VA$Tissue_N_Percent[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'])
summary(model)
pairwise.t.test(VA$Tissue_N_Percent[VA$Ploidy=='Diploid'], VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], p.adjust.method = "bonf")

## MD Triploid N
model <- aov(MD$Tissue_N_Percent[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'])
summary(model)
pairwise.t.test(MD$Tissue_N_Percent[MD$Ploidy=='Triploid'], MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], p.adjust.method = "bonf")
## MD Diploid N
model <- aov(MD$Tissue_N_Percent[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'])
summary(model)
pairwise.t.test(MD$Tissue_N_Percent[MD$Ploidy=='Diploid'], MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], p.adjust.method = "bonf")

### compare %N between sites (MD vs VA)
model <- aov(PCB$Tissue_N_Percent ~ PCB$Location_Index)
summary(model) # -> difference
### compare differences in Ploidy by site -> VA diploid different from triploid
ggplot(PCB, aes(x = as.factor(State), y = Tissue_N_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("") + 
  ylab("Percent N") + 
  ylim(5,11) +
  labs(title = "Ploidy") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
model <- aov(MD$Tissue_N_Percent ~ MD$Ploidy)
summary(model)
model <- aov(VA$Tissue_N_Percent ~ VA$Ploidy)
summary(model)

### Difference in Tissue %P between sites (yes)
model <- aov(PCB$Tissue_TP_Percent ~ PCB$Location_Index)
summary(model) # -> difference
### compare differences in Ploidy by site -> VA diploid different from triploid
ggplot(PCB, aes(x = as.factor(Location_Index), y = Tissue_TP_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Site") + 
  ylab("Percent P") + 
  ylim(0,2) +
  labs(title = "Ploidy") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
model <- aov(MD$Tissue_TP_Percent ~ MD$Ploidy)
summary(model)
model <- aov(VA$Tissue_TP_Percent ~ VA$Ploidy)
summary(model)



### Difference in Tissue %C between sites (no)
model <- aov(PCB$Tissue_C_Percent ~ PCB$Location_Index)
summary(model) # -> difference
### compare differences in Ploidy by site -> VA diploid different from triploid
ggplot(PCB, aes(x = as.factor(Location_Index), y = Tissue_C_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Site") + 
  ylab("Percent C") + 
  ylim(30,50) +
  labs(title = "Ploidy") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
model <- aov(MD$Tissue_TP_Percent ~ MD$Ploidy)
summary(model)
model <- aov(VA$Tissue_TP_Percent ~ VA$Ploidy)
summary(model)
## individual box plots Tissue P
boxplot(VA$Tissue_TP_Percent[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], main='VA Triploid', ylab='Tissue %P', xlab='Month', ylim=c(0,2), las=1, col='gray40')
boxplot(VA$Tissue_TP_Percent[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], main='VA Diploid', ylab='Tissue %P', xlab='Month', ylim=c(0,2), las=1, col='orange')
boxplot(MD$Tissue_TP_Percent[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], main='MD Triploid', ylab='Tissue %P', xlab='Month', ylim=c(0,2), las=1, col='gray')
boxplot(MD$Tissue_TP_Percent[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], main='MD Diploid', ylab='Tissue %P', xlab='Month', ylim=c(0,2), las=1, col='red')
## VA Triploid N
model <- aov(VA$Tissue_TP_Percent[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'])
summary(model)
pairwise.t.test(VA$Tissue_TP_Percent[VA$Ploidy=='Triploid'], VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], p.adjust.method = "bonf")
## VA Diploid N
model <- aov(VA$Tissue_TP_Percent[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'])
summary(model)
pairwise.t.test(VA$Tissue_TP_Percent[VA$Ploidy=='Diploid'], VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], p.adjust.method = "bonf")
## MD Triploid N
model <- aov(MD$Tissue_TP_Percent[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'])
summary(model)
pairwise.t.test(MD$Tissue_TP_Percent[MD$Ploidy=='Triploid'], MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], p.adjust.method = "bonf")
## MD Diploid N
model <- aov(MD$Tissue_TP_Percent[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'])
summary(model)
pairwise.t.test(MD$Tissue_TP_Percent[MD$Ploidy=='Diploid'], MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], p.adjust.method = "bonf")

### Shell N ###
### Difference in Tissue %P between sites (yes)
model <- aov(PCB$Shell_N_Percent ~ PCB$Location_Index)
summary(model) # -> difference
### compare differences in Ploidy by site -> VA diploid different from triploid
ggplot(PCB, aes(x = as.factor(Location_Index), y = Shell_N_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Site") + 
  ylab("Shell Percent N") + 
  ylim(0,0.5) +
  labs(title = "Ploidy") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
model <- aov(MD$Shell_N_Percent ~ MD$Ploidy)
summary(model)
model <- aov(VA$Shell_N_Percent ~ VA$Ploidy)
summary(model)
## individual box plots Tissue P (not sufficiently sampled)
boxplot(VA$Shell_N_Percent[VA$Ploidy=='Triploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Triploid'], main='VA Triploid', ylab='Tissue %P', xlab='Month', ylim=c(0,0.5), las=1, col='gray40')
boxplot(VA$Shell_N_Percent[VA$Ploidy=='Diploid'] ~ VA$Month_Oysters_Removed[VA$Ploidy=='Diploid'], main='VA Diploid', ylab='Tissue %P', xlab='Month', ylim=c(0,0.5), las=1, col='orange')
boxplot(MD$Shell_N_Percent[MD$Ploidy=='Triploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Triploid'], main='MD Triploid', ylab='Tissue %P', xlab='Month', ylim=c(0,0.5), las=1, col='gray')
boxplot(MD$Shell_N_Percent[MD$Ploidy=='Diploid'] ~ MD$Month_Oysters_Removed[MD$Ploidy=='Diploid'], main='MD Diploid', ylab='Tissue %P', xlab='Month', ylim=c(0,0.5), las=1, col='red')

### Shell P ###
### Difference in Tissue %P between sites (yes)
model <- aov(PCB$Shell_TP_Percent ~ PCB$Location_Index)
summary(model) # -> difference
### compare differences in Ploidy by site -> VA diploid different from triploid
ggplot(PCB, aes(x = as.factor(Location_Index), y = Shell_TP_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Site") + 
  ylab("Shell %P") + 
  ylim(0.03,0.06) +
  labs(title = "Ploidy") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
model <- aov(MD$Shell_TP_Percent ~ MD$Ploidy)
summary(model)
model <- aov(VA$Shell_TP_Percent ~ VA$Ploidy)
summary(model)

### Shell C ###
### Difference in Tissue %P between sites (yes)
model <- aov(PCB$Shell_C_Percent ~ PCB$Location_Index)
summary(model) # -> difference
### compare differences in Ploidy by site -> VA diploid different from triploid
ggplot(PCB, aes(x = as.factor(Location_Index), y = Shell_C_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Site") + 
  ylab("Shell %P") + 
  ylim(10,14) +
  labs(title = "Ploidy") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
model <- aov(MD$Shell_TP_Percent ~ MD$Ploidy)
summary(model)
model <- aov(VA$Shell_TP_Percent ~ VA$Ploidy)
summary(model)



### ggplot boxplots with both sites, ploidy 
ggplot(VA, aes(x = as.factor(Month_Oysters_Removed), y = Tissue_N_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Month") + 
  ylab("Percent N") + 
  ylim(5,11) +
  labs(title = "Rappahannock River") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggplot(MD, aes(x = as.factor(Month_Oysters_Removed), y = Tissue_N_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Month") + 
  ylab("Percent N") + 
  ylim(5,11) +
  labs(title = "Chester River") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggplot(VA, aes(x = as.factor(Month_Oysters_Removed), y = Tissue_TP_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Month") + 
  ylab("Percent P") + 
  ylim(0.5,1.5) +
  labs(title = "Rappahannock River") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()
ggplot(MD, aes(x = as.factor(Month_Oysters_Removed), y = Tissue_TP_Percent, fill = Ploidy)) +
  geom_boxplot()+ 
  stat_boxplot(geom = "errorbar") +
  geom_boxplot(outlier.colour = NA) +
  xlab("Month") + 
  ylab("Percent P") + 
  ylim(0.5,1.5) +
  labs(title = "Chester River") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()







### compare distribution of SH by ploidy
tbl=with(poachdiploid, table(round(Total_Shell_Height_Length_mm,-1), Waterbody_Name))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Waterbody_Name)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Site", title="Diploid")
tbl=with(poachtriploid, table(round(Total_Shell_Height_Length_mm,-1), Waterbody_Name))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Waterbody_Name)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Site", title="Triploid")
### compare distribution of SH by month removed
tbl=with(poach, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="Size at removal")
tbl=with(MD, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="MD Size at removal")
tbl=with(VA, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="VA Size at removal")
tbl=with(poachdiploid, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="Dip Size at removal")
tbl=with(poachtriploid, table(round(Total_Shell_Height_Length_mm,-1), Month_Oysters_Removed))
ggplot(as.data.frame(tbl), aes(factor(Var1), Freq, fill = Month_Oysters_Removed)) +     
  geom_col(position = 'dodge')+
  labs(x="Shell height (mm)", y="Frequency", fill="Month Removed", title="Trip Size at removal")



### Shell N %
plot(PCBVA$Shell_N_Percent[which(PCBVA$Ploidy=="Triploid")] ~ 
       PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Triploid")], 
     ylim=c(0,0.5), xlim=c(20,150), type='p', pch=17, col='black', ylab='Shell %N', xlab='SH (mm)', las=1, main='Shell N')
points(PCBVA$Shell_N_Percent[which(PCBVA$Ploidy=="Diploid")] ~ 
         PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Diploid")],
       pch=17, col='orange')
points(PCBMD$Shell_N_Percent[which(PCBMD$Ploidy=="Triploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Triploid")],  pch=19, col='gray')
points(PCBMD$Shell_N_Percent[which(PCBMD$Ploidy=="Diploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Diploid")], pch=19, col='red')
legend('topright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))
### Tissue N %
plot(PCBVA$Tissue_N_Percent[which(PCBVA$Ploidy=="Triploid")] ~ 
       PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Triploid")], 
     ylim=c(4,11), xlim=c(20,150), type='p', pch=17, col='black', ylab='Tissue %N', xlab='SH (mm)', las=1, main='Tissue N')
points(PCBVA$Tissue_N_Percent[which(PCBVA$Ploidy=="Diploid")] ~ 
         PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Diploid")],
       pch=17, col='orange')
points(PCBMD$Tissue_N_Percent[which(PCBMD$Ploidy=="Triploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Triploid")],  pch=19, col='gray')
points(PCBMD$Tissue_N_Percent[which(PCBMD$Ploidy=="Diploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Diploid")], pch=19, col='red')
legend('bottomright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))
### Shell P %
plot(PCB$Shell_TP_Percent[which(PCB$Ploidy=="Triploid" & PCB$Location_Index=="RAPP")] ~ 
       PCB$Total_Shell_Height_Length_mm[which(PCB$Ploidy=="Triploid" & PCB$Location_Index=="RAPP")], 
     ylim=c(0.03,0.06), xlim=c(20,150), type='p', pch=17, col='black', ylab='Shell %P', xlab='SH (mm)', las=1, main='Shell P')
points(PCB$Shell_TP_Percent[which(PCB$Ploidy=="Diploid"& PCB$Location_Index=="RAPP")] ~ 
         PCB$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Diploid" & PCB$Location_Index=="RAPP")],
       pch=17, col='orange')
points(PCB$Shell_TP_Percent[which(PCBMD$Ploidy=="Triploid"& PCB$Location_Index=="Chester")] ~ 
         PCB$Total_Shell_Height_Length_mm[which(PCB$Ploidy=="Triploid"& PCB$Location_Index=="Chester")],  pch=19, col='gray')
points(PCB$Shell_TP_Percent[which(PCBMD$Ploidy=="Diploid"& PCB$Location_Index=="Chester")] ~ 
         PCB$Total_Shell_Height_Length_mm[which(PCB$Ploidy=="Diploid"& PCB$Location_Index=="Chester")], pch=19, col='red')
legend('topright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))
### Tissue P %
plot(PCBVA$Tissue_TP_Percent[which(PCBVA$Ploidy=="Triploid")] ~ 
       PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Triploid")], 
     ylim=c(0,2), xlim=c(0,150), type='p', pch=17, col='black', ylab='Tissue %P', xlab='SH (mm)', las=1, main='Tissue P')
points(PCBVA$Tissue_TP_Percent[which(PCBVA$Ploidy=="Diploid")] ~ 
         PCBVA$Total_Shell_Height_Length_mm[which(PCBVA$Ploidy=="Diploid")],
       pch=17, col='orange')
points(PCBMD$Tissue_TP_Percent[which(PCBMD$Ploidy=="Triploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Triploid")],  pch=19, col='gray')
points(PCBMD$Tissue_TP_Percent[which(PCBMD$Ploidy=="Diploid")] ~ 
         PCBMD$Total_Shell_Height_Length_mm[which(PCBMD$Ploidy=="Diploid")], pch=19, col='red')
legend('topright', bty = 'n', horiz = F, pch=c(19,19,17,17), col=c('red', 'gray','orange', 'black'),legend=c("Chester diploid", "Chester triploid", "Rapp diploid", "Rapp triploid"))

### Water Quality ###
## CBP data for Chester River and Rappahannock River near farms for Poach data
# WQ=read.csv("C:/Users/ryan.morse/Downloads/WaterQualityWaterQualityStation.csv")
wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/"
wd="C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Nitrogen data/Poach"
WQ=read.csv(paste(wd, 'WaterQualityWaterQualityStation.csv', sep=''))
## most recent, 2015-2017 with secchi
WQ=read.csv("C:/Users/ryan.morse/Documents/Aquaculture/Shellfish permitting and ecosystem services/Shellfish Calculators/Nitrogen data/Poach/CBP.csv")
WQ$SampleDate=mdy(WQ$SampleDate)
## remove replicates (chl)
WQ2=WQ[order(WQ$Station, WQ$SampleDate),] %>% 
  filter(Layer=="S " | Layer=="B ") %>% 
  group_by(SampleDate, Layer, Parameter, Station) %>%
  summarise(across(everything(), mean))
## limit time 2015-2017
# WQf=WQ2 %>% filter(year(SampleDate)<2018, year(SampleDate)>2014)
## choose to plot: 
unique(WQ2$Parameter)
k <- "mu"
param="SECCHI"; plab="Secchi depth (m)"; pt="SZ.tiff"
param="TURB_NTU"; plab="Turbidity NTU"; pt="Turb.tiff"

param="SALINITY"; plab="Salinity (PSU)"; pt="S.tiff"
param="WTEMP"; plab="Water temperature (°C)"; pt="T.tiff"
param="CHLA"; plab=expression(paste("Chlorophyll a (", mu, "g/L)", sep='')); pt='Chl2.tiff'
param="DIN"; plab="DIN (mg/L)"; pt="DIN.tiff"
param="DO"; plab="Dissolved oxygen (mg/L)"; pt="DO.tiff"
WQ2 %>% filter(Parameter==param, year(SampleDate)<2018, year(SampleDate)>2014) %>%
  ggplot(mapping=aes(x=SampleDate, y=MeasureValue)) + 
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    text = element_text(size = 15),
    legend.position = "none",
    # legend.position = c(0.4,0.8),
    legend.box = 'horizontal'
  ) +
  labs(y=plab, x='') +
  scale_color_manual(labels = c("MD ET4.2", "VA LE3.4"), values = c("black", "red")) +
  # scale_y_break(c(45, 80)) +
  geom_line(aes(color=Station, linetype=Layer)) + 
  geom_point(aes(color=Station, shape=Layer)) #+
  # geom_hline(yintercept=1)
ggsave(filename = pt, plot=last_plot(), path=wd, device='tiff', width = 4, height=3, units="in", dpi=300, bg='white')

# trying to get them all aligned, does not work with scale_y_break for Chl
# library(cowplot)
# plot_grid(
#   p1, p2, p3, p4, p5, p6,
#   nrow=3,
#   align="hv"
# )
## this works, but x-axis lengths are different (ok?)
aplot::plot_list(p1, p2, p3, p4, p5, p6, nrow=3)
ggsave(filename = 'Fig_2.tiff', plot=last_plot(), path=wd, device='tiff', width = 11, height=8.5, units="in", dpi=300, bg='white')


test=WQ2 %>% filter(Parameter==param, year(SampleDate)<2018, year(SampleDate)>2014) %>%
  group_by(Station, Layer, )%>% 
  summarize(min = min(MeasureValue),
            median = median(MeasureValue),
            mean = mean(MeasureValue),
            max = max(MeasureValue))
t=WQ2 %>% filter(Parameter==param, year(SampleDate)<2018, year(SampleDate)>2014)
tva=t[which(t$Station=='LE3.4'),]
tmd=t[which(t$Station=='ET4.2'),]
range(tmd$MeasureValue[tmd$Layer=='B '],na.rm=T)

table(tmd$MeasureValue[tmd$Layer=='B ']<14)
table(tmd$MeasureValue[tmd$Layer=='S ']<14)
table(tva$MeasureValue[tva$Layer=='B ']<14)
table(tva$MeasureValue[tva$Layer=='S ']<14)

# WQ2 %>% filter(Parameter=="WTEMP") %>%
#   ggplot(mapping=aes(x=SampleDate, y=MeasureValue)) +
#   geom_line(aes(color=Station, linetype=Layer))


### Shell height vs DW ###
# testing ploidy on fits from CBay Poach data
plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"], type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='Tissue dry weight (g)', xlab='Shell height (mm)', pch=19, las=1,
     cex.axis=1.5,cex.lab=1.5)
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], pch=1, col='purple', )
clmt=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"]))
# summary(clmt)
# xval=seq(20,140,by=.5)
# yval=exp(clmt$coefficients[1])*xval^clmt$coefficients[2]
# lines(xval, yval, col='red', lwd=2)
clmd=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"]))
# summary(clmd)
# yval=exp(clmd$coefficients[1])*xval^clmd$coefficients[2]
# lines(xval, yval, col='black', lwd=2)
t=PCB %>% filter(Ploidy=="Diploid")
qr2=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
xval=seq(20,140,by=.5)
yval=exp(qr2$coefficients[1])*xval^qr2$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=1)
t=PCB %>% filter(Ploidy=="Triploid")
qr3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
yval=exp(qr3$coefficients[1])*xval^qr3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
# legend('topleft', lty=c(1,1,2,2), pch=c(NA,NA,NA,NA),lwd=c(2,2,2,2),col=c('black', 'red','black', 'red'), 
#        bty = 'n', horiz = F, legend=c("Diploid", "Triploid","CB Diploid", "CB Triploid"), 
#        text.col = c('gray', 'purple', 'black', 'black'), cex=c(1.5, 1.5, 1.5, 1.5))
# legend('topleft', lty=c(1,1,2,2), pch=c(19,1,NA,NA),col=c('gray', 'purple',NA, NA), 
#        bty = 'n', horiz = F, legend=c("Diploid", "Triploid","CB Diploid", "CB Triploid"), 
#        text.col = c('gray', 'purple', 'black', 'black'), cex=c(1.5, 1.5, 1.5, 1.5))
# yval=exp(cbdlm$coefficients[1])*xval^cbdlm$coefficients[2]
# yval=(0.00037)*xval^1.83359 # CBP all data BMP Second Report Appendix -> diploids w/out gear (see below)
yval2a=(0.0004)*xval^1.82 # CBP bay data (first report diploid)
yval3a=(0.00005)*xval^2.39 # CBP bay data (first report triploid)
lines(xval, yval2a, col='black', lwd=1, lty=2)
# yval2=exp(cbtlm$coefficients[1])*xval^cbtlm$coefficients[2]
lines(xval, yval3a, col='red', lwd=1, lty=2)

## 2023 report updates
yval=(0.00037)*xval^1.83359 # CB BMP diploids w/out gear BMP Second Report Appendix 2023
yval2=(0.00016)*xval^2.07714 # CB BMP diploids w/ gear BMP Second Report Appendix 2023
yval3=(0.00002)*xval^2.607 # CB BMP triploids w/ gear BMP Second Report Appendix 2023
lines(xval, yval, col='black', lwd=2, lty=2)
lines(xval, yval2, col='black', lwd=2, lty=3)
lines(xval, yval3, col='red', lwd=2, lty=2)
legend('topleft', lty=c(1,1,2,2,3), pch=c(NA,NA,NA,NA,NA),lwd=c(2,2,2,2,2),col=c('black', 'red','black', 'red', 'black'), 
       bty = 'n', horiz = F, legend=c("Diploid", "Triploid","CB Diploid", "CB Triploid w/gear","CB Diploid w/gear"), 
       text.col = c('gray', 'purple', 'black', 'black', 'black'), cex=c(1.5, 1.5, 1.5, 1.5, 1.5))
legend('topleft', lty=c(1,1,2,2,3), pch=c(19,1,NA,NA,NA),col=c('gray', 'purple',NA, NA, NA), 
       bty = 'n', horiz = F, legend=c(NA, NA, NA, NA, NA), 
       text.col = c('gray', 'purple', 'black', 'black', 'black'), cex=c(1.5, 1.5, 1.5, 1.5, 1.5))

## with all 7 curves....
legend('topleft', lty=c(1,1,2,2,3,3,3), pch=c(NA,NA,NA,NA,NA,NA,NA),lwd=c(2,2,2,2,2,1,1),col=c('black', 'red','black', 'red', 'black','black', 'red'), 
       bty = 'n', horiz = F, legend=c("2N+g", "3N+g","CB 2N", "CB 3N+g","CB 2n+g", "CB 2N 2016", "CB 3N 2016"), 
       text.col = c('gray', 'purple', 'black', 'black', 'black','black','black'), cex=c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5))

## for shells DW:SH
plot(PCB$Shell_Dry_Weight_g[PCB$Ploidy=="Diploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"], type='p', 
     col='gray', ylim=c(0,200), xlim=c(0,140), ylab='Shell dry weight (g)', xlab='Shell height (mm)', pch=19, las=1,
     cex.axis=1.5,cex.lab=1.5)
points(PCB$Shell_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], pch=1, col='purple')
t=PCB %>% filter(Ploidy=="Diploid")
qr2=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
xval=seq(20,140,by=.5)
yval=exp(qr2$coefficients[1])*xval^qr2$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=1)
t=PCB %>% filter(Ploidy=="Triploid")
qr3=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
yval=exp(qr3$coefficients[1])*xval^qr3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
qr1=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=PCB, tau=0.5, na.action = 'na.omit')
yval=exp(qr1$coefficients[1])*xval^qr1$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=2)
legend('topleft', lty=c(1,1,2), lwd=c(2,2,2),col=c('black', 'red','black'), 
       bty = 'n', horiz = F, legend=c("Diploid", "Triploid","Combined"), 
       text.col = c('gray','purple','black'), cex=c(1.5, 1.5, 1.5))

legend('topleft', pch=c(19,1,NA,NA), col=c('gray','purple',NA, NA),
       bty = 'n', horiz = F, legend=c("Diploid", "Triploid", "Combined", "CB Diploid"), 
       text.col = c('black'), cex=c(1.5,1.5,1.5,1.5), lty=c(NA, NA, 1, 2), lwd=c(NA, NA, 2,2))



# plot(CB.shell$Shell_Dry_Weight_g[CB.shell$Ploidy=="Diploid"] ~ CB.shell$Total_Shell_Height_Length_mm[CB.shell$Ploidy=="Diploid"], type='p', 
#      col='gray85', ylim=c(0,200), xlim=c(0,140), ylab='Shell dry weight (g)', xlab='Shell height (mm)', pch=19, las=1,
#      cex.axis=1.5,cex.lab=1.5)
# points(PCB$Shell_Dry_Weight_g[PCB$Ploidy=="Diploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"], col='black', pch=1)
# points(PCB$Shell_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], col='purple', pch=1)

plot(CB.shell$Shell_Dry_Weight_g[CB.shell$Ploidy=="Diploid"] ~ CB.shell$Total_Shell_Height_Length_mm[CB.shell$Ploidy=="Diploid"], type='p', 
     bg='white', col='gray', ylim=c(0,200), xlim=c(0,140), ylab='Shell dry weight (g)', xlab='Shell height (mm)', pch=21, las=1,
     cex.axis=1.5,cex.lab=1.5)
points(PCB$Shell_Dry_Weight_g[PCB$Ploidy=="Diploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"], type='p', col='black', pch=21, bg='gray')
points(PCB$Shell_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], type='p', col='black',bg='purple', pch=21)
legend('topleft', pch=c(21,21,21,NA, NA), col=c('black', 'black','gray', 'black', 'black'), pt.bg=c('gray','purple','white', NA, NA),
       bty = 'n', horiz = F, legend=c("Diploid", "Triploid","CB Diploid", "Combined", "CB Diploid"), 
       text.col = c('black'), cex=c(1.5), lty=c(NA, NA, NA, 1, 2), lwd=c(NA, NA, NA, 2,2))
qr1=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=PCB, tau=0.5, na.action = 'na.omit')
yval=exp(qr1$coefficients[1])*xval^qr1$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=1)
#CB shell
yval=(0.00147)*xval^2.3964
lines(xval, yval, col='black', lwd=2, lty=2)



plot(CB.shell$Shell_Dry_Weight_g[CB.shell$Ploidy=="Diploid"] ~ CB.shell$Total_Shell_Height_Length_mm[CB.shell$Ploidy=="Diploid"], type='n', 
           bg='white', col='gray', ylim=c(0,200), xlim=c(0,140), ylab='Shell dry weight (g)', xlab='Shell height (mm)', pch=21, las=1,
           cex.axis=1.5,cex.lab=1.5)
points(PCB$Shell_Dry_Weight_g[PCB$Ploidy=="Diploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"], type='p', pch=19, col='gray')
points(PCB$Shell_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], type='p', col='purple', pch=1)
yval=exp(qr1$coefficients[1])*xval^qr1$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=1)
yval=(0.00147)*xval^2.3964
lines(xval, yval, col='black', lwd=2, lty=2)
# legend('topleft', pch=c(19,1,NA, NA), col=c('gray', 'purple', 'black', 'black'), 
#        bty = 'n', horiz = F, legend=c("Diploid", "Triploid", "Combined", "CB Diploid"), 
#        text.col = c('black'), cex=c(1.5), lty=c(NA, NA, 1, 2), lwd=c(NA, NA, 2,2))
t=PCB %>% filter(Ploidy=="Diploid")
qr2=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
yval=exp(qr2$coefficients[1])*xval^qr2$coefficients[2]
lines(xval, yval, col='black', lwd=1, lty=1)
t=PCB %>% filter(Ploidy=="Triploid")
qr3=rq(log(Shell_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
yval=exp(qr3$coefficients[1])*xval^qr3$coefficients[2]
lines(xval, yval, col='red', lwd=1, lty=1)
legend('topleft', lty=c(1,1,1,2), pch=c(NA,NA,NA,NA),lwd=c(1,1,2,2),col=c('black', 'red','black', 'black'), 
       bty = 'n', horiz = F, legend=c("Diploid", "Triploid","Combined","CB Diploid"), 
       text.col = c('gray', 'purple', 'black', 'black'), cex=c(1.5, 1.5, 1.5, 1.5))
legend('topleft', lty=c(NA,NA,1,2), pch=c(19,1,NA,NA),col=c('gray', 'purple',NA, NA), 
       bty = 'n', horiz = F, legend=c(NA, NA, NA, NA), 
       text.col = c('gray', 'purple', 'black', 'black'), cex=c(1.5, 1.5, 1.5, 1.5))

plot(CB$Tissue_Dry_Weight_g~CB$Tissue_AFDW_g, type='p')
abline(a=0, b=1, col='red', lwd=2)



### compare triploid BMP data with Poach data (updated data forthcoming)
plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], type='n', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='Dry weight (g)', xlab='Shell height (mm)', pch=19, las=1,
     cex.axis=1.5,cex.lab=1.5)
points(CB$Tissue_Dry_Weight_g[which(CB$Ploidy=="Triploid")]~ 
         CB$Total_Shell_Height_Length_mm[which(CB$Ploidy=="Triploid")], type='p', col='gray')
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"] ~ 
         PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"], type='p', col='black', pch=19)
legend('topleft', pch=c(19,1), col=c('black', 'gray'), 
       bty = 'n', horiz = F, legend=c("Triploid","CB BMP Triploid"), 
       text.col = c('black','black'), cex=c(1.5, 1.5))



### plot differences in SH:DW by state
plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid" & PCB$State=='Virginia'] ~ 
       PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"& PCB$State=='Virginia'], type='p', 
     col='red', ylim=c(0,7), xlim=c(0,140), ylab='Tissue Dry weight (g)', xlab='Shell height (mm)', pch=19, las=1, 
     cex.axis=1.5, cex.lab =1.5)
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid" & PCB$State=='Maryland'] ~ 
         PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid" & PCB$State=='Maryland'], pch=19, col='gray')
clmd=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"]))
yval=exp(clmd$coefficients[1])*xval^clmd$coefficients[2]
lines(xval, yval, col='black', lwd=2)
# legend('topleft', bty = 'n', horiz = F, legend=c("VA Diploid", "MD Diploid"), cex=1.5, text.col = c('red','gray'))
legend('topleft', bty = 'n', horiz = F, legend=c("MD Diploid", "VA Diploid"), cex=1.5, text.col = c('gray','red'))

plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid" & PCB$State=='Virginia'] ~ 
       PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"& PCB$State=='Virginia'], type='p', 
     col='red', ylim=c(0,7), xlim=c(0,140), ylab='Dry weight (g)', xlab='Shell height (mm)', pch=19, las=1, 
     cex.axis=1.5, cex.lab =1.5)
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid" & PCB$State=='Maryland'] ~ 
         PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid" & PCB$State=='Maryland'], pch=19, col='gray')
clmt=lm(log(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Triploid"])~log(PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Triploid"]))
xval=seq(20,140,by=.5)
yval=exp(clmt$coefficients[1])*xval^clmt$coefficients[2]
lines(xval, yval, col='black', lwd=2)
# legend('topleft', bty = 'n', horiz = F, cex=1.5, legend=c("VA Triploid", "MD Triploid"), text.col = c('red','gray'))
# legend('topleft', bty = 'n', horiz = F, legend=c("Diploid", "Triploid"), text.col = c('gray', 'purple'))
legend('topleft', bty = 'n', horiz = F, legend=c("MD Diploid", "VA Diploid"), cex=1.5, text.col = c('gray','red'))


#### plot for paper now with quantile regression
plot(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid" & PCB$State=='Virginia'] ~ 
       PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid"& PCB$State=='Virginia'], type='p', 
     col='red', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1)
points(PCB$Tissue_Dry_Weight_g[PCB$Ploidy=="Diploid" & PCB$State=='Maryland'] ~ 
         PCB$Total_Shell_Height_Length_mm[PCB$Ploidy=="Diploid" & PCB$State=='Maryland'], pch=19, col='gray')
t=PCB %>% filter(Ploidy=="Diploid")
qr2=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
xval=seq(20,140,by=.5)
yval=exp(qr2$coefficients[1])*xval^qr2$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=1)
t=PCB %>% filter(Ploidy=="Triploid")
qr2=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
yval=exp(qr2$coefficients[1])*xval^qr2$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
## add CBP data fits
# yval=exp(cbdlm$coefficients[1])*xval^cbdlm$coefficients[2]
yval=(0.00037)*xval^1.83359 # CBP all data BMP Second Report Appendix
# yval=(0.0004)*xval^1.82 # CBP bay data (first report diploid)
yval2=(0.00005)*xval^2.39 # CBP bay data (first report triploid)
lines(xval, yval, col='black', lwd=2, lty=2)
# yval2=exp(cbtlm$coefficients[1])*xval^cbtlm$coefficients[2]
lines(xval, yval2, col='red', lwd=2, lty=2)
legend('topleft', lty=c(1,1,2,2), lwd=c(2,2,2,2),col=c('black', 'red','black', 'red'), 
       bty = 'n', horiz = F, legend=c("Diploid", "Triploid","CBP Diploid", "CBP Triploid"), 
       text.col = c('gray', 'purple', 'black', 'black'), cex=c(1.5, 1.5, 1.5, 1.5))




### get error from power function for Poach data
summary(clmd)
summary(clmt)
exp(summary(clmd)$coefficients[1,1])
exp(summary(clmt)$coefficients[1,1])
# error a (need to take exponent of intercept +/- error)
exp(summary(clmd)$coefficients[1,1]+summary(clmd)$coefficients[1,2])-exp(summary(clmd)$coefficients[1,1]-summary(clmd)$coefficients[1,2])
exp(summary(clmt)$coefficients[1,1]+summary(clmt)$coefficients[1,2])-exp(summary(clmt)$coefficients[1,1]-summary(clmt)$coefficients[1,2])
# error b (right from summary)
summary(clmd)$coefficients[2,2]
summary(clmt)$coefficients[2,2]






anova(clmd, clmt, test="Chisq")

### testing quantile regression
t=PCB %>% filter(State=='Virginia', Ploidy=="Diploid")
t=PCB %>% filter(Ploidy=="Diploid")
qr2=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
plot(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1)
xval=seq(20,140,by=.5)
yval=exp(qr2$coefficients[1])*xval^qr2$coefficients[2]
lines(xval, yval, col='black', lwd=2, lty=2)
tlmd=lm(log(t$Tissue_Dry_Weight_g)~log(t$Total_Shell_Height_Length_mm))
yval=exp(tlmd$coefficients[1])*xval^tlmd$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
t=PCB %>% filter(Ploidy=="Triploid")
qr3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
points(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, col='red', pch=19)
yval=exp(qr3$coefficients[1])*xval^qr3$coefficients[2]
lines(xval, yval, col='purple', lwd=2, lty=2)

## get values of power model:
#Diploids: (qr2)
exp(qr2$coefficients[1])
qr2$coefficients[2]
#Diploids: (qr3)
exp(qr3$coefficients[1])
qr3$coefficients[2]
# error a (need to take exponent of intercept +/- error)
exp(summary(qr2)$coefficients[1,3])-exp(summary(qr2)$coefficients[1,2])
exp(summary(qr3)$coefficients[1,3])-exp(summary(qr3)$coefficients[1,2])
# error b (right from summary)
summary(qr2)$coefficients[2,3]-summary(qr2)$coefficients[2,2]
summary(qr3)$coefficients[2,3]-summary(qr3)$coefficients[2,2]


confint(t3)
confint(tlmd)

## or plot in log log as linear
plot(log(t$Tissue_Dry_Weight_g) ~ log(t$Total_Shell_Height_Length_mm), type='p', 
     col='gray', ylab='DW (g)', xlab='SH (mm)', pch=19, las=1)
abline(tlmd)
x=confint(tlmd, level = 0.95)
abline(a=x[1,1], b=x[2,1], col='red', lty=3)
abline(a=x[1,2], b=x[2,2], col='red', lty=3)
## quantile
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
plot(log(t$Tissue_Dry_Weight_g) ~ log(t$Total_Shell_Height_Length_mm), type='p', 
     col='gray', ylab='DW (g)', xlab='SH (mm)', pch=19, las=1, ylim=c(log(0.001),log(8)), xlim=c(log(0.001), log(140)))
abline(t3)
x=summary(t3)$coefficients
abline(a=x[1,2], b=x[2,2], col='red', lty=3)
abline(a=x[1,3], b=x[2,3], col='red', lty=3)

# xval=seq(min(t$Total_Shell_Height_Length_mm),max(t$Total_Shell_Height_Length_mm),by=.5)
# xval=max(t$Total_Shell_Height_Length_mm)-min(t$Total_Shell_Height_Length_mm))/length(t$Total_Shell_Height_Length_mm)
# lx=1/(length(t$Total_Shell_Height_Length_mm)/(max(t$Total_Shell_Height_Length_mm)-min(t$Total_Shell_Height_Length_mm)))
# xval=seq(min(t$Total_Shell_Height_Length_mm),max(t$Total_Shell_Height_Length_mm),by=lx)
# xval=seq(min(t$Total_Shell_Height_Length_mm),max(t$Total_Shell_Height_Length_mm), length.out=length(t$Total_Shell_Height_Length_mm))
# newx=log(xval)
# conf_interval <- predict(tlmd, newdata=data.frame(x=newx), interval="confidence", level = 0.95)
# lines(newx, conf_interval[,2], col="blue", lty=2)
# lines(newx, conf_interval[,3], col="red", lty=2)

## std error of slope term
# sqrt(diag(vcov(tlmd)))[2]
CI_lower <- coefficients(tlmd)[2] - 1.96*summary(tlmd)$coefficients[2,2]
CI_upper <- coefficients(tlmd)[2] + 1.96*summary(tlmd)$coefficients[2,2]

yval=exp(tlmd$coefficients[1])*xval^tlmd$coefficients[2]
yvalup=exp(tlmd$coefficients[1])*xval^CI_upper
yvallow=exp(tlmd$coefficients[1])*xval^CI_lower

lines(xval, yvalup, col='red', lwd=1, lty=2)
lines(xval, yvallow, col='red', lwd=1, lty=2)

### plot 95% CI with log model (huge CI...)
t=PCB %>% filter(Ploidy=="Diploid")
plot(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1, main='lm +- 95%CI')
xval=seq(20,140,by=.5)
tlmd=lm(log(t$Tissue_Dry_Weight_g)~log(t$Total_Shell_Height_Length_mm))
CI_lower <- coefficients(tlmd)[2] - 1.96*summary(tlmd)$coefficients[2,2]
CI_upper <- coefficients(tlmd)[2] + 1.96*summary(tlmd)$coefficients[2,2]
yval=exp(tlmd$coefficients[1])*xval^tlmd$coefficients[2]
yvalup=exp(tlmd$coefficients[1])*xval^CI_upper
yvallow=exp(tlmd$coefficients[1])*xval^CI_lower
lines(xval, yvalup, col='red', lwd=1, lty=2)
lines(xval, yvallow, col='red', lwd=1, lty=2)
lines(xval, yval, col='red', lwd=2, lty=1)
t=PCB %>% filter(Ploidy=="Triploid")
points(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, col='black', pch=1)
tlmt=lm(log(t$Tissue_Dry_Weight_g)~log(t$Total_Shell_Height_Length_mm))
CI_lower <- coefficients(tlmt)[2] - 1.96*summary(tlmt)$coefficients[2,2]
CI_upper <- coefficients(tlmt)[2] + 1.96*summary(tlmt)$coefficients[2,2]
yval=exp(tlmt$coefficients[1])*xval^tlmt$coefficients[2]
yvalup=exp(tlmt$coefficients[1])*xval^CI_upper
yvallow=exp(tlmt$coefficients[1])*xval^CI_lower
lines(xval, yvalup, col='purple', lwd=1, lty=2)
lines(xval, yvallow, col='purple', lwd=1, lty=2)
lines(xval, yval, col='purple', lwd=2, lty=1)

### testing quantile regression at 50, 90, and 10
t=PCB %>% filter(Ploidy=="Diploid")
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
plot(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1, main='quantiles: 50,97.5, 2.5')
xval=seq(20,140,by=.5)
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.975, na.action = 'na.omit')
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='red', lwd=1, lty=2)
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.025, na.action = 'na.omit')
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='red', lwd=1, lty=2)
t=PCB %>% filter(Ploidy=="Triploid")
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
points(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, col='black', pch=1)
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='purple', lwd=2, lty=1)
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.975, na.action = 'na.omit')
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='purple', lwd=1, lty=2)
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.025, na.action = 'na.omit')
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='purple', lwd=1, lty=2)

### quantiles 50 and using lower bd and upper bd from summary
t=PCB %>% filter(Ploidy=="Diploid")
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
plot(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1, main='quantiles: 50, upper lower bd')
xval=seq(20,140,by=.5)
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
yval=exp(t3$coefficients[1])*xval^summary(t3)$coefficients[2,2] #lower bd
lines(xval, yval, col='red', lwd=1, lty=2)
yval=exp(t3$coefficients[1])*xval^summary(t3)$coefficients[2,3] #upper bd
lines(xval, yval, col='red', lwd=1, lty=2)
## with CI from summary (bad...)
t=PCB %>% filter(Ploidy=="Diploid")
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
plot(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1, main='quantiles: 50 +-CI boot fit')
xval=seq(20,140,by=.5)
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
sefit=summary(t3, se='boot')$coefficients[2,2]
CI_lower <- coefficients(t3)[2] - 1.96*sefit
CI_upper <- coefficients(t3)[2] + 1.96*sefit
yval=exp(t3$coefficients[1])*xval^CI_lower #lower bd
lines(xval, yval, col='red', lwd=1, lty=2)
yval=exp(t3$coefficients[1])*xval^CI_upper #upper bd
lines(xval, yval, col='red', lwd=1, lty=2)

### quantiles 50 and using lower bd and upper bd from summary
t=PCB %>% filter(Ploidy=="Diploid")
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.5, na.action = 'na.omit')
plot(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, type='p', 
     col='gray', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1, main='quantiles: 40,50,60')
xval=seq(20,140,by=.5)
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='red', lwd=2, lty=1)
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.6, na.action = 'na.omit')
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='red', lwd=1, lty=2)
t3=rq(log(Tissue_Dry_Weight_g) ~ log(Total_Shell_Height_Length_mm), data=t, tau=0.4, na.action = 'na.omit')
yval=exp(t3$coefficients[1])*xval^t3$coefficients[2]
lines(xval, yval, col='red', lwd=1, lty=2)



Dat.nlrq <- nlrq(y ~ SSlogis(x, Asym, mid, scal), data=Dat, tau=0.5, trace=TRUE)
lines(1:25, predict(Dat.nlrq, newdata=list(x=1:25)), col=2)

t=PCB %>% filter(Ploidy=="Diploid") %>% mutate(y=Tissue_Dry_Weight_g, x=Total_Shell_Height_Length_mm)
t3=nlrq(y ~ SSlogis(x, Asym=400, xmid=67.5, scal = 12), data=t, tau=0.5, trace=TRUE)
t3=nlrq(y ~ SSlogis(x, Asym, mid, scal), data=t, tau=0.5, trace=TRUE)

y=t$Tissue_Dry_Weight_g
x=t$Tissue_N_Percent
# not working yet
t3=nlrq(Tissue_Dry_Weight_g~(a*(Tissue_N_Percent^b)), data=t, start=list(a=0.00037,b=1.83359), tau=0.5)
plot(t$Tissue_Dry_Weight_g ~ t$Total_Shell_Height_Length_mm, type='p', 
     +      col='gray', ylim=c(0,7), xlim=c(0,140), ylab='DW (g)', xlab='SH (mm)', pch=19, las=1, main='')
lines(0:140, predict(t3, newdata=list(Tissue_N_Percent=0:140)), col=3)
xval=seq(20,140,by=.5)
yval=predict(t3,xval)
summary(t3)



### summary stats used in table 1
test=PCB %>% select(State, Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State, Ploidy) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
write.csv(test, file='mean_by_state_and_ploidy.csv')

testsd=PCB %>% select(State, Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State, Ploidy) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))
write.csv(testsd, file='sd_by_state_and_ploidy.csv')

test=PCB %>% select(State, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
write.csv(test, file='mean_by_state.csv')

test=PCB %>% select(Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(Ploidy) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
write.csv(test, file='mean_by_ploidy.csv')

testsd=PCB %>% select(State, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(State) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))
write.csv(testsd, file='sd_by_state.csv')

testsd=PCB %>% select(Ploidy, Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  group_by(Ploidy) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))
write.csv(testsd, file='sd_by_ploidy.csv')


test=PCB %>% select(Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))
write.csv(test, file='mean.csv')
testsd=PCB %>% select(Tissue_N_Percent, Tissue_C_Percent, Tissue_TP_Percent, Shell_N_Percent, Shell_TP_Percent, Shell_C_Percent) %>% 
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE)))
write.csv(testsd, file='sd.csv')
