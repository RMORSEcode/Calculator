### 20230911 RM
### Functions for Shellfish Calculator - nutrient removal, shell height to dry weight relationships ###
### see Load_data.R, build_main_file.R, SH_DW.R 
library(rstatix)
library(tidyverse)
library(ggpubr)
library(quantreg)
library(colorspace)
library(here)

here::here()
Main=file.choose()

q11 <- qualitative_hcl(11, "Dark3")
#plot with CB
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, col='gray70', ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)

my_comparisons=list(unique(Main$State[complete.cases(Main$Tissue_N_Percent)]))

Main[complete.cases(Main$Tissue_N_Percent),] %>% select(State, Ploidy, Tissue_N_Percent) %>% 
  ggboxplot(y='Tissue_N_Percent', x='State', fill='Ploidy', ylab = 'Tissue N Percent', xlab='', ylim=c(2.5,14)) + 
  stat_compare_means(label = "p.signif", method = "wilcox.test", ref.group = ".all.") #+
  # stat_compare_means(comparisons = my_comparisons, method = "anova", label.y = c(12.5),label= "p.signif") #+

Main[complete.cases(Main$Tissue_TP_Percent),] %>% select(State, Ploidy, Tissue_TP_Percent) %>% 
  ggboxplot(y='Tissue_TP_Percent', x='State', fill='Ploidy',ylab = 'Tissue P Percent', xlab='', ylim=c(0.25,2)) +
  stat_compare_means(label = "p.signif", method = "anova", ref.group = ".all.") +
  stat_compare_means(comparisons = my_comparisons, label.y = c(1.75), label= "p.signif")

### CB Panel report 2nd model
xval=seq(0,180,by=.5)
yval=(0.00037)*xval^1.83359 # CBP all data BMP Second Report Appendix
mod.BMP=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)


s1=match("Poach et al. in prep 2023", Main$Data_Source) # start of Poach data (after end of CB)
s2=match("Levinton J, et al. 2011 PLoS ONE 6(4)", Main$Data_Source)-1 #Last data before Levinton
s3=match("Barr et al. submitted 2022", Main$Data_Source) # Start of Barr (after end of Levinton)
s4=match("Sebastiano et al 2015", Main$Data_Source)-1 #Last data before Sebastiano
vec=c(s1:s2, s3:dim(Main)[1]) # No CB, No Levinton
vecNoCBNY=c(s1:s4, s3:dim(Main)[1]) # No CB, Sebastiano, Levinton

# No CB
qr.x=rq(log(Tissue_Dry_Weight_g) ~ 
          log(Total_Shell_Height_Length_mm), 
        data=Main[s1:dim(Main)[1],], tau=0.5, na.action = 'na.omit')
xval=seq(0,180,by=.5)
yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
mod.noCB=data.frame(Total_Shell_Height_Length_mm=xval, Tissue_Dry_Weight_g=yval)

stt=sort(unique(Main$State))
MainNoCB=Main[s1:dim(Main)[1],]
MainNoCBLev=Main[vec,]
for(i in 1:length(unique(Main$State))){
  # dataa=Main[Main$State==stt[i],]
  dataa=MainNoCB[MainNoCB$State==stt[i],]
  # dataa=MainNoCBLev[MainNoCBLev$State==stt[i],]
  qr.x=rq(log(Tissue_Dry_Weight_g) ~  log(Total_Shell_Height_Length_mm),
          data=dataa, tau=0.5, na.action = 'na.omit')
  xval=seq(0,180,by=.5)
  yval=exp(qr.x$coefficients[1])*xval^qr.x$coefficients[2]
  # points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col=q11[i], ylim=c(0,8), xlim=c(0,200))
  lines(yval ~xval, col=q11[i], lwd=2)
  qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, 
           data = dataa, start = list(a = 0.00037, b = 1.83359), tau=0.5)
  x <- seq(0, 180, length = 250)
  # lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 2, lwd=2, col = q11[i])
}
legend('topleft', bty='n', 
       legend = c('CT', 'DE', 'ME', 'MD', 'MA', 'NH', 'NJ', 'NY', 'NC', 'RI', 'VA'), 
       text.col=q11)


### adding in routine from BMP
# cb2023qr=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = Main[1:s1-1,], start = list(a = 0.0001, b = 2.1056), tau=0.5)
cb2023qr=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = Main[1:s1-1,], start = list(a = 0.00037, b = 1.83359), tau=0.5)
x <- seq(0, 180, length = 250)
lines(predict(cb2023qr, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 2, lwd=2, col = "red")


## nlrq for all data (no CB)
plot(Main$Tissue_Dry_Weight_g[1:9727] ~ Main$Total_Shell_Height_Length_mm[1:9727], type='p', 
     pch=19, col='gray70', ylim=c(0,8), xlim=c(0,200), ylab="Dry weight (g)", xlab="Shell height (mm)", las=1)
dataa=MainNoCB
points(dataa$Tissue_Dry_Weight_g ~dataa$Total_Shell_Height_Length_mm, pch=21, col='gray30', ylim=c(0,8), xlim=c(0,200))
qrx=nlrq(Tissue_Dry_Weight_g ~ a*Total_Shell_Height_Length_mm^b, data = dataa, start = list(a = 0.00037, b = 1.83359), tau=0.5)
x <- seq(0, 180, length = 250)
lines(predict(qrx, list(Total_Shell_Height_Length_mm = x)) ~ x, lty = 1, lwd=2, col = 'black')
summary(qrx)

