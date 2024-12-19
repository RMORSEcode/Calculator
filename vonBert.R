library(tidyverse)
library(FSA)
library(car)
library(lubridate)
library(ggpubr)
library(gplots)



## load survdat.bio from Andy 2024
survey.bio=readRDS("C:/Users/ryan.morse/Downloads/SurvdatBio.rds")
survdat.bio <- survey.bio$survdat

### Choose season ###
Season="Fall" # Spring | Fall | Combined
### Choose species ###
Species="Black sea bass"; snum=141 # Centropristis striata
# Species="Scup"; snum=143
# Species="Cunner"; snum=176
# Species="Tautog"; snum=177

spp.sp=survdat.bio %>% 
  dplyr::filter(SVSPP==snum, SEASON=="SPRING") %>%
  dplyr::filter(complete.cases(AGE))
spp.fl=survdat.bio %>% 
  dplyr::filter(SVSPP==snum, SEASON=="FALL") %>% 
  dplyr::filter(complete.cases(AGE))
spp.both=survdat.bio %>% 
  dplyr::filter(SVSPP==snum) %>% 
  dplyr::filter(complete.cases(AGE))

if(Season=="Spring"){
  dataX=spp.sp
} else if(Season=="Fall"){
  dataX=spp.fl
} else {
  dataX=spp.both
}
# 1=Male 2=Female;
spp.fl4=dataX %>% filter(SEX==1 | SEX==2)
agesum <- group_by(spp.fl4,SEX) %>%
  summarize(minage=min(AGE),maxage=max(AGE))
agesum

vb <- vbFuns(param="Typical")
( f.starts <- vbStarts(LENGTH~AGE,data=dataX) )
f.fit <- nls(LENGTH~vb(AGE,Linf,K,t0),data=dataX,start=f.starts)
coef(f.fit)
f.boot1 <- Boot(f.fit)  # Be patient! Be aware of some non-convergence
confint(f.boot1)
predict(f.fit,data.frame(AGE=1:10))
predict2 <- function(x) predict(x,data.frame(AGE=AGEs))
# AGEs <- 2:7
# predict2(f.fit)  # demonstrates same result as predict() above
AGEs <- seq(-1,12,by=0.2)
f.boot2 <- Boot(f.fit,f=predict2)  # Be patient! Be aware of some non-convergence
preds1 <- data.frame(AGEs,
                     predict(f.fit,data.frame(AGE=AGEs)),
                     confint(f.boot2))
names(preds1) <- c("AGE","fit","LCI","UCI")
# headtail(preds1)
preds2 <- filter(preds1,AGE>=agesum$minage,AGE<=agesum$maxage)
headtail(preds2)

makeVBEqnLabel <- function(fit) {
  # Isolate coefficients (and control decimals)
  cfs <- coef(fit)
  Linf <- formatC(cfs[["Linf"]],format="f",digits=1)
  K <- formatC(cfs[["K"]],format="f",digits=3)
  # Handle t0 differenLENGTHy because of minus in the equation
  t0 <- cfs[["t0"]]
  t0 <- paste0(ifelse(t0<0,"+","-"),formatC(abs(t0),format="f",digits=3))
  # Put together and return
  paste0("LENGTH==",Linf,"~bgroup('(',1-e^{-",K,"~(AGE",t0,")},')')")
}

## Figure 3 Mercaldo-Allen et al 2025 in prep ##
vbFitPlot <- ggplot() + 
  geom_ribbon(data=preds2,aes(x=AGE,ymin=LCI,ymax=UCI),fill="gray90") +
  geom_point(data=dataX,aes(y=LENGTH,x=AGE),size=2,alpha=0.1) +
  geom_line(data=preds1,aes(y=fit,x=AGE),linewidth=1,linetype=2) +
  geom_line(data=preds2,aes(y=fit,x=AGE),linewidth=1) +
  scale_y_continuous(name="Length (cm)",limits=c(0,round(max(dataX$LENGTH*1.2),-1)),expand=c(0,0)) +
  scale_x_continuous(name="Age (years)",expand=c(0,0),
                     limits=c(-1,12),breaks=seq(0,12,2)) +
  labs(title=paste(Species, Season, sep=' ')) +
  theme_bw() +
  theme(panel.grid=element_blank())
vbFitPlot + annotate(geom="text",label=makeVBEqnLabel(f.fit),parse=TRUE,
                     size=4,x=Inf,y=-Inf,hjust=1.1,vjust=-0.5)
ggsave(filename='vB300dpi.tiff',path = wd, width = 5, height = 3.5, device='tiff', dpi=300)
