###run selection.R first
library(ggplot2)
library(gridExtra)
setwd("~/Dropbox/2018WorkingFiles/Marc2018/Trait restoration selection/R")
source("selection.R")
source("probilizer.R")


trait<-read.csv("data/species_traits_final2016.csv",row.names=1)


##run it
out<-probilizer(trait,iter=99)

###no weights no costs
species_order <- out$Species[order(out$Mean,decreasing = TRUE)]

fp <- ggplot(data=out, aes(x=factor(Species,level=species_order), y=Mean, ymin=Lower, ymax=Upper)) +
  geom_pointrange() + 
  #geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Species") + ylab("Selection priority (95% CI)") +
  theme_bw() + # use a white background 
  annotate("segment", 
           y = 13, yend = 4, x = 14.2, xend = 14.2,
           arrow=arrow(length=unit(0.5, "cm"),type="closed"),color="orange3",linewidth=2)+
  annotate("text",x=13.9,y=8,label="Species preference",color="orange3",
           fontface="bold",size=5)+
  annotate("rect",xmin=c(11.75,3.75,0.75),xmax=c(14.25,11.25,3.25),
           ymin=c(1,6,9.8),ymax=c(3,9.8,13.2),alpha = 0.2,fill="cornflowerblue")+
  annotate("text",x=c(11.5,11.5,3.5),y=c(2.1,7.9,11.6),
           label=c("Group 1","Group 2","Group 3"),color = "cornflowerblue",
           fontface="bold",size=4)
quartz()
fp

ggsave(path = "figs",filename="BEF_selection_noWorC.pdf", plot=fp, dpi=300, units="cm")


#add in weights 
rho<-rep(0.25,nrow(trait))
names(rho)<-row.names(trait)
##we want ASTU, ANGE, and MOFI
rho[match(c("ASTU", "ANGE",  "MOFI"),names(rho))]<-1
##run it
out2<-probilizer(trait,rho=rho)
species_order2 <- out2$Species[order(out2$Mean,decreasing = TRUE)]

#rho used in final figure
#ANGE ASTU DECA ELCA ELTR LECA MOFI PEHI PYTE PYVI RUHI SCSC SOAL SONE 
#1.00 1.00 0.25 0.25 0.25 0.25 1.00 0.25 0.25 0.25 0.25 0.25 0.25 0.25 

fp2 <- ggplot(data=out2, aes(x=factor(Species,level=species_order2), y=Mean, ymin=Lower, ymax=Upper)) +
  geom_pointrange() + 
  #geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Species") + ylab("Selection priority (95% CI)") +
  theme_bw() + # use a white background 
  annotate("rect",xmin=11.75,xmax=14.25, ymin=0.75,ymax=3.25,
           alpha = 0.2,fill="cornflowerblue")+
  annotate("text",x=13.5,y=3.6, label="Priority species",color = "cornflowerblue",
           fontface="bold",size=4,hjust=0)+
  ggtitle("A) Add weighting for priority species")

#quartz()
#fp2

##add in costs
C<-sample(c(1.25,1.45,1.55),size=nrow(trait),replace=TRUE)
names(C)<-row.names(trait)
out3<-probilizer(trait,rho=1,c=C)
species_order3 <- out3$Species[order(out3$Mean,decreasing = TRUE)]

#costs used in figure
#ANGE ASTU DECA ELCA ELTR LECA MOFI PEHI PYTE PYVI RUHI SCSC SOAL SONE 
#1.25 1.45 1.25 1.45 1.45 1.55 1.45 1.25 1.45 1.55 1.45 1.45 1.45 1.25 

fp3 <- ggplot(data=out3, aes(x=factor(Species,level=species_order3), y=Mean, ymin=Lower, ymax=Upper)) +
  geom_pointrange() + 
  #geom_hline(yintercept=1, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Species") + ylab("Selection priority (95% CI)") +
  theme_bw() + # use a white background 
  ggtitle("B) Add cost of species")

#quartz()
#fp3



quartz()
grid.arrange(fp2, fp3, ncol=2)
fp23<-grid.arrange(fp2, fp3, ncol=2)

ggsave(path = "figs",filename="BEF_selection_withWC.pdf", plot=fp23, dpi=300, units="cm")
