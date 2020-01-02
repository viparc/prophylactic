library(purrr)
library(ggplot2)
library(dplyr) 
library(gghighlight)
library(tidyr)
library(scales)
###############
rm(list = ls())
tmp <- readRDS("therapeutic.rds")
colnames(tmp)
df <- tmp %>%
  mutate(clinical_signs=as.character(clinical_signs),
         class=as.character(class),
         gap=as.character(gap),
         amu=as.character(x),
      
        
         p=as.numeric(p),
     
         or=as.numeric(exp(or)),
       
         nrow(tmp), replace=TRUE)
df[df=="penecillins"] <- "penicillins"
df[df=="unclassify"] <- "methenamines"
df[df=="aminolycosides"] <- "aminoglycosides"

df1 <- df %>%  
  filter(!clinical_signs==c("malaise"))%>%
  filter(!clinical_signs==c("sudden_death"))%>%
  mutate(p_value=ifelse(p<=0.05, "T","F"))


  
df1[df1 == Inf] = 0

########## or, p_value 
p1 <-ggplot(df1, aes(x=amu, y=or, colour=p_value,shape =gap)) +
  geom_point(size=1.5, stroke = 0.9,position = position_dodge(width = 0.5))+
  #geom_errorbar(data=subset(df1, p_value=="T"),
                #aes(ymin = lower, ymax = upper))+
  facet_grid(clinical_signs~class)+

  geom_hline(yintercept=1)+

  labs(y="Odd ratio", x="Number of weeks not using antimicrobials before disease episodes", title="") +
  #scale_y_continuous(limits=c(0,10))+
  scale_color_manual(values=c( "lightblue","red"))+
  scale_shape_manual(values=c(1,2,3))+
  theme_bw()+
  theme(strip.background = element_rect(colour = "black", fill = "white", size =1)+
  theme(strip.text.y = element_text(size = 5)))+
theme(legend.position="")+
  theme( panel.grid.minor.x = element_blank())+
  theme(strip.text.x = element_text(size = 7))+
  theme( panel.grid.major.x = element_line(size=0.1))
 
p1   

