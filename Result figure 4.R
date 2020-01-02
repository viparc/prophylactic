library(purrr)
library(ggplot2)
library(dplyr) 
library(gghighlight)
library(tidyr)
library(scales)
###############
rm(list = ls())
tmp <- readRDS("tmp.rds")
colnames(tmp)
df <- tmp %>%
  mutate(clinical_signs=as.character(clinical_signs),
         class=as.character(class),
         filtering=as.character(filtering),
         watching=as.character(watching),
         first_wks=as.character(first_wks),
      
         ct=as.character(ct),
         p=as.numeric(p),
         prob=as.character(prob),
         or=as.numeric(exp(or)),
         lower=as.numeric(exp(lower)), 
         upper=as.numeric(exp(upper)),
         nrow(tmp), replace=TRUE)
      

df1 <- df %>%  
  filter(!clinical_signs==c("malaise"))%>%
  filter(!clinical_signs==c("sudden_death"))%>%
  mutate(p_value=ifelse(p<=0.05, "T","F"))%>%
  mutate(filtering1=filtering)%>%
  unite("ff",filtering1,first_wks, sep = "")
df1%>%
  mutate(ff=as.character(ff))
df1[df1=="penecillins"] <- "penicillins"
df1[df1=="unclassify"] <- "methenamines"
df1[df1=="aminolycosides"] <- "aminoglycosides"
  
df1[df1 == Inf] = 0

########## or, p_value 
p1 <-ggplot(df1, aes(x=ff, y=or, colour=p_value,shape =watching)) +
  geom_point(size=1.5, stroke = 0.9,position = position_dodge(width = 0.5))+
  #geom_errorbar(data=subset(df1, p_value=="T"),
                #aes(ymin = lower, ymax = upper))+
  facet_grid(clinical_signs~class)+
  geom_hline(yintercept=1)+
  scale_x_discrete(limits=c("11","12","13","11",
                           
                            "21","22","23","11",
                            
                            "31", "32","33"),

                   labels=c("1",
                            
                            "2","3","","1","2",
                            
                            "3","","1","2","3"))+
  labs(y="Odd ratio", x="Number of weeks not using antimicrobials before prophylactic events", title="") +
  #scale_y_continuous(limits=c(0,10))+
  scale_color_manual(values=c( "lightblue","red"))+
  scale_shape_manual(values=c(1,2,3))+
  theme_bw()+
  theme(strip.background = element_rect(colour = "black", fill = "white", size =1)+
  theme(strip.text.y = element_text(size = 5)))+
theme(legend.position="")+
  theme( panel.grid.minor.x = element_blank())+
  theme( panel.grid.major.x = element_line(size=0.1))
 

p1   

