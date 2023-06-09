library(gstat)
library(writexl)
library(readxl)
library(reshape2)
library(ape)
library(clhs)
library(tidyverse)
library(ggplot2)

dattt = data_frame(id = data3$id,a = alp$alfa,F_vT =data$F_vT,
                   F_vB =data$F_vB ,IM = data2$IMorant , Num = data3$Num, ot =alp$OT)

colll = ifelse(dattt$IM >= 0.05,"p value > 5%", "p value < 5 %")
dattt$colll = colll
colll2 = ifelse(dattt$a >= 0.01,"> 0.01", ifelse( dattt$a <= -0.01 ,
                                                  "<    -0.01","[-0.01,0.01]"))
dattt$colll2 = colll2

f_t = qf(p = 0.05,2 ,56, lower.tail = F)
seq(from=0.1,to= 4,length.out=1000)-> fp_tab
pf = NULL
ph = NULL
quantile(dattt$F_vT[which(dattt$id == "iter_0")],probs =c(fp_tab2) ) 

seq(from=0.01,to= 1,length.out=1000)-> fp_tab2

pf$prob= quantile(dattt$F_vT[which(dattt$id == "iter_0")],
                  probs =c(fp_tab2),lower.tail = F)

pf=as.data.frame(pf)
pf$id=fp_tab2
pf$prob2=rownames(pf)


pf$prob2=gsub('.{4}%', '', pf$prob2)
pf$prob2=as.numeric(pf$prob2)

ph$prob= pf(fp_tab,2,56)
ph$id=fp_tab

dattt |> 
  filter(id %in% c("iter_0","iter_12","iter_24","iter_36",
                   "iter_48","iter_60")) |> 
  group_by(id) |> 
  reframe(A = length(which(ot >= 3.81 & F_vT <= 3.16)),
          B = length(which(ot <= 3.81 & F_vT <= 3.16)),
          C = length(which(ot >= 3.81 & F_vT >= 3.16)),
          D = length(which(ot <= 3.81 & F_vT >= 3.16)),
          Total = sum(A,B,C,D),
          Ad = length(which(ot >= 3.81 & F_vT <= 3.16 & 
                              colll == "p value < 5 %")),
          Bd = length(which(ot <= 3.81 & F_vT <= 3.16 & 
                              colll == "p value < 5 %")),
          Cd = length(which(ot >= 3.81 & F_vT >= 3.16 & 
                              colll == "p value < 5 %")),
          Dd = length(which(ot <= 3.81 & F_vT >= 3.16 & 
                              colll == "p value < 5 %")),
          
          Ai = c(A-Ad),
          Bi = c(A-Ad),
          Ci = c(C-Cd),
          Di = c(D-Dd)) -> t2
dattt |> 
  filter(id %in% c("iter_0","iter_12","iter_24","iter_36",
                   "iter_48","iter_60")) |> 
  ggplot( aes(y=a,x=F_vT, color = colll) )+
  geom_point() +
  labs(x = "F value", y = expression(paste(alpha[~"s"]^"^")),
       color= "Moran index") +
  theme_minimal()+facet_grid(~Num)+
  geom_vline(xintercept = f_t, color= "black",linetype = 2)+
  theme(axis.text = element_text(size=13),axis.title =element_text(size=18), 
        legend.title =element_text(size=18),
        legend.text =element_text(size=18))+
  scale_color_manual(values=c("black","grey"))->p11

dattt |> 
  filter(id %in% c("iter_0","iter_12","iter_24","iter_36",
                   "iter_48","iter_60")) |> 
  ggplot( aes(y=ot,x=F_vT, color = colll) )+
  geom_point() +
  labs(x = "F value", y = "OT") +
  theme_minimal()+
  facet_grid(~Num)+
  scale_fill_hue(labels = c("p value >5%", "p value <5%"))+
  labs(color= "Moran index")+
  theme(axis.text = element_text(size=13),axis.title =element_text(size=18), 
        legend.title =element_text(size=18),
        legend.text =element_text(size=18))+
  scale_color_manual(values=c("black","grey"))+
  geom_vline(aes(xintercept = f_t,linetype = "F tabulado"), color= "black")+
  geom_hline(aes(yintercept =3.81,linetype = "Chi-square"),color="black")+
  scale_linetype_manual(name = "limit", values = c(2, 1), 
                        guide = guide_legend(override.aes = list(color = c("black", "black"))))+
  
  annotate('text', label='A', 
           x=1, y=10, hjust=0.5, vjust=0, size=8, color='grey')+
  annotate('text', label='B', 
           x=1, y=1, hjust=0.5, vjust=0, size=8, color='white')+
  annotate('text', label='C', 
           x=3.7, y=10, hjust=0.5, vjust=0, size=8, color='black')+
  annotate('text', label='D', 
           x=3.6, y=1, hjust=0.5, vjust=0, size=8, color="black") -> f21
dattt |> 
  filter(id %in% c("iter_60")) |> 
  ggplot(aes(x = F_vT)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "white", color = "black") +
  geom_density(color = "black") +
  stat_function(fun = function(x) df(x, 2, 56), color = "black") +
  labs(x = "Quantile", y = "Density", size=20) +
  theme_minimal() +
  geom_vline(aes(xintercept = f_t,linetype = "P(without overlap)"), color= "black")+
  geom_vline(aes(xintercept =1.65,linetype = "P(with overlap)"),color="black")+
  theme(axis.text = element_text(size=13),axis.title =element_text(size=18), 
        legend.title =element_text(size=18),
        legend.text =element_text(size=18))+
  scale_linetype_manual(name = "", values = c(2,1), 
                        guide = guide_legend(override.aes = list(color = c( "black","black"))))-> f5.1

dattt |> 
  filter(id %in% c("iter_0",
                   "iter_30","iter_60")) |> 
  ggplot( aes(y=y,x=x, color= colll) )+
  geom_point(pch =20) +
  labs(x =  "P(without overlap)", y = "P(with overlap)",cex = 2)+
  theme_minimal()+
  facet_wrap(~Num)+
  scale_fill_hue(labels = c("p value >5%", "p value <5%"))+
  labs(color= "Moran index")+
  scale_color_manual(values=c("black","grey"))+
  coord_equal(xlim = c(0,1),ylim = c(0,1))+
  theme(axis.text = element_text(size=13),axis.title =element_text(size=18), 
        legend.title =element_text(size=18),
        legend.text =element_text(size=18))+
  
  geom_abline(intercept = 0, slope = 1, color = "black",linetype = 2)-> f6.1

dattt |> 
  filter(id %in% c("iter_0","iter_30",
                   "iter_60")) |> 
  ggplot( aes(y=y,x=x, color = colll2) )+
  geom_point() +
  labs(x = "P(whitout overlap)", y = "P(whit overlap)")+
  theme_minimal()+
  facet_wrap(~Num)+
  scale_fill_hue(labels = c("p value >5%", "p value <5%"))+
  labs(color=expression(alpha[~"S"] ^ "^"))+
  coord_equal(xlim = c(0,1),ylim = c(0,1))+
  scale_color_manual(values = c("grey","white","black") )+
  theme(axis.text = element_text(size=13),axis.title =element_text(size=18), 
        legend.title =element_text(size=18),
        legend.text =element_text(size=18))+
  
  geom_abline(intercept = 0, slope = 1, color = "black",linetype= 2)-> f8 

dattt |> 
  filter(id %in% c("iter_0",
                   "iter_30","iter_60")) |> 
  ggplot( aes(y=y,x=x, color= colll) )+
  geom_point(pch =20) +
  labs(x =  "P(without overlap)", y = "P(with overlap)",cex = 2)+
  theme_minimal()+
  facet_wrap(~Num)+
  scale_fill_hue(labels = c("p value >5%", "p value <5%"))+
  labs(color= "Moran index")+
  scale_color_manual(values=c("black","grey"))+
  coord_equal(xlim = c(0,1),ylim = c(0,1))+
  theme(axis.text = element_text(size=13),axis.title =element_text(size=18), 
        legend.title =element_text(size=18),
        legend.text =element_text(size=18))+
  
  geom_abline(intercept = 0, slope = 1, color = "black",linetype = 2)-> f6.1