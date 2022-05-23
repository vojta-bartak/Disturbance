datahab4<-read.delim2("clipboard")
attach(datahab4)


library(nlme)


m1<- lme(Celk_PD ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")

summary(m1)

library(multcomp)
m1a<-glht(m1,linfct=mcp(Kat_hustZS="Tukey"))



summary(m1a)




Bar plot s vykreslením odhadu LMM modelu a 95 % konfidenèních intervalù.



Tabulkove zobrazení odhadù (stejných jako v summary(LMM) a konfidenèních intervalù - html


install.packages("sjPlot")
library(sjPlot)

sjPlot:: tab_model(m1)


Pøepis hodnot do vektorù:



Estim_m1<-c(9.379361, 9.379361+1.23,9.379361+1.57,9.379361+3.26,9.379361+2.91)
CI_m1_low<-c(8.82, 9.379361+0.51, 9.379361+0.69, 9.379361+2.21, 9.379361+1.90)
CI_m1_up<-c(9.94, 9.379361+1.94, 9.379361+2.46, 9.379361+4.30, 9.379361+3.93)
Kat_hust_ZS<-c("0","0<>1", "1<>3", "3<>9", "R")


Nadefinování barblotu s barevnou škálou


barplot(Estim_m1,names.arg=Kat_hust_ZS,xlab="Number of deciduous trees >70 cm DBH/ha",ylab="Total number of species",font.lab=2,
col = c("#f2f2f2","#d8d8d8" , "#a6a6a6", "#808080", "#595959"), ylim=c(0, 20), xlim=c(0,5.5))

Výpoèet støedù sloupcù barplotu


mid <- barplot(Estim_m1)


Dokreslení error èar


arrows(x0=mid, x1=mid, y0=CI_m1_low,y1=CI_m1_up, code=3, angle=90, length=0.1, lwd=3)


text(x=mid,y=CI_m1_up+3,labels=c("a","b","bc","d", "cd"), cex=1.5)
text(x=mid,y=CI_m1_up+1.5,labels=c("n=69","n=60","n=31","n=20","n=20"))
text(x=0.8,y=19,labels="p < 0.001 ***",font=2)
legend("topright", title="Mean number of decid. trees>70 / ha",
   c("0","0.6","1.9","4.9","22.8"), fill=c("#f2f2f2","#d8d8d8" , "#a6a6a6", "#808080", "#595959"), horiz=TRUE, cex=0.8)









mS2<- lme(sqrt(Specialist) ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")
summary(mS2)
anova(mS2)
mS2a<-glht(mS2,linfct=mcp(Kat_hustZS="Tukey"))
summary(mS2a)




sjPlot:: tab_model(mS2)


Pøepis hodnot do vektorù:



Estim_mS2<-c(2.1174244, 2.1174244+0.0850282,2.1174244+0.0341577,2.1174244+0.1987087,2.1174244+(-0.1277937))
CI_mS2_low<-c(1.96, 2.1174244+(-0.06), 2.1174244+(-0.15), 2.1174244+(-0.02), 2.1174244+(-0.34))
CI_mS2_up<-c(2.27, 2.1174244+0.23, 2.1174244+0.22, 2.1174244+0.42, 2.1174244+0.08)
Kat_hust_ZS<-c("0","0<>1", "1<>3", "3<>9", "R")


Nadefinování barblotu s barevnou škálou

midS <- barplot(Estim_mS2)

barplot(Estim_mS2^2,names.arg=Kat_hust_ZS,xlab="Number of deciduous trees >70 cm DBH/ha",ylab="Number of specialist species",font.lab=2,
col = c("#f2f2f2","#d8d8d8" , "#a6a6a6", "#808080", "#595959"), ylim=c(0, 20), xlim=c(0,5.5))

Výpoèet støedù sloupcù barplotu



Dokreslení error èar

cld(mS2a)

arrows(x0=midS, x1=midS, y0=CI_mS2_low^2,y1=CI_mS2_up^2, code=3, angle=90, length=0.1, lwd=3)


text(x=midS,y=CI_mS2_up^2+5,labels=c("a","a","a","a", "a"), cex=1.5)
text(x=midS,y=CI_mS2_up^2+3.5,labels=c("n=69","n=60","n=31","n=20","n=20"))


text(x=0.8,y=19,labels="p > 0.05 ns",font=2)
legend("topright", title="Mean number of decid. trees>70 / ha",
   c("0","0.6","1.9","4.9","22.8"), fill=c("#f2f2f2","#d8d8d8" , "#a6a6a6", "#808080", "#595959"), horiz=TRUE, cex=0.8)




mG2<- lme(sqrt(Generalist) ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")

summary(mG2)

mG2a<-glht(mG2,linfct=mcp(Kat_hustZS="Tukey"))
summary(mG2a)
cld(mG2a)

   A=0 B 0<>1  C1<>3  D3<>9      R 
   "a"   "ab"   "bc"   "cd"    "d" 


sjPlot:: tab_model(mG2)



Estim_mG2<-c(2.7052694, 2.7052694+0.1645197,2.7052694+0.2818455,2.7052694+0.5379505,2.7052694+0.6746537)
CI_mG2_low<-c(2.58, 2.7052694+(0.02), 2.7052694+(0.11), 2.7052694+(0.33), 2.7052694+(0.48))
CI_mG2_up<-c(2.83, 2.7052694+0.30, 2.7052694+0.46, 2.7052694+0.74, 2.7052694+0.87)




midG <- barplot(Estim_mG2)



barplot(Estim_mG2^2,names.arg=Kat_hust_ZS,xlab="Number of deciduous trees >70 cm DBH/ha",ylab="Number of generalist species",font.lab=2,
col = c("#f2f2f2","#d8d8d8" , "#a6a6a6", "#808080", "#595959"), ylim=c(0, 20), xlim=c(0,5.5))

Výpoèet støedù sloupcù barplotu


Dokreslení error èar


arrows(x0=midG, x1=midG, y0=CI_mG2_low^2,y1=CI_mG2_up^2, code=3, angle=90, length=0.1, lwd=3)


text(x=midG,y=CI_mG2_up^2+3,labels=c("a","ab","bc","cd", "d"), cex=1.5)
text(x=midG,y=CI_mG2_up^2+1.5,labels=c("n=69","n=60","n=31","n=20","n=20"))


text(x=0.8,y=19,labels="p < 0.001 ***",font=2)

legend("topright", title="Mean number of decid. trees>70 / ha",
   c("0","0.6","1.9","4.9","22.8"), fill=c("#f2f2f2","#d8d8d8" , "#a6a6a6", "#808080", "#595959"), horiz=TRUE, cex=0.8)

