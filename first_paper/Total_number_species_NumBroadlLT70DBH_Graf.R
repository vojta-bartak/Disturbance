daGrafy GGplot2 - s konfidenèáky


library(nlme)
library(MASS)
library(MuMIn)


full.PD<- lme(Celk_PD ~ Age_SD + H_Trees+Pocet_List+Elev+Decid+Conifer+Young+AgeWMean+ClearCutDist+DistLogg+PocetZSnad70+Objem_MS, data = datahab2, random = ~1|ID, method = "ML")

summary(full.PD)

bestfit.PD <- stepAIC(full.PD)# Postupný výbìr promìnných, v tomto pøípadì defaultnì "backward". Pokud bych zadal scope a vyplnil promìnné, mohu zvolit i ostatní zpùsoby.


summary(bestfit.PD)


library(ggplot2)
library(multcomp)



install.packages("effects")
library(effects)


effects<- effects::effect(term= "Pocet_List", mod= bestfit.PD)
summary(effects) #output of what the values are


# Save the effects values as a df:
x_list <- as.data.frame(effects)




#1
list_plot <- ggplot() + 
#2
geom_point(data=datahab2, aes(Pocet_List, Celk_PD)) + 
ylim(0,20)+
#4
geom_line(data=x_list, aes(x=Pocet_List, y=fit), color="red") +
#5
geom_ribbon(data= x_list, aes(x=Pocet_List, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
#6
labs(x="Number of deciduous trees over 70 cm DBH", y="Total number of species")+

theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
annotate(geom="text", x=2.5, y=19, label="p < 0.001", fontface="bold")+
theme(
  panel.background = element_rect(fill = "white", colour = "red",
                                size = 2, linetype = "solid")



list_plot


Bílý GRAF


#1
list_plot <- ggplot() + 
#2
geom_point(data=datahab2, aes(Pocet_List, Celk_PD), shape = 1, colour = "black", fill = "white", size = 3, stroke = 1) + 
ylim(0,20)+
#4
geom_line(data=x_list, aes(x=Pocet_List, y=fit), color="black", size=1.2) +
#5
geom_ribbon(data= x_list, aes(x=Pocet_List, ymin=lower, ymax=upper), alpha= 0.3, fill="#808080") +
#6
labs(x="Number of deciduous trees over 70 cm DBH", y="Total number of species")+

theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
annotate(geom="text", x=3.8, y=19, label="p < 0.001 ***", fontface="bold", size=4.5)+

theme(
  panel.background = element_rect(fill = "white", colour = "black",
                                size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "#d8d8d8"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "#d8d8d8")
  )

list_plot










effects1<- effects::effect(term= "Decid", mod= bestfit.PD)
summary(effects1) #output of what the values are


# Save the effects values as a df:
x_decid <- as.data.frame(effects1)




#1
list_plot <- ggplot() + 
#2
geom_point(data=datahab2, aes(Decid, Celk_PD)) + 
ylim(0,20)+

#4
geom_line(data=x_decid, aes(x=Decid, y=fit), color="red") +
#5
geom_ribbon(data= x_decid, aes(x=Decid, ymin=lower, ymax=upper), alpha= 0.3, fill="blue") +
#6
labs(x="Proportion of deciduous tree crown cover", y="Total number of species")+

theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
annotate(geom="text", x=2.5, y=19, label="p < 0.01", fontface="bold")



list_plot




Bílý graf

#1
list_plot <- ggplot() + 
#2
geom_point(data=datahab2, aes(Decid, Celk_PD),shape = 1, colour = "black", fill = "white", size = 3, stroke = 1) + 
ylim(0,20)+

#4
geom_line(data=x_decid, aes(x=Decid, y=fit), color="black", size=1.2) +
#5
geom_ribbon(data= x_decid, aes(x=Decid, ymin=lower, ymax=upper), alpha= 0.3, fill="#808080") +
#6
labs(x="Proportion of deciduous tree crown cover", y="Total number of species")+

theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
annotate(geom="text", x=5.8, y=19, label="p = 0.01 **", fontface="bold", size=4.5)+

theme(
  panel.background = element_rect(fill = "white", colour = "black",
                                size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "#d8d8d8"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "#d8d8d8")
  )

list_plot









effects2<- effects::effect(term= "ClearCutDist", mod= bestfit.PD)
summary(effects2) #output of what the values are


# Save the effects values as a df:
x_clearcut <- as.data.frame(effects2)




#1
list_plot <- ggplot() + 
#2
geom_point(data=datahab2, aes(ClearCutDist, Celk_PD), shape = 1, colour = "black", fill = "white", size = 3, stroke = 1) + 
ylim(0,20)+

#4
geom_line(data=x_clearcut, aes(x=ClearCutDist, y=fit), color="black", size=1.2) +
#5
geom_ribbon(data= x_clearcut, aes(x=ClearCutDist, ymin=lower, ymax=upper), alpha= 0.3, fill="#808080") +
#6
labs(x="Distance to the nearest clear cut (m)", y="Total number of species")+

theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
annotate(geom="text", x=50, y=19, label="p = 0.002 **", fontface="bold",size=4.5)+

theme(
  panel.background = element_rect(fill = "white", colour = "black",
                                size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "#d8d8d8"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "#d8d8d8")
  )

list_plot



