library(tidyverse)
library(performance)
library(multcomp)
library(car)
library(corrplot)
library(dismo)

df <- read.table("data2.txt", header=T)
df$Subtyp_final <- as.factor(df$Subtyp_final)
df$pomer <- df$Total_LT_ha1/(df$Total_LT_ha1 + df$deadw_ha1)

# Total_LT_ha1 - nelinear?
# Struct_Can
# Underg_.

m <- lm(Total_Rich ~ poly(Total_LT_ha1, 2)+poly(Underg_.,2)+poly(Struct_Can,2)+Elev, data=df)
m <- lm(Total_Rich ~ Total_LT_ha1+ Total_LT_ha1**2+Underg_.+Underg_.**2+Struct_Can+Struct_Can**2+Elev, data=df)
Anova(m)
drop1(m)

effect_plots(m, c("Total_LT_ha1","Struct_Can","Underg_.","Elev", factors=c(), others=c()))
summary(m)

m2 <- lm(Total_Rich ~ poly(Total_LT_ha1, 2)+poly(Underg_.,2)+Struct_Can+Elev, data=df)
anova(m,m2)
AIC(m,m2)
effect_plots(m2, c("Total_LT_ha1","Struct_Can","Underg_.","Elev", factors=c(), others=c()))
summary(m2)

nd <- data.frame(Struct_Can = seq(min(df$Struct_Can), max(df$Struct_Can), l=100),
                 Total_LT_ha1 = mean(df$Total_LT_ha1),
                 Underg_. = mean(df$Underg_.),
                 Elev = mean(df$Elev))
nd <- cbind(nd, predict(m, se=T, newdata = nd))
ggplot(nd, aes(x=Struct_Can, y=fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3)

Anova(stepAIC(m, direction = "backward", trace = 0))

effect_plots <- function(model, preds, resp="Total_Rich", others=c("Struct_Can"), factors=c("Subtyp_final")){
  nd <- do.call(rbind, lapply(preds, function(pred){
    ndd <- data.frame(x = seq(min(df[,pred]), max(df[,pred]), l=100))
    ndd[,pred] <- ndd[,"x"]
    for (p in preds[which(preds!=pred)]){
      ndd[,p] = mean(df[,p])
    }
    for (p in others){
      ndd[,p] = mean(df[,p])
    }
    for (f in factors){
      ndd[,f] = factor(levels(df[,f])[1], levels=levels(df[,f]))
    }
    ndd[,"var"] = pred
    ndd
  }))
  nd <- cbind(nd, predict(model, se=T, newdata=nd))
  ggplot(nd, aes(x=x, y=fit)) +
    geom_line() +
    geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3) +
    facet_wrap(~var, scales="free_x") +
    labs(x="", y=resp)
}

interact_plot <- function(model, pred, fact, resp="Total_Rich", others, factors=c(), facets=FALSE){
  nd <- expand.grid(
    pred = seq(min(df[,pred]), max(df[,pred]), l=100),
    fact = levels(df[,fact])
  )
  colnames(nd) <- c(pred, fact)
  nd[,"x"] <- nd[,pred]
  for (p in others){
    nd[,p] = mean(df[,p])
  }
  for (f in factors){
    nd[,f] = factor(levels(df[,f])[1], levels=levels(df[,f]))
  }
  nd <- cbind(nd, predict(model, se=T, newdata=nd))
  if (facets) {
  p <- ggplot(nd, aes(x=x, y=fit)) +
    geom_line() +
    geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3) +
    facet_grid(.~get(fact)) +
    labs(x=pred, y=resp)  
  } else {
    p <- ggplot(nd, aes(x=x, y=fit, color=get(fact), fill=get(fact))) +
      geom_line() +
      geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3) +
      labs(x=pred, y=resp)
  }
  p
}


# Total richness -----------------------------------------------------------------------------
m.vif <- lm(Total_Rich ~ 
               Subtyp_final   + 
               Total_LT_ha1   +
               BiologLegac1   +
               Underg_.       +
               Struct_Can     +
               Windfall1      +
               StandDeadtree1 +
               pomer +
               Elev, data=df)
vif(m.vif)
vif(update(m.vif, ~.-BiologLegac1))
vif(update(m.vif, ~.-Windfall1-StandDeadtree1-BiologLegac1-Total_LT_ha1))

(cors <- cor(df %>% dplyr::select(c(Total_LT_ha1,
                                    BiologLegac1,
                                    Underg_.,
                                    Struct_Can,
                                    Windfall1,
                                    StandDeadtree1,
                                    pomer,
                                    Can_cover,
                                    deadw_ha1))))
corrplot(cors)
corrplot(cor(df %>% dplyr::select(c(Underg_.,
                                    Struct_Can,
                                    pomer))))

m.full1 <- lm(Total_Rich ~ 
               Subtyp_final   + 
               #Total_LT_ha1   + Subtyp_final:Total_LT_ha1 +
               #BiologLegac1   + Subtyp_final:BiologLegac1 +
               Underg_.       + Subtyp_final:Underg_. +
               Struct_Can     + Subtyp_final:Struct_Can +
               #Windfall1      + Subtyp_final:Windfall1 +
               #StandDeadtree1 + Subtyp_final:StandDeadtree1 +
               pomer          + Subtyp_final:pomer +
               Elev, data=df)
Anova(m.full1)
m.final1 <- stepAIC(m.full1, direction = "backward", trace = 0)
Anova(m.final1)

m.full2 <- lm(Total_Rich ~ 
                Subtyp_final   + 
                Total_LT_ha1   + Subtyp_final:Total_LT_ha1 +
                #BiologLegac1   + Subtyp_final:BiologLegac1 +
                Underg_.       + Subtyp_final:Underg_. +
                Struct_Can     + Subtyp_final:Struct_Can +
                #Windfall1      + Subtyp_final:Windfall1 +
                #StandDeadtree1 + Subtyp_final:StandDeadtree1 +
                #pomer          + Subtyp_final:pomer +
                Elev, data=df)
Anova(m.full2)
m.final2 <- stepAIC(m.full2, direction = "backward", trace = 0)
Anova(m.final2)

m.full3 <- lm(Total_Rich ~ 
                Subtyp_final   + 
                #Total_LT_ha1   + Subtyp_final:Total_LT_ha1 +
                BiologLegac1   + Subtyp_final:BiologLegac1 +
                Underg_.       + Subtyp_final:Underg_. +
                Struct_Can     + Subtyp_final:Struct_Can +
                #Windfall1      + Subtyp_final:Windfall1 +
                #StandDeadtree1 + Subtyp_final:StandDeadtree1 +
                #pomer          + Subtyp_final:pomer +
                Elev, data=df)
Anova(m.full3)
m.final3 <- stepAIC(m.full3, direction = "backward", trace = 0)
Anova(m.final3)

m.full4 <- lm(Total_Rich ~ 
                Total_LT_ha1 +
                #BiologLegac1 +
                Underg_. +
                Struct_Can +
                #Windfall1      + Subtyp_final:Windfall1 +
                #StandDeadtree1 + Subtyp_final:StandDeadtree1 +
                #pomer          + Subtyp_final:pomer +
                Elev, data=df)
Anova(m.full4)
m.full4 <- lm(Total_Rich ~ 
                #Total_LT_ha1 +
                BiologLegac1 +
                Underg_. +
                Struct_Can +
                #Windfall1      + Subtyp_final:Windfall1 +
                #StandDeadtree1 + Subtyp_final:StandDeadtree1 +
                #pomer          + Subtyp_final:pomer +
                Elev, data=df)
Anova(m.full4)
m.full4 <- lm(Total_Rich ~ 
                #Total_LT_ha1 +
                #BiologLegac1 +
                Underg_. +
                Struct_Can +
                #Windfall1      + Subtyp_final:Windfall1 +
                #StandDeadtree1 + Subtyp_final:StandDeadtree1 +
                Can_cover +
                Elev, data=df)
Anova(m.full4)

AIC(lm(Total_Rich ~ Underg_.+ Can_cover + Elev, data=df),
    lm(Total_Rich ~ Total_LT_ha1 + Underg_. + Elev, data=df))

m.final4 <- stepAIC(m.full4, direction = "backward", trace = 0)
Anova(m.final4)
summary(m.final4)

m.full5 <- lm(Total_Rich ~ 
                Subtyp_final   + 
                Total_LT_ha1   + Subtyp_final:Total_LT_ha1 +
                #BiologLegac1   + Subtyp_final:BiologLegac1 +
                Underg_.       + Subtyp_final:Underg_. +
                #Struct_Can     + Subtyp_final:Struct_Can +
                #Windfall1      + Subtyp_final:Windfall1 +
                #StandDeadtree1 + Subtyp_final:StandDeadtree1 +
                #pomer          + Subtyp_final:pomer +
                Elev, data=df)
Anova(m.full5)
m.final5 <- stepAIC(m.full5, direction = "backward", trace = 0)
Anova(m.final5)


m.full <- lm(Total_Rich ~ 
               Subtyp_final   + 
               Total_LT_ha3   + Subtyp_final:Total_LT_ha3 +
               #BiologLegac1   + Subtyp_final:BiologLegac1 +
               Underg_.       + Subtyp_final:Underg_. +
               Struct_Can     + Subtyp_final:Struct_Can +
               Windfall3      + Subtyp_final:Windfall3 +
               StandDeadtree1 + Subtyp_final:StandDeadtree1 +
               Elev, data=df)
m.final <- stepAIC(m.full, direction = "backward", trace=0)
summary(m.final)
Anova(m.final)

Anova(stepAIC(update(m.full, ~.-Underg_.-Subtyp_final:Underg_.), direction = "backward", trace=0))
Anova(stepAIC(update(m.full, ~.-Windfall3-Subtyp_final:Windfall3), direction = "backward", trace=0))
Anova(stepAIC(update(m.full, ~.-C-Subtyp_final:Total_LT_ha3), direction = "backward", trace=0))
Anova(stepAIC(update(m.full, ~.-Windfall3-Subtyp_final:Windfall3-StandDeadtree1-Subtyp_final:StandDeadtree1), 
              direction = "backward", trace=0))

# Subtyp_final and Struct_Can
nd <- expand.grid(Subtyp_final=as.factor(levels(df$Subtyp_final)),
                  Struct_Can=seq(min(df$Struct_Can), max(df$Struct_Can), l=100))
nd$Total_LT_ha3 <- mean(df$Total_LT_ha3)
nd$Underg_. <- mean(df$Underg_.)
nd$Windfall3 <- mean(df$Windfall3)
nd$Elev <- mean(df$Elev)
nd <- cbind(nd, predict(m.final, se=T, newdata = nd))
ggplot(nd, aes(x=Struct_Can, y=fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3) +
  geom_point(data=df, aes(y=Total_Rich), alpha=.2) +
  facet_wrap(~Subtyp_final)

effect_plots(m.final, c("Total_LT_ha3","Underg_.","Windfall3","Elev"))
interact_plot(m.final, pred="Struct_Can", fact="Subtyp_final", others=c("Total_LT_ha3","Underg_.","Windfall3","Elev"))
interact_plot(m.final, pred="Struct_Can", fact="Subtyp_final", others=c("Total_LT_ha3","Underg_.","Windfall3","Elev"), facets = T)

# predictors vs subtypes ---------------------------------
ggplot(df %>% pivot_longer(cols=c("Total_LT_ha1", "BiologLegac1",
                                  "Underg_.", "Struct_Can",
                                  "Windfall1", "StandDeadtree1", "pomer")), 
       aes(y=value, x=Subtyp_final)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_y")
