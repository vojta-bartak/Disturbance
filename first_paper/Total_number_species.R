


library(nlme)
library(MASS)
library(MuMIn)


null.PD<- lme(Celk_PD ~ 1, data = datahab2, random = ~1|ID, method = "ML")

summary(null.PD)



Linear mixed-effects model fit by maximum likelihood
 Data: datahab2 
       AIC      BIC    logLik
  824.2218 833.8006 -409.1109

Random effects:
 Formula: ~1 | ID
        (Intercept) Residual
StdDev:   0.6803639 2.273924

Fixed effects: Celk_PD ~ 1 
               Value Std.Error  DF  t-value p-value
(Intercept) 10.42254 0.2307745 160 45.16333       0

Standardized Within-Group Residuals:
        Min          Q1         Med          Q3         Max 
-2.24870175 -0.73991814  0.02539008  0.82558353  2.37438069 

Number of Observations: 180
Number of Groups: 20




full.PD<- lme(Celk_PD ~ Age_SD + H_Trees+Pocet_List+Elev+Decid+Conifer+Young+AgeWMean+ClearCutDist+DistLogg+PocetZSnad70+Objem_MS, data = datahab2, random = ~1|ID, method = "ML")

summary(full.PD)

bestfit.PD <- stepAIC(full.PD)# Postupný výbìr promìnných, v tomto pøípadì defaultnì "backward". Pokud bych zadal scope a vyplnil promìnné, mohu zvolit i ostatní zpùsoby.


summary(bestfit.PD)

Linear mixed-effects model fit by maximum likelihood
 Data: datahab2 
       AIC      BIC    logLik
  779.1271 801.4778 -382.5636

Random effects:
 Formula: ~1 | ID
        (Intercept) Residual
StdDev:   0.5618582 1.966397

Fixed effects: Celk_PD ~ Age_SD + Pocet_List + Decid + ClearCutDist 
                 Value Std.Error  DF   t-value p-value
(Intercept)   9.917864 0.4264612 156 23.256192  0.0000
Age_SD        0.015589 0.0111366 156  1.399796  0.1636
Pocet_List    0.130181 0.0353286 156  3.684859  0.0003
Decid         0.032312 0.0123624 156  2.613730  0.0098
ClearCutDist -0.007589 0.0023548 156 -3.222665  0.0015
 Correlation: 
             (Intr) Age_SD Pct_Ls Decid 
Age_SD       -0.646                     
Pocet_List   -0.064 -0.089              
Decid        -0.010 -0.239 -0.466       
ClearCutDist -0.618  0.184  0.048 -0.159

Standardized Within-Group Residuals:
         Min           Q1          Med           Q3          Max 
-2.245409910 -0.691250849  0.009928278  0.711002315  2.544259561 

Number of Observations: 180
Number of Groups: 20 



bestfit.PDF<-stepAIC(null.PD, direction="forward", scope=list(lower=null.PD, upper=~Age_SD + H_Trees+Pocet_List+Elev+Decid+Conifer+Young+AgeWMean+ClearCutDist+DistLogg+PocetZSnad70+Objem_MS, data = datahab2))
summary(bestfit.PDF)

Linear mixed-effects model fit by maximum likelihood
 Data: datahab2 
       AIC      BIC    logLik
  779.1271 801.4778 -382.5636

Random effects:
 Formula: ~1 | ID
        (Intercept) Residual
StdDev:   0.5618582 1.966397

Fixed effects: Celk_PD ~ Pocet_List + ClearCutDist + Decid + Age_SD 
                 Value Std.Error  DF   t-value p-value
(Intercept)   9.917864 0.4264612 156 23.256192  0.0000
Pocet_List    0.130181 0.0353286 156  3.684859  0.0003
ClearCutDist -0.007589 0.0023548 156 -3.222665  0.0015
Decid         0.032312 0.0123624 156  2.613730  0.0098
Age_SD        0.015589 0.0111366 156  1.399796  0.1636
 Correlation: 
             (Intr) Pct_Ls ClrCtD Decid 
Pocet_List   -0.064                     
ClearCutDist -0.618  0.048              
Decid        -0.010 -0.466 -0.159       
Age_SD       -0.646 -0.089  0.184 -0.239

Standardized Within-Group Residuals:
         Min           Q1          Med           Q3          Max 
-2.245409913 -0.691250845  0.009928281  0.711002319  2.544259563 

Number of Observations: 180
Number of Groups: 20





Backward a Forward selection = stejný model!!

plot(fitted(bestfit), resid(bestfit, type = "pearson"))# this will create the plot
abline(0,0, col="red")

Obdoba
scatter.smooth(fitted(bestfit),resid(bestfit),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

Snesitelné


qqnorm(resid(bestfit)) 
qqline(resid(bestfit), col = "red") # add a perfect fit line


shapiro.test(resid(bestfit))# hodnota p pod 0,05 = residuály nemají normální rozdìlení (nutnost transformace závislé promìnné, nebo zvolit jiný test - GLM - Poisson apod.)

Shapiro-Wilk normality test

data:  resid(bestfit)
W = 0.99477, p-value = 0.7815




qqnorm(bestfit, ~ resid(., type = "p") | ID, abline = c(0, 1))


qqnorm(bestfit, ~ranef(.))



# Vysvìtlená variabilita


# null.PD:        Residual             2.273924
# bestfit.PD:     Residual             1.966397


(2.273924-1.966397)/2.273924

# 0.1352407


v <- VarCorr(bestfit.PD)
v
ID = pdLogChol(1) 
            Variance  StdDev   
(Intercept) 0.3156847 0.5618582
Residual    3.8667167 1.9663969

as.numeric(v[,"Variance"])
0.3156847 3.8667167


TotalVar<-0.3156847+3.8667167

0.3156847/TotalVar
0.07547929

3.8667167/TotalVar
0.9245207

r.squaredGLMM(bestfit.PD) #i Nakagawa & Schielzeth (2013)
           R2m       R2c
[1,] 0.2498254 0.3064481
Warning message:
'r.squaredGLMM' now calculates a revised statistic. See the help page. 

R2m: marginal (okrajová) R squared value associated with fixed effects

R2c conditional (podmínìná) R2 value associated with fixed effects plus the random effects.



# we extract the variance components
  Var_Random_effect <- as.numeric(VarCorr(bestfit.PD)) #we extract the variance of the random effect (which capture individual variation not captured by fixed effects)
  Var_Residual <- attr(VarCorr(bestfit.PD), "sc")^2 #we extract the residual variance (which corresponds to within individual variation in this case where no fixed effects differ within indiv)
  Var_Fix_effect <- var(predict(lm(Celk_PD~Pocet_List + ClearCutDist + Decid + Age_SD))) #we extract the variance explained by the fixed effect

data <- Pocet_List + ClearCutDist + Decid + Age_SD


# look, the sum of the variance components is close to the total variance of the data
var(data)
5244.122

Var_Random_effect+Var_Residual+Var_Fix_effect

nic


install.packages("mitml")
library(mitml)
require(lme4)
multilevelR2(bestfit.PD)
nic



# Mìl bych vzít v úvahu také rozdílné sklony?




bestfit.PD

sklon.PD<- lme(Celk_PD ~ Age_SD + Pocet_List + Decid + ClearCutDist, data = datahab2, random = ~Pocet_List|ID, method = "ML")

summary(sklon.PD)



install.packages("lmerTest")
library(lmerTest)

ranova(sklon.PD)



