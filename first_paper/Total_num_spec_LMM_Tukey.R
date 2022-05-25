
library(nlme)


m1<- lme(Celk_PD ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")





plot(fitted(m1), resid(m1, type = "pearson"))# this will create the plot
abline(0,0, col="red")

Obdoba
scatter.smooth(fitted(m1),resid(m1),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(m1)) 
qqline(resid(m1), col = "red") # add a perfect fit line


shapiro.test(resid(m1))#


 Shapiro-Wilk normality test

data:  resid(m1)
W = 0.99204, p-value = 0.3467



summary(m1)



Linear mixed-effects model fit by maximum likelihood
 Data: datahab4 
       AIC      BIC    logLik
  874.9626 898.0508 -430.4813

Random effects:
 Formula: ~1 | ID_up
        (Intercept) Residual
StdDev:    0.593186 2.020267

Fixed effects: Celk_PD ~ Kat_hustZS 
                    Value Std.Error  DF  t-value p-value
(Intercept)      9.379361 0.2860950 177 32.78408   0e+00
Kat_hustZSB 0<>1 1.225885 0.3654750 177  3.35422   1e-03
Kat_hustZSC1<>3  1.570461 0.4541207 177  3.45825   7e-04
Kat_hustZSD3<>9  3.256302 0.5354708 177  6.08120   0e+00
Kat_hustZSR      2.914649 0.5219704 177  5.58394   0e+00
 Correlation: 
                 (Intr) K_ZSB0 K_ZSC1 K_ZSD3
Kat_hustZSB 0<>1 -0.595                     
Kat_hustZSC1<>3  -0.484  0.369              
Kat_hustZSD3<>9  -0.411  0.309  0.275       
Kat_hustZSR      -0.420  0.325  0.264  0.228

Standardized Within-Group Residuals:
       Min         Q1        Med         Q3        Max 
-2.5135820 -0.7861784  0.0377882  0.7029307  2.6442492 

Number of Observations: 200
Number of Groups: 19



anova(m1)



            numDF denDF   F-value p-value
(Intercept)     1   177 2759.7282  <.0001
Kat_hustZS      4   177   14.1193  <.0001



library(multcomp)
m1a<-glht(m1,linfct=mcp(Kat_hustZS="Tukey"))



summary(m1a)







         Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts


Fit: lme.formula(fixed = Celk_PD ~ Kat_hustZS, data = datahab4, random = ~1 | 
    ID_up, method = "ML")

Linear Hypotheses:
                    Estimate Std. Error z value Pr(>|z|)    
B 0<>1 - A=0 == 0     1.2259     0.3609   3.397  0.00569 ** 
C1<>3 - A=0 == 0      1.5705     0.4484   3.502  0.00397 ** 
D3<>9 - A=0 == 0      3.2563     0.5287   6.159  < 0.001 ***
R - A=0 == 0          2.9146     0.5154   5.655  < 0.001 ***
C1<>3 - B 0<>1 == 0   0.3446     0.4604   0.748  0.94283    
D3<>9 - B 0<>1 == 0   2.0304     0.5402   3.758  0.00158 ** 
R - B 0<>1 == 0       1.6888     0.5244   3.220  0.01074 *  
D3<>9 - C1<>3 == 0    1.6858     0.5918   2.849  0.03390 *  
R - C1<>3 == 0        1.3442     0.5870   2.290  0.14265    
R - D3<>9 == 0       -0.3417     0.6487  -0.527  0.98400    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Adjusted p values reported -- single-step method)




cld(m1a)

A=0 B 0<>1  C1<>3  D3<>9      R 
   "a"    "b"   "bc"    "d"   "cd" 



- výsledek stejný, nejde mi predict.lme
- boxplolt jako výsledek












Specialiste





mS1<- lme(Specialist ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")





plot(fitted(mS1), resid(mS1, type = "pearson"))# this will create the plot
abline(0,0, col="red")

Obdoba
scatter.smooth(fitted(mS1),resid(mS1),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(mS1)) 
qqline(resid(mS1), col = "red") # add a perfect fit line


shapiro.test(resid(mS1))#


Shapiro-Wilk normality test

data:  resid(mS1)
W = 0.94258, p-value = 3.872e-07




mS2<- lme(sqrt(Specialist) ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")

plot(fitted(mS2), resid(mS2, type = "pearson"))# this will create the plot
abline(0,0, col="red")

Obdoba
scatter.smooth(fitted(mS2),resid(mS2),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(mS2)) 
qqline(resid(mS2), col = "red") # add a perfect fit line


shapiro.test(resid(mS2))#



Shapiro-Wilk normality test

data:  resid(mS2)
W = 0.99291, p-value = 0.4482



ok




summary(mS2)

anova(mS2)
            numDF denDF  F-value p-value
(Intercept)     1   177 995.0793  <.0001
Kat_hustZS      4   177   1.7511  0.1408




mS2a<-glht(mS2,linfct=mcp(Kat_hustZS="Tukey"))
summary(mS2a)
      Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts


Fit: lme.formula(fixed = sqrt(Specialist) ~ Kat_hustZS, data = datahab4, 
    random = ~1 | ID_up, method = "ML")

Linear Hypotheses:
                    Estimate Std. Error z value Pr(>|z|)
B 0<>1 - A=0 == 0    0.08503    0.07521   1.131    0.784
C1<>3 - A=0 == 0     0.03416    0.09437   0.362    0.996
D3<>9 - A=0 == 0     0.19871    0.11153   1.782    0.375
R - A=0 == 0        -0.12779    0.10681  -1.196    0.746
C1<>3 - B 0<>1 == 0 -0.05087    0.09721  -0.523    0.984
D3<>9 - B 0<>1 == 0  0.11368    0.11443   0.993    0.854
R - B 0<>1 == 0     -0.21282    0.10870  -1.958    0.279
D3<>9 - C1<>3 == 0   0.16455    0.12400   1.327    0.666
R - C1<>3 == 0      -0.16195    0.12241  -1.323    0.668
R - D3<>9 == 0      -0.32650    0.13555  -2.409    0.108
(Adjusted p values reported -- single-step method)
cld(mS2a)





Generaliste





mG1<- lme(Generalist ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")

plot(fitted(mG1), resid(mG1, type = "pearson"))# this will create the plot
abline(0,0, col="red")

Obdoba
scatter.smooth(fitted(mG1),resid(mG1),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(mG1)) 
qqline(resid(mG1), col = "red") # add a perfect fit line


shapiro.test(resid(mG1))#




Shapiro-Wilk normality test

data:  resid(mG1)
W = 0.98411, p-value = 0.0235






mG2<- lme(sqrt(Generalist) ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")



plot(fitted(mG2), resid(mG2, type = "pearson"))# this will create the plot
abline(0,0, col="red")

Obdoba
scatter.smooth(fitted(mG2),resid(mG2),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(mG2)) 
qqline(resid(mG2), col = "red") # add a perfect fit line


shapiro.test(resid(mG2))#



        Shapiro-Wilk normality test

data:  resid(mG2)
W = 0.9964, p-value = 0.9238





summary(mG2)
Linear mixed-effects model fit by maximum likelihood
 Data: datahab4 
       AIC      BIC    logLik
  230.3834 253.4716 -108.1917

Random effects:
 Formula: ~1 | ID_up
        (Intercept) Residual
StdDev:   0.1790816 0.393784

Fixed effects: sqrt(Generalist) ~ Kat_hustZS 
                     Value  Std.Error  DF  t-value p-value
(Intercept)      2.7052694 0.06450548 177 41.93860  0.0000
Kat_hustZSB 0<>1 0.1645197 0.07164276 177  2.29639  0.0228
Kat_hustZSC1<>3  0.2818455 0.08957756 177  3.14639  0.0019
Kat_hustZSD3<>9  0.5379505 0.10578177 177  5.08547  0.0000
Kat_hustZSR      0.6746537 0.10196191 177  6.61672  0.0000
 Correlation: 
                 (Intr) K_ZSB0 K_ZSC1 K_ZSD3
Kat_hustZSB 0<>1 -0.518                     
Kat_hustZSC1<>3  -0.420  0.363              
Kat_hustZSD3<>9  -0.355  0.302  0.280       
Kat_hustZSR      -0.369  0.326  0.264  0.229

Standardized Within-Group Residuals:
         Min           Q1          Med           Q3          Max 
-2.333349285 -0.630060636 -0.007701326  0.696822010  2.760094989 

Number of Observations: 200
Number of Groups: 19 




anova(mG2)
            numDF denDF  F-value p-value
(Intercept)     1   177 3310.725  <.0001
Kat_hustZS      4   177   14.634  <.0001





mG2a<-glht(mG2,linfct=mcp(Kat_hustZS="Tukey"))
summary(mG2a)


         Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts


Fit: lme.formula(fixed = sqrt(Generalist) ~ Kat_hustZS, data = datahab4, 
    random = ~1 | ID_up, method = "ML")

Linear Hypotheses:
                    Estimate Std. Error z value Pr(>|z|)    
B 0<>1 - A=0 == 0    0.16452    0.07074   2.326  0.13158    
C1<>3 - A=0 == 0     0.28185    0.08845   3.186  0.01181 *  
D3<>9 - A=0 == 0     0.53795    0.10445   5.150  < 0.001 ***
R - A=0 == 0         0.67465    0.10068   6.701  < 0.001 ***
C1<>3 - B 0<>1 == 0  0.11733    0.09099   1.289  0.68970    
D3<>9 - B 0<>1 == 0  0.37343    0.10699   3.490  0.00429 ** 
R - B 0<>1 == 0      0.51013    0.10246   4.979  < 0.001 ***
D3<>9 - C1<>3 == 0   0.25611    0.11642   2.200  0.17341    
R - C1<>3 == 0       0.39281    0.11511   3.413  0.00548 ** 
R - D3<>9 == 0       0.13670    0.12737   1.073  0.81456    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Adjusted p values reported -- single-step method)




pred.gen<-data.frame(Kat_hustZS=as.factor(levels(Kat_hustZS)),fit=0,lcl=0,ucl=0)
pred.gen[,2:4]<-(predict(mG2,pred.gen,level=0,interval="confidence"))^2
pred.gen





intervalmG2<-intervals(mG2, level=0.95, which="fixed")

intervalmG2


Approximate 95% confidence intervals

 Fixed effects:
                      lower      est.     upper
(Intercept)      2.57957186 2.7052694 2.8309669
Kat_hustZSB 0<>1 0.02491431 0.1645197 0.3041252
Kat_hustZSC1<>3  0.10729172 0.2818455 0.4563993
Kat_hustZSD3<>9  0.33182068 0.5379505 0.7440803
Kat_hustZSR      0.47596739 0.6746537 0.8733401
attr(,"label")
[1] "Fixed effects:"

 Random Effects:
  Level: ID_up 
                    lower      est.     upper
sd((Intercept)) 0.1118892 0.1790816 0.2866248

 Within-group standard error:
    lower      est.     upper 
0.3552799 0.3937840 0.4364611 

2.7052694^2
[1] 7.318483

2.8309669^2

(2.7052694+0.1645197)^2
[1] 8.235689


Kat_hustZS       fit       lcl       ucl
1        A=0  7.318482  7.318482  7.318482
2     B 0<>1  8.235689  8.235689  8.235689
3      C1<>3  8.922855  8.922855  8.922855
4      D3<>9 10.518475 10.518475 10.518475
5          R 11.423880 11.423880 11.423880






prum<-tapply(Generalist,Kat_hustZS,mean)

      A=0    B 0<>1     C1<>3     D3<>9         R 
 7.492754  8.483333  9.193548 10.350000 11.600000 


se<-function(x){sd(x)/sqrt(length(x))}
chyb<-tapply(Generalist,Kat_hustZS,se)

      A=0    B 0<>1     C1<>3     D3<>9         R 
0.2759338 0.3686068 0.4872997 0.5632378 0.6341177




cld(mG2a)
A=0 B 0<>1  C1<>3  D3<>9      R 
   "a"   "ab"   "bc"   "cd"    "d" 








#create data.frame with new values for predictors
#more than one predictor is possible
new.dat <- data.frame(x=0:200)


#predict response
new.dat$pred <- predict(mG2, newdata=new.dat,level=0)

#create design matrix
Designmat <- model.matrix(eval(eval(model.mx$call$fixed)[-2]), new.dat[-ncol(new.dat)])

#compute standard error for predictions
predvar <- diag(Designmat %*% model.mx$varFix %*% t(Designmat))
new.dat$SE <- sqrt(predvar) 
new.dat$SE2 <- sqrt(predvar+model.mx$sigma^2)











Cavity





mC1<- lme(Cavity ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")




scatter.smooth(fitted(mC1),resid(mC1),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(mC1)) 
qqline(resid(mC1), col = "red") # add a perfect fit line


shapiro.test(resid(mC1))#

  Shapiro-Wilk normality test

data:  resid(mC1)
W = 0.96688, p-value = 0.0001186






mC2<- lme(sqrt(Cavity) ~Kat_hustZS, data = datahab4, random = ~1|ID_up, method = "ML")




scatter.smooth(fitted(mC2),resid(mC2),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(mC2)) 
qqline(resid(mC2), col = "red") # add a perfect fit line


shapiro.test(resid(mC2))#





Shapiro-Wilk normality test

data:  resid(mC2)
W = 0.97262, p-value = 0.0006026






library(MASS)


mC3<-glmmPQL(Cavity~Kat_hustZS, data = datahab4, random = ~1|ID_up,family=poisson)
plot(mC3,residuals(.) ~log(fitted(.)))



scatter.smooth(fitted(mC3),resid(mC3),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(mC3)) 
qqline(resid(mC3), col = "red") # add a perfect fit line


- graf vypadá dost dobøe


shapiro.test(resid(mC3))#
Shapiro-Wilk normality test

data:  resid(mC3)
W = 0.98335, p-value = 0.01814




library(ggplot2)



ggplot(fortify(mC3),
       aes(x=.fitted,y=sqrt(abs(.scresid))))+geom_point()+
    geom_smooth(colour="red",alpha=0.3)




plot(mC3,ID_up~resid(.,type="pearson"))


summary(mC3)





library(lme4)





mC4<-glmer(Cavity~Kat_hustZS+ (1 | ID_up), data = datahab4,family=poisson)
scatter.smooth(fitted(mC3),resid(mC3),col='red');abline(h=0, lty=2)# køivka by mìla být co nejblíže èerchované èáøe, body by nemìly ležet akumulovanì v rohu grafu

ok



qqnorm(resid(mC3)) 
qqline(resid(mC3), col = "red") # add a perfect fit line


- graf vypadá dost dobøe


shapiro.test(resid(mC3))#
 Shapiro-Wilk normality test

data:  resid(mC3)
W = 0.98335, p-value = 0.01814





summary(mC4)

Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: poisson  ( log )
Formula: Cavity ~ Kat_hustZS + (1 | ID_up)
   Data: datahab4

     AIC      BIC   logLik deviance df.resid 
   766.2    786.0   -377.1    754.2      194 

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-1.82489 -0.54626 -0.02068  0.44947  2.56238 

Random effects:
 Groups Name        Variance Std.Dev.
 ID_up  (Intercept) 0.01925  0.1387  
Number of obs: 200, groups:  ID_up, 19

Fixed effects:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)       1.02738    0.07935  12.948  < 2e-16 ***
Kat_hustZSB 0<>1  0.27698    0.09995   2.771 0.005585 ** 
Kat_hustZSC1<>3   0.40586    0.11495   3.531 0.000415 ***
Kat_hustZSD3<>9   0.60774    0.12461   4.877 1.08e-06 ***
Kat_hustZSR       0.70837    0.11806   6.000 1.97e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
            (Intr) K_ZSB0 K_ZSC1 K_ZSD3
Kt_hZSB0<>1 -0.666                     
Kt_hZSC1<>3 -0.569  0.442              
Kt_hZSD3<>9 -0.519  0.397  0.385       
Kat_hustZSR -0.558  0.438  0.385  0.361






mC4K<-glht(mC4, linfct = mcp(Kat_hustZS = "Tukey"))
summary(mC4K)





         Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts


Fit: glmer(formula = Cavity ~ Kat_hustZS + (1 | ID_up), data = datahab4, 
    family = poisson)

Linear Hypotheses:
                    Estimate Std. Error z value Pr(>|z|)    
B 0<>1 - A=0 == 0    0.27698    0.09995   2.771  0.04368 *  
C1<>3 - A=0 == 0     0.40586    0.11495   3.531  0.00366 ** 
D3<>9 - A=0 == 0     0.60774    0.12461   4.877  < 0.001 ***
R - A=0 == 0         0.70837    0.11806   6.000  < 0.001 ***
C1<>3 - B 0<>1 == 0  0.12888    0.11421   1.128  0.78947    
D3<>9 - B 0<>1 == 0  0.33076    0.12503   2.646  0.06150 .  
R - B 0<>1 == 0      0.43139    0.11660   3.700  0.00206 ** 
D3<>9 - C1<>3 == 0   0.20188    0.13310   1.517  0.54844    
R - C1<>3 == 0       0.30251    0.12924   2.341  0.13061    
R - D3<>9 == 0       0.10063    0.13726   0.733  0.94819



cld(mC4K)

A=0 B 0<>1  C1<>3  D3<>9      R 
   "a"    "b"   "bc"   "bc"    "c" 



boxplot(Cavity~Kat_hustZS, ylab="Number of species - Cavity", xlab="Number of deciduous trees over 70 cm DBH/ha", names=c("0","0<>1","1<>3","3<>9","R"))
text(x=c(1:5),y=tapply(Cavity,Kat_hustZS,median)+0.3,labels=c("a","b","bc","bc", "c"))
text(x=c(1:5),y=tapply(Cavity,Kat_hustZS,median)+6,labels=c("(69)","(60)","(31)","(20)","(20)"))
text(x=c(1:5),y=10,labels=c("(69)","(60)","(31)","(20)","(20)"))




pred.cav<-data.frame(Kat_hustZS=as.factor(levels(Kat_hustZS)),fit=0,lcl=0,ucl=0)
pred.cav[,2:4]<-exp(predict(mC4,newdata=pred.cav,interval="confidence"))
pred.cav



# Tvorba grafu
plot(pred.cav$fit~c(1:5),pch=16,ylim=c(0,8),xlim=c(0.5,5.5),
     xaxt="n",xlab="Number of deciduous trees over 70 cm DBH/ha",ylab="Number of species - Cavity")
arrows(x0=c(1:5),x1=c(1:5),y0=pred.cav$lcl,y1=pred.cav$ucl,
       code=3,angle=90,length=0.05)
axis(side=1,at=c(1:5),labels=c("0","0<>1","1<>3","3<>9","R"))
text(x=c(1:5),y=pred.cav$lcl-0.75,labels=c("a","b","bc","bc", "c"))
text(x=c(1:5),y=pred.cav$ucl+0.75,labels=c("(69)","(60)","(31)","(20)","(20)"))

