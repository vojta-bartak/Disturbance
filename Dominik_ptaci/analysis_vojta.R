# Libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(MuMIn)
library(car)
library(MASS)
library(performance)

# Load and rename data
df <- read.table("data_birds_trees.txt", sep="\t", header=T) %>%
  filter(Forest_type=="Production_forest") %>%
  mutate(
    ID = ID_study_plot,
    rich = Total_num_of_species,
    rich_sp = Specialist_species,
    rich_gen = Generalist_species,
    sh = as.numeric(scale(H_Trees)),
    ageSD = as.numeric(scale(Age_SD)),
    age = as.numeric(scale(AgeWMean)),
    young = as.numeric(scale(Young_cover)),
    conif = as.numeric(scale(Conifer_cover)),
    broadl = as.numeric(scale(Broadl_cover)),
    n70 = as.numeric(scale(NumLT70DBH)),
    bl70 = as.numeric(scale(NumBroadlLT70DBH)),
    cat70 = NumBroadlLT70DBH_ha
  )

# Multicollinearity check
vif(lmer(rich ~ conif+broadl+young+n70+bl70+age+ageSD+sh+(1|ID), data=df))
cor(df$conif, df$broadl, method = "spearman")

# conif excluded!
vif(lmer(rich ~ broadl+young+n70+bl70+age+ageSD+sh+(1|ID), data=df))

# All species richness -------------------------------------------------------

# Full model
m.full.glmm <- glmer(rich ~ broadl+young+n70+bl70+age+ageSD+sh+(1|ID), 
                data = df, family = poisson)
summary(m.full.glmm)
m0 <- glm(rich ~ broadl+young+n70+bl70+age+ageSD+sh, data = df, family = poisson)
pchisq(2*(logLik(m.full.glmm)-logLik(m0)), df=1, lower.tail=FALSE)/2

# There is singular fit, obviously caused by very little variation among the random effect 
# levels. This is confirmed by highly insignificant random effects (LR test above).
# So I suggest to switch to standard GLM.

m.full <- glm(rich ~ broadl+young+n70+bl70+age+ageSD+sh, data = df, family = poisson)
summary(m.full)
Anova(m.full)

# Just an old-fashion backward selection based on p-values, leading to the same results as
# the AIC-based one
Anova(update(m.full, ~.-young))
Anova(update(m.full, ~.-young-n70))
Anova(update(m.full, ~.-young-n70-age))
Anova(update(m.full, ~.-young-n70-age-broadl))
Anova(update(m.full, ~.-young-n70-age-broadl-ageSD))

# AIC backward selection
m <- stepAIC(m.full, trace=0)
summary(m)
Anova(m)

# Old broad-leaved trees are highly significant, forest diversity is weakly significant 

# Checking overdispersion
m$deviance/m$df.residual
library(performance)
check_overdispersion(m)

# We're good

# Model performance
library(rsq)
rsq(m)

# Not great, but OK. Beware of driving too strong conclusions.

# Effect plots:
nd <- data.frame(
  bl70 = seq(min(df$bl70), max(df$bl70), length.out = 100),
  sh = mean(df$sh)
)
nd <- cbind(nd, predict(m, type = "link", se = TRUE, newdata=nd))
nd$rich <- exp(nd$fit)
nd$upr <- exp(nd$fit + 1.96*nd$se.fit)
nd$lwr <- exp(nd$fit - 1.96*nd$se.fit)
ggplot(nd, aes(y=rich, x=bl70*sd(df$NumBroadlLT70DBH)+mean(df$NumBroadlLT70DBH))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df) +
  labs(x="Number of broadleaved trees over 70 cm",
       y="Bird species richness") +
  ggsave("All_species_vs_bl70_glm.png", dpi=600)

nd <- data.frame(
   sh = seq(min(df$sh), max(df$sh), length.out = 100),
   bl70 = mean(df$bl70)
)
nd <- cbind(nd, predict(m, type = "link", se = TRUE, newdata=nd))
nd$rich <- exp(nd$fit)
nd$upr <- exp(nd$fit + 1.96*nd$se.fit)
nd$lwr <- exp(nd$fit - 1.96*nd$se.fit)
ggplot(nd, aes(y=rich, x=sh*sd(df$H_Trees)+mean(df$H_Trees))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df) +
  labs(x="Shannon diversity of trees",
       y="Bird species richness") +
  ggsave("All_species_vs_Shannon_glm.png", dpi=600)

# I wouldn't report the second plot, because Shannon was only weakly significant
# and the plot looks accordingly.

# Specialists ----------------------------------------------------------------

# Full model
m.full.glmm <- glmer(rich_sp ~ broadl+young+n70+bl70+age+ageSD+sh+(1|ID), 
                     data = df, family = poisson, nAGQ = 1)
summary(m.full.glmm)
m0 <- glm(rich_sp ~ broadl+young+n70+bl70+age+ageSD+sh, data = df, family = poisson)
pchisq(2*(logLik(m.full.glmm)-logLik(m0)), df=1, lower.tail=FALSE)/2

# Here the random effects are highly significant, so we stay at GLMM.
# The problem is that the AIC-based backward selection cannot be used with stepAIC.

# p-value based backward selection
Anova(m.full.glmm)
Anova(update(m.full.glmm, ~.-n70))
Anova(update(m.full.glmm, ~.-n70-broadl))
Anova(update(m.full.glmm, ~.-n70-broadl-ageSD))
Anova(update(m.full.glmm, ~.-n70-broadl-ageSD-young))
Anova(update(m.full.glmm, ~.-n70-broadl-ageSD-young-age))

# Here we have both old broadleave trees and shannon diversity significant.
m <- glmer(rich_sp ~ bl70+sh+(1|ID), data = df, family = poisson)
summary(m)
Anova(m)

nd <- expand.grid(bl70=(c(0,20)-mean(df$NumBroadlLT70DBH))/sd(df$NumBroadlLT70DBH), 
                  sh=(c(0,1)-mean(df$H_Trees))/sd(df$H_Trees), 
                  ID=unique(df$ID))
(pred <- cbind(nd, fit=predict(m, type="response", newdata=nd)) %>%
    group_by(bl70, sh) %>%
    summarize(predicted=mean(fit)))


# Checking overdispersion
library(performance)
check_overdispersion(m)

# We're good

# Model performance
r.squaredGLMM(m)

# We should report the value computed using trigamma function.
# Not great, but OK. Beware of driving too strong conclusions.

# Effect plots:
nd <- data.frame(
  bl70 = seq(min(df$bl70), max(df$bl70), length.out = 100),
  sh = mean(df$sh)
)
mm <- model.matrix(~bl70+sh, nd)
y <- mm %*% fixef(m)
pvar <- diag(mm %*% tcrossprod(vcov(m), mm))
nd$rich_sp <- exp(y)
nd$upr <- exp(y+1.96*sqrt(pvar))
nd$lwr <- exp(y-1.96*sqrt(pvar))
ggplot(nd, aes(y=rich_sp, x=bl70*sd(df$NumBroadlLT70DBH)+mean(df$NumBroadlLT70DBH))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df) +
  labs(x="Number of broadleaved trees over 70 cm",
       y="Specialist species richness") +
  ggsave("Specialists_vs_bl70_glm.png", dpi=600)

nd <- data.frame(
  sh = seq(min(df$sh), max(df$sh), length.out = 100),
  bl70 = mean(df$bl70)
)
mm <- model.matrix(~bl70+sh, nd)
y <- mm %*% fixef(m)
pvar <- diag(mm %*% tcrossprod(vcov(m), mm))
nd$rich_sp <- exp(y)
nd$upr <- exp(y+1.96*sqrt(pvar))
nd$lwr <- exp(y-1.96*sqrt(pvar))
ggplot(nd, aes(y=rich_sp, x=sh*sd(df$H_Trees)+mean(df$H_Trees))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df) +
  labs(x="Shannon diversity of trees",
       y="Specialist species richness") +
  ggsave("Specialists_vs_Shannon_glm.png", dpi=600)

# Generalists ----------------------------------------------------------------

# Full model
m.full.glmm <- glmer(rich_gen ~ broadl+young+n70+bl70+age+ageSD+sh+(1|ID), 
                     data = df, family = poisson, nAGQ = 1)
summary(m.full.glmm)
m0 <- glm(rich_gen ~ broadl+young+n70+bl70+age+ageSD+sh, data = df, family = poisson)
pchisq(2*(logLik(m.full.glmm)-logLik(m0)), df=1, lower.tail=FALSE)/2

# Here the random effects are weakly significant, still a reason to stay at GLMM.
# The problem is that the AIC-based backward selection cannot be used with stepAIC.

# p-value based backward selection
Anova(m.full.glmm)
Anova(update(m.full.glmm, ~.-young))
Anova(update(m.full.glmm, ~.-young-age))
Anova(update(m.full.glmm, ~.-young-age-n70))
Anova(update(m.full.glmm, ~.-young-age-n70-ageSD))
Anova(update(m.full.glmm, ~.-young-age-n70-ageSD-broadl))

# Here we have both old broadleave trees and shannon diversity significant.
m <- glmer(rich_gen ~ bl70+sh+(1|ID), data = df, family = poisson)
summary(m)
Anova(m)

nd <- expand.grid(bl70=(c(0,10,20)-mean(df$NumBroadlLT70DBH))/sd(df$NumBroadlLT70DBH), 
                  sh=mean(df$sh), 
                  ID=unique(df$ID))
(pred <- cbind(nd, fit=predict(m, type="response", newdata=nd)) %>%
    group_by(bl70) %>%
    summarize(predicted=mean(fit)))

nd <- expand.grid(sh=(c(0,1)-mean(df$H_Trees))/sd(df$H_Trees), 
                  bl70=mean(df$bl70), 
                  ID=unique(df$ID))
(pred <- cbind(nd, fit=predict(m, type="response", newdata=nd)) %>%
    group_by(sh) %>%
    summarize(predicted=mean(fit)))



# Checking overdispersion
check_overdispersion(m)

# We're good

# Model performance
r.squaredGLMM(m)

# We should report the value computed using trigamma function.
# Not great, but OK. Beware of driving too strong conclusions.

# Effect plots:
nd <- data.frame(
  bl70 = seq(min(df$bl70), max(df$bl70), length.out = 100),
  sh = mean(df$sh)
)
mm <- model.matrix(~bl70+sh, nd)
y <- mm %*% fixef(m)
pvar <- diag(mm %*% tcrossprod(vcov(m), mm))
nd$rich_gen <- exp(y)
nd$upr <- exp(y+1.96*sqrt(pvar))
nd$lwr <- exp(y-1.96*sqrt(pvar))
ggplot(nd, aes(y=rich_gen, x=bl70*sd(df$NumBroadlLT70DBH)+mean(df$NumBroadlLT70DBH))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df) +
  labs(x="Number of broadleaved trees over 70 cm",
       y="Generalist species richness") +
  ggsave("Generalists_vs_bl70_glmm.png", dpi=600)

nd <- data.frame(
  sh = seq(min(df$sh), max(df$sh), length.out = 100),
  bl70 = mean(df$bl70)
)
mm <- model.matrix(~bl70+sh, nd)
y <- mm %*% fixef(m)
pvar <- diag(mm %*% tcrossprod(vcov(m), mm))
nd$rich_gen <- exp(y)
nd$upr <- exp(y+1.96*sqrt(pvar))
nd$lwr <- exp(y-1.96*sqrt(pvar))
ggplot(nd, aes(y=rich_gen, x=sh*sd(df$H_Trees)+mean(df$H_Trees))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_jitter(data=df, height = 0.01, width = 0.01, alpha=.3) +
  labs(x="Shannon diversity of trees",
       y="Generalist species richness") +
  ggsave("Generalists_vs_Shannon_glmm.png", dpi=600)
