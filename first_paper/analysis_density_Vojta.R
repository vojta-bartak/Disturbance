library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(MuMIn)
library(performance)
library(rsq)
library(multcomp)

df <- read.table("data_birds_trees2.txt", sep="\t", header=T) %>%
  mutate(
    ID = ID_study_plot,
    rich = Total_num_of_species,
    rich_sp = Specialist_species,
    rich_gen = Generalist_species,
    cat70 = as.factor(NumBroadlLT70DBH_ha),
    dens = DensityNumBroadlLT70DBH_ha,
    res = factor(cat70=="R", levels=c(TRUE,FALSE), 
                 labels=c("Reservation","Production Forest"))
  )

# Density as continuous predictor
ggplot(df, aes(x=dens, y=rich)) +
  geom_point(aes(color=res), alpha=.5) +
  geom_smooth() +
  theme(legend.title = element_blank()) +
  labs(x="Density of veteran trees","Species richness")

m.cont <- glmer(rich~dens+(1|ID), data=df, family=poisson)
summary(m.cont)
m.cont <- glm(rich~dens, data=df, family=poisson)
check_overdispersion(m.cont)
Anova(m.cont)

m.cont2 <- glm(rich~poly(dens, 2), data=df, family=poisson)
anova(m.cont, m.cont2, test = "LR")
AIC(m.cont, m.cont2)

check_overdispersion(m.cont2)
Anova(m.cont2)
rsq(m.cont2)

nd <- data.frame(dens = seq(min(df$dens), max(df$dens), length.out=100))
nd <- cbind(nd, predict(m.cont2, type="link", se=TRUE, newdata=nd))
nd$rich <- exp(nd$fit)
nd$upr <- exp(nd$fit + 1.96*nd$se.fit)
nd$lwr <- exp(nd$fit - 1.96*nd$se.fit)
ggplot(nd, aes(x=dens, y=rich)) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df, aes(color=res), alpha=.5) +
  theme(legend.title = element_blank()) +
  labs(x="Density of veteran trees",y="Species richness") +
  ggsave("Richness_vs_density.png", dpi=600)

# Density as discrete predictor
ggplot(df, aes(x=cat70, y=rich)) +
  geom_boxplot()

m.disc <- glmer(rich~cat70+(1|ID), data=df, family=poisson)
m.disc <- glm(rich~cat70, data=df, family=poisson)

check_overdispersion(m.disc)
summary(m.disc)
Anova(m.disc)
rsq(m.disc)

nd <- data.frame(cat70=factor(levels(df$cat70), levels=levels(df$cat70)))
nd <- cbind(nd, predict(m.disc, type="link", se=TRUE, newdata=nd))
nd$rich <- exp(nd$fit)
nd$upr <- exp(nd$fit + 1.96*nd$se.fit)
nd$lwr <- exp(nd$fit - 1.96*nd$se.fit)
ggplot(nd, aes(x=cat70, y=rich)) +
  geom_jitter(data=df, alpha=.3, width = .1, height = .1) +
  geom_point(color="red") +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, color="red") +
  labs(x="Density of veteran trees",y="Species richness") +
  ggsave("Richness_vs_density_discrete.png", dpi=600)

mult.comp <- glht(m.disc, linfct=mcp(cat70="Tukey"))
summary(mult.comp)

# Specialists -----------------------------------------------------
m.sp <- glmer(rich_sp~cat70+(1|ID), data=df, family=poisson)
check_overdispersion(m.sp)
summary(m.sp)
Anova(m.sp)
r.squaredGLMM(m.sp)

# no significance...

# Generalists -----------------------------------------------------
m.gen <- glmer(rich_gen~cat70+(1|ID), data=df, family=poisson)
check_overdispersion(m.gen)
summary(m.gen)
Anova(m.gen)
r.squaredGLMM(m.gen)

# it'c clear that the whole thing is because of generalists..

nd <- data.frame(cat70=factor(levels(df$cat70), levels=levels(df$cat70)))
mm <- model.matrix(~cat70, nd)
y <- mm %*% fixef(m.gen)
pvar <- diag(mm %*% tcrossprod(vcov(m.gen), mm))
nd$rich_gen <- exp(y)
nd$upr <- exp(y+1.96*sqrt(pvar))
nd$lwr <- exp(y-1.96*sqrt(pvar))
ggplot(nd, aes(x=cat70, y=rich_gen)) +
  geom_jitter(data=df, alpha=.3, width = .1, height = .1) +
  geom_point(color="red") +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, color="red") +
  labs(x="Density of veteran trees",y="Generalists bird species richness") +
  ggsave("Richness_vs_density_discrete_generalists.png", dpi=600)

mult.comp <- glht(m.gen, linfct=mcp(cat70="Tukey"))
summary(mult.comp)


