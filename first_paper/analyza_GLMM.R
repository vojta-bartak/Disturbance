# Full model
m.full.glmm <- glmer(rich ~ broadl+young+n70+bl70+age+ageSD+sh+(1|ID), 
                     data = df, family = poisson, nAGQ = 1)
summary(m.full.glmm)
m0 <- glm(rich ~ broadl+young+n70+bl70+age+ageSD+sh, data = df, family = poisson)
pchisq(2*(logLik(m.full.glmm)-logLik(m0)), df=1, lower.tail=FALSE)/2

# Here the random effects are highly significant, so we stay at GLMM.
# The problem is that the AIC-based backward selection cannot be used with stepAIC.

# p-value based backward selection
Anova(m.full.glmm)
Anova(update(m.full.glmm, ~.-young))
Anova(update(m.full.glmm, ~.-young-n70))
Anova(update(m.full.glmm, ~.-young-n70-age))
Anova(update(m.full.glmm, ~.-young-n70-age-broadl))
Anova(update(m.full.glmm, ~.-young-n70-age-broadl-ageSD))

# Here we have both old broadleave trees and shannon diversity significant.
m <- glmer(rich ~ bl70+sh+(1|ID), data = df, family = poisson)
summary(m)
Anova(m)

# Checking overdispersion
library(performance)
check_overdispersion(m)

# We're good

# Model performance
r.squaredGLMM(m)
rsq(m)

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
nd$rich <- exp(y)
nd$upr <- exp(y+1.96*sqrt(pvar))
nd$lwr <- exp(y-1.96*sqrt(pvar))
ggplot(nd, aes(y=rich, x=bl70*sd(df$NumBroadlLT70DBH)+mean(df$NumBroadlLT70DBH))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df) +
  labs(x="Number of broadleaved trees over 70 cm",
       y="Specialist species richness") 

nd <- data.frame(
  sh = seq(min(df$sh), max(df$sh), length.out = 100),
  bl70 = mean(df$bl70)
)
mm <- model.matrix(~bl70+sh, nd)
y <- mm %*% fixef(m)
pvar <- diag(mm %*% tcrossprod(vcov(m), mm))
nd$rich <- exp(y)
nd$upr <- exp(y+1.96*sqrt(pvar))
nd$lwr <- exp(y-1.96*sqrt(pvar))
ggplot(nd, aes(y=rich, x=sh*sd(df$H_Trees)+mean(df$H_Trees))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df) +
  labs(x="Shannon diversity of trees",
       y="Specialist species richness") +
  ggsave("Specialists_vs_Shannon_glm.png", dpi=600)

# Numerical predictions
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


df %>%
  dplyr::select(ID,rich,rich_sp,rich_gen) %>%
  tidyr::pivot_longer(2:4, names_to = "species", values_to = "richness") %>%
  ggplot(aes(y=log(richness), x=ID)) +
  geom_boxplot() +
  facet_wrap(~species)

df$resid.glm <- resid(glm(rich ~ bl70+sh, data = df, family = poisson))
df$resid.glm_sp <- resid(glm(rich_sp ~ bl70+sh, data = df, family = poisson))
df$resid.glm_gen <- resid(glm(rich_gen ~ bl70+sh, data = df, family = poisson))
df %>%
  dplyr::select(ID,resid.glm,resid.glm_sp,resid.glm_gen) %>%
  tidyr::pivot_longer(2:4, names_to = "species", values_to = "richness") %>%
  ggplot(aes(y=richness, x=ID)) +
  geom_boxplot() +
  facet_wrap(~species)

library(rstanarm)
m.b <- stan_glmer(rich ~ broadl+young+n70+bl70+age+ageSD+sh+(1|ID), 
                  data = df, family = poisson)
summary(m.b)

library(shinystan)
launch_shinystan(m.b, ppd = FALSE)

y_rep <- posterior_predict(m.b) %>%
  t() %>%
  as.data.frame() %>%
  mutate(ID_sampling_plot=df$ID_sampling_plot) %>%
  tidyr::pivot_longer(1:4000, names_to="nsim", values_to = "simulation") %>%
  left_join(df)
ggplot(y_rep, aes())


df %>%
  left_join()