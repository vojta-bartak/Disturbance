library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(performance)
library(rsq)

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

m <- glmer(rich ~ bl70+sh+(1|ID), data = df, family = poisson)
m.sp <- glmer(rich_sp ~ bl70+sh+(1|ID), data = df, family = poisson)
m.gen <- glmer(rich_gen ~ bl70+sh+(1|ID), data = df, family = poisson)

nd <- data.frame(
  bl70 = seq(min(df$bl70), max(df$bl70), length.out = 100),
  sh = mean(df$sh)
)
mm <- model.matrix(~bl70+sh, nd)
y <- mm %*% fixef(m)
pvar <- diag(mm %*% tcrossprod(vcov(m), mm))
y.sp <- mm %*% fixef(m.sp)
pvar.sp <- diag(mm %*% tcrossprod(vcov(m.sp), mm))
y.gen <- mm %*% fixef(m.gen)
pvar.gen <- diag(mm %*% tcrossprod(vcov(m.gen), mm))
df.plot <- rbind(
  data.frame(bl70=nd$bl70, sh=nd$sh, rich=exp(y), 
             upr=exp(y+1.96*sqrt(pvar)), 
             lwr=exp(y-1.96*sqrt(pvar)),
             response="All bird species"),
  data.frame(bl70=nd$bl70, sh=nd$sh, rich=exp(y.sp), 
             upr=exp(y.sp+1.96*sqrt(pvar.sp)), 
             lwr=exp(y.sp-1.96*sqrt(pvar.sp)),
             response="Specialists"),
  data.frame(bl70=nd$bl70, sh=nd$sh, rich=exp(y.gen), 
             upr=exp(y.gen+1.96*sqrt(pvar.gen)), 
             lwr=exp(y.gen-1.96*sqrt(pvar.gen)),
             response="Generalists")
)
ggplot(df.plot %>% filter(sh==sh[10]), aes(y=rich, x=bl70*sd(df$NumBroadlLT70DBH)+mean(df$NumBroadlLT70DBH))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_jitter(data=df %>% 
                dplyr::select(bl70, rich, rich_sp, rich_gen) %>%
                rename("All bird species"=rich,
                       "Specialists"=rich_sp,
                       "Generalists"=rich_gen) %>%
                tidyr::pivot_longer(2:4, names_to = "response", values_to = "rich"),
              width = 0.01, height = 0.01, alpha = 0.2) +
  facet_wrap(~response, nrow=2) +
  labs(x="Number of broadleaved trees over 70 cm",
       y="Species richness") +
  ggsave("Veterans_vs_richness.png", dpi=600)


nd <- data.frame(
  bl70 = seq(min(df$bl70), max(df$bl70), length.out = 100),
  sh = seq(min(df$sh), max(df$sh), length.out = 100)
)

ggplot(df.plot, aes(y=rich, x=sh*sd(df$H_Trees)+mean(df$H_Trees))) +
  geom_line() +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=.3) +
  geom_point(data=df) +
  labs(x="Shannon diversity of trees",
       y="Specialist species richness")



