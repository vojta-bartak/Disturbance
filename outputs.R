library(tidyverse)
library(multcomp)
library(multcompView)

load("models.RData")
m.rich <- models[names(models)[grepl("rich", names(models))]]
m.abund <- models[names(models)[grepl("abund", names(models))]]
df <- read.table("data_disturbances.csv", header=T, sep=";") %>% 
  mutate(Biotope_type = factor(Biotope_type, levels = c("SSD", "LSD", "ELT", "NDF")))

#m <- models$m.tot.rich

# tables of coefficients
m.rich %>%
  lapply(function(m){
    summary(m)$coef %>%
      round(4) %>%
      as.data.frame() %>%
      "["(,-3) %>%
      rownames_to_column("term")
  }) %>%
  reduce(left_join, by="term") %>%
  write.table("coef.rich.csv", row.names = F, sep=",")

m.abund %>%
  lapply(function(m){
    summary(m)$coef %>%
      round(4) %>%
      as.data.frame() %>%
      "["(,-3) %>%
      rownames_to_column("term")
  }) %>%
  reduce(left_join, by="term") %>%
  write.table("coef.abund.csv", row.names = F, sep=",")

# anova tables
m.rich %>%
  lapply(function(m){
    car::Anova(m)[1:2,3:4] %>%
      round(4) %>%
      as.data.frame() %>%
      rownames_to_column("term")
  }) %>%
  reduce(left_join, by="term") %>%
  rbind(m.abund %>%
          lapply(function(m){
            car::Anova(m)[1:2,3:4] %>%
              round(4) %>%
              as.data.frame() %>%
              rownames_to_column("term")
          }) %>%
          reduce(left_join, by="term")) %>%
  write.table("anova.csv", row.names = F, sep=",")

# biotope types --------------------------------------------------------------------------------------
nd <- data.frame(Biotope_type=unique(df$Biotope_type),
                 Elev=mean(df$Elev))
models %>%
  names %>%
  lapply(function(nm){
    m <- models[[nm]]
    pr <- predict(m, newdata = nd, se=T)
    smcp <- summary(glht(m, linfct = mcp(Biotope_type="Tukey")))
    pvs <- smcp$test$pvalues
    names(pvs) <- str_replace_all(names(smcp$test$coefficients), " ", "")
    ltrs <- data.frame(multcompLetters(pvs)$Letters) %>%
      set_names("letter") %>%
      rownames_to_column("Biotope_type")
    ndd <- nd %>%
      mutate(fit=pr$fit, lwr=pr$fit - 1.96*pr$se.fit, upr=pr$fit + 1.96*pr$se.fit, model=nm) %>%
      left_join(ltrs)
    ndd
  }) %>%
  bind_rows() %>%
  mutate(group = factor(ifelse(grepl(".rich", model), "Richness", "Abundance"), levels = c("Richness", "Abundance")),
         response = sub("m.", "", sub(".abund","",sub(c(".rich"),"",model))) %>%
           factor(levels=c("tot","spec","gen","can","cav","gr"),
                  labels=c("All species", "Specialists", "Generalists", "Canopy nesters", 
                           "Cavity nesters", "Ground/Shrub nesters")),
         Biotope_type = factor(Biotope_type, levels = c("SSD", "LSD", "ELT", "NDF"))) %>%
  ggplot(aes(x=Biotope_type, y=fit)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2) +
  geom_text(aes(y=lwr - 0.5, label = letter), size=2) +
  facet_grid(group~response) +
  labs(x="Disturbance class", y="Species abundance / richness") +
  theme_bw()
ggsave("habitat_models.png", dpi=600, height = 16, width = 22, units = "cm")


# multiple comparisons ----------------------------------------------------------------------------------
lapply(m.rich, function(m){
  t <- summary(glht(m, linfct = mcp(Biotope_type="Tukey")))$test
  data.frame(
    pair = names(t$coefficients),
    diff = round(t$coefficients,3),
    se = round(t$sigma,3),
    p = round(t$pvalues,3)
  )
}) %>%
  reduce(left_join, by="pair") %>%
  rbind(lapply(m.abund, function(m){
    t <- summary(glht(m, linfct = mcp(Biotope_type="Tukey")))$test
    data.frame(
      pair = names(t$coefficients),
      diff = round(t$coefficients,3),
      se = round(t$sigma,3),
      p = round(t$pvalues,3)
    )
  }) %>%
    reduce(left_join, by="pair")) %>%
  write.table("multcomp.csv", row.names = F, sep = ",")
names(m.rich)

# Diagnostic plots ----------------------------------------------------------
png("diag.plot.m.tot.rich.png", res=300, width = 18, height = 18, units = "cm")
par(mfrow=c(2,2))
plot(m.tot.rich, which=c(1,2,3,5))
dev.off()
par(mfrow=c(1,1))

diag.plot <- function(model.name){
  png(paste("diag",model.name,"png",sep="."), res=300, width = 18, height = 18, units = "cm")
  par(mfrow=c(2,2))
  plot(models[[model.name]], which=c(1,2,3,5))
  dev.off()
  par(mfrow=c(1,1))
}
for (mn in names(models)) diag.plot(mn)
