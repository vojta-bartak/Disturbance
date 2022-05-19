library(tidyverse)
library(multcompView)

load("models.RData")
m.rich <- models[names(models)[grepl("rich", names(models))]]
m.abund <- models[names(models)[grepl("abund", names(models))]]

m <- models$m.tot.rich

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

summary(m)$coef

nd <- data.frame(Subtyp_final=levels(df$Subtyp_final), Elev=mean(df$Elev))

unique(unlist(strsplit(names(pvs), split=" - ", fixed=T)))



models %>%
  names %>%
  lapply(function(nm){
    m <- models[[nm]]
    pr <- predict(m, newdata = nd, se=T)
    smcp <- summary(glht(m, linfct = mcp(Subtyp_final="Tukey")))
    pvs <- smcp$test$pvalues
    names(pvs) <- str_replace_all(names(smcp$test$coefficients), " ", "")
    ltrs <- data.frame(multcompLetters(pvs)$Letters) %>%
      set_names("letter") %>%
      rownames_to_column("Subtyp_final")
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
         habitat = factor(Subtyp_final, levels = c("MD", "VD", "ZPS", "ZPZ"),
                          labels = c("SSD", "LSD", "ELT", "NDF"))) %>%
  ggplot(aes(x=habitat, y=fit)) +
  geom_point() +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2) +
  geom_text(aes(y=lwr - 0.5, label = letter), size=2) +
  facet_grid(group~response) +
  labs(x="Habitat type", y="Species abundance / richness") +
  theme_bw()
ggsave("habitat_models.png", dpi=600, height = 16, width = 22, units = "cm")




# multiple comparisons ----------------------------------------------------------------------------------
m <- models$m.tot.rich
Anova(m)
library(multcomp)
s <- summary(glht(models[[4]], linfct = mcp(Subtyp_final="Tukey")))
s$test$coefficients

lapply(m.rich, function(m){
  t <- summary(glht(m, linfct = mcp(Subtyp_final="Tukey")))$test
  data.frame(
    pair = names(t$coefficients),
    diff = round(t$coefficients,4),
    se = round(t$sigma,4),
    p = round(t$pvalues,4)
  )
}) %>%
  reduce(left_join, by="pair") %>%
  rbind(lapply(m.abund, function(m){
    t <- summary(glht(m, linfct = mcp(Subtyp_final="Tukey")))$test
    data.frame(
      pair = names(t$coefficients),
      diff = round(t$coefficients,4),
      se = round(t$sigma,4),
      p = round(t$pvalues,4)
    )
  }) %>%
    reduce(left_join, by="pair")) %>%
  write.table("multcomp.csv", row.names = F, sep = ",")
names(m.rich)
