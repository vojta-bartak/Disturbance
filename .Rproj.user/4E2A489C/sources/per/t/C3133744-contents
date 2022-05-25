library(tidyverse)
library(car)
library(scales)

df <- read.table("data_disturbances.csv", header=T, sep=";") %>% 
  mutate(Biotope_type = factor(Biotope_type, levels = c("SSD", "LSD", "ELT", "NDF")))

responses <- c("Total_Rich", "Total_Abun", "Canopy_Rich", "Canopy_Abun", "Cavity_Rich", "Cavity_Abun", 
               "Ground_Shrub_Rich", "Ground_Shrub_Abun", "RedList_Rich", "RedList_Abun", "Rarity_index",
               "Generalist_Rich", "Generalist_Abun", "Specialist_Rich", "Specialist_Abun")

predictors <- c("LiveTrees", "Complexity", "Understory", "Elev", "BioLegacy", "Windfalls")

# collinearity check
vif(lm(paste(responses[3], paste(paste("poly(", predictors, ",2)", sep=""), collapse = "+"), sep="~"), data=df))
corrplot::corrplot(cor((df[,predictors])), method="number")
predictors <- c("LiveTrees", "Complexity", "Understory", "Elev")

# fitting models -----------------------------------------------------------------------------------------------------
models <- lapply(responses, function(resp){
  step(lm(paste(resp, paste(paste("poly(", predictors, ",2)", sep=""), collapse = "+"), sep="~"), data=df), 
       direction = "backward", trace=FALSE)
}) %>%
  set_names(responses)

# r squared ----------------------------------------------------------------------------------------------------------
r2 <- sapply(models, function(m) summary(m)$r.squared)
data.frame(r2)

# coefficients -------------------------------------------------------------------------------------------------------
coefs <- do.call(rbind, lapply(1:length(responses), function(i) {
  coef <- as.data.frame(summary(models[[i]])$coefficients)
  coef$response <- responses[i]
  coef
}))

# effects ------------------------------------------------------------------------------------------------------------
ndfs <- do.call(rbind, lapply(1:length(responses), function(i){
  terms <- sapply(attr(models[[i]]$terms, "term.labels"), function(term) {
    ifelse(grepl("poly", term), strsplit(substr(term, 6, 1000), ",")[[1]][1], term)
  })
  ndf <- do.call(rbind, lapply(terms, function(term){
    nd <- data.frame(x=seq(min(df[,term]), max(df[,term]), l=100))
    nd[,term] <- nd$x
    for (trm in terms[terms!=term]) nd[,trm] <- mean(df[,trm])
    nd$var <- term
    nd$term <- names(terms[terms==term])
    nd
  }))
  if (is.null(ndf)) {
    ndff <- data.frame(fit=numeric(0), se.fit=numeric(0), df=numeric(0), residual.scale=numeric(0), 
                       x=numeric(0), var=numeric(0), response=numeric(0), significance=numeric(0))
    } else {
      ndff <- as.data.frame(predict(models[[i]], se=T, newdata=ndf)) %>%
        mutate(
          x = ndf$x,
          var = ndf$var,
          response = responses[i],
          term = ndf$term
        ) %>% 
        left_join(models[[i]] %>% 
                    drop1(test="F") %>% 
                    data.frame %>% 
                    rownames_to_column("term") %>% 
                    "["(2:nrow(.),c("term", "Pr..F."))) %>% 
        rename("Significance"="Pr..F.")
    }
  ndff
})) %>%
  mutate(
    type=ifelse(grepl("Rich", response),"Richness","Abundance"),
    group=sapply(strsplit(response, "_"), function(.) .[1]),
    var = factor(var, levels=c("Elev", "Complexity", "LiveTrees", "Understory", "Windfalls"),
                 labels = c("Elevation\n(m a.s.l.)", "Structural complexity\n(score)", "Live trees density\n(trees/ha)", 
                            "Understory\n(%)", "Windfalls")),
    group = factor(group, levels=c("Total","Canopy","Cavity","Ground","Generalist","Specialist","RedList","Rarity"),
                   labels = c("All species", "Canopy nesters", "Cavity nesters", "Ground/Shrub nesters", 
                              "Generalists", "Specialists", "Red List species", "Rarity index"))
  )

# all predictors  
ggplot(ndfs, aes(x=x, y=fit, color=group, fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3, color=NA) +
  facet_grid(type~var, scales="free") +
  scale_fill_manual(values=c(hue_pal()(6), "grey40", "grey80")) +
  scale_color_manual(values=c(hue_pal()(6), "grey40", "grey80")) +
  labs(x="", y="Species richness / abundance", color="", fill="") +
  theme_bw()

# Generalists, specialists
ggplot(ndfs %>% filter(group %in% c("Generalists", "Specialists")) %>% 
         mutate(type = factor(type, levels = c("Richness", "Abundance"))), 
       aes(x=x, y=fit, color=group, fill=group)) + #, lty=Significance>.05)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3, color=NA) +
  scale_color_manual(values=c("#0072B2", "#E69F00")) +
  scale_fill_manual(values=c("#0072B2", "#E69F00")) +
  facet_grid(type~var, scales="free") +
  labs(x="", y="Species richness / abundance", color="", fill="", lty="") +
  theme_bw()
ggsave("figR2_genspec.png", height = 16, width = 22, units = "cm", dpi=300)


ggplot(ndfs %>% filter(group %in% c("All species", "Canopy nesters", "Cavity nesters", "Ground/Shrub nesters") & 
                         Significance<.05) %>% 
         mutate(type = factor(type, levels = c("Richness", "Abundance"))), 
       aes(x=x, y=fit, color=group, fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3, color=NA) +
  #  scale_size_manual(values = c(1,2)) +
  facet_grid(type~var, scales="free") +
  labs(x="", y="Species abundance / richness", color="", fill="", lty="") +
  theme_bw()
ggsave("figR2_genspec_significant_only.png", height = 16, width = 22, units = "cm", dpi=300)

ggplot(ndfs %>% filter(group %in% c("All species", "Canopy nesters", "Cavity nesters", "Ground/Shrub nesters")) %>% 
         mutate(type = factor(type, levels = c("Richness", "Abundance"))), 
       aes(x=x, y=fit, color=group, fill=group, lty=Significance>.05)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), alpha=.3, color=NA) +
#  scale_size_manual(values = c(1,2)) +
  scale_linetype_manual(values=c(1,2), labels=c("Significant", "Non-significant")) +
  facet_grid(type~var, scales="free") +
  labs(x="", y="Species richness / abundance", color="", fill="", lty="") +
  theme_bw()
ggsave("figR2_nesting.png", height = 16, width = 22, units = "cm", dpi=300)

# anova tables -------------------------------------------------------------------------------------------------------
anovas <- do.call(rbind, lapply(1:length(responses), function(i) {
  Anova(models[[i]]) %>%
    round(4) %>%
    as.data.frame() %>%
    set_names(c("Sum.sq", "Df", "F.value", "p.value")) %>%
    rownames_to_column("predictor") %>%
    filter(!predictor %in% c("Residuals", "(Intercept)")) %>%
    mutate(
      response = responses[i]
    )
}))

lapply(responses[grepl("_Rich", responses)], function(i) {
  Anova(models[[i]]) %>%
    round(4) %>%
    as.data.frame() %>%
    set_names(c("Sum.sq", "Df", "F.value", "p.value")) %>%
    dplyr::select("F.value", "p.value") %>%
    add_rownames("predictor") %>%
    filter(!predictor %in% c("Residuals", "(Intercept)"))
}) %>%
  reduce(full_join, by="predictor") %>% 
  rbind(lapply(responses[grepl("_Abun", responses)], function(i) {
  Anova(models[[i]]) %>%
    round(4) %>%
    as.data.frame() %>%
    set_names(c("Sum.sq", "Df", "F.value", "p.value")) %>%
    dplyr::select("F.value", "p.value") %>%
    add_rownames("predictor") %>%
    filter(!predictor %in% c("Residuals", "(Intercept)"))
}) %>%
  reduce(full_join, by="predictor")
) %>% 
  write.table("anova.habitat.characteristics.csv", row.names = F, sep=",")

anovas.p <- anovas%>%
  dplyr::select(c("predictor","p.value","response")) %>%
  pivot_wider(names_from = predictor, values_from = p.value)
View(anovas.p)

