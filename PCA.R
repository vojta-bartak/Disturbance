library(factoextra)
library(tidyverse)
library(ranger)
library(caret)
library(gridExtra)

# data ------------------------------------------------------------------------------------------------
df <- read.table("data.txt", header=T) %>%
  mutate(
    Subtyp_final = as.factor(Subtyp_final),
    pomer = deadw_ha1/(Total_LT_ha1 + deadw_ha1)
  ) %>%
  rename(LiveTrees = "Total_LT_ha1",
         Complexity = "Struct_Can",
         Understory = "Underg_.",
         BioLegacy = "BiologLegac1",
         Windfalls = "Windfall1")
predictors <- c("LiveTrees", "Complexity", "Understory", "Elev", 
                "BioLegacy", "Windfalls")

# PCA -------------------------------------------------------------------------------------------------
pca <- prcomp(df[,predictors], scale=T)
df <- cbind(df, pca$x)
fviz_pca_biplot(pca, geom.ind = "point", col.ind = df$Subtyp_final, addEllipses = T) +
  theme_bw()
ggsave("pca_1-2_ellipses.png", height = 14, width = 18, units = "cm", dpi = 300)

p12 <- fviz_pca_biplot(pca, geom.ind = "point", 
                col.ind = factor(df$Subtyp_final, levels=c("MD", "VD", "ZPZ", "ZPS"), 
                                 labels=c("SSD", "LSD", "NDF", "ELT"))) +
  theme_bw() +
  labs(title="", x="PC1 (43.6%)", y="PC2 (24.8%)", 
       color="Habitat type", shape="Habitat type") +
  lims(x=c(-5, 2.8)) +
  theme(legend.position = "bottom")
p34 <- fviz_pca_biplot(pca, geom.ind = "point", axes = c(3,4), 
                       col.ind = factor(df$Subtyp_final, levels=c("MD", "VD", "ZPZ", "ZPS"), 
                                        labels=c("SSD", "LSD", "NDF", "ELT"))) +
  theme_bw() +
  labs(title="", x="PC3 (16.0%)", y="PC4 (9.3%)", 
       color="Habitat type", shape="Habitat type") +
  theme(legend.position = "bottom")

library(cowplot)
ggsave(plot=plot_grid(p12,p34), filename = "pca.png", dpi=300, width = 26, height = 14, units = "cm")

p34

ggsave("pca_1-2.png", height = 14, width = 18, units = "cm", dpi = 300)

fviz_pca_biplot(pca, axes = c(3,4), geom.ind = "point", col.ind = df$Subtyp_final, addEllipses = T) +
  theme_bw()
ggsave("pca_3-4_ellipses.png", height = 14, width = 18, units = "cm", dpi = 300)
fviz_pca_biplot(pca, axes = c(3,4), geom.ind = "point", col.ind = df$Subtyp_final, addEllipses = F) +
  theme_bw()
ggsave("pca_3-4.png", height = 14, width = 18, units = "cm", dpi = 300)

# Classification -----------------------------------------------------------------------------------
rf <- ranger(Subtyp_final~., data = df[,c("Subtyp_final",predictors)], 
             num.trees = 500,
             mtry = 3,
             min.node.size = 1,
             max.depth = NULL,
             write.forest = TRUE,
             importance = "impurity_corrected")
rf <- ranger(Subtyp_final~., data = df[,c("Subtyp_final",predictors)], 
             num.trees = 500,
             importance = "impurity")
rf$predictions
(cm1 <- confusionMatrix(rf$predictions, df$Subtyp_final))
rf$variable.importance
pvi1 <- rf$variable.importance %>%
  as.data.frame %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable") %>%
  arrange(desc(importance)) %>%
  mutate(variable = factor(variable, levels = unique(variable)),
         importance = 100*importance/sum(.$importance)) %>%
  ggplot(aes(x=variable, y=importance)) +
  geom_col() +
  labs(y="Variable importance (%)", x="") +
  theme_bw()

# PCA-based classification ----------------------------------------------------------------------------
dd <- data.frame(typ=df$Subtyp_final, pca$x)
rf.pca <- ranger(typ~., data = dd, 
             num.trees = 500,
             mtry = 3,
             min.node.size = 5,
             max.depth = NULL,
             write.forest = TRUE,
             importance = "impurity_corrected")
rf.pca <- ranger(typ~., data = dd, 
                 num.trees = 500,
                 importance = "impurity")
rf.pca
(cm2 <- confusionMatrix(rf.pca$predictions, dd$typ))
rf.pca$variable.importance
pvi2 <- rf.pca$variable.importance %>%
  as.data.frame %>%
  rename("importance" = ".") %>%
  rownames_to_column("variable") %>%
  arrange(desc(importance)) %>%
  mutate(variable = factor(variable, levels = unique(variable)),
         importance = 100*importance/sum(.$importance)) %>%
  ggplot(aes(x=variable, y=importance)) +
  geom_col() +
  labs(y="Variable importance (%)", x="") +
  theme_bw()

ggsave(plot=plot_grid(pvi1, pvi2), filename = "varimp.png", dpi=300, height = 12, width = 22, units = "cm")

pca %>% get_eig %>% rownames_to_column("variable") %>% ggplot(aes(x=variable, y=variance.percent)) + geom_col()

cm1$byClass %>% 
  t %>% 
  "["(c(1,2,8,9,11),) %>%
  cbind(cm2$byClass %>% 
          t %>% 
          "["(c(1,2,8,9,11),)) %>%
  round(3) %>%
  as.data.frame() %>%
  rownames_to_column("metric") %>%
  write.table("confusion.matrix.csv", row.names = F, sep=",")

