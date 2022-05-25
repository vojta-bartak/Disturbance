library(factoextra)
library(tidyverse)
library(ranger)
library(caret)
library(gridExtra)
library(cowplot)

# data ------------------------------------------------------------------------------------------------
df <- read.table("data_disturbances.csv", header=T, sep=";") %>% 
  mutate(Biotope_type = factor(Biotope_type, levels = c("SSD", "LSD", "ELT", "NDF")))
predictors <- c("LiveTrees", "Complexity", "Understory", "Elev", 
                "BioLegacy", "Windfalls")

# PCA -------------------------------------------------------------------------------------------------
pca <- prcomp(df[,predictors], scale=T)
df <- cbind(df, pca$x)
fviz_pca_biplot(pca, geom.ind = "point", col.ind = df$Biotope_type, addEllipses = T) +
  theme_bw()
ggsave("pca_1-2_ellipses.png", height = 14, width = 18, units = "cm", dpi = 300)

p12 <- fviz_pca_biplot(pca, geom.ind = "point", col.ind = df$Biotope_type) +
  theme_bw() +
  labs(title="", x="PC1 (43.6%)", y="PC2 (24.8%)", 
       color="Habitat type", shape="Habitat type") +
  lims(x=c(-5, 2.8)) +
  theme(legend.position = "bottom")
p34 <- fviz_pca_biplot(pca, geom.ind = "point", axes = c(3,4), col.ind = df$Biotope_type) +
  theme_bw() +
  labs(title="", x="PC3 (16.0%)", y="PC4 (9.3%)", 
       color="Habitat type", shape="Habitat type") +
  theme(legend.position = "bottom")

ggsave(plot=cowplot::plot_grid(p12,p34), filename = "pca.png", dpi=300, width = 26, height = 14, units = "cm")


# Classification -----------------------------------------------------------------------------------
rf <- ranger(Biotope_type~., data = df[,c("Biotope_type",predictors)], 
             num.trees = 500,
             mtry = 3,
             min.node.size = 1,
             max.depth = NULL,
             write.forest = TRUE,
             importance = "impurity_corrected")
(cm1 <- confusionMatrix(rf$predictions, df$Biotope_type))
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
dd <- data.frame(typ=df$Biotope_type, pca$x)
rf.pca <- ranger(typ~., data = dd, 
             num.trees = 500,
             mtry = 3,
             min.node.size = 5,
             max.depth = NULL,
             write.forest = TRUE,
             importance = "impurity_corrected")
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

ggsave(plot=cowplot::plot_grid(pvi1, pvi2), filename = "varimp.png", dpi=300, height = 12, width = 22, units = "cm")

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

