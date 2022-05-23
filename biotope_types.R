library(tidyverse)
library(performance)
library(multcomp)

df <- read.table("data_disturbances.csv", header=T, sep=";") %>% 
  mutate(Biotope_type = factor(Biotope_type, levels = c("SSD", "LSD", "ELT", "NDF")))

# Total richness -----------------------------------------------------------------------------

## Linear model
m.tot.rich <- lm(Total_Rich ~ Biotope_type + Elev, data=df)
summary(m.tot.rich)
par(mfrow=c(2,2)); plot(m.tot.rich); par(mfrow=c(1,1))
ggplot(df, aes(x=Total_Rich)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.tot.rich, test="F")

## comparison with GLM
m2.tot.rich <- glm(Total_Rich ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.tot.rich)
check_overdispersion(m2.tot.rich)
AIC(m.tot.rich,m2.tot.rich)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type),
  Elev = mean(df$Elev)
)
df.new <- cbind(df.new, predict(m.tot.rich, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Total_Rich), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.tot.rich, linfct = mcp(Biotope_type = "Tukey")))

# Total abundance -----------------------------------------------------------------------------

## Linear model
m.tot.abund <- lm(Total_Abun ~ Biotope_type + Elev, data=df)
summary(m.tot.abund)
par(mfrow=c(2,2)); plot(m.tot.abund); par(mfrow=c(1,1))
ggplot(df, aes(x=Total_Abun)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.tot.abund, test="F")

## comparison with GLM
m2.tot.abund <- glm(Total_Abun ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.tot.abund)
check_overdispersion(m2.tot.abund)
AIC(m.tot.abund,m2.tot.abund)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type),
  Elev = mean(df$Elev)
)
df.new <- cbind(df.new, predict(m.tot.abund, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Total_Abun), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.tot.abund, linfct = mcp(Biotope_type = "Tukey")))


# Canopy richness -----------------------------------------------------------------------------

## Linear model
m.can.rich <- lm(Canopy_Rich ~ Biotope_type + Elev, data=df)
summary(m.can.rich)
par(mfrow=c(2,2)); plot(m.can.rich); par(mfrow=c(1,1))
ggplot(df, aes(x=Canopy_Rich)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.can.rich, test="F")

## comparison with GLM
m2.can.rich <- glm(Canopy_Rich ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.can.rich)
check_overdispersion(m2.can.rich)
AIC(m.can.rich,m2.can.rich)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type),
  Elev = mean(df$Elev)
)
df.new <- cbind(df.new, predict(m.can.rich, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Canopy_Rich), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.can.rich, linfct = mcp(Biotope_type = "Tukey")))

# Canopy abundance -----------------------------------------------------------------------------

## Linear model
m.can.abund <- lm(Canopy_Abun ~ Biotope_type + Elev, data=df)
summary(m.can.abund)
par(mfrow=c(2,2)); plot(m.can.abund); par(mfrow=c(1,1))
ggplot(df, aes(x=Canopy_Abun)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.can.abund, test="F")

## comparison with GLM
m2.can.abund <- glm(Canopy_Abun ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.can.abund)
check_overdispersion(m2.can.abund)
AIC(m.can.abund,m2.can.abund)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type),
  Elev = mean(df$Elev)
)
df.new <- cbind(df.new, predict(m.can.abund, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Canopy_Abun), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.can.abund, linfct = mcp(Biotope_type = "Tukey")))

# Cavity richness -----------------------------------------------------------------------------

## Linear model
m.cav.rich <- lm(Cavity_Rich ~ Biotope_type + Elev, data=df)
summary(m.cav.rich)
par(mfrow=c(2,2)); plot(m.cav.rich); par(mfrow=c(1,1))
ggplot(df, aes(x=Cavity_Rich)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.cav.rich, test="F")

## comparison with GLM
m2.cav.rich <- glm(Cavity_Rich ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.cav.rich)
check_overdispersion(m2.cav.rich)
AIC(m.cav.rich,m2.cav.rich)

# Cavity abundance -----------------------------------------------------------------------------

## Linear model
m.cav.abund <- lm(Cavity_Abun ~ Biotope_type + Elev, data=df)
summary(m.cav.abund)
par(mfrow=c(2,2)); plot(m.cav.abund); par(mfrow=c(1,1))
ggplot(df, aes(x=Cavity_Abun)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.cav.abund, test="F")

## comparison with GLM
m2.cav.abund <- glm(Cavity_Abun ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.cav.abund)
check_overdispersion(m2.cav.abund)
AIC(m.cav.abund,m2.cav.abund)

# Ground shrub richness -----------------------------------------------------------------------------

## Linear model
m.gr.rich <- lm(Ground_Shrub_Rich ~ Biotope_type + Elev, data=df)
summary(m.gr.rich)
par(mfrow=c(2,2)); plot(m.gr.rich); par(mfrow=c(1,1))
ggplot(df, aes(x=Ground_Shrub_Rich)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.gr.rich, test="F")
m.gr.rich <- update(m.gr.rich, ~.-Elev)

## comparison with GLM
m2.gr.rich <- glm(Ground_Shrub_Rich ~ Biotope_type, data=df, family="poisson")
summary(m2.gr.rich)
check_overdispersion(m2.gr.rich)
AIC(m.gr.rich,m2.gr.rich)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type)
)
df.new <- cbind(df.new, predict(m.gr.rich, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Ground_Shrub_Rich), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.gr.rich, linfct = mcp(Biotope_type = "Tukey")))

# Ground shrub abundance -----------------------------------------------------------------------------

## Linear model
m.gr.abund <- lm(Ground_Shrub_Abun ~ Biotope_type + Elev, data=df)
summary(m.gr.abund)
par(mfrow=c(2,2)); plot(m.gr.abund); par(mfrow=c(1,1))
ggplot(df, aes(x=Ground_Shrub_Abun)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.gr.abund, test="F")
m.gr.abund <- update(m.gr.abund, ~.-Elev)

## comparison with GLM
m2.gr.abund <- glm(Ground_Shrub_Abun ~ Biotope_type, data=df, family="poisson")
summary(m2.gr.abund)
check_overdispersion(m2.gr.abund)
AIC(m.gr.abund,m2.gr.abund)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type)
)
df.new <- cbind(df.new, predict(m.gr.abund, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Ground_Shrub_Abun), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.gr.abund, linfct = mcp(Biotope_type = "Tukey")))

# Generalist richness -----------------------------------------------------------------------------

## Linear model
m.gen.rich <- lm(Generalist_Rich ~ Biotope_type + Elev, data=df)
summary(m.gen.rich)
par(mfrow=c(2,2)); plot(m.gen.rich); par(mfrow=c(1,1))
ggplot(df, aes(x=Generalist_Rich)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.gen.rich, test="F")

## comparison with GLM
m2.gen.rich <- glm(Generalist_Rich ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.gen.rich)
check_overdispersion(m2.gen.rich)
AIC(m.gen.rich,m2.gen.rich)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type),
  Elev = mean(df$Elev)
)
df.new <- cbind(df.new, predict(m.gen.rich, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Generalist_Rich), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.gen.rich, linfct = mcp(Biotope_type = "Tukey")))

# Generalist abundance -----------------------------------------------------------------------------

## Linear model
m.gen.abund <- lm(Generalist_Abun ~ Biotope_type + Elev, data=df)
summary(m.gen.abund)
par(mfrow=c(2,2)); plot(m.gen.abund); par(mfrow=c(1,1))
ggplot(df, aes(x=Generalist_Abun)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.gen.abund, test="F")

## comparison with GLM
m2.gen.abund <- glm(Generalist_Abun ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.gen.abund)
check_overdispersion(m2.gen.abund)
AIC(m.gen.abund,m2.gen.abund)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type),
  Elev = mean(df$Elev)
)
df.new <- cbind(df.new, predict(m.gen.abund, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Generalist_Abun), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.gen.abund, linfct = mcp(Biotope_type = "Tukey")))

# Specialist richness -----------------------------------------------------------------------------

## Linear model
m.spec.rich <- lm(Specialist_Rich ~ Biotope_type + Elev, data=df)
summary(m.spec.rich)
par(mfrow=c(2,2)); plot(m.spec.rich); par(mfrow=c(1,1))
ggplot(df, aes(x=Specialist_Rich)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.spec.rich, test="F")

## comparison with GLM
m2.spec.rich <- glm(Specialist_Rich ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.spec.rich)
check_overdispersion(m2.spec.rich)
AIC(m.spec.rich,m2.spec.rich)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type),
  Elev = mean(df$Elev)
)
df.new <- cbind(df.new, predict(m.spec.rich, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Specialist_Rich), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.spec.rich, linfct = mcp(Biotope_type = "Tukey")))

# Specialist abundance -----------------------------------------------------------------------------

## Linear model
m.spec.abund <- lm(Specialist_Abun ~ Biotope_type + Elev, data=df)
summary(m.spec.abund)
par(mfrow=c(2,2)); plot(m.spec.abund); par(mfrow=c(1,1))
ggplot(df, aes(x=Specialist_Abun)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.spec.abund, test="F")

## comparison with GLM
m2.spec.abund <- glm(Specialist_Abun ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.spec.abund)
check_overdispersion(m2.spec.abund)
AIC(m.spec.abund,m2.spec.abund)

## effect plot
df.new <- data.frame(
  Biotope_type = levels(df$Biotope_type),
  Elev = mean(df$Elev)
)
df.new <- cbind(df.new, predict(m.spec.abund, se.fit = T, newdata = df.new))
ggplot(df.new, aes(x=Biotope_type, y=fit)) + 
  geom_point(color="red") +
  geom_errorbar(aes(ymin=fit-1.96*se.fit, ymax=fit+1.96*se.fit), width=.2, color="red") +
  geom_jitter(data=df, aes(y=Specialist_Abun), alpha=.3, width=.2)

## mutliple comparisons
summary(glht(m.spec.abund, linfct = mcp(Biotope_type = "Tukey")))

# Red List richness --------------------------------------------------------------------------------

## Linear model
m.rl.rich <- lm(RedList_Rich ~ Biotope_type + Elev, data=df)
summary(m.rli)
par(mfrow=c(2,2)); plot(m.rl.rich); par(mfrow=c(1,1))
ggplot(df, aes(x=RedList_Rich)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.rl.rich, test="F")

## comparison with GLM
m2.rl.rich <- glm(RedList_Rich ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.rl.rich)
check_overdispersion(m2.rl.rich)
AIC(m.rl.rich,m2.rl.rich)
drop1(m2.rl.rich, test="LR")

# Red List abundance -------------------------------------------------------------------------------

## Linear model
m.rl.abund <- lm(RedList_Abun ~ Biotope_type + Elev, data=df)
summary(m.rli)
par(mfrow=c(2,2)); plot(m.rl.abund); par(mfrow=c(1,1))
ggplot(df, aes(x=RedList_Abun)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.rl.abund, test="F")

## comparison with GLM
m2.rl.abund <- glm(RedList_Abun ~ Biotope_type + Elev, data=df, family="poisson")
summary(m2.rl.abund)
check_overdispersion(m2.rl.abund)
AIC(m.rl.abund,m2.rl.abund)
drop1(m2.rl.abund, test="LR")

# Rarity index -------------------------------------------------------------------------------------

## Linear model
m.rarin <- lm(Rarity_index ~ Biotope_type + Elev, data=df)
summary(m.rarin)
par(mfrow=c(2,2)); plot(m.rarin); par(mfrow=c(1,1))
ggplot(df, aes(x=Rarity_index)) +
  geom_histogram(binwidth = 1, color="white") +
  facet_wrap(~Biotope_type)
drop1(m.rarin, test="F")

# Saving the models --------------------------------------------------------------------------------
models <- list(m.tot.rich, m.spec.rich, m.gen.rich, m.can.rich, m.cav.rich, m.gr.rich,
               m.tot.abund, m.spec.abund, m.gen.abund, m.can.abund, m.cav.abund, m.gr.abund)
names(models) <- c("m.tot.rich", "m.spec.rich", "m.gen.rich", "m.can.rich", "m.cav.rich", "m.gr.rich",
                   "m.tot.abund", "m.spec.abund", "m.gen.abund", "m.can.abund", "m.cav.abund", "m.gr.abund")
save(models, file = "models.RData")
