library(patchwork)
library(ggbeeswarm)
library(RColorBrewer)

# Obtain R-squareds and MAEs
results <- linear$results %>%
  select(MAE, Rsquared) %>% 
  mutate(model = "Linear") %>% 
  rbind(glmnet$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "Elastic Net")) %>%
  rbind(xgboost$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "XGBoost")) %>% 
  rbind(knn$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "KNN")) %>% 
  rbind(mars$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "MARS")) %>% 
  rbind(svm$results %>% 
          select(MAE, Rsquared) %>% 
          mutate(model = "SVM")) %>% 
  group_by(model) %>% 
  mutate(max_Rsquared = max(Rsquared),
         max_MAE = max(MAE)) %>% 
  ungroup()

# Make the first plot with R-squareds
p1 <- results %>% 
  ggplot(aes(x = reorder(model, max_Rsquared), y = Rsquared, color = model)) +
  geom_quasirandom() +
  ggtitle("Cross-validated accuracy measures for different models",
          subtitle = "Using default hyperparameter search by the caret library") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("")

# Make the second plot with MAEs
p2 <- results %>% 
  ggplot(aes(x = reorder(model, max_Rsquared), y = MAE, color = model)) +
  geom_quasirandom() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_brewer(type = "qual", palette = "Dark2") +
  scale_y_continuous(limits = c(0, max(results$MAE))) +
  xlab("")

# Plot vertically using patchwork
p1 / p2
