library(performance)
library(ggeffects)
library(lme4)
library(tidyverse)
library(MuMIn)
library(patchwork)

# Turn off scientific notation
options(scipen=999)

# Define function for z-scoring
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# Read in data
counts <- read_csv2("feature_output/POS_feature_counts.txt")

# Data wrangling
counts <- counts %>% 
  # Add formality column
  mutate(formality = case_when(grepl("acad", text) ~ 64.97,
                               grepl("fic", text) ~ 49.26, 
                               grepl("news", text) ~ 59.07,
                               grepl("spok", text) ~ 47.21, 
                               grepl("mag", text) ~ 58.94)) %>% 
  # Add register column
  mutate(Register = case_when(grepl("acad", text) ~ "academic",
                               grepl("fic", text) ~ "fiction", 
                               grepl("news", text) ~ "newspaper",
                               grepl("spok", text) ~ "spoken", 
                               grepl("mag", text) ~ "magazine")) %>% 
  # Add relative AI
  mutate(AI_rel = round((AI/total_no_tokens) * 1000)) %>% 
  # Add relative SI
  mutate(SI_rel = round((SI/total_no_tokens) * 1000)) %>% 
  # Z-score formality
  mutate(formality_z = scale_this(formality)) %>% 
  # Reorder register based on formality
  mutate(Register = fct_reorder(Register, formality))

#str(counts)

# Text to factor
counts <- counts %>% 
  mutate(text = as_factor(text))

# Model fitting
mdl_synth <- glmer(SI_rel ~ formality_z + (1|text), 
                   data = counts, 
                   family = "poisson")
                   

mdl_ana <- glmer(AI_rel ~ formality_z + (1|text), 
                   data = counts, 
                   family = "poisson")

# Compute r-squared
r.squaredGLMM(mdl_synth)
r.squaredGLMM(mdl_ana)

# Check assumptions 
check_overdispersion(mdl_synth)
check_zeroinflation(mdl_synth)

check_overdispersion(mdl_ana)
check_zeroinflation(mdl_ana)

# Model summaries
summary(mdl_synth)
summary(mdl_ana)

# Graphs
SI_box <- ggplot(counts) +
  aes(x = Register, y = SI_rel) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Syntheticity by Register",
       x = "Register",
       y = "Syntheticity Score")

ggsave("C:/Users/user/Documents/Uni/Master Linguistik Freiburg/2. Semester/MorphoSynt Variations/Term Paper/graphs/SI_box.jpg", SI_box)

AI_box <- ggplot(counts) +
  aes(x = Register, y = AI_rel) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Analyticity by Register",
       x = "Register",
       y = "Analyticity Score")

ggsave("C:/Users/user/Documents/Uni/Master Linguistik Freiburg/2. Semester/MorphoSynt Variations/Term Paper/graphs/AI_box.jpg", AI_box)


SI_pred <- plot(ggpredict(mdl_synth, "formality_z")) + 
  labs(title = "Predicted Syntheticity by Formality",
  x = "Formality (z-scored)",      
  y = "Predicted Syntheticity") +
  theme_minimal()

ggsave("C:/Users/user/Documents/Uni/Master Linguistik Freiburg/2. Semester/MorphoSynt Variations/Term Paper/graphs/SI_pred.jpg", SI_pred)


AI_pred <- plot(ggpredict(mdl_ana, "formality_z")) + 
  labs(title = "Predicted Analyticity by Formality",
    x = "Formality (z-scored)",      
    y = "Predicted Analyticity") +
  theme_minimal()

ggsave("C:/Users/user/Documents/Uni/Master Linguistik Freiburg/2. Semester/MorphoSynt Variations/Term Paper/graphs/AI_pred.jpg", AI_pred)

# Combined prediction graphs
SI_pred_data <- as.data.frame(ggpredict(mdl_synth, "formality_z"))
AI_pred_data <- as.data.frame(ggpredict(mdl_ana, "formality_z"))

SI_pred_data$model <- "Syntheticity"
AI_pred_data$model <- "Analyticity"

combined_data <- rbind(SI_pred_data, AI_pred_data)

plot_combined <- ggplot(combined_data, aes(x = x, y = predicted, color = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = model), alpha = 0.2) +  
  labs(title = "Predicted Syntheticity and Analyticity by Formality",
       x = "Formality (z-scored)",
       y = "Predicted Value") +
  theme_minimal() +
  theme(legend.title = element_blank())  


ggsave("C:/Users/user/Documents/Uni/Master Linguistik Freiburg/2. Semester/MorphoSynt Variations/Term Paper/graphs/plot_combined.jpg", plot_combined)

# Create summary table
summary <- counts %>% 
  group_by(Register) %>% 
  summarise(
    AI_mean = mean(AI_rel),
    SI_mean = mean(SI_rel),
    AI_sd = sd(AI_rel),
    SI_sd = sd(SI_rel)) %>% 
  mutate(AI_mean = round(AI_mean, 1),
         SI_mean = round(SI_mean, 1),
         AI_sd = round(AI_sd, 1),
         SI_sd = round(SI_sd, 1))

write_csv(summary, "C:\\Users\\user\\Documents\\Uni\\Master Linguistik Freiburg\\2. Semester\\MorphoSynt Variations\\Term Paper\\graphs\\summary_table.csv")

# Compute data loss
1000000-sum(counts$total_no_tokens)

# Descriptive stats
range(counts$SI_rel)
min(counts$SI_rel)

# Most frequent categories
counts %>%
  pivot_longer(cols = contains("ANA"), names_to = "category", values_to = "ANA_score") %>%
  group_by(category) %>%
  summarise(total_hits = sum(ANA_score, na.rm = TRUE)) %>%
  arrange(desc(total_hits)) %>%
  slice(1:5) %>% 
  arrange(desc(total_hits))

counts %>%
  pivot_longer(cols = contains("SYN"), names_to = "category", values_to = "SYN_score") %>%
  group_by(category) %>%
  summarise(total_hits = sum(SYN_score, na.rm = TRUE)) %>%
  arrange(desc(total_hits)) %>%
  slice(1:4) %>% 
  arrange(desc(total_hits))
