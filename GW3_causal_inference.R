# Clean memory
rm(list=ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(tableone)
library(MatchIt)
library(cobalt)
library(logistf) 

# Load dataset
setwd("C:\\Users\\oem\\Desktop\\shkolla\\casual inference")
df <- read_excel("data_rdd (2).xlsx")
df = na.omit(df)
df <- df %>% filter(class != "no stars")

# Define Treatment: Hotels with bubble rating >= 4
df$treatment <- ifelse(df$score_adjusted > 4, 1, 0)

# Analisi descrittiva
summary(df$views)
table(df$treatment)
summary(df)

# Test t per differenze nei click tra trattati e non
with(df, t.test(views ~ treatment, var.equal = FALSE))

# Regressione semplice per vedere l'effetto grezzo
lm1 <- lm(views ~ treatment, data = df)
summary(lm1)

df$class <- as.factor(ifelse(df$class %in% c(1, 2), "Low Stars", 
                             ifelse(df$class == 3, "Mid Stars", "High Stars")))

df$price_min <- as.numeric(gsub("[^0-9.]", "", df$price_min))
df$price_max <- as.numeric(gsub("[^0-9.]", "", df$price_max))
df <- df %>% filter(!is.na(price_min) & !is.na(price_max))
sapply(df, class)
df$price_mean <- (df$price_min + df$price_max) / 2

df$location_grade<-as.numeric(gsub("[^0-9.]", "", df$location_grade))
df <- df %>% filter(!is.na(location_grade))
df$location_grade_grouped <- cut(df$location_grade, 
                                 breaks = c(-Inf, 50, 70, 90, Inf), 
                                 labels = c("Low", "Medium", "High", "Very High"))

# Compare treatment vs control before matching
covariates <- c("class", "location_grade_grouped", "n_reviews", "price_mean", "discount", 
                "discount_perc", "award_travellers_choice", "award_greenleaders", 
                "award_cert_excellence", "photos")

tableone::CreateTableOne(vars = covariates, strata = "treatment", data = df)


library(reshape2)
library(dplyr)

library(ggcorrplot)

# Select only continuous variables including views
df$views<-log1p(df$views)
cont_vars <- df %>% select(views, n_reviews, price_mean, discount_perc, photos)

# Compute correlation matrix
cor_matrix <- cor(cont_vars, use = "pairwise.complete.obs")

# Plot heatmap
ggcorrplot(cor_matrix, method = "square", type = "lower",
           lab = TRUE, outline.color = "white",
           colors = c("red", "white", "blue"),
           title = "Correlation Heatmap (Continuous Variables)",
           ggtheme = theme_minimal())

# Remove only the row where views == 88
df_filtered <- df %>% filter(views != 88)

# Plot the updated boxplot
ggplot(df, aes(x = class, y = views, fill = class)) +
  geom_boxplot() +
  scale_y_log10() +  # Apply log transformation
  theme_minimal() +
  labs(title = "Distribution of Views by Class (Log Scale)",
       x = "Hotel Class",
       y = "Log( Views )")



ggplot(df, aes(x = location_grade_grouped, y = views, fill = location_grade_grouped)) +
  geom_boxplot() +
  scale_y_log10() +  # Apply log transformation
  theme_minimal() +
  labs(title = "Distribution of Views by Class (Log Scale)",
       x = "Hotel Class",
       y = "Log( Views )")

ggplot(df_filtered, aes(x = class, y = views, fill = class)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log1p") +  # Uses log(views + 1)
  theme_minimal() +
  labs(title = "Distribution of Views by Location Grade (Log Scale)",
       x = "Location Grade",
       y = "Log( Views + 1 )")




binary_vars <- c("award_travellers_choice", "award_greenleaders", "award_cert_excellence")

# Loop through each binary variable
for (var in binary_vars) {
  print(paste("T-test for:", var))
  print(t.test(df_filtered$views ~ df_filtered[[var]], var.equal = FALSE))
}

library(car)
vif(lm(views ~ award_travellers_choice + location_grade_grouped + price_mean+treatment, data = df_filtered))


# ==========================
# STEP 4: Propensity Score Matching (PSM)
# ==========================

lm2 <- lm(log(views+1) ~ treatment+class + location_grade_grouped + n_reviews + price_mean + 
            discount_perc + award_travellers_choice + award_greenleaders + 
            award_cert_excellence, data = df)
summary(lm2)

#nearest
psm_model <- matchit(treatment ~ class + location_grade_grouped + n_reviews + price_mean + discount + 
                       discount_perc + award_travellers_choice + award_greenleaders + 
                       award_cert_excellence, 
                     data = df, method = "nearest", distance = "glm", ratio =3, caliper = 0.1)
summary(psm_model)

psm_model$distance


head(psm_model$distance)
plot(psm_model$distance ~ df$treatment) #check for overlap between the treated and control groups
bal.tab(psm_model)

# Estrarre i dati matched
matched_data <- match.data(psm_model)
matched_data

# Check balance after matching
tableone::CreateTableOne(vars = covariates, strata = "treatment", data = matched_data)
love.plot(psm_model, abs = TRUE, stats = "mean.diffs", threshold = 0.1, stars = "std")
bal.tab(psm_model)

------------------
  

  balance_post_categorical <- df %>%
  group_by(treatment) %>%
  summarise(across(where(is.factor), ~ sum(. == first(.), na.rm = TRUE) / n(), .names = "prop_{.col}"))

print(balance_post_categorical)

------------
  # Compute means for views BEFORE matching
  outcome_balance_pre <- df %>%
  group_by(treatment) %>%
  summarise(Mean_Views = mean(views, na.rm = TRUE))

print(outcome_balance_pre)

# Compute means for views AFTER matching
outcome_balance_post <- matched_data %>%
  group_by(treatment) %>%
  summarise(Mean_Views = mean(views, na.rm = TRUE))

print(outcome_balance_post)




library(ggplot2)
ggplot(matched_data, aes(x = factor(treatment), fill = factor(class))) +
  geom_bar(position = "fill") +
  labs(x = "Treatment Group", y = "Proportion", fill = "High Stars") +
  theme_minimal()




plot(psm_model, type = "qq")
plot(psm_model, type = "hist", maintitle= "Propensity Score Distribution")
plot(psm_model, type = "jitter")
install.packages ("sandwich") 
library (sandwich)
plot(psm_model, type= "density")


# Load required libraries
library(gridExtra)

# Add Propensity Scores to the Original and Matched Data
df$propensity_score <- psm_model$distance
matched_data$propensity_score <- df$propensity_score[match(row.names(matched_data), row.names(df))]

# Define raw treated and control groups
raw_treated <- df[df$treatment == 1, ]
raw_control <- df[df$treatment == 0, ]

# Define matched treated and control groups
matched_treated <- matched_data[matched_data$treatment == 1, ]
matched_control <- matched_data[matched_data$treatment == 0, ]


# Load necessary library
library(cobalt)
library(kableExtra)

# Generate balance summary
balance_summary <- bal.tab(psm_model, un = TRUE)  # Include unmatched (before matching)

# Create DataFrame with means before and after matching
balance_means <- data.frame(
  Covariate = rownames(balance_summary$Balance),
  Means_Treated_Pre = balance_summary$Balance[, "M.0.Un"],   # Mean in Treated Group (before matching)
  Means_Control_Pre = balance_summary$Balance[, "M.1.Un"],   # Mean in Control Group (before matching)
  Means_Treated_Post = balance_summary$Balance[, "M.0.Adj"], # Mean in Treated Group (after matching)
  Means_Control_Post = balance_summary$Balance[, "M.1.Adj"]  # Mean in Control Group (after matching)
)

# Print the table
print(balance_means)



# Create density histograms for each group
p1 <- ggplot(raw_treated, aes(x = propensity_score)) +
  geom_histogram(binwidth = 0.05, fill = "gray", color = "black", aes(y = ..density..)) +
  ggtitle("Raw Treated") + labs(x = "Propensity Score", y = "Proportion") + theme_minimal()

p2 <- ggplot(matched_treated, aes(x = propensity_score)) +
  geom_histogram(binwidth = 0.05, fill = "gray", color = "black", aes(y = ..density..)) +
  ggtitle("Matched Treated") + labs(x = "Propensity Score", y = "Proportion") + theme_minimal()

p3 <- ggplot(raw_control, aes(x = propensity_score)) +
  geom_histogram(binwidth = 0.05, fill = "gray", color = "black", aes(y = ..density..)) +
  ggtitle("Raw Control") + labs(x = "Propensity Score", y = "Proportion") + theme_minimal()

p4 <- ggplot(matched_control, aes(x = propensity_score)) +
  geom_histogram(binwidth = 0.05, fill = "gray", color = "black", aes(y = ..density..)) +
  ggtitle("Matched Control") + labs(x = "Propensity Score", y = "Proportion") + theme_minimal()

# Arrange all four plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


# Calculate Standardized Mean Differences (SMD)
balance_summary <- bal.tab(psm_model, un = TRUE)

# Extract Pre-Matching and Post-Matching SMD values
smd_table <- data.frame(
  Covariate = rownames(balance_summary$Balance),
  SMD_Pre = balance_summary$Balance[, "Diff.Un"],
  SMD_Post = balance_summary$Balance[, "Diff.Adj"]
)

# Print the table
print(smd_table)




###Analisi degli Effetti del Trattamento
library(dplyr)
attach(df)

# Calcola la media della variabile di esito per i due gruppi
matched_data %>%
  group_by(treatment) %>%
  summarise(media_esito = mean(views, na.rm = TRUE))
#l'output sono le medie di PI per i due gruppi (C e T), 
# i trattati hanno una media delle views più alte


# Modello di regressione lineare sull'effetto del trattamento (ATT)
treatment_effect_model <- lm(views ~ treatment, data = matched_data)
summary(treatment_effect_model)

###t-test
t_test <- t.test(views ~ treatment, data = matched_data)
print(t_test)

#OPTIMAL
psm_model1 <- matchit(treatment ~ class + location_grade_grouped + n_reviews + price_mean + discount + 
                       discount_perc + award_travellers_choice + award_greenleaders + 
                       award_cert_excellence, 
                     data = df, method = "optimal", distance = "glm", ratio =2)
summary(psm_model1)
head(psm_model1$distance)
plot(psm_model1$distance ~ df$treatment) #check for overlap between the treated and control groups


# Estrarre i dati matched
matched_data1 <- match.data(psm_model1)
matched_data1

# Check balance after matching
tableone::CreateTableOne(vars = covariates, strata = "treatment", data = matched_data1)
love.plot(psm_model1, abs = TRUE, stats = "mean.diffs", threshold = 0.1, stars = "std")

plot(psm_model1, type = "qq")
plot(psm_model1, type = "hist", maintitle= "Propensity Score Distribution")
plot(psm_model1, type = "jitter")
#install.packages ("sandwich") 
library (sandwich)
plot(psm_model1, type= "density")

###Analisi degli Effetti del Trattamento
#library(dplyr)
attach(df)

# Calcola la media della variabile di esito per i due gruppi
matched_data1 %>%
  group_by(treatment) %>%
  summarise(media_esito = mean(views, na.rm = TRUE))
#l'output sono le medie di PI per i due gruppi (C e T), 
# i trattati hanno una media delle views più alte


# Modello di regressione lineare sull'effetto del trattamento
treatment_effect_model1 <- lm(views ~ treatment, data = matched_data1)
summary(treatment_effect_model1)

###t-test
t_test <- t.test(views ~ treatment, data = matched_data1)
print(t_test)

#Mahalanobis Distance Matching
mdm_model <- matchit(treatment ~ class + location_grade_grouped + n_reviews + price_mean + discount + 
                       discount_perc + award_travellers_choice + award_greenleaders + 
                       award_cert_excellence,data = df, method = "nearest",
                    distance = "mahalanobis")

summary(mdm_model)
matched_data_maha <- match.data(mdm_model)
tableone::CreateTableOne(vars = covariates, strata = "treatment", data = matched_data_maha)
love.plot(mdm_model, threshold = 0.1,stars = "std")

# Modello di regressione lineare sull'effetto del trattamento
treatment_effect_model <- lm(views ~ treatment, data = matched_data_maha)
summary(treatment_effect_model)

###t-test
t_test <- t.test(views ~ treatment, data = matched_data_maha)
print(t_test)

