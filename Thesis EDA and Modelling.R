######### EXPLORATORY DATA ANALYSIS ###########
library(dplyr)
library(ggplot2)
library(lubridate)
library(tsibble)
library(tidyr)
library(scales)
library(ggthemes)
library(forecast)

# Making sure won is a factor
data <- data %>%
  mutate(won = ifelse(won == 1, "Yes", "No") %>% factor(levels = c("No", "Yes")))

# Creating time series data frame
ts_data <- data %>%
  filter(!is.na(contract_signing_date), !is.na(revenue)) %>%
  mutate(month = lubridate::floor_date(as.Date(contract_signing_date), "month")) %>%
  filter(lubridate::year(month) >= 2010) %>% # <-- filter here
  filter(lubridate::year(month) <= 2024) # <-- filter here


# Aggregate total revenue per month
monthly_revenue <- ts_data %>%
  group_by(month) %>%
  summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop")

# Fill in missing months with 0 revenue
all_months <- data.frame(month = seq(min(monthly_revenue$month), max(monthly_revenue$month), by = "month"))

monthly_revenue <- full_join(all_months, monthly_revenue, by = "month") %>%
  arrange(month) %>%
  mutate(revenue = replace_na(revenue, 0))

# Recalculate zero stats
zero_stats <- monthly_revenue %>%
  summarise(
    zero_months = sum(revenue == 0),
    total_months = n(),
    prop_zero = zero_months / total_months
  )

print(zero_stats) # only one month out of 180 months has 0 revenue

# Plot line chart of revenue per month
ggplot(monthly_revenue, aes(x = month, y = revenue)) +
  geom_line(color = "darkblue") +
  labs(title = "Monthly Revenue Over Time", x = "Month", y = "Total Revenue") +
  theme_minimal() +
  scale_y_continuous(labels = comma)

# Add Year column
monthly_revenue <- monthly_revenue %>%
  mutate(year = format(month, "%Y"))


# Proposal success rate
data %>%
  count(won) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = factor(won), y = percentage, fill = factor(won))) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Proposal Success Rate", x = "Proposal Won (1 = Yes, 0 = No)", y = "Percentage") +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  theme_minimal()

# Prepare data for win rate per main product, revenue etc.
win_rate_data <- data %>%
  group_by(main_product, won) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(main_product) %>%
  mutate(total = sum(n),
         win_rate = n / total) %>%
  filter(won == "Yes")  # Keep only where proposal was won

# Boxplot of revenue by win status
ggplot(data, aes(x = factor(won), y = revenue)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_log10(labels = label_comma()) +
  labs(title = "Revenue Distribution by Proposal Outcome", x = "Won", y = "Revenue (log scale)") +
  theme_minimal()  

# T-TEST
data <- data %>% filter(revenue > 0)  # remove zeros to avoid log(0)

t_test_result <- t.test(log(revenue) ~ won, data = data)

# Print the result
print(t_test_result) # we reject the null hypothesis

# log revenue by lead partner
data %>%
  filter(won == 1) %>%
  ggplot(aes(x = factor(lead_partner), y = revenue)) +
  geom_boxplot(fill = "lightcoral", color = "darkred") +
  scale_y_log10(labels = label_comma()) +
  labs(title = "Log Revenue by Lead Partner (Won Proposals Only)",
       x = "Lead Partner (0 = No, 1 = Yes)",
       y = "Revenue (log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Log revenue by approach
data %>%
  filter(won == 1) %>%
  ggplot(aes(x = factor(approach), y = revenue)) +
  geom_boxplot(fill = "lightblue", color = "steelblue") +
  scale_y_log10(labels = label_comma()) +
  labs(title = "Log Revenue by Approach (Won Proposals Only)",
       x = "Approach (0 = Consortium, 1 = Solo Bid)",
       y = "Revenue (log scale)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Log revenue by main product
data %>%
  filter(won == 1) %>%
  ggplot(aes(x = reorder(main_product, revenue, FUN = median), y = revenue)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  scale_y_log10(labels = label_comma()) +
  coord_flip() +
  labs(title = "Log Revenue by Main Product (Won Proposals Only)",
       x = "Main Product",
       y = "Revenue (log scale)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 9)
  )

# Donor Type distribution
ggplot(data, aes(x = donor_type)) +
  geom_bar(fill = "darkcyan") +
  coord_flip() +
  labs(title = "Frequency of Donor Types", x = "Donor Type", y = "Count") +
  theme_minimal()

# Calculate win rate per year
win_rate_by_year <- data %>%
  group_by(year) %>%
  summarise(
    total = n(),
    won_count = sum(won, na.rm = TRUE),
    win_rate = won_count / total
  ) %>%
  filter(!is.na(year))  # Remove NA years if any

# Plot the win rate
ggplot(win_rate_by_year, aes(x = factor(year), y = win_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::percent(win_rate, accuracy = 1)), 
            vjust = -0.3, size = 3) +
  labs(title = "Proposal Win Rate by Year",
       x = "Year",
       y = "Win Rate") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()


# Competitive vs Non-Competitive and win rates
data %>%
  group_by(competitive, won) %>%
  summarise(count = n()) %>%
  mutate(competitive = ifelse(competitive == 1, "Competitive", "Non-Competitive")) %>%
  ggplot(aes(x = competitive, y = count, fill = factor(won))) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Win Rate by Proposal Type", x = "Proposal Type", y = "Proportion", fill = "Won") +
  theme_minimal()

data %>%
  group_by(competitive, won) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(competitive) %>%
  mutate(
    pct = count / sum(count) * 100,
    competitive = ifelse(competitive == 1, "Competitive", "Non-Competitive")
  )
# now doing EDA on revenue as the outcome variable
# Donor Type (Top 5)
top5_donor_types <- data %>%
  filter(won == 1) %>%
  count(donor_type, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  pull(donor_type)

data %>%
  filter(won == 1, year < 2025, donor_type %in% top5_donor_types) %>%
  group_by(year, donor_type) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year), y = (total_revenue), fill = donor_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Revenue by Donor Type",
       x = "Year", y = "Revenue in USD", fill = "Donor Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.4, "cm")) +
  scale_fill_brewer(palette="Set2") +
  scale_y_continuous(labels = label_comma()) 


# Donor Country (Top 5)
top5_donor_countries <- data %>%
  filter(won == 1) %>%
  count(donor_country, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  pull(donor_country)

data %>%
  filter(won == 1, year < 2025, donor_country %in% top5_donor_countries) %>%
  group_by(year, donor_country) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year), y = (total_revenue), fill = donor_country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Revenue by Donor Country",
       x = "Year", y = "Revenue in USD", fill = "Donor Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.4, "cm")) +
  scale_fill_brewer(palette="Set2") +
  scale_y_continuous(labels = label_comma()) 


# Lead Country (Top 5)
top5_lead_countries <- data %>%
  filter(won == 1) %>%
  count(lead_country, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  pull(lead_country)

data %>%
  filter(won == 1, year < 2025, lead_country %in% top5_lead_countries) %>%
  group_by(year, lead_country) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year), y = (total_revenue), fill = lead_country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Revenue by Lead Country",
       x = "Year", y = "Total Revenue in USD", fill = "Lead Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.4, "cm")) +
  scale_fill_brewer(palette="Set2") +
  scale_y_continuous(labels = label_comma()) 

# Now doing the same for the external indicators (grouped by tertiles)
# Create simplified categories for each indicator
data_simplified <- data %>%
  filter(won == 1, year < 2025) %>%
  mutate(
    gdp_group = cut(lead_country_gdp_percapita,
                    breaks = quantile(lead_country_gdp_percapita, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                    labels = c("Low GDP per capita", "Medium", "High"), include.lowest = TRUE),
    fragility_group = cut(lead_fragility_index,
                          breaks = quantile(lead_fragility_index, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                          labels = c("Low Fragility", "Medium", "High"), include.lowest = TRUE),
    democracy_group = cut(lead_electoral_democracy,
                          breaks = quantile(lead_electoral_democracy, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                          labels = c("Low Democracy", "Medium", "High"), include.lowest = TRUE)
  )

# Filter dataset: only won proposals, exclude 2025
won_data <- data %>% filter(won == 1, year < 2025) %>%
  filter(won == 1, year < 2025) %>%
  mutate(
    gdp_group = cut(lead_country_gdp_percapita,
                    breaks = quantile(lead_country_gdp_percapita, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                    labels = c("Low GDP per capita", "Medium", "High"), include.lowest = TRUE),
    fragility_group = cut(lead_fragility_index,
                          breaks = quantile(lead_fragility_index, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                          labels = c("Low Fragility", "Medium", "High"), include.lowest = TRUE),
    democracy_group = cut(lead_electoral_democracy,
                          breaks = quantile(lead_electoral_democracy, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                          labels = c("Low Democracy", "Medium", "High"), include.lowest = TRUE)
  )

# Boxplot: Revenue by Democracy Group
ggplot(won_data, aes(x = democracy_group, y = log1p(revenue))) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Revenue by Democracy Group (Won Proposals Only)",
       x = "Democracy Group", y = "Log(1 + Revenue)") +
  theme_minimal()

# Boxplot: Revenue by Fragility Group
ggplot(won_data, aes(x = fragility_group, y = log1p(revenue))) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Revenue by Fragility Group (Won Proposals Only)",
       x = "Fragility Group", y = "Log(1 + Revenue)") +
  theme_minimal()

# Boxplot: Revenue by GDP per Capita Group
ggplot(won_data, aes(x = gdp_group, y = log1p(revenue))) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Revenue by GDP Group (Won Proposals Only)",
       x = "GDP Group", y = "Log(1 + Revenue)") +
  theme_minimal()

library(scales)
# show non-linear relationships with scatterplots
ggplot(data, aes(x = lead_country_gdp, y = revenue)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "darkgreen") +
  scale_y_log10(labels=label_comma()) +
  theme_minimal() +
  labs(title = "Revenue vs Lead Country GDP")

ggplot(data, aes(x = co_financing, y = revenue)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "darkgreen") +
  scale_y_log10(labels=label_comma()) +
  theme_minimal() +
  labs(title = "Revenue vs Co-Financing")

ggplot(data, aes(x = lead_oda_received, y = revenue)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "darkgreen") +
  scale_y_log10(labels=label_comma()) +
  theme_minimal() +
  labs(title = "Revenue vs Lead Country ODA Received")

ggplot(data, aes(x = lead_country_gdp_percapita, y = revenue)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "darkgreen") +
  scale_y_log10(labels=label_comma()) +
  theme_minimal() +
  labs(title = "Revenue vs Lead Country GDP Per Capita")

## Correlation map
library(GGally)
num_vars <- data %>% 
  select(where(is.numeric)) %>% 
  select_if(~ var(., na.rm = TRUE) > 1e-5)

ggcorr(num_vars, label = TRUE)

# Filter numeric columns only
numeric_data <- data %>%
  select(where(is.numeric)) %>%
  drop_na()  # optional: remove rows with NAs

# Create correlation matrix
cor_matrix <- cor(numeric_data)

# Plot it
corrplot::corrplot(cor_matrix, method = "color", type = "lower",
                   tl.cex = 0.6, tl.col = "black", tl.srt = 45)


############################ MODELLING ############################ 
######### FIRST STAGE OF TWO STAGE MODEL ###########
library(tidymodels)
library(xgboost)
library(lightgbm)
library(randomForest)
library(themis)
library(vip)
library(dplyr)

# Ensure 'won' is a factor with proper levels and labels
data <- data %>%
  mutate(won = ifelse(won == 1, "Yes", "No"),
         won = factor(won, levels = c("No", "Yes")))

table(data$won)
prop.table(table(data$won))

# Removing the year 2023 as that is going to be the out-of-sample forecasting
revenue_2024_won <- data %>%
  filter(won == "Yes", year(contract_signing_date) == 2024) %>%
  summarise(total_revenue_2024 = sum(revenue, na.rm = TRUE))

print(revenue_2024_won)#total revenue from 2024 is 198627670

#Creating data set for only until 2023. 2024 is out of sample
# remove observations signed before 2010 for data
data <- data %>%
  filter(
    is.na(contract_signing_date) | lubridate::year(contract_signing_date) > 2009
  )

unique(data$signing_year)

data <- data %>%
  mutate(date_proposal_submitted = as.Date(date_proposal_submitted))

write.csv(data, "data_clean_finally.csv")

# create a new dataframe, Filter to exclude 2024
data_until_2023 <- data %>%
  filter(
    lubridate::year(date_proposal_submitted) <= 2023,
    is.na(contract_signing_date) | lubridate::year(contract_signing_date) <= 2023
  )
unique(data_until_2023$signing_year) # only has until 2023

write.csv(data_until_2023, "data_until_2023.csv")

data_until_2023 <- read.csv("data_until_2023.csv")
data_until_2023 <- data_until_2023 %>%
  select(-...1)
table(data_until_2023$won)

# Model 1, creating a classification model (regularized regression)
# Make sure it's a Date type
data_until_2023 <- data_until_2023 %>%
  mutate(date_proposal_submitted = as.Date(date_proposal_submitted))

# Split data (chronologically)
set.seed(123)
data_train <- data_until_2023 %>% filter(lubridate::year(date_proposal_submitted) <= 2018)
data_test  <- data_until_2023 %>% filter(lubridate::year(date_proposal_submitted) >= 2019)

table(data_train$won)
table(data_test$won)

unique(data_train$year)
unique(data_test$year)

# Make sure data is ordered by time
data_train <- data_train %>% arrange(date_proposal_submitted)

# Set up time series CV
library(timetk)

set.seed(123)
ts_cv_folds <- time_series_cv(
  data = data_train,
  date_var = date_proposal_submitted,
  initial = "3 years",     
  assess = "1 year",       
  skip = "1 year",         
  cumulative = TRUE,
  slice_limit = 10          
)

length(ts_cv_folds$splits) # has 10 slices

# Check class balance in each fold
fold_class_balance <- map_df(seq_along(ts_cv_folds$splits), function(i) {
  analysis_set <- analysis(ts_cv_folds$splits[[i]])
  prop_yes <- mean(analysis_set$won == "Yes", na.rm = TRUE)
  tibble(fold = i, prop_yes = prop_yes)
})

ggplot(fold_class_balance, aes(x = fold, y = prop_yes)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = c(0.05, 0.95), linetype = "dashed", color = "red") +
  labs(
    title = "Class Balance Across Folds",
    x = "Fold Index",
    y = "Proportion of 'Yes' in Assessment Set"
  ) +
  theme_minimal()

# recipe with downsampling
classification_recipe <- recipe(won ~ ., data = data_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, signing_month, signing_year, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_downsample(won)

# Lasso logistic model
lasso_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# Workflow
lasso_wf <- workflow() %>%
  add_model(lasso_model) %>%
  add_recipe(classification_recipe)

# Auto tuning grid
grid <- grid_space_filling(penalty(), size = 20)

# Tune with CV
lasso_tune <- tune_grid(
  lasso_wf,
  resamples = ts_cv_folds,
  grid = grid,
  metrics = metric_set(bal_accuracy, accuracy, precision, recall, roc_auc)
)

# Select best models
lasso_best <- lasso_tune %>% select_by_one_std_err(metric = "roc_auc", desc(penalty))

# Finalize workflows
lasso_final <- finalize_workflow(lasso_wf, lasso_best)

# Fit the finalized workflow on training data
lasso_fit_final <- fit(lasso_final, data = data_train)

# Get class + probability predictions
lasso_predictions <- bind_cols(
  predict(lasso_fit_final, data_test, type = "prob"),   
  predict(lasso_fit_final, data_test, type = "class"),  
  data_test %>% select(won)
)

lasso_predictions <- lasso_predictions %>%
  mutate(
    won = factor(won, levels = c("Yes", "No")),
    .pred_class = factor(.pred_class, levels = c("Yes", "No"))
  )

class_metrics(data = lasso_predictions, truth = won, estimate = .pred_class, .pred_Yes)

# Extract fitted model from last_fit
lasso_fit_final %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  print(n=70)


# Confusion matrix
lasso_predictions %>%
  conf_mat(truth = won, estimate = .pred_class)

# ROC curve data
roc_curve(lasso_predictions, truth = won, .pred_Yes, event_level = "first") %>%
  autoplot() +
  labs(title = "ROC Curve - LASSO Model")

# Coefficient plot
lasso_fit_final %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0, term != "(Intercept)") %>%
  mutate(abs_estimate = abs(estimate)) %>%
  slice_max(order_by = abs_estimate, n =20) %>%
  ggplot(aes(x = term, y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Non-Zero Coefficients from Lasso Regression",
    x = "Feature",
    y = "Coefficient Estimate"
  ) +
  theme_minimal()

### Boosted model with XGBoost
# Create recipe with downsampling (this recipe is different as it doesn't have step_normalize)
xgb_recipe <- 
  recipe(won ~ ., data = data_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, signing_month, signing_year, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_downsample(won)

# Tuning main 3 hyperparameters:
xgb_model_tune <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    stop_iter = 500
  ) |>
  set_mode("classification") |>
  set_engine("xgboost")


# Defining the workflow:
xgb_tune_wf <-
  workflow() |>
  add_recipe(xgb_recipe) |>
  add_model(xgb_model_tune)
xgb_tune_wf


# Setting up class metrics
class_metrics <- metric_set(
  accuracy, bal_accuracy, precision, recall, f_meas, roc_auc
)

# Using parallel processing to speed up computation:
num_cores <- parallel::detectCores()
num_cores


doParallel::registerDoParallel(cores = num_cores - 1L)


# Setting up a random tuning grid:
set.seed(8504)
grid_max_entropy(trees(range = c(0, 10000)),
                 learn_rate(range = c(-2, -1)),
                 tree_depth(),
                 size = 20
)


# Doing adaptive tuning:
tuning_params <-
  parameters(
    trees(range = c(0, 10000)),
    learn_rate(range = c(-5, -1)),
    tree_depth(range = c(1, 3))
  )


set.seed(1)
xgb_tune_res_1 <- tune_bayes(
  xgb_tune_wf,
  resamples = ts_cv_folds,
  iter = 1,
  param_info = tuning_params,
  metrics = class_metrics
)

# Looking at initial results:
xgb_tune_res_1 |>
  autoplot(metric = "roc_auc")

# Next, we will run this procedure again. However, this time we will pass in 
# xgb_tune_res_1 as the value of the named argument initial. This tells tune_bayes 
# to leverage the information in the previous tuning results (in xgb_tune_res_1) 
# when choosing which hyperparameters to try first.

set.seed(2)
xgb_tune_res_2 <- # updated results
  tune_bayes(
    xgb_tune_wf,
    initial = xgb_tune_res_1, # previous results
    resamples = ts_cv_folds,
    iter = 5, # num. new models to try
    param_info = tuning_params,
    metrics = class_metrics
  )

# Look at updated tuning results:
xgb_tune_res_2 |>
  autoplot(metric = "roc_auc")

# Keep repeating tuning procedure:
set.seed(3)
xgb_tune_res_3 <- # updated results
  tune_bayes(
    xgb_tune_wf,
    initial = xgb_tune_res_2, # previous results
    resamples = ts_cv_folds,
    iter = 5,
    param_info = tuning_params,
    metrics = class_metrics
  )

# Autoplot
xgb_tune_res_3 |>
  autoplot(metric = "roc_auc") # PErformance is starting to level off

# Keep repeating tuning procedure:
set.seed(4)
xgb_tune_res_4 <- # updated results
  tune_bayes(
    xgb_tune_wf,
    initial = xgb_tune_res_3, # previous results
    resamples = ts_cv_folds,
    iter = 5,
    param_info = tuning_params,
    metrics = class_metrics
  )

# Autoplot
xgb_tune_res_4 |>
  autoplot(metric = "roc_auc") # Performance is starting to level off

# Extracting the metrics
xgb_tune_metrics <-
  xgb_tune_res_4 |>
  collect_metrics()
xgb_tune_metrics


# Visualizing three hyperparameters:
xgb_tune_metrics |>
  filter(.metric == "roc_auc") |>
  filter(mean >= median(mean)) |> # only show the best options
  ggplot(aes(
    x = trees, y = learn_rate,
    shape = factor(tree_depth),
    fill = mean
  )) +
  geom_point(size = 3) +
  scale_shape_manual(values = 21:23) +
  scale_fill_gradient(low = "white", high = "#D55E00") +
  theme_bw() +
  labs(x = "Trees", y = "Learning rate", shape = "Max tree depth", fill = "ROC-AUC")

# Doing 1SE procedure:
xgb_best_point <-
  xgb_tune_metrics |>
  filter(.metric == "roc_auc" & tree_depth == 2) |>
  arrange(desc(mean)) |>
  slice_head(n = 1)
xgb_best_roc_auc <- xgb_best_point$mean
xgb_selected_model <-
  xgb_tune_metrics |>
  filter(.metric == "roc_auc" & tree_depth == 2) |>
  filter(mean + std_err >= xgb_best_roc_auc) |>
  arrange(desc(learn_rate)) |>
  slice_head(n = 1)
xgb_selected_model


# Finalize the workflow
xgb_final_wf <-
  xgb_tune_wf |>
  finalize_workflow(xgb_selected_model)
xgb_final_wf

# Fit the finalized workflow on training data
xgb_final_fit <- fit(xgb_final_wf, data = data_train)

# Ensure 'won' is a factor with consistent levels
# Get class + probability predictions
xgb_predictions <- bind_cols(
  predict(xgb_final_fit, data_test, type = "prob"),   # gives .pred_Yes
  predict(xgb_final_fit, data_test, type = "class"),  # gives .pred_class
  data_test %>% select(won)
)

xgb_predictions <- xgb_predictions %>%
  mutate(
    won = factor(won, levels = c("Yes", "No")),
    .pred_class = factor(.pred_class, levels = c("Yes", "No"))
  )

class_metrics(data = xgb_predictions, truth = won, estimate = .pred_class, .pred_Yes)



# Final fit:
xgb_predictions |>
  roc_curve(won, .pred_Yes, event_level = "first") |>
  autoplot() +
  labs(title = "ROC Curve - XGBoost")


# Extract variable importance
vip_df_xgb <- xgb_final_fit |>
  extract_fit_parsnip() |>
  vi()

vip_df_xgb %>%
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = paste0(round(Importance, 3))),
            hjust = -0.1, size = 3.2) +
  labs(
    title = "Top 20 Most Important Features - XGBoost",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal() +
  expand_limits(y = max(vip_df_xgb$Importance) * 1.1)  # add padding for labels

# Confusion matrix
xgb_predictions %>%
  conf_mat(truth = won, estimate = .pred_class)

### Random Forest
# Create recipe with SMOTE
rf_recipe <- 
  recipe(won ~ ., data = data_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, signing_month, signing_year,new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_smote(won, over_ratio = 1)

# Determining thenumber of p
# Prep the recipe
rf_prep <- prep(rf_recipe)

# Bake the prepped data
baked_rf_data <- bake(rf_prep, new_data = NULL)

# Calculate number of predictors (excluding outcome)
p <- ncol(baked_rf_data) - 1  # subtract 1 for 'won'

# Print p
print(p)

# Class metrics + tuning grid
rf_model <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")

rf_tune_wf <-
  workflow() |>
  add_recipe(rf_recipe) |>
  add_model(rf_model)

rf_tune_wf

# Setting the class metrics:
class_metrics <- metric_set(
  accuracy, bal_accuracy, precision,
  recall, f_meas, roc_auc
)


# Tune from 1 to total number of predictors after preprocessing
# Smaller tuning grid
rf_tune_grid <- grid_max_entropy(
  parameters(mtry(range = c(1, p)), min_n(range = c(2, 40))),
  size = 20
)

# Tune using cv
library(doParallel)
registerDoParallel(cores = parallel::detectCores() - 1)

set.seed(20250401)
rf_tune <- tune_grid(
  rf_tune_wf,
  resamples = ts_cv_folds,
  grid = rf_tune_grid,
  metrics = metric_set(roc_auc)
)

# Visualize results
rf_tune %>%
  collect_metrics() %>%
  filter(.metric %in% c("roc_auc")) %>%
  ggplot(aes(x = mtry, min_n, y = mean, color = .metric)) +
  geom_point() +
  geom_line() +
  facet_wrap(~.metric, scales = "free_y") +
  theme_bw()

# Select the best model based on roc_auc
best_rf <- rf_tune |>
  select_by_one_std_err(mtry, metric = "roc_auc")

rf_final_wf <- finalize_workflow(rf_tune_wf, best_rf)
rf_final_wf

# Fit the finalized workflow on training data
rf_final_fit <- fit(rf_final_wf, data = data_train)

# Ensure 'won' is a factor with consistent levels
# Get class + probability predictions
rf_predictions <- bind_cols(
  predict(rf_final_fit, data_test, type = "prob"),   # gives .pred_Yes
  predict(rf_final_fit, data_test, type = "class"),  # gives .pred_class
  data_test %>% select(won)
)

rf_predictions <- rf_predictions %>%
  mutate(
    won = factor(won, levels = c("Yes", "No")),
    .pred_class = factor(.pred_class, levels = c("Yes", "No"))
  )

class_metrics(data = rf_predictions, truth = won, estimate = .pred_class, .pred_Yes)

# Confusion matrix
rf_predictions %>%
  conf_mat(truth = won, estimate = .pred_class)

# ROC Curve
rf_predictions %>%
  roc_curve(won, .pred_Yes, event_level = "first") %>%
  autoplot() +
  labs(title = "ROC Curve - Random Forest")

# Extract variable importance
vip_df <- rf_final_fit |>
  extract_fit_parsnip() |>
  vi()

vip_df %>%
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = paste0(round(Importance, 3))),
            hjust = -0.1, size = 3.2) +
  labs(
    title = "Top 20 Most Important Features",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal() +
  expand_limits(y = max(vip_df$Importance) * 1.1)  # add padding for labels


## Saving the first stage models
dir.create("saved_models")

# Save LASSO models
saveRDS(lasso_final, file = "saved_models/lasso_final_workflow.rds")
saveRDS(lasso_fit_final, file = "saved_models/lasso_last_fit.rds")

# Save XGBoost model
saveRDS(xgb_final_wf, file = "saved_models/xgb_final_workflow.rds")
saveRDS(xgb_final_fit, file = "saved_models/xgb_final_fit.rds")

# Save Random Forest model
saveRDS(rf_final_wf, file = "saved_models/rf_final_workflow.rds")
saveRDS(rf_final_fit, file = "saved_models/rf_last_fit.rds")

#### Now fitting the best model (LASSO) on the whole data to use for the second stage
lasso_final_full_fit <- lasso_final %>%
  fit(data = data_until_2023)  

# Save model to file
saveRDS(lasso_final_full_fit, file = "saved_models/lasso_final_full_fit.rds")

### SHAP on best performing tree model (rf)
# Recipe and preprocessing
rf_recipe <- 
  recipe(won ~ ., data = data_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, signing_month, signing_year, new_role = "id") %>%
  step_rm(has_role("id")) %>%  
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) 

final_recipe <- prep(rf_recipe, training = data_train, retain = TRUE)
train_baked <- bake(final_recipe, new_data = data_train)
test_baked  <- bake(final_recipe, new_data = data_test)

# Clean up and making sure columns are matched
common_vars <- intersect(colnames(train_baked), colnames(test_baked))
train_baked <- train_baked %>% select(all_of(common_vars))
test_baked  <- test_baked  %>% select(all_of(common_vars))

# Split features and labels
X_train <- train_baked %>% select(-won)
y_train <- train_baked$won
X_test  <- test_baked %>% select(-won)
y_test  <- test_baked$won

# Training the model
train_df <- bind_cols(X_train, won = y_train)
rf_model_shap <- ranger(won ~ ., data = train_df, probability = TRUE)

# Make sure X_test is a data.frame
X_test <- as.data.frame(X_test)

# Define prediction function
predict_function <- function(model, newdata) {
  predict(model, data = newdata)$predictions[, "Yes"]
}

# Create augmented test set
X_test_aug <- X_test %>%
  mutate(
    pred_prob = predict_function(rf_model_shap, .),
    true_label = y_test,
    correct = (pred_prob > 0.5) == (true_label == "Yes")
  )

# Identify rows of interest
most_confident_correct <- X_test_aug %>%
  filter(correct) %>%
  arrange(desc(abs(pred_prob - 0.5))) %>%
  dplyr::slice(1)

least_confident_correct <- X_test_aug %>%
  filter(correct) %>%
  arrange(abs(pred_prob - 0.5)) %>%
  dplyr::slice(1)

most_confident_wrong <- X_test_aug %>%
  filter(!correct) %>%
  arrange(desc(abs(pred_prob - 0.5))) %>%
  dplyr::slice(1)

# fastshap explanation function
fastshap_explain <- function(row_df, i, title) {
  X_df <- as.data.frame(X_test)
  row_df <- as.data.frame(row_df)
  
  shap <- fastshap::explain(
    object = rf_model_shap,
    X = X_df,
    pred_wrapper = predict_function,
    nsim = 100,
    newdata = row_df
  )
  
  df <- data.frame(
    Feature = names(shap[i, ]),
    SHAP = as.numeric(shap[i, ]),
    Value = round(as.numeric(row_df[i, ]), 3)
  ) %>%
    arrange(desc(abs(SHAP))) %>%
    head(15)
  
  ggplot(df, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    geom_text(aes(label = Value), hjust = ifelse(df$SHAP > 0, -0.1, 1.1), size = 3) +
    labs(
      title = title,
      subtitle = "Top 15 features by SHAP impact",
      x = "Feature",
      y = "SHAP value"
    ) +
    theme_minimal()
}

# Plots
plot1 <- fastshap_explain(most_confident_correct[, names(X_test)], 1, "Most Confident Correct Prediction")
plot2 <- fastshap_explain(least_confident_correct[, names(X_test)], 1, "Least Confident Correct Prediction")
plot3 <- fastshap_explain(most_confident_wrong[, names(X_test)], 1, "Most Confident Wrong Prediction")

print(plot1)
print(plot2)
print(plot3)


######### SECOND STAGE OF TWO STAGE MODEL ###########
# Filter data for second stage
data_with_preds <- data_until_2023 %>%
  mutate(won_pred = predict(lasso_final_full_fit, new_data = data_until_2023)$.pred_class) %>%
  filter(won_pred == "Yes")

data_with_preds <- data_with_preds %>%
  mutate(date_proposal_submitted = as.Date(date_proposal_submitted),
         contract_signing_date = as.Date(contract_signing_date))

unique(data_with_preds$won_pred)

### Lasso regularized regression
# Split data
set.seed(123)
revenue_train <- data_with_preds %>% filter(lubridate::year(date_proposal_submitted) <= 2018)
revenue_test  <- data_with_preds %>% filter(lubridate::year(date_proposal_submitted) >= 2019)

table(revenue_train$won)
table(revenue_test$won)

unique(revenue_train$year)
unique(revenue_test$year)

# Make sure data is ordered by time
revenue_train <- revenue_train %>% arrange(date_proposal_submitted)

# Set up time series CV
set.seed(123)
cv_folds_rev <- time_series_cv(
  data = revenue_train,
  date_var = date_proposal_submitted,
  initial = "3 years",    
  assess = "1 year",     
  skip = "1 year",         
  cumulative = TRUE,
  slice_limit = 10
)

length(cv_folds_rev$splits)

# Visualizing it
# Convert folds into a tibble for plotting
cv_folds_viz <- cv_folds_rev %>%
  mutate(
    split_id = row_number(),
    train_start = map_dbl(splits, ~ min(as.numeric(training(.x)$date_proposal_submitted))),
    train_end   = map_dbl(splits, ~ max(as.numeric(training(.x)$date_proposal_submitted))),
    test_start  = map_dbl(splits, ~ min(as.numeric(testing(.x)$date_proposal_submitted))),
    test_end    = map_dbl(splits, ~ max(as.numeric(testing(.x)$date_proposal_submitted)))
  ) %>%
  pivot_longer(cols = c(train_start, train_end, test_start, test_end),
               names_to = c("set", ".value"),
               names_sep = "_")

# Convert numeric dates back to Date type
cv_folds_viz <- cv_folds_viz %>%
  mutate(
    start = as.Date(start, origin = "1970-01-01"),
    end = as.Date(end, origin = "1970-01-01")
  )

# Plot
ggplot(cv_folds_viz, aes(x = start, xend = end, y = split_id, yend = split_id, color = set)) +
  geom_segment(size = 3) +
  scale_color_manual(values = c("train" = "blue", "test" = "red")) +
  labs(
    title = "Time Series Cross-Validation Folds",
    x = "Date (Proposal Submitted)",
    y = "Fold Index",
    color = "Data Set"
  ) +
  theme_minimal()

# Recipe with log-transform + feature engineering
revenue_recipe <- recipe(revenue ~ ., data = revenue_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, won, won_pred, signing_month, signing_year, new_role = "id") %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_novel(all_nominal_predictors()) %>%
  step_interact(~ donor_country:year + 
                  donor_type:year +
                  donor_country:lead_country +
                  co_financing:competitive +
                  donor_type:sector +
                  proposal_month:competitive +
                  lead_country_gdp:lead_fragility_index) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Lasso logistic model
lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Workflow
lasso_wf <- workflow() %>%
  add_model(lasso_model) %>%
  add_recipe(revenue_recipe)

# Auto tuning grid
grid <- grid_space_filling(penalty(), size = 20)

# Tune with CV
lasso_tune <- tune_grid(
  lasso_wf,
  resamples = cv_folds_rev,
  grid = grid,
  metrics = yardstick::metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq)
)

# Select best models
lasso_best <- lasso_tune %>% select_by_one_std_err(metric = "rmse", desc(penalty))

# Finalize workflows
lasso_final <- finalize_workflow(lasso_wf, lasso_best)

lasso_fit_final <- fit(lasso_final, data = revenue_train)

# Predict on the test set (keep 'revenue' in new_data)
lasso_predictions <- predict(lasso_fit_final, new_data = revenue_test) %>%
  bind_cols(revenue_test %>% select(revenue)) %>%
  mutate(
    .pred_real = .pred,
    revenue_real = revenue
  )

real_metrics <- yardstick::metric_set(yardstick::rmse, yardstick::mae, yardstick::rsq)

real_metrics(data = lasso_predictions, truth = revenue, estimate = .pred_real)

# Coefficients
lasso_fit_final %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  print(n = 100)

# Augment and compute residuals on original scale
augmented_df <- augment(lasso_fit_final, new_data = revenue_test) %>%
  mutate(
    .resid = revenue - .pred
  )

# Plot residuals vs fitted values
ggplot(augmented_df, aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted, LASSO",
    x = "Predicted Revenue",
    y = "Residual"
  ) +
  theme_minimal() + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)


# Coefficient plot
lasso_fit_final %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0, term != "(Intercept)") %>%
  mutate(term = reorder(term, estimate)) %>%
  head(n=20)%>%
  ggplot(aes(x = term, y = estimate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Non-Zero Coefficients from Lasso Regression",
    x = "Feature",
    y = "Coefficient Estimate"
  ) +
  theme_minimal() 

# Saving the models
saveRDS(lasso_fit_final, file = "saved_models/lasso_2_last_fit_external.rds")
saveRDS(lasso_final, file = "saved_models/lasso_2_final_workflow_external.rds")

#### RANDOM FOREST ####
revenue_recipe <- recipe(revenue ~ ., data = revenue_train) %>%
  update_role(id, lead_country_code, donor_country_code, won_pred, signing_month, signing_year, new_role = "id") %>%
  step_date(date_proposal_submitted,
            features = c("month", "dow", "year")) %>%
  step_rm(date_proposal_submitted) %>%  
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%  
  step_dummy(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Print the recipe
print(revenue_recipe)

# Define RF model
rf_2_model <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "permutation")

# Workflow
rf_2_wf <- workflow() %>%
  add_recipe(revenue_recipe) %>%
  add_model(rf_2_model)

# Tuning Grid
p <- revenue_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>%
  select(-revenue) %>%
  ncol()

rf_2_grid <- grid_max_entropy(
  parameters(mtry(range = c(1, p)), min_n(range = c(2, 40))),
  size = 20
)

# Parallel setup
registerDoParallel(cores = parallel::detectCores() - 1)

# Tune the model
set.seed(20250425)
rf_2_tune <- tune_grid(
  rf_2_wf,
  resamples = cv_folds_rev,
  grid = rf_2_grid,
  metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
)

# Plot tuning results
autoplot(rf_2_tune) + theme_bw()

# Select best model using 1-SE rule
rf_2_best <- rf_2_tune %>%
  select_by_one_std_err(metric = "rmse", desc(mtry))

# Finalize and fit on test set
rf_2_final_wf <- finalize_workflow(rf_2_wf, rf_2_best)

metrics <- metric_set(rmse, mae, rsq)

rf_2_final_fit <- fit(rf_2_final_wf, data = revenue_train)

# Predict on the test set (keep 'revenue' in new_data)
rf_2_predictions <- predict(rf_2_final_fit, new_data = revenue_test) %>%
  bind_cols(revenue_test %>% select(revenue)) %>%
  mutate(
    .pred_real = .pred,
    revenue_real = revenue
  )

real_metrics <- metric_set(rmse, mae, rsq)

real_metrics(data = rf_2_predictions, truth = revenue, estimate = .pred_real)


# VI plot
rf_2_final_fit %>%
  extract_fit_parsnip() %>%
  vi() %>%
  slice_max(Importance, n = 20) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Important Features - RF Regression") +
  theme_minimal()

# Residuals vs fitted plot
# Augment and compute residuals on original scale
augmented_df <- augment(rf_2_final_fit, new_data = revenue_test) %>%
  mutate(
    .resid = revenue - .pred
  )

# Plot residuals vs fitted values
ggplot(augmented_df, aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted, RF Regression",
    x = "Predicted Revenue",
    y = "Residual"
  ) +
  theme_minimal() + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)


# Saving the models
saveRDS(rf_2_final_fit, file = "saved_models/rf_2_last_fit_external.rds")
saveRDS(rf_2_final_wf, file = "saved_models/rf_2_final_wf_external.rds")

# Checking 
rf_last_fit_check <- readRDS("saved_models/rf_2_last_fit_external.rds")

rf_2_last_fit %>% collect_metrics()
rf_last_fit_check %>% collect_metrics()

# Fitting on whole data set
# Fit the finalized workflow on full data_with_preds
rf_2_final_full_fit <- rf_2_final_wf %>%
  fit(data = data_with_preds)

# Save the full model for future use
saveRDS(rf_2_final_full_fit, file = "rf_2_final_full_fit.rds")

### Boosting
revenue_recipe <- recipe(revenue ~ ., data = revenue_train) %>%
  update_role(id, contract_signing_date, lead_country_code, 
              donor_country_code, won_pred, won, signing_month, signing_year, new_role = "id") %>%
  step_rm(has_role("id")) %>%  # remove all ID columns
  step_date(date_proposal_submitted, features = c("month", "dow", "year")) %>%
  step_rm(date_proposal_submitted) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Tuning main 3 hyperparameters:
xgb_model_tune <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    stop_iter = 500
  ) |>
  set_mode("regression") |>
  set_engine("xgboost")

# Defining the workflow:
xgb_tune_wf <-
  workflow() |>
  add_recipe(revenue_recipe) |>
  add_model(xgb_model_tune)
xgb_tune_wf

# Using parallel processing to speed up computation:
num_cores <- parallel::detectCores()
num_cores

doParallel::registerDoParallel(cores = num_cores - 1L)

# Doing adaptive tuning:
# Expanded parameter tuning space
tuning_params <-
  parameters(
    trees(range = c(0, 10000)),
    learn_rate(range = c(-5, -1)),
    tree_depth(range = c(1, 3))
  )

# Now generate the grid
set.seed(8504)
xgb_initial_grid <- grid_max_entropy(
  tuning_params,
  size = 10
)

set.seed(123)
xgb_initial_fit <- tune_grid(
  xgb_tune_wf,
  resamples = cv_folds_rev,
  grid = xgb_initial_grid,  
  metrics = metric_set(rmse, rsq, mae)
)

# Bayesian optimization
set.seed(1)
xgb_tune_res <- tune_bayes(
  xgb_tune_wf,
  resamples = cv_folds_rev,
  initial = xgb_initial_fit,
  param_info = tuning_params,
  iter = 30,                   
  metrics = metric_set(rmse, rsq, mae)
)

# Look at updated tuning results:
xgb_tune_res |>
  autoplot(metric = "rmse")

# Next, we will run this procedure again. However, this time we will pass in 
# xgb_tune_res_1 as the value of the named argument initial. This tells tune_bayes 
# to leverage the information in the previous tuning results (in xgb_tune_res_1) 
# when choosing which hyperparameters to try first.

set.seed(2)
xgb_tune_res_2 <- tune_bayes(
  xgb_tune_wf,
  resamples = cv_folds_rev,
  iter = 5,
  initial = xgb_tune_res,
  param_info = tuning_params,
  metrics = metric_set(rmse, rsq, mae)
)


# Keep repeating tuning procedure:
set.seed(3)
xgb_tune_res_3 <- tune_bayes(
  xgb_tune_wf,
  resamples = cv_folds_rev,
  iter = 5,
  initial = xgb_tune_res_2,
  param_info = tuning_params,
  metrics = metric_set(rmse, rsq, mae, mase)
)


# Keep repeating tuning procedure:
set.seed(4)
xgb_tune_res_4 <- tune_bayes(
  xgb_tune_wf,
  resamples = cv_folds_rev,
  iter = 5,
  initial = xgb_tune_res_3,
  param_info = tuning_params,
  metrics = metric_set(rmse, rsq, mae, mase)
)

# Extracting the metrics
xgb_tune_metrics <-
  xgb_tune_res_4 |>
  collect_metrics()
xgb_tune_metrics


# Visualizing three hyperparameters:
xgb_tune_metrics %>%
  filter(.metric == "rmse") %>%
  filter(mean >= median(mean)) %>% # only show the best options
  ggplot(aes(
    x = trees, y = learn_rate,
    shape = factor(tree_depth),
    fill = mean
  )) +
  geom_point(size = 3) +
  scale_fill_gradient(low = "white", high = "#D55E00") +
  theme_bw() +
  labs(x = "Trees", y = "Learning rate", shape = "Max tree depth", fill = "RMSE")

# Find the best RMSE (smallest mean)
xgb_best_rmse <- xgb_tune_metrics %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice_head(n = 1)

# Save the mean + std_err of the best model
best_rmse_mean <- xgb_best_rmse$mean
best_rmse_se <- xgb_best_rmse$std_err

# Apply the 1SE rule:
xgb_1se_candidates <- xgb_tune_metrics %>%
  filter(.metric == "rmse") %>%
  filter(mean <= (best_rmse_mean + best_rmse_se))

# From the candidates, pick the simplest model
xgb_selected_model <- xgb_1se_candidates %>%
  arrange(tree_depth, desc(learn_rate)) %>%  # prefer simpler trees and faster learn_rate
  slice_head(n = 1)


# Finalize the workflow
xgb_final_wf <-
  xgb_tune_wf |>
  finalize_workflow(xgb_selected_model)
xgb_final_wf

# Fitting on set
xgb_final_fit <- fit(xgb_final_wf, data = revenue_train)


# Predict on the test set (keep 'revenue' in new_data)
xgb_predictions <- predict(xgb_final_fit, new_data = revenue_test) %>%
  bind_cols(revenue_test %>% select(revenue)) %>%
  mutate(
    .pred_real = .pred,
    revenue_real = revenue
  )

real_metrics <- metric_set(rmse, mae, rsq)

real_metrics(data = xgb_predictions, truth = revenue, estimate = .pred_real)

# Coefficients
xgb_final_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  
  
  # Augment and compute residuals on original scale
  augmented_df <- augment(xgb_final_fit, new_data = revenue_test) %>%
  mutate(
    .resid = revenue - .pred
  )

ggplot(augmented_df, aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted, XGBoost Regression",
    x = "Predicted Revenue",
    y = "Residual"
  ) +
  theme_minimal() + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)


# vi plot
# Extract variable importance
vip_df_xgb <- xgb_final_fit |>
  extract_fit_parsnip() |>
  vi()

# Plot top 20 features
vip_df_xgb %>%
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = paste0(round(Importance, 3))),
            hjust = -0.1, size = 3.2) +
  labs(
    title = "Top 20 Most Important Features - XGBoost",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal() +
  expand_limits(y = max(vip_df_xgb$Importance) * 1.1)  # add padding for labels


# saving the model
saveRDS(xgb_final_fit, file = "xgb_2_final_fit.rds")
saveRDS(xgb_final_wf, file = "xgb_2_final_wf.rds")

### Fitting the best performing regression model (LASSO) on whole data
data_until_2024 <- read_csv("data_clean_finally.csv")

classifier_final_fit <- readRDS("saved_models/lasso_final_full_fit.rds")
lasso_2_final_workflow <- readRDS("saved_models/lasso_2_final_workflow_external.rds")

# Get 2024 data from full dataset
data_2024 <- data_until_2024 %>%
  filter(lubridate::year(date_proposal_submitted) == 2024)

# Ensure the date column is in Date format
data_2024 <- data_2024 %>%
  mutate(date_proposal_submitted = as.Date(date_proposal_submitted))

data_2024 <- data_2024 %>%
  mutate(contract_signing_date = as.character(contract_signing_date))

# Apply first-stage classifier to predict which 2024 proposals are likely to be "won"
first_stage_preds <- predict(classifier_final_fit, new_data = data_2024, type = "class")

# Bind predictions to data_2024 and filter for predicted "Yes"
data_2024_with_preds <- data_2024 %>%
  mutate(won_pred = first_stage_preds$.pred_class) %>%
  filter(won_pred == "Yes")

# Fit the final LASSO workflow on the full training + test data (until 2023)
lasso_2_fit_all <- fit(lasso_2_final_workflow, data = data_with_preds)

# Predict revenue using finalized second-stage LASSO model
data_2024_with_preds <- data_2024_with_preds %>%
  mutate(contract_signing_date = as.Date(contract_signing_date))

data_2024_with_preds <- data_2024_with_preds %>%
  mutate(date_proposal_submitted = as.Date(date_proposal_submitted))

revenue_preds_2024 <- predict(lasso_2_fit_all, new_data = data_2024_with_preds) %>%
  bind_cols(data_2024_with_preds %>% select(id, donor_country, revenue, contract_signing_date))  # add relevant cols

# View or export results
head(revenue_preds_2024)

# Visualizing and summarizing
actual_2024 <- data_2024_with_preds %>%
  filter(!is.na(revenue)) %>%
  select(id, revenue) %>%
  rename(actual_revenue = revenue)

comparison_df <- revenue_preds_2024 %>%
  rename(predicted_revenue = .pred) %>%
  left_join(actual_2024, by = "id")

summary(comparison_df$predicted_revenue)
summary(comparison_df$actual_revenue)

metric_set(mae, rmse)(comparison_df, truth = actual_revenue, estimate = predicted_revenue)

ggplot(comparison_df, aes(x = actual_revenue, y = predicted_revenue)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual Revenue (2024) - External Variables Included",
       x = "Actual Revenue",
       y = "Predicted Revenue") +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) 

# Seeing monthly data
monthly_preds <- revenue_preds_2024 %>%
  mutate(signing_month = lubridate::floor_date(as.Date(contract_signing_date), "month")) %>%
  filter(lubridate::year(signing_month) == 2024) %>%   # keep only 2024
  group_by(signing_month) %>%
  summarise(
    total_predicted_revenue = sum(.pred, na.rm = TRUE),
    n_proposals = n()
  )

monthly_actuals <- data_2024 %>%
  filter(!is.na(revenue)) %>%
  mutate(signing_month = lubridate::floor_date(as.Date(contract_signing_date), "month")) %>%
  group_by(signing_month) %>%
  summarise(
    total_actual_revenue = sum(revenue, na.rm = TRUE)
  )

comparison_df <- left_join(monthly_preds, monthly_actuals, by = "signing_month")

ggplot(comparison_df, aes(x = signing_month)) +
  geom_line(aes(y = total_predicted_revenue), color = "blue", size = 1.2) +
  geom_line(aes(y = total_actual_revenue), color = "red", size = 1.2, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Revenue by Month (2024)",
    x = "Month",
    y = "Revenue (USD)",
    color = "Legend"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Filter and match predicted and actuals
comparison_df <- revenue_preds_2024 %>%
  filter(!is.na(revenue)) %>%  
  mutate(
    .pred_real = .pred  
  )

# Compute RMSE and MAE
revenue_metrics_2024 <- metric_set(rmse, mae)

revenue_metrics_2024(data = comparison_df, truth = revenue, estimate = .pred_real)


### SHAP for best performing tree model (XGB)
# Extract the fitted model
xgb_model <- extract_fit_parsnip(xgb_final_fit)

# Prepare the data
prepped_recipe <- prep(revenue_recipe)
baked_train <- bake(prepped_recipe, new_data = revenue_train)
baked_test  <- bake(prepped_recipe, new_data = revenue_test)

X_train <- baked_train %>% select(-revenue) %>% as.data.frame()
X_test  <- baked_test %>% select(-revenue) %>% as.data.frame()

# Define prediction wrapper
xgb_predict_wrapper <- function(object, newdata) {
  predict(object, new_data = newdata) %>% pull(.pred)
}

# Compute SHAP values
library(fastshap)

set.seed(123)
xgb_shap_values <- fastshap::explain(
  object = xgb_model,
  X = X_train,
  newdata = X_test,
  pred_wrapper = xgb_predict_wrapper,
  nsim = 100
)

# Get SHAP values for first observation
shap_row <- xgb_shap_values[1, ]
shap_plot_df <- tibble(
  feature = names(shap_row),
  shap_value = as.numeric(shap_row)
) %>%
  arrange(desc(abs(shap_value))) %>%
  slice_head(n = 20)

# Plot
ggplot(shap_plot_df, aes(x = reorder(feature, shap_value), y = shap_value)) +
  geom_col(fill = ifelse(shap_plot_df$shap_value > 0, "steelblue", "tomato")) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 20 SHAP Values - Observation 1",
    x = "Feature",
    y = "SHAP Value"
  )

# Computing for each revenue group
X_test_labeled <- X_test %>%
  mutate(revenue = revenue_test) %>%
  mutate(revenue_group = ifelse(revenue > 5e6, ">5M", "<=5M"))

shap_long <- as.data.frame(xgb_shap_values) %>%
  mutate(revenue_group = X_test_labeled$revenue_group) %>%
  pivot_longer(
    cols = -revenue_group,
    names_to = "feature",
    values_to = "shap_value"
  )

shap_summary <- shap_long %>%
  group_by(revenue_group, feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE), .groups = "drop")

shap_long <- xgb_shap_values %>%
  as_tibble() %>%
  mutate(obs_id = row_number()) %>%
  pivot_longer(
    -obs_id,
    names_to = "feature",
    values_to = "shap_value"
  ) %>%
  left_join(tibble(obs_id = 1:nrow(X_test), revenue = revenue_test$revenue), by = "obs_id")


# Group revenue
shap_summary_top <- shap_long %>%
  mutate(revenue_group = ifelse(revenue > 5e6, ">5M", "≤5M")) %>%
  group_by(revenue_group, feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE), .groups = "drop")

# Plot
ggplot(shap_summary_top, aes(
  x = reorder_within(feature, mean_abs_shap, revenue_group),
  y = mean_abs_shap,
  fill = revenue_group
)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Mean Absolute SHAP Value by Revenue Group",
    x = "Feature",
    y = "Mean |SHAP|"
  ) +
  theme_minimal()

# Looks bad so gonna try again
# Limit to top 20 features across all groups
top_features <- shap_summary_top %>%
  group_by(feature) %>%
  summarise(total_mean_abs = sum(mean_abs_shap)) %>%
  top_n(20, total_mean_abs) %>%
  pull(feature)

# Filter to top features
shap_summary_top_filtered <- shap_summary_top %>%
  filter(feature %in% top_features)

# Plot
ggplot(shap_summary_top_filtered, aes(
  x = reorder_within(feature, mean_abs_shap, revenue_group),
  y = mean_abs_shap,
  fill = revenue_group
)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top 20 Features by Mean Absolute SHAP",
    x = "Feature",
    y = "Mean |SHAP|"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

#### VISUAL COMPARING THE SECOND STAGE MODELS
model_comparison <- tibble(
  Model = c("LASSO", "Random Forest", "XGBoost"),
  RMSE_real = c(7180875, 7796408, 7300576),
  MAE_real = c(3075368, 3075368, 3434475)
)

model_comparison %>%
  pivot_longer(cols = c(RMSE_real, MAE_real), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Comparison of ML Models on Real Revenue Scale",
    y = "Error Value ($)",
    x = "Model"
  )

# Create revenue bins (adjust bin ranges as needed)
lasso_predictions <- lasso_predictions %>%
  mutate(revenue_bin = case_when(
    revenue <= 100000 ~ "<=100K",
    revenue <= 500000 ~ "100K-500K",
    revenue <= 1000000 ~ "500K-1M",
    revenue <= 5000000 ~ "1M-5M",
    TRUE ~ ">5M"
  ))

# Repeat for rf and xgb if needed, or bind all in one frame

# Compute metrics by bin
lasso_stratified <- lasso_predictions %>%
  group_by(revenue_bin) %>%
  summarise(
    rmse = rmse_vec(revenue, .pred_real),
    mae = mae_vec(revenue, .pred_real)
  )

# Set revenue_bin as an ordered factor
lasso_stratified <- lasso_stratified %>%
  mutate(revenue_bin = factor(revenue_bin, levels = c("<=100K", "100K-500K", "500K-1M", "1M-5M", ">5M")))

# Plot MAE with ordered x-axis
ggplot(lasso_stratified, aes(x = revenue_bin, y = mae)) +
  geom_col(fill = "darkgreen") +
  labs(title = "MAE by Revenue Bin - LASSO", x = "Revenue Bin", y = "MAE") +
  theme_minimal() +
  scale_y_continuous(labels=comma)

# for xgb
xgb_stratified <- xgb_predictions %>%
  mutate(revenue_bin = case_when(
    revenue <= 100000 ~ "<=100K",
    revenue <= 500000 ~ "100K-500K",
    revenue <= 1000000 ~ "500K-1M",
    revenue <= 5000000 ~ "1M-5M",
    TRUE ~ ">5M"
  )) %>%
  group_by(revenue_bin) %>%
  summarise(
    mae = mae_vec(revenue, .pred_real),
    rmse = rmse_vec(revenue, .pred_real),
    .groups = "drop"
  ) %>%
  mutate(revenue_bin = factor(revenue_bin, levels = c("<=100K", "100K-500K", "500K-1M", "1M-5M", ">5M")))

# Now plot using the correct dataset
ggplot(xgb_stratified, aes(x = revenue_bin, y = mae)) +
  geom_col(fill = "blue") +
  labs(title = "MAE by Revenue Bin - XGBoost", x = "Revenue Bin", y = "MAE") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# for rf
rf_stratified <- rf_2_predictions %>%
  mutate(revenue_bin = case_when(
    revenue <= 100000 ~ "<=100K",
    revenue <= 500000 ~ "100K-500K",
    revenue <= 1000000 ~ "500K-1M",
    revenue <= 5000000 ~ "1M-5M",
    TRUE ~ ">5M"
  )) %>%
  group_by(revenue_bin) %>%
  summarise(
    mae = mae_vec(revenue, .pred_real),
    rmse = rmse_vec(revenue, .pred_real),
    .groups = "drop"
  ) %>%
  mutate(revenue_bin = factor(revenue_bin, levels = c("<=100K", "100K-500K", "500K-1M", "1M-5M", ">5M")))

ggplot(rf_stratified, aes(x = revenue_bin, y = mae)) +
  geom_col(fill = "salmon") +
  labs(title = "MAE by Revenue Bin - RF", x = "Revenue Bin", y = "MAE") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# More plots
# For Random Forest
rf_preds <- collect_predictions(rf_2_last_fit)

# For XGBoost
xgb_preds <- collect_predictions(xgb_final_fit)

# For LASSO
lasso_preds <- collect_predictions(lasso_last_fit)

# Random Forest
rf_2_predictions %>%
  mutate(
    true_revenue = (revenue),
    predicted_revenue = (.pred) 
  ) %>%
  filter(true_revenue > 0, predicted_revenue > 0) %>%   # <- IMPORTANT
  ggplot(aes(x = true_revenue, y = predicted_revenue)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  scale_x_log10(labels = scales::dollar_format()) +
  scale_y_log10(labels = scales::dollar_format()) +
  theme_minimal() +
  labs(
    title = "Random Forest Predictions vs True Revenue",
    x = "True Revenue",
    y = "Predicted Revenue"
  )

# XGBoost
xgb_predictions %>%
  mutate(
    true_revenue = (revenue),
    predicted_revenue = (.pred)
  ) %>%
  filter(true_revenue > 0, predicted_revenue > 0) %>%   # <- IMPORTANT
  ggplot(aes(x = true_revenue, y = predicted_revenue)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  scale_x_log10(labels = scales::dollar_format()) +
  scale_y_log10(labels = scales::dollar_format()) +
  theme_minimal() +
  labs(
    title = "XGB Predictions vs True Revenue",
    x = "True Revenue",
    y = "Predicted Revenue"
  )

# LASSO
lasso_predictions %>%
  mutate(
    true_revenue = (revenue),
    predicted_revenue = (.pred)
  ) %>%
  filter(true_revenue > 0, predicted_revenue > 0) %>%   # <- IMPORTANT
  ggplot(aes(x = true_revenue, y = predicted_revenue)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  scale_x_log10(labels = scales::dollar_format()) +
  scale_y_log10(labels = scales::dollar_format()) +
  theme_minimal() +
  labs(
    title = "LASSO Predictions vs True Revenue",
    x = "True Revenue",
    y = "Predicted Revenue"
  )


### Same thing without external indicators
# getting external indicators 
colnames(data_until_2023)
external_vars <- c(
  "lead_gov_spending", "lead_oda_received","lead_gov_effectiveness","lead_corruption_control",
  "lead_political_stability","lead_electoral_democracy", "lead_liberal_democracy",
  "lead_agri_employment", "lead_fragility_index", "lead_security_threats",
  "lead_economic_decline", "lead_public_services", "lead_human_rights",
  "donor_gov_spending", "donor_gov_effectiveness", "donor_corruption_control",
  "donor_political_stability", "donor_electoral_democracy", "donor_liberal_democracy",
  "donor_human_rights", "donor_country_gdp", "lead_country_gdp", "donor_country_gdp_percapita",
  "lead_country_gdp_percapita", "donor_country_inflation", "lead_country_inflation",
  "donor_countryconversion_rate", "lead_countryconversion_rate", "oda_per_capita")

# Use the same split as before (data_train) and data_test with time_series_cv

# With downsampling
classification_recipe <- recipe(won ~ ., data = data_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, signing_month, 
              signing_year, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_downsample(won)

#Internal only
classification_recipe_internal_only <- 
  classification_recipe %>%
  step_rm(all_of(external_vars))


# Lasso logistic model
lasso_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# Workflow
lasso_wf <- workflow() %>%
  add_model(lasso_model) %>%
  add_recipe(classification_recipe_internal_only)

# Auto tuning grid
grid <- grid_space_filling(penalty(), size = 20)

# Tune with CV
lasso_tune <- tune_grid(
  lasso_wf,
  resamples = ts_cv_folds,
  grid = grid,
  metrics = metric_set(bal_accuracy, accuracy, precision, recall, roc_auc)
)

# 9. Select best models
lasso_best <- lasso_tune %>% select_by_one_std_err(metric = "roc_auc", desc(penalty))

# 10. Finalize workflows
lasso_final <- finalize_workflow(lasso_wf, lasso_best)


# 11. Fit last models
class_metrics <- metric_set(
  accuracy, bal_accuracy, precision, recall, f_meas, roc_auc
)


# Fit the finalized workflow on training data
lasso_fit_final <- fit(lasso_final, data = data_train)

# Ensure 'won' is a factor with consistent levels
# Get class + probability predictions
lasso_predictions <- bind_cols(
  predict(lasso_fit_final, data_test, type = "prob"),   # gives .pred_Yes
  predict(lasso_fit_final, data_test, type = "class"),  # gives .pred_class
  data_test %>% select(won)
)

lasso_predictions <- lasso_predictions %>%
  mutate(
    won = factor(won, levels = c("Yes", "No")),
    .pred_class = factor(.pred_class, levels = c("Yes", "No"))
  )

class_metrics(data = lasso_predictions, truth = won, estimate = .pred_class, .pred_Yes)


# Extract fitted model from last_fit
lasso_fit_final %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  print(n=70)


# Confusion matrix
lasso_predictions %>%
  conf_mat(truth = won, estimate = .pred_class)

# ROC curve data
roc_curve(lasso_predictions, truth = won, .pred_Yes, event_level = "first") %>%
  autoplot() +
  labs(title = "ROC Curve - LASSO Model")

# Coefficient plot
lasso_fit_final %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0, term != "(Intercept)") %>%
  mutate(abs_estimate = abs(estimate)) %>%
  slice_max(order_by = abs_estimate, n =20) %>%
  ggplot(aes(x = term, y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Non-Zero Coefficients from Lasso Regression",
    x = "Feature",
    y = "Coefficient Estimate"
  ) +
  theme_minimal()

# saving the model
saveRDS(lasso_fit_final, file = "lasso_last_fit_internal.rds")
saveRDS(lasso_final, file = "lasso_final_wf_internal.rds")

# Checking 
lasso_final_check <- readRDS("lasso_final_wf_internal.rds")
lasso_final
lasso_final_check

### XGB INTERNAL ONLY
# Create recipe with downsampling
xgb_recipe_internal_only <- 
  recipe(won ~ ., data = data_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, signing_month, signing_year,
              new_role = "id") %>%
  step_rm(all_of(external_vars)) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_downsample(won)

# Tuning main 3 hyperparameters:
xgb_model_tune <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    stop_iter = 500
  ) |>
  set_mode("classification") |>
  set_engine("xgboost")


# Defining the workflow:
xgb_tune_wf <-
  workflow() |>
  add_recipe(xgb_recipe_internal_only) |>
  add_model(xgb_model_tune)
xgb_tune_wf


# Setting up class metrics
class_metrics <- metric_set(
  accuracy, bal_accuracy, precision, recall, f_meas, roc_auc
)

# Using parallel processing to speed up computation:
num_cores <- parallel::detectCores()
num_cores


doParallel::registerDoParallel(cores = num_cores - 1L)


# Setting up a random tuning grid:
set.seed(8504)
grid_max_entropy(trees(range = c(0, 10000)),
                 learn_rate(range = c(-2, -1)),
                 tree_depth(),
                 size = 20
)


# Doing adaptive tuning:
tuning_params <-
  parameters(
    trees(range = c(0, 10000)),
    learn_rate(range = c(-5, -1)),
    tree_depth(range = c(1, 3))
  )


set.seed(1)
xgb_tune_res_1 <- tune_bayes(
  xgb_tune_wf,
  resamples = ts_cv_folds,
  iter = 1,
  param_info = tuning_params,
  metrics = class_metrics
)

# Looking at initial results:
xgb_tune_res_1 |>
  autoplot(metric = "roc_auc")

show_notes(.Last.tune.result)
# Next, we will run this procedure again. However, this time we will pass in 
# xgb_tune_res_1 as the value of the named argument initial. This tells tune_bayes 
# to leverage the information in the previous tuning results (in xgb_tune_res_1) 
# when choosing which hyperparameters to try first.

set.seed(2)
xgb_tune_res_2 <- # updated results
  tune_bayes(
    xgb_tune_wf,
    initial = xgb_tune_res_1, # previous results
    resamples = cv_folds,
    iter = 5, # num. new models to try
    param_info = tuning_params,
    metrics = class_metrics
  )

# Look at updated tuning results:
xgb_tune_res_2 |>
  autoplot(metric = "roc_auc")

# Keep repeating tuning procedure:
set.seed(3)
xgb_tune_res_3 <- # updated results
  tune_bayes(
    xgb_tune_wf,
    initial = xgb_tune_res_2, # previous results
    resamples = cv_folds,
    iter = 5,
    param_info = tuning_params,
    metrics = class_metrics
  )

# Autoplot
xgb_tune_res_3 |>
  autoplot(metric = "roc_auc") # PErformance is starting to level off

# Keep repeating tuning procedure:
set.seed(4)
xgb_tune_res_4 <- # updated results
  tune_bayes(
    xgb_tune_wf,
    initial = xgb_tune_res_3, # previous results
    resamples = cv_folds,
    iter = 5,
    param_info = tuning_params,
    metrics = class_metrics
  )

# Autoplot
xgb_tune_res_4 |>
  autoplot(metric = "roc_auc") # PErformance is starting to level off

# Extracting the metrics
xgb_tune_metrics <-
  xgb_tune_res_4 |>
  collect_metrics()
xgb_tune_metrics


# Visualizing three hyperparameters:
xgb_tune_metrics |>
  filter(.metric == "roc_auc") |>
  filter(mean >= median(mean)) |> # only show the best options
  ggplot(aes(
    x = trees, y = learn_rate,
    shape = factor(tree_depth),
    fill = mean
  )) +
  geom_point(size = 3) +
  scale_shape_manual(values = 21:23) +
  scale_fill_gradient(low = "white", high = "#D55E00") +
  theme_bw() +
  labs(x = "Trees", y = "Learning rate", shape = "Max tree depth", fill = "ROC-AUC")

# Doing 1SE procedure:
xgb_best_point <-
  xgb_tune_metrics |>
  filter(.metric == "roc_auc" & tree_depth == 2) |>
  arrange(desc(mean)) |>
  slice_head(n = 1)
xgb_best_roc_auc <- xgb_best_point$mean
xgb_selected_model <-
  xgb_tune_metrics |>
  filter(.metric == "roc_auc" & tree_depth == 2) |>
  filter(mean + std_err >= xgb_best_roc_auc) |>
  arrange(desc(learn_rate)) |>
  slice_head(n = 1)
xgb_selected_model


# Finalize the workflow
xgb_final_wf <-
  xgb_tune_wf |>
  finalize_workflow(xgb_selected_model)
xgb_final_wf

# Fit the finalized workflow on training data
xgb_final_fit <- fit(xgb_final_wf, data = data_train)

# Ensure 'won' is a factor with consistent levels
# Get class + probability predictions
xgb_predictions <- bind_cols(
  predict(xgb_final_fit, data_test, type = "prob"),   # gives .pred_Yes
  predict(xgb_final_fit, data_test, type = "class"),  # gives .pred_class
  data_test %>% select(won)
)

xgb_predictions <- xgb_predictions %>%
  mutate(
    won = factor(won, levels = c("Yes", "No")),
    .pred_class = factor(.pred_class, levels = c("Yes", "No"))
  )

class_metrics(data = xgb_predictions, truth = won, estimate = .pred_class, .pred_Yes)


# Extract fitted model from last_fit
xgb_predictions %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  print(n=70)


# Confusion matrix
xgb_predictions %>%
  conf_mat(truth = won, estimate = .pred_class)

# ROC curve data
roc_curve(xgb_predictions, truth = won, .pred_Yes, event_level = "first") %>%
  autoplot() +
  labs(title = "ROC Curve - XGB Model - Internal Only")

# See predictors
final_recipe <- prep(xgb_recipe, training = data_train, retain = TRUE)

baked_data <- bake(final_recipe, new_data = NULL)

selected_features <- baked_data %>%
  select(-won) %>%      
  colnames()

original_vars <- colnames(data_train) %>% setdiff("won")
removed_vars <- setdiff(original_vars, selected_features)

# Extract variable importance
vip_df_xgb <- xgb_final_fit |>
  extract_fit_parsnip() |>
  vi()

# Plot top 20 features
vip_df_xgb %>%
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = paste0(round(Importance, 3))),
            hjust = -0.1, size = 3.2) +
  labs(
    title = "Top 20 Most Important Features - XGBoost (Internal Only)",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal() +
  expand_limits(y = max(vip_df_xgb$Importance) * 1.1)  # add padding for labels


# saving the model
saveRDS(xgb_final_fit, file = "xgb_final_fit_internal.rds")
saveRDS(xgb_final_wf, file = "xgb_final_wf_internal.rds")

# Checking 
xgb_final_wf_check <- readRDS("xgb_final_wf_internal.rds")

xgb_final_wf_check
xgb_final_wf


#### RANDOM FOREST INTERNAL ONLY
# Create recipe with SMOTE
rf_recipe_internal <- 
  recipe(won ~ ., data = data_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, signing_month, signing_year, new_role = "id") %>%
  step_rm(all_of(external_vars)) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_smote(won, over_ratio = 1)

# Determining thenumber of p
# Prep the recipe
rf_prep <- prep(rf_recipe_internal)

# Bake the prepped data
baked_rf_data <- bake(rf_prep, new_data = NULL)

# Calculate number of predictors (excluding outcome)
p <- ncol(baked_rf_data) - 1  # subtract 1 for 'won'

# Print p
print(p)

# Class metrics + tuning grid
rf_model <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")

rf_tune_wf <-
  workflow() |>
  add_recipe(rf_recipe_internal) |>
  add_model(rf_model)

rf_tune_wf

# Setting the class metrics:
class_metrics <- metric_set(
  accuracy, bal_accuracy, precision,
  recall, f_meas, roc_auc
)


# Tune from 1 to total number of predictors after preprocessing
# Smaller tuning grid
rf_tune_grid <- grid_max_entropy(
  parameters(mtry(range = c(1, p)), min_n(range = c(2, 40))),
  size = 20
)

# Tune using cv
library(doParallel)
registerDoParallel(cores = parallel::detectCores() - 1)

set.seed(20250401)
rf_tune <- tune_grid(
  rf_tune_wf,
  resamples = ts_cv_folds,
  grid = rf_tune_grid,
  metrics = metric_set(roc_auc)
)

# Visualize results
rf_tune %>%
  collect_metrics() %>%
  filter(.metric %in% c("roc_auc")) %>%
  ggplot(aes(x = mtry, min_n, y = mean, color = .metric)) +
  geom_point() +
  geom_line() +
  facet_wrap(~.metric, scales = "free_y") +
  theme_bw()

# Select the best model based on roc_auc
best_rf <- rf_tune |>
  select_by_one_std_err(mtry, metric = "roc_auc")

rf_final_wf <- finalize_workflow(rf_tune_wf, best_rf)
rf_final_wf

# Evaluate on test set
rf_final_fit <- fit(rf_final_wf, data = data_train)

# Ensure 'won' is a factor with consistent levels
# Get class + probability predictions
rf_predictions <- bind_cols(
  predict(rf_final_fit, data_test, type = "prob"),   
  predict(rf_final_fit, data_test, type = "class"), 
  data_test %>% select(won)
)

rf_predictions <- rf_predictions %>%
  mutate(
    won = factor(won, levels = c("Yes", "No")),
    .pred_class = factor(.pred_class, levels = c("Yes", "No"))
  )

class_metrics(data = rf_predictions, truth = won, estimate = .pred_class, .pred_Yes)


# Extract fitted model from last_fit
rf_predictions %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  print(n=70)


# Confusion matrix
rf_predictions %>%
  conf_mat(truth = won, estimate = .pred_class)

# ROC curve data
roc_curve(rf_predictions, truth = won, .pred_Yes, event_level = "first") %>%
  autoplot() +
  labs(title = "ROC Curve - RF Model - Internal Only")


# See predictors
final_recipe <- prep(rf_recipe, training = training(data_split), retain = TRUE)

baked_data <- bake(final_recipe, new_data = NULL)

selected_features <- baked_data %>%
  select(-won) %>%    
  colnames()

original_vars <- colnames(training(data_split)) %>% setdiff("won")
removed_vars <- setdiff(original_vars, selected_features)

# Extract variable importance
vip_df <- rf_final_fit |>
  extract_fit_parsnip() |>
  vi()

# Plot top 20 features
vip_df %>%
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = paste0(round(Importance, 3))),
            hjust = -0.1, size = 3.2) +
  labs(
    title = "Top 20 Most Important Features",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal() +
  expand_limits(y = max(vip_df$Importance) * 1.1)  # add padding for labels

# saving the model
saveRDS(rf_final_fit, file = "rf_last_fit_internal.rds")
saveRDS(rf_final_wf, file = "rf_final_wf_internal.rds")

# Checking 
rf_final_wf_check <- readRDS("rf_final_wf_internal.rds")

rf_final_wf
rf_final_wf_check

#### Now fitting the best model (LASSO) on the whole data to use for the second stage
lasso_final_wf_internal <- readRDS("lasso_final_wf_internal.rds")

lasso_final_full_fit_internal <- lasso_final_wf_internal %>%
  fit(data = data_until_2023)  

# Save model to file
saveRDS(lasso_final_full_fit_internal, file = "lasso_final_full_fit_internal.rds")

lasso_final_full_fit_internal <- readRDS("lasso_final_full_fit_internal.rds")

#### SECOND STAGE OF THE TWO STAGE MODEL INTERNAL
# predict classes (Yes/No) using the already fitted model
lasso_predictions_internal <- predict(lasso_final_full_fit_internal, new_data = data_until_2023, type = "class") %>%
  pull(.pred_class)

# Bind predictions to the original data
data_with_preds_internal <- data_until_2023 %>%
  mutate(predicted_won = lasso_predictions_internal)

# Filter to get only those predicted as "Yes"
data_with_preds_internal <- data_with_preds_internal %>%
  filter(predicted_won == "Yes")

colnames(data_with_preds_internal)

summary(data_with_preds_internal$revenue)
hist(data_with_preds_internal$revenue, breaks = 100)
unique(data_with_preds_internal$predicted_won)


colnames(data_with_preds_internal)

external_vars <- c(
  "lead_gov_spending", "lead_oda_received","lead_gov_effectiveness","lead_corruption_control",
  "lead_political_stability","lead_electoral_democracy", "lead_liberal_democracy",
  "lead_agri_employment", "lead_fragility_index", "lead_security_threats",
  "lead_economic_decline", "lead_public_services", "lead_human_rights",
  "donor_gov_spending", "donor_gov_effectiveness", "donor_corruption_control",
  "donor_political_stability", "donor_electoral_democracy", "donor_liberal_democracy",
  "donor_human_rights", "donor_country_gdp", "lead_country_gdp", "donor_country_gdp_percapita",
  "lead_country_gdp_percapita", "donor_country_inflation", "lead_country_inflation",
  "donor_countryconversion_rate", "lead_countryconversion_rate", "oda_per_capita")

data_with_preds_internal <- data_with_preds_internal %>%
  mutate(
    # Force them back to Date format
    date_proposal_submitted = as.Date(date_proposal_submitted),
    contract_signing_date = as.Date(contract_signing_date)
  )

# Setting up the training test split and cv folds
# Creating split
set.seed(123)
revenue_train_internal <- data_with_preds_internal %>% filter(lubridate::year(date_proposal_submitted) <= 2018)
revenue_test_internal  <- data_with_preds_internal %>% filter(lubridate::year(date_proposal_submitted) >= 2019)
colnames(revenue_train_internal)

# cv folds
# Set up time series CV
# arranging first by date proposal submitted
revenue_train_internal <- revenue_train_internal %>% arrange(date_proposal_submitted)

set.seed(123)
cv_folds_rev_internal <- time_series_cv(
  data = revenue_train_internal,
  date_var = date_proposal_submitted,
  initial = "3 years",     
  assess = "1 year",       
  skip = "1 year",         
  cumulative = TRUE,
  slice_limit = 10         
)

### Starting with LASSO 
revenue_recipe_internal <- recipe(revenue ~ ., data = revenue_train_internal) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, won, predicted_won, new_role = "id") %>%
  step_rm(all_of(external_vars)) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_novel(all_nominal_predictors()) %>%
  step_interact(~ co_financing:lead_country) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())


# Lasso logistic model
lasso_model_2_internal <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Workflow
lasso_wf_2_internal <- workflow() %>%
  add_model(lasso_model_2_internal) %>%
  add_recipe(revenue_recipe_internal)

# Auto tuning grid
grid <- grid_space_filling(penalty(), size = 20)

# Tune with CV
lasso_tune_2_internal <- tune_grid(
  lasso_wf_2_internal,
  resamples = cv_folds_rev_internal,
  grid = grid,
  metrics = metric_set(rmse, mae, mase, rsq)
)

# Select best models
lasso_best_2_internal <- lasso_tune_2_internal %>% select_by_one_std_err(metric = "rmse", desc(penalty))

# Finalize workflows
lasso_final_2_internal <- finalize_workflow(lasso_wf_2_internal, lasso_best_2_internal)

# fitting on final set
metrics <- metric_set(rmse, mae, rsq)

lasso_2_final_fit <- fit(lasso_final_2_internal, data = revenue_train_internal)

# Predict on the test set (keep 'revenue' in new_data)
lasso_2_predictions_internal <- predict(lasso_2_final_fit, new_data = revenue_test_internal) %>%
  bind_cols(revenue_test_internal %>% select(revenue)) %>%
  mutate(
    .pred_real = .pred,
    revenue_real = revenue
  )

real_metrics <- metric_set(rmse, mae, rsq)

real_metrics(data = lasso_2_predictions_internal, truth = revenue, estimate = .pred_real)


# Non-zero coefficients
lasso_2_final_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(estimate != 0, term != "(Intercept)") %>%
  mutate(abs_estimate = abs(estimate)) %>%
  slice_max(order_by = abs_estimate, n =20) %>%
  ggplot(aes(x = term, y = estimate, fill = estimate > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Non-Zero Coefficients from Lasso Regression (Internal)",
    x = "Feature",
    y = "Coefficient Estimate"
  ) 

# Residuals vs fitted plot
# Augment and compute residuals on original scale
augmented_df <- augment(lasso_2_final_fit, new_data = revenue_test_internal) %>%
  mutate(
    .resid = revenue - .pred
  )

# Plot residuals vs fitted values
ggplot(augmented_df, aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted, Lasso Regression (Internal Only)",
    x = "Predicted Revenue",
    y = "Residual"
  ) +
  theme_minimal() + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

# Saving the models
saveRDS(lasso_2_final_fit, file = "lasso_2_last_fit_internal.rds")
saveRDS(lasso_final_2_internal, file = "lasso_2_final_workflow_internal.rds")



#### RANDOM FOREST INTERNAL ONLY REGRESSION ####
revenue_recipe <- recipe(revenue ~ ., data = revenue_train_internal) %>%
  update_role(id, contract_signing_date, lead_country_code, donor_country_code, predicted_won, signing_month, signing_year, won, predicted_won, new_role = "id") %>%
  step_rm(has_role("id")) %>%  
  step_date(date_proposal_submitted,
            features = c("month", "dow", "year")) %>%
  step_rm(date_proposal_submitted) %>%  
  step_rm(all_of(external_vars)) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%  
  step_dummy(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Print the recipe
print(revenue_recipe)

# Define RF model
rf_2_model <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "permutation")

# Workflow
rf_2_wf <- workflow() %>%
  add_recipe(revenue_recipe) %>%
  add_model(rf_2_model)

# Tuning grid
p <- revenue_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>%
  select(-revenue) %>%
  ncol()

rf_2_grid <- grid_max_entropy(
  parameters(mtry(range = c(1, p)), min_n(range = c(2, 40))),
  size = 20
)

# Parallel setup
registerDoParallel(cores = parallel::detectCores() - 1)

# Tune model
set.seed(20250425)
rf_2_tune_internal <- tune_grid(
  rf_2_wf,
  resamples = cv_folds_rev_internal,
  grid = rf_2_grid,
  metrics = metric_set(rmse, rsq, mae, mase)
)

#Plot tuning results
autoplot(rf_2_tune) + theme_bw()

# Select best model using 1-SE rule 
rf_2_best_internal <- rf_2_tune %>%
  select_by_one_std_err(metric = "rmse", desc(mtry))

# Finalize and fit on test set
rf_2_final_wf_internal <- finalize_workflow(rf_2_wf, rf_2_best_internal)

# fitting on final set
metrics <- metric_set(rmse, mae, rsq)

rf_2_final_fit_internal <- fit(rf_2_final_wf_internal, data = revenue_train_internal)

# Predict on the test set (keep 'revenue' in new_data)
rf_2_predictions_internal <- predict(rf_2_final_fit_internal, new_data = revenue_test_internal) %>%
  bind_cols(revenue_test_internal %>% select(revenue)) %>%
  mutate(
    .pred_real = .pred,
    revenue_real = revenue
  )

real_metrics <- metric_set(rmse, mae, rsq)

real_metrics(data = rf_2_predictions_internal, truth = revenue, estimate = .pred_real)

# VI Plot
rf_2_final_fit_internal %>%
  extract_fit_parsnip() %>%
  vi() %>%
  slice_max(Importance, n = 20) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Most Important Features - RF Regression (Internal)") +
  theme_minimal()


# Residuals vs fitted plot
# Augment and compute residuals on original scale
augmented_df <- augment(rf_2_final_fit_internal, new_data = revenue_test_internal) %>%
  mutate(
    .resid = revenue - .pred
  )

# Plot residuals vs fitted values
ggplot(augmented_df, aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted, RF Regression - Internal Only",
    x = "Predicted Revenue",
    y = "Residual"
  ) +
  theme_minimal() + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)


# Saving the models
saveRDS(rf_2_final_fit_internal, file = "saved_models/rf_2_last_fit_internal.rds")
saveRDS(rf_2_final_wf_internal, file = "saved_models/rf_2_final_wf_internal.rds")

# Checking 
rf_wf_check <- readRDS("saved_models/rf_2_final_wf_internal.rds")
rf_2_final_wf_internal

### Boosting
revenue_recipe <- recipe(revenue ~ ., data = revenue_train_internal) %>%
  update_role(id, contract_signing_date, lead_country_code, donor_country_code, predicted_won, signing_month, signing_year, won, predicted_won, new_role = "id") %>%
  step_rm(has_role("id")) %>%  
  step_date(date_proposal_submitted,
            features = c("month", "dow", "year")) %>%
  step_rm(date_proposal_submitted) %>%  
  step_rm(all_of(external_vars)) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


# Tuning main 3 hyperparameters:
xgb_model_tune <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    stop_iter = 500
  ) |>
  set_mode("regression") |>
  set_engine("xgboost")

# Defining the workflow:
xgb_tune_wf <-
  workflow() |>
  add_recipe(revenue_recipe) |>
  add_model(xgb_model_tune)
xgb_tune_wf

# Using parallel processing to speed up computation:
num_cores <- parallel::detectCores()
num_cores

doParallel::registerDoParallel(cores = num_cores - 1L)

# Doing adaptive tuning:
# Expanded parameter tuning space
tuning_params <-
  parameters(
    trees(range = c(0, 10000)),
    learn_rate(range = c(-5, -1)),
    tree_depth(range = c(1, 3))
  )

# Now generate the grid AFTER finalizing
set.seed(8504)
xgb_initial_grid <- grid_max_entropy(
  tuning_params,
  size = 10
)

set.seed(123)
xgb_initial_fit <- tune_grid(
  xgb_tune_wf,
  resamples = cv_folds_rev_internal,
  grid = xgb_initial_grid,  # you generate this after finalizing tuning_params
  metrics = metric_set(rmse, rsq, mae)
)

# Bayesian optimization
set.seed(1)
xgb_tune_res <- tune_bayes(
  xgb_tune_wf,
  resamples = cv_folds_rev_internal,
  initial = xgb_initial_fit,
  param_info = tuning_params,
  iter = 30,                   # *at least 30 iterations* (not 5!!)
  metrics = metric_set(rmse, rsq, mae)
)

# Look at updated tuning results:
xgb_tune_res |>
  autoplot(metric = "rmse")

# Next, we will run this procedure again. However, this time we will pass in 
# xgb_tune_res_1 as the value of the named argument initial. This tells tune_bayes 
# to leverage the information in the previous tuning results (in xgb_tune_res_1) 
# when choosing which hyperparameters to try first.

set.seed(2)
xgb_tune_res_2 <- tune_bayes(
  xgb_tune_wf,
  resamples = cv_folds_rev_internal,
  iter = 5,
  initial = xgb_tune_res,
  param_info = tuning_params,
  metrics = metric_set(rmse, rsq, mae)
)


# Keep repeating tuning procedure:
set.seed(3)
xgb_tune_res_3 <- tune_bayes(
  xgb_tune_wf,
  resamples = cv_folds_rev_internal,
  iter = 5,
  initial = xgb_tune_res_2,
  param_info = tuning_params,
  metrics = metric_set(rmse, rsq, mae, mase)
)


# Keep repeating tuning procedure:
set.seed(4)
xgb_tune_res_4 <- tune_bayes(
  xgb_tune_wf,
  resamples = cv_folds_rev_internal,
  iter = 5,
  initial = xgb_tune_res_3,
  param_info = tuning_params,
  metrics = metric_set(rmse, rsq, mae, mase)
)

# Extracting the metrics
xgb_tune_metrics <-
  xgb_tune_res_4 |>
  collect_metrics()
xgb_tune_metrics


# Visualizing three hyperparameters:
xgb_tune_metrics %>%
  filter(.metric == "rmse") %>%
  filter(mean >= median(mean)) %>% # only show the best options
  ggplot(aes(
    x = trees, y = learn_rate,
    shape = factor(tree_depth),
    fill = mean
  )) +
  geom_point(size = 3) +
  scale_fill_gradient(low = "white", high = "#D55E00") +
  theme_bw() +
  labs(x = "Trees", y = "Learning rate", shape = "Max tree depth", fill = "RMSE")

# 1. Find the best RMSE (smallest mean)
xgb_best_rmse <- xgb_tune_metrics %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice_head(n = 1)

# Save the mean + std_err of the best model
best_rmse_mean <- xgb_best_rmse$mean
best_rmse_se <- xgb_best_rmse$std_err

# 2. Apply the 1SE rule:
# Allow any model whose RMSE <= (best RMSE + 1 std err)
xgb_1se_candidates <- xgb_tune_metrics %>%
  filter(.metric == "rmse") %>%
  filter(mean <= (best_rmse_mean + best_rmse_se))

# 3. From the candidates, pick the simplest model
# You can define "simplest" however you like. Example: shallowest tree

xgb_selected_model <- xgb_1se_candidates %>%
  arrange(tree_depth, desc(learn_rate)) %>%  # prefer simpler trees and faster learn_rate
  slice_head(n = 1)


# Finalize the workflow
xgb_final_wf <-
  xgb_tune_wf |>
  finalize_workflow(xgb_selected_model)
xgb_final_wf

# Fitting on set
xgb_final_fit <- fit(xgb_final_wf, data = revenue_train_internal)


# Predict on the test set (keep 'revenue' in new_data)
xgb_predictions <- predict(xgb_final_fit, new_data = revenue_test_internal) %>%
  bind_cols(revenue_test_internal %>% select(revenue)) %>%
  mutate(
    .pred_real = .pred,
    revenue_real = revenue
  )

real_metrics <- metric_set(rmse, mae, rsq)

real_metrics(data = xgb_predictions, truth = revenue, estimate = .pred_real)

# Augment and compute residuals on original scale
augmented_df <- augment(xgb_final_fit, new_data = revenue_test_internal) %>%
  mutate(
    .resid = revenue - .pred
  )

# Plot residuals vs fitted values
ggplot(augmented_df, aes(x = .pred, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted, XGBoost Regression (Internal)",
    x = "Predicted Revenue",
    y = "Residual"
  ) +
  theme_minimal() + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)


# vi plot
# Extract variable importance
vip_df_xgb <- xgb_final_fit |>
  extract_fit_parsnip() |>
  vi()

# Plot top 20 features
vip_df_xgb %>%
  slice_max(order_by = Importance, n = 20) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = paste0(round(Importance, 3))),
            hjust = -0.1, size = 3.2) +
  labs(
    title = "Top 20 Most Important Features - XGBoost (Internal)",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal() +
  expand_limits(y = max(vip_df_xgb$Importance) * 1.1)  # add padding for labels


# saving the model
saveRDS(xgb_final_fit, file = "xgb_2_final_fit_internal.rds")
saveRDS(xgb_final_wf, file = "xgb_2_final_wf_internal.rds")

# Checking 
xgb_final_fit_check <- readRDS("xgb_2_final_fit_internal.rds")

xgb_final_fit %>% collect_metrics()
xgb_final_fit_check %>% collect_metrics()

# Fitting the best model (LASSO) to the full dataset (data_with_preds_internal)
classifier_internal_fit <- readRDS("lasso_final_full_fit_internal.rds")
lasso_internal_workflow <- readRDS("lasso_2_last_fit_internal.rds")

# Classify proposals likely to be won
first_stage_preds_internal <- predict(classifier_internal_fit, new_data = data_2024, type = "class")

# Filter those predicted to be won
data_with_preds_internal <- data_until_2024 %>%
  filter(lubridate::year(as.Date(date_proposal_submitted)) <= 2023) %>%
  mutate(
    date_proposal_submitted = as.Date(date_proposal_submitted),
    contract_signing_date = as.Date(contract_signing_date),
    predicted_won = factor("Yes", levels = c("No", "Yes"))
  )

# Fit
lasso_internal_fit_all <- fit(lasso_internal_workflow, data = data_with_preds_internal)

# Predict revenue for 2024 predicted-won proposals
data_2024_with_preds_internal <- data_2024_with_preds_internal %>%
  mutate(contract_signing_date = as.Date(contract_signing_date))

revenue_preds_internal_2024 <- predict(lasso_internal_fit_all, new_data = data_2024_with_preds_internal) %>%
  bind_cols(data_2024_with_preds_internal %>% select(id, donor_country, revenue, contract_signing_date)) %>%
  rename(predicted_revenue = .pred)

# Join actuals (where available)
actual_internal_2024 <- data_2024_with_preds_internal %>%
  filter(!is.na(revenue)) %>%
  select(id, revenue) %>%
  rename(actual_revenue = revenue)

comparison_internal_df <- revenue_preds_internal_2024 %>%
  left_join(actual_internal_2024, by = "id")

# Evaluate performance
metric_set(mae, rmse)(comparison_internal_df, truth = actual_revenue, estimate = predicted_revenue)

# Predicted vs Actual plot
ggplot(comparison_internal_df, aes(x = actual_revenue, y = predicted_revenue)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual Revenue (2024) - Internal Variables Only",
       x = "Actual Revenue",
       y = "Predicted Revenue") +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

# Monthly predicted vs actual revenue
monthly_preds_internal <- revenue_preds_internal_2024 %>%
  mutate(signing_month = lubridate::floor_date(contract_signing_date, "month")) %>%
  filter(lubridate::year(signing_month) == 2024) %>%
  group_by(signing_month) %>%
  summarise(total_predicted_revenue = sum(predicted_revenue, na.rm = TRUE))

monthly_actuals_internal <- data_2024_with_preds_internal %>%
  filter(!is.na(revenue)) %>%
  mutate(signing_month = lubridate::floor_date(contract_signing_date, "month")) %>%
  group_by(signing_month) %>%
  summarise(total_actual_revenue = sum(revenue, na.rm = TRUE))

comparison_monthly <- left_join(monthly_preds_internal, monthly_actuals_internal, by = "signing_month")

ggplot(comparison_monthly, aes(x = signing_month)) +
  geom_line(aes(y = total_predicted_revenue), color = "blue", size = 1.2) +
  geom_line(aes(y = total_actual_revenue), color = "red", size = 1.2, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Revenue by Month (2024) - Internal Variables Only",
    x = "Month",
    y = "Revenue ($)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# downloading the predictions from the monthly internal and external ml models
write.csv(monthly_preds, "monthly_preds_external_ml.csv", row.names =FALSE)
write.csv(monthly_preds_internal, "monthly_preds_internal_ml.csv", row.names =FALSE)

# Plot comparing the two 2-stage MLmodels
# Make sure signing_month is of Date type
monthly_preds <- monthly_preds %>%
  mutate(signing_month = as.Date(signing_month)) %>%
  rename(predicted_external = total_predicted_revenue)

monthly_preds_internal <- monthly_preds_internal %>%
  mutate(signing_month = as.Date(signing_month)) %>%
  rename(predicted_internal = total_predicted_revenue)

monthly_actuals <- monthly_actuals %>%
  mutate(signing_month = as.Date(signing_month))

# Join all three into one data frame
monthly_comparison <- full_join(monthly_preds, monthly_preds_internal, by = "signing_month") %>%
  full_join(monthly_actuals, by = "signing_month")

monthly_comparison_clean <- monthly_comparison %>%
  filter(!is.na(predicted_internal)) 

# Plot
ggplot(monthly_comparison_clean, aes(x = signing_month)) +
  geom_line(aes(y = predicted_external, color = "Predicted (External)"), size = 1.2) +
  geom_line(aes(y = predicted_internal, color = "Predicted (Internal)"), size = 1.2,) +
  geom_line(aes(y = total_actual_revenue, color = "Actual Revenue"), size = 1.2, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Revenue by Month (2024)",
    x = "Month",
    y = "Revenue (USD)",
    color = "Legend"
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal()

### TIME SERIES SECTION
library(lubridate)

# Filter and aggregate monthly revenue
monthly_revenue_ts <- data_until_2024 %>%
  mutate(contract_signing_date = as.Date(contract_signing_date)) %>%
  filter(!is.na(revenue), !is.na(contract_signing_date)) %>%
  filter(lubridate::year(contract_signing_date) >= 2010 & lubridate::year(contract_signing_date) <= 2024) %>%
  mutate(month = lubridate::floor_date(contract_signing_date, "month")) %>%
  group_by(month) %>%
  summarise(monthly_revenue = sum(revenue, na.rm = TRUE)) %>%
  ungroup()

# Create time series object
ts_data <- ts(monthly_revenue_ts$monthly_revenue, 
              start = c(2010, 1), frequency = 12)

decomp <- stl(ts_data, s.window = "periodic")
plot(decomp)

# Split into train (2010–2023) and test (2024)
train_ts <- window(ts_data, end = c(2023, 12))
test_ts  <- window(ts_data, start = c(2024, 1))

# Fit models
library(forecast)
fit_arima <- auto.arima(train_ts)
fit_ets   <- ets(train_ts)
fit_tbats <- tbats(train_ts)

# Forecast 12 months ahead
fc_arima <- forecast(fit_arima, h = 12)
fc_ets   <- forecast(fit_ets, h = 12)
fc_tbats <- forecast(fit_tbats, h = 12)

# Compare forecasts to actuals
autoplot(ts_data) +
  autolayer(fc_arima$mean, series = "ARIMA", color = "blue") +
  autolayer(fc_ets$mean, series = "ETS", color = "green") +
  autolayer(fc_tbats$mean, series = "TBATS", color = "purple") +
  autolayer(test_ts, series = "Actual 2024", color = "red", linetype = "dashed") +
  labs(title = "Monthly Revenue Forecast (2024)",
       x = "Time", y = "Revenue",
       color = "Model") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Compute accuracy
accuracy(fc_arima, test_ts)
accuracy(fc_ets, test_ts)
accuracy(fc_tbats, test_ts)

# Create dates for 2024 months
forecast_dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)

# Save ARIMA predictions
arima_preds <- tibble(
  date = forecast_dates,
  predicted_revenue = as.numeric(fc_arima$mean)
)
write.csv(arima_preds, "arima_predictions_2024.csv", row.names = FALSE)

# Save ETS predictions
ets_preds <- tibble(
  date = forecast_dates,
  predicted_revenue = as.numeric(fc_ets$mean)
)
write.csv(ets_preds, "ets_predictions_2024.csv", row.names = FALSE)

# Save TBATS predictions
tbats_preds <- tibble(
  date = forecast_dates,
  predicted_revenue = as.numeric(fc_tbats$mean)
)
write.csv(tbats_preds, "tbats_predictions_2024.csv", row.names = FALSE)

#Plotting all models together
# Convert time series predictions to tibble format
arima_preds <- tibble(
  month = seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = 12),
  revenue = as.numeric(fc_arima$mean),
  model = "ARIMA"
)

ets_preds <- tibble(
  month = seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = 12),
  revenue = as.numeric(fc_ets$mean),
  model = "ETS"
)

tbats_preds <- tibble(
  month = seq.Date(from = as.Date("2024-01-01"), by = "month", length.out = 12),
  revenue = as.numeric(fc_tbats$mean),
  model = "TBATS"
)

# Add model labels
monthly_preds$model <- "ML External"
monthly_preds_internal$model <- "ML Internal"
monthly_actuals$model <- "Actual"

# Rename month column if needed
colnames(monthly_preds)[colnames(monthly_preds) == "signing_month"] <- "month"
colnames(monthly_preds_internal)[colnames(monthly_preds_internal) == "signing_month"] <- "month"
colnames(monthly_actuals)[colnames(monthly_actuals) == "signing_month"] <- "month"

# Ensure date format consistency
monthly_actuals <- monthly_actuals %>%
  select(month, revenue = total_actual_revenue) %>%
  mutate(model = "Actual")

# Combine all model predictions
combined_monthly <- bind_rows(
  monthly_preds %>% mutate(model = "ML External") %>% rename(revenue = predicted_external),
  monthly_preds_internal %>% mutate(model = "ML Internal") %>% rename(revenue = predicted_internal),
  arima_preds %>% mutate(model = "ARIMA"),
  ets_preds %>% mutate(model = "ETS"),
  tbats_preds %>% mutate(model = "TBATS"),
  monthly_actuals
)

# Remove any rows with NA revenue or month
combined_monthly_clean <- combined_monthly %>%
  filter(lubridate::year(month) == 2024)

# Plot
ggplot(combined_monthly_clean, aes(x = month, y = revenue, color = model)) +
  geom_line(size = 1.2) +
  labs(
    title = "Monthly Revenue Predictions for 2024",
    x = "Month",
    y = "Revenue (USD)",
    color = "Model"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(values = c(
    "Actual" = "red",
    "ML External" = "blue",
    "ML Internal" = "darkgreen",
    "ARIMA" = "orange",
    "ETS" = "purple",
    "TBATS" = "black"
  )) +
  theme_minimal()

# Join each model with actuals for metric comparison
get_metrics <- function(pred_df, model_name) {
  joined <- left_join(pred_df, monthly_actuals, by = "month", suffix = c("_pred", "_actual")) %>%
    filter(!is.na(revenue_actual))
  
  metrics <- metric_set(rmse, mae)(joined, truth = revenue_actual, estimate = revenue_pred)
  metrics$model <- model_name
  return(metrics)
}

results <- bind_rows(
  get_metrics(monthly_preds, "ML External"),
  get_metrics(monthly_preds_internal, "ML Internal"),
  get_metrics(arima_preds, "ARIMA"),
  get_metrics(ets_preds, "ETS"),
  get_metrics(tbats_preds, "TBATS")
)

results

# getting the tuning metrics for ts methods
summary(fit_arima)
summary(fit_ets)
summary(fit_tbats)
fit_tbats$AIC

####### Calculating SHAP for the internal models
library(recipes)
library(fastshap)
library(ranger)
library(ggplot2)
library(workflows)
library(themis)

### SHAP for RF classifier
# Extract and prep the recipe
rf_recipe <- 
  recipe(won ~ ., data = data_train) %>%
  update_role(id, date_proposal_submitted, contract_signing_date,
              lead_country_code, donor_country_code, signing_month, signing_year,new_role = "id") %>%
  step_rm(has_role("id")) %>%
  step_rm(all_of(external_vars)) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9) %>%
  step_nzv(all_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) 

final_recipe <- prep(rf_recipe, training = data_train, retain = TRUE)

# Bake training and test sets
data_train <- data_until_2023 %>% filter(lubridate::year(date_proposal_submitted) <= 2018)
data_test <- data_until_2023 %>% filter(lubridate::year(date_proposal_submitted) <= 2019)

train_baked <- bake(final_recipe, new_data = data_train)
test_baked <- bake(final_recipe, new_data = data_test)

# Split predictors and outcome
X_train <- train_baked %>% select(-won)
y_train <- train_baked$won
X_test  <- test_baked %>% select(-won)
y_test  <- test_baked$won

X_test  <- as.data.frame(X_test)

# Fit the RF model directly for fastshap
rf_model <- ranger(won ~ ., data = bind_cols(X_train, won = y_train), probability = TRUE)

# Prediction wrapper
predict_function <- function(model, newdata) {
  predict(model, data = newdata)$predictions[, "Yes"]
}

# Create augmented test set
X_test_aug <- X_test %>%
  mutate(
    pred_prob = predict_function(rf_model, .),
    true_label = y_test,
    correct = (pred_prob > 0.5) == (y_test == "Yes")
  )

# Identify rows of interest
most_confident_correct <- X_test_aug %>% filter(correct) %>% arrange(desc(abs(pred_prob - 0.5))) %>% slice(1)
least_confident_correct <- X_test_aug %>% filter(correct) %>% arrange(abs(pred_prob - 0.5)) %>% slice(1)
most_confident_wrong <- X_test_aug %>% filter(!correct) %>% arrange(desc(abs(pred_prob - 0.5))) %>% slice(1)

# SHAP plotting function
fastshap_explain <- function(row_df, i, title) {
  shap <- fastshap::explain(
    object = rf_model,
    X = X_test,
    pred_wrapper = predict_function,
    nsim = 100,
    newdata = as.data.frame(row_df)
  )
  df <- data.frame(
    Feature = names(shap[i, ]),
    SHAP = as.numeric(shap[i, ]),
    Value = round(as.numeric(row_df[i, ]), 3)
  ) %>%
    arrange(desc(abs(SHAP))) %>%
    head(15)
  
  ggplot(df, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    geom_text(aes(label = Value), hjust = ifelse(df$SHAP > 0, -0.1, 1.1), size = 3) +
    labs(
      title = title,
      subtitle = "Top 15 SHAP Features",
      x = "Feature",
      y = "SHAP value"
    ) +
    theme_minimal()
}

# Plot all 3
plot1 <- fastshap_explain(most_confident_correct[, names(X_test)], 1, "Most Confident Correct")
plot2 <- fastshap_explain(least_confident_correct[, names(X_test)], 1, "Least Confident Correct")
plot3 <- fastshap_explain(most_confident_wrong[, names(X_test)], 1, "Most Confident Wrong")

print(plot1)
print(plot2)
print(plot3)

### SHAP for XGB regressioon
# Extract the fitted model
xgb_2_final_fit_internal <- readRDS("xgb_2_final_fit_internal.rds")
xgb_model <- extract_fit_parsnip(xgb_2_final_fit_internal)

# Prepare the data
prepped_recipe <- prep(revenue_recipe)
baked_train <- bake(prepped_recipe, new_data = revenue_train_internal)
baked_test  <- bake(prepped_recipe, new_data = revenue_test_internal)

X_train <- baked_train %>% select(-revenue) %>% as.data.frame()
X_test  <- baked_test %>% select(-revenue) %>% as.data.frame()

# Define prediction wrapper
xgb_predict_wrapper <- function(object, newdata) {
  predict(object, new_data = newdata) %>% pull(.pred)
}

# Compute SHAP values
library(fastshap)

set.seed(123)
xgb_shap_values <- fastshap::explain(
  object = xgb_model,
  X = X_train,
  newdata = X_test,
  pred_wrapper = xgb_predict_wrapper,
  nsim = 100
)

# Computing for each revenue group
X_test_labeled <- X_test %>%
  mutate(revenue = revenue_test_internal) %>%
  mutate(revenue_group = ifelse(revenue > 5e6, ">5M", "<=5M"))

shap_long <- as.data.frame(xgb_shap_values) %>%
  mutate(revenue_group = X_test_labeled$revenue_group) %>%
  pivot_longer(
    cols = -revenue_group,
    names_to = "feature",
    values_to = "shap_value"
  )

shap_summary <- shap_long %>%
  group_by(revenue_group, feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE), .groups = "drop")

shap_long <- xgb_shap_values %>%
  as_tibble() %>%
  mutate(obs_id = row_number()) %>%
  pivot_longer(
    -obs_id,
    names_to = "feature",
    values_to = "shap_value"
  ) %>%
  left_join(tibble(obs_id = 1:nrow(X_test), revenue = revenue_test_internal$revenue), by = "obs_id")


# Group revenue
shap_summary_top <- shap_long %>%
  mutate(revenue_group = ifelse(revenue > 5e6, ">5M", "≤5M")) %>%
  group_by(revenue_group, feature) %>%
  summarise(mean_abs_shap = mean(abs(shap_value), na.rm = TRUE), .groups = "drop")

# Looks bad so gonna try again
# Limit to top 20 features across all groups
top_features <- shap_summary_top %>%
  group_by(feature) %>%
  summarise(total_mean_abs = sum(mean_abs_shap)) %>%
  top_n(20, total_mean_abs) %>%
  pull(feature)

# Filter to top features
shap_summary_top_filtered <- shap_summary_top %>%
  filter(feature %in% top_features)

# Plot
ggplot(shap_summary_top_filtered, aes(
  x = reorder_within(feature, mean_abs_shap, revenue_group),
  y = mean_abs_shap,
  fill = revenue_group
)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top 20 Features by Mean Absolute SHAP",
    x = "Feature",
    y = "Mean |SHAP|"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
