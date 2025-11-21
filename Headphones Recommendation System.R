setwd("C:/Users/wanis/OneDrive/Desktop/ds")
# ============================================================
# 🎧 FULL HEADPHONE RECOMMENDATION & DATA ANALYSIS PROJECT
# ============================================================

# -------------------------------
# 📦 INSTALL & LOAD REQUIRED LIBRARIES
# -------------------------------
packages <- c("readr", "dplyr", "ggplot2", "corrplot", "randomForest",
              "caret", "e1071", "rpart", "scales", "proxy")
install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# -------------------------------
# 📂 LOAD DATASET
# -------------------------------
# Make sure your working directory is correct:
# setwd("C:/Users/wanis/OneDrive/Desktop/ds")
data <- read_csv("ds sset.csv")

# -------------------------------
# 🧹 DATA CLEANING
# -------------------------------
names(data) <- gsub(" ", "_", names(data))   # Clean column names
data <- na.omit(data)                        # Remove missing rows
data <- data %>% mutate_if(is.character, as.factor)
data$Selling_Price <- abs(as.numeric(data$Selling_Price))  # Ensure no negatives

# -------------------------------
# 📊 DATA SUMMARY
# -------------------------------
cat("\n===== DATA SUMMARY =====\n")
print(summary(data))
cat("\nRows:", nrow(data), " Columns:", ncol(data), "\n")

# -------------------------------
# 💰 PRICE SEGMENT CREATION
# -------------------------------
data <- data %>%
  mutate(Price_Segment = case_when(
    Selling_Price < 300 ~ "Low",
    Selling_Price < 800 ~ "Mid",
    TRUE ~ "High"
  ))

# ============================================================
# 🎯 RECOMMENDATION SYSTEM (MODEL + PRICE BASED)
# ============================================================

recommend_by_price <- function(model_name, price_range = 500) {
  selected <- data %>% filter(Model == model_name)
  
  if (nrow(selected) == 0) {
    return("Model not found in dataset.")
  }
  
  target_price <- selected$Selling_Price
  recs <- data %>%
    filter(Selling_Price >= target_price - price_range,
           Selling_Price <= target_price + price_range,
           Model != model_name) %>%
    arrange(abs(Selling_Price - target_price))
  
  return(head(recs, 10))
}

recommend_by_model <- function(model_name, top_n = 5) {
  if (!(model_name %in% data$Model)) {
    return("Model not found in dataset.")
  }
  
  data_features <- data %>%
    mutate(
      Type = as.numeric(as.factor(Type)),
      Company = as.numeric(as.factor(Company)),
      Color = as.numeric(as.factor(Color))
    ) %>%
    select(Company, Type, Color, Average_Rating, Selling_Price)
  
  sim_matrix <- as.matrix(simil(data_features, method = "cosine"))
  model_index <- which(data$Model == model_name)
  similar_idx <- order(sim_matrix[model_index, ], decreasing = TRUE)
  top_idx <- similar_idx[2:(top_n + 1)]
  
  recs <- data[top_idx, c("Model", "Company", "Type", "Average_Rating", "Selling_Price")]
  return(recs)
}

# 🧪 Test Recommendations
print(recommend_by_price("Aerizo Wireless Touch R100 Earbuds (Black) Bluetooth He...", 1000))
print(recommend_by_model("Aerizo Wireless Touch R100 Earbuds (Black) Bluetooth He...", 5))

# ============================================================
# ⚙️ MACHINE LEARNING MODELS (PATTERN DISCOVERY)
# ============================================================

target <- "Selling_Price"
formula <- as.formula(paste(target, "~ ."))

# Remove columns with too many levels
data_model <- data[, sapply(data, function(x) !(is.factor(x) && nlevels(x) > 50))]

# Train/test split
set.seed(123)
trainIndex <- createDataPartition(data_model[[target]], p = 0.7, list = FALSE)
train_df <- data_model[trainIndex, ]
test_df  <- data_model[-trainIndex, ]

# --- Decision Tree ---
dt_model <- rpart(formula, data = train_df, method = "anova")
dt_pred <- predict(dt_model, test_df)
dt_rmse <- sqrt(mean((dt_pred - test_df[[target]])^2))

# --- Random Forest ---
rf_model_reg <- randomForest(formula, data = train_df, ntree = 100, importance = TRUE)
rf_pred_reg <- predict(rf_model_reg, test_df)
rf_rmse <- sqrt(mean((rf_pred_reg - test_df[[target]])^2))

# --- Linear Regression ---
lr_model <- lm(formula, data = train_df)
lr_pred <- predict(lr_model, test_df)
lr_rmse <- sqrt(mean((lr_pred - test_df[[target]])^2))

# --- Compare Performance ---
results <- data.frame(
  Model = c("Decision Tree", "Random Forest", "Linear Regression"),
  RMSE = c(dt_rmse, rf_rmse, lr_rmse)
)
print(results)

# ============================================================
# 📈 VISUAL ANALYTICS (ALL 7 CHARTS)
# ============================================================

# -------------------------------
# 1️⃣ HEATMAP
# -------------------------------
corrplot(cor(select_if(data, is.numeric)),
         method = "color", type = "upper",
         tl.col = "black", addCoef.col = "black",
         title = "Heatmap of Numeric Correlations")

# -------------------------------
# 2️⃣ TOP 15 COMPANIES (Horizontal Bar Chart)
# -------------------------------
top_comp <- data %>%
  group_by(Company) %>%
  summarise(avg_price = mean(Selling_Price, na.rm=TRUE)) %>%
  arrange(desc(avg_price)) %>%
  slice_head(n=15)

ggplot(top_comp, aes(x = reorder(Company, avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "#2c7fb8") +
  coord_flip() +
  labs(title = "Top 15 Companies by Average Selling Price",
       x = "Company", y = "Average Selling Price") +
  theme_minimal()

# -------------------------------
# 3️⃣ AVERAGE PRICE BY TYPE (Horizontal Bar Chart)
# -------------------------------
avg_type <- data %>%
  group_by(Type) %>%
  summarise(avg_price = mean(Selling_Price, na.rm=TRUE)) %>%
  arrange(desc(avg_price))

ggplot(avg_type, aes(x = reorder(Type, avg_price), y = avg_price)) +
  geom_bar(stat="identity", fill="#b2182b") +
  coord_flip() +
  labs(title = "Average Selling Price by Headphone Type",
       x = "Type", y = "Average Selling Price") +
  theme_minimal()

# -------------------------------
# 4️⃣ CLUSTER SCATTER (Selling Price vs Rating by Segment)
# -------------------------------
ggplot(data, aes(x = Average_Rating, y = Selling_Price, color = Price_Segment)) +
  geom_jitter(alpha = 0.6, size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Clusters — Price vs Rating by Segment",
       x = "Average Rating", y = "Selling Price") +
  theme_minimal()

# -------------------------------
# 5️⃣ RANDOM FOREST FEATURE IMPORTANCE
# -------------------------------
imp <- as.data.frame(importance(rf_model_reg))
imp$Feature <- rownames(imp)
imp <- imp %>% arrange(desc(IncNodePurity)) %>% slice_head(n = 15)

ggplot(imp, aes(x = reorder(Feature, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat="identity", fill="#1f78b4") +
  coord_flip() +
  labs(title = "Top 15 Important Features (Random Forest)",
       x = "Feature", y = "Importance") +
  theme_minimal()

# -------------------------------
# 6️⃣ ACTUAL vs PREDICTED (Random Forest)
# -------------------------------
df_ap <- data.frame(Actual = test_df$Selling_Price, Predicted_RF = rf_pred_reg)

ggplot(df_ap, aes(x = Actual, y = Predicted_RF)) +
  geom_point(alpha=0.6, color="#238b45") +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  labs(title = "Actual vs Predicted Selling Price (Random Forest)",
       x = "Actual Selling Price", y = "Predicted Selling Price") +
  theme_minimal()

# -------------------------------
# 7️⃣ BOXPLOT: PRICE BY SEGMENT
# -------------------------------
ggplot(data, aes(x = Price_Segment, y = Selling_Price, fill = Price_Segment)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price Distribution by Segment",
       x = "Price Segment", y = "Selling Price") +
  theme_minimal()

# ============================================================
# 🧩 ADDITIONAL INSIGHT — K-MEANS CLUSTERING
# ============================================================
num_data <- select_if(data, is.numeric)
set.seed(123)
k_res <- kmeans(scale(num_data), centers = 3)
data$Cluster <- as.factor(k_res$cluster)

ggplot(data, aes(x = Average_Rating, y = Selling_Price, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "K-Means Clustering on Headphones",
       x = "Average Rating", y = "Selling Price") +
  theme_minimal() +
  scale_color_brewer(palette = "Blues")

cat("\n✅ All Models Trained and Charts Generated Successfully!\n")


