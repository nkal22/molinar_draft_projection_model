bart_data <- read.csv("https://www.barttorvik.com/getadvstats.php?year=all&csv=1", header = FALSE)

colnames(bart_data) = c("player_name",	"team",	"conf",	"GP",	"Min_per",	"ORtg",	"usg",	"eFG",	"TS_per",	"ORB_per",	"DRB_per",	"AST_per",	"TO_per",	"FTM",	"FTA",	"FT_per",	"twoPM",	"twoPA",	"twoP_per",	"TPM",	"TPA",	"TP_per",	"blk_per",	"stl_per",	"ftr",	"yr",	"height",	"num",	"porpag",	"adjoe",	"pfr",	"year",	"pid",	"type",	"Rec_Rank",	 "ast_tov",	 "rimmade",	 "totalrimshots",	 "midmade",	 "totalmidshots",	 "rim_make_percent",	 "mid_make_percent",	 "dunksmade",	 "dunkattempts",	 "dunk_make_percentage", "pick",	 "drtg",	"adrtg",	 "dporpag",	 "stops",	 "bpm",	 "obpm",	 "dbpm",	 "gbpm",	"mp",	"ogbpm",	"dgbpm",	"oreb",	"dreb",	"treb",	"ast",	"stl",	"blk",	"pts",	"position",	"no_idea")

library(tidyverse)
library(dplyr)
library(DT)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(randomForest)
library(plotly)
library(kableExtra)
library(rio)
library(ROCR)
library(class)
library(gmodels)
library(glmnet)
library(xgboost)
library(ada)
library(gbm)

head(bart_data)
#bart_data$yr <- factor(bart_data$yr,labels = c("Fr", "So", "Jr", "Sr"))
unique(bart_data$yr)

new_bart_data <- bart_data[which(bart_data$yr == 'Fr' | bart_data$yr == 'So' | bart_data$yr == 'Jr' | bart_data$yr == 'Sr'),]
new_bart_data$yr <- factor(new_bart_data$yr,labels = c("Fr", "So", "Jr", "Sr"))

new_bart_data <- new_bart_data[which(new_bart_data$yr != 'So' & new_bart_data$yr != 'Fr'), ]



unique(new_bart_data$conf)
new_bart_data <- new_bart_data[which(new_bart_data$height != 'None' & new_bart_data$height != '-' & new_bart_data$height != '0' & new_bart_data$height != '4-0' & new_bart_data$height != '4-5' & new_bart_data$height != ''),]


new_bart_data[is.na(new_bart_data)] = 0

new_bart_data$drafted <- 0

for(i in 1:nrow(new_bart_data)){
  if(new_bart_data$pick[i] != 0){
    new_bart_data$drafted[i] = 1
  }
  else{
    new_bart_data$drafted[i] = 0
  }
}

drafted_players = new_bart_data[new_bart_data$drafted == 1,]
undrafted_players = new_bart_data[new_bart_data$drafted == 0,]

drafted_players <- drafted_players %>%
  group_by(pid) %>%
  mutate(drafted = ifelse(year == max(year), 1, 0))


new_bart_data = rbind(drafted_players, undrafted_players)


new_bart_data$p6 <- 0

for(i in 1:nrow(new_bart_data)){
  if(new_bart_data$conf[i] == "ACC" | new_bart_data$conf[i] == "P10" | new_bart_data$conf[i] == "SEC" | new_bart_data$conf[i] == "B10" | new_bart_data$conf[i] == "BE" | new_bart_data$conf[i] == "B12"){
    new_bart_data$p6[i] = 1
  }
  else if(new_bart_data$conf[i] == "Amer" | new_bart_data$conf[i] == "A10" | new_bart_data$team[i] == "Gonzaga" ){
    new_bart_data$p6[i] = 0.6
  }
  else{
    new_bart_data$p6[i] = 0
  }
}

library(stringr)
feet <- vector()
inches <- vector() #Create wins and losses vector
total_height <- vector()
for(i in 1:nrow(new_bart_data)) { 
  feet[i] <- as.numeric(word(new_bart_data$height[i],1,sep = "-")) 
  inches[i] <- as.numeric(word(new_bart_data$height[i],2,sep = "-"))
  total_height[i] <- (feet[i]*12) + inches[i]
}

new_bart_data$height <- total_height

#new_bart_data$yr <- factor(new_bart_data$yr,labels = c("Fr", "So", "Jr", "Sr"))

new_bart_data$position[new_bart_data$position==""] <- "Unknown"
unique(new_bart_data$position)
new_bart_data$position <- factor(new_bart_data$position,labels = c("Unknown", "Wing G", "Combo G", "Pure PG", "Wing F", "PF/C", "C", "Stretch 4", "Scoring PG"))
#new_bart_data$p6 = as.factor(new_bart_data$p6)

data_2023 <- new_bart_data[which(new_bart_data$year == 2025),]

new_bart_data <- new_bart_data[which(new_bart_data$year != 2025),]


drops <- c("ht_DELETE","type", "no_idea", "num", "team",  "pick", "position", "pid")
new_bart_data <- new_bart_data[ , !(names(new_bart_data) %in% drops)]

data_with_names <- new_bart_data
#data_with_names$drafted = as.factor(data_with_names$drafted)


drops2 <- c("player_name", "conf", "year")
new_bart_data <- new_bart_data[ , !(names(new_bart_data) %in% drops2)]

new_bart_data$drafted = as.numeric(new_bart_data$drafted)

new_bart_data[is.na(new_bart_data)] = 0

sample_rows = 1:nrow(new_bart_data)
set.seed(10271999) #sample(x, size, replace = FALSE, prob = NULL)
test_rows = sample(sample_rows,
                   dim(new_bart_data)[1]*.10, #start with 10% of our dataset, could do 20%
                   # but random forest does require more training data because of the 
                   # sampling so 90% might be a better approach with this small of a dataset
                   replace = FALSE)# We don't want duplicate samples
popular_train = new_bart_data[-test_rows,]
popular_test = new_bart_data[test_rows,]


library(xgboost)

# Assuming 'drafted' is your response variable
# Assuming 'popular_train' is your training dataset

library(caret)

# Assuming 'popular_train' is your training dataset

# Identify and one-hot encode categorical variables
categorical_cols <- sapply(popular_train, is.factor)
if (any(categorical_cols)) {
  dummies <- dummyVars(" ~ .", data = popular_train[, categorical_cols])
  sparse_matrix <- cbind(popular_train[, !categorical_cols, drop = FALSE], predict(dummies, newdata = popular_train))
}

# Convert data to DMatrix format (required by xgboost)
dtrain <- xgb.DMatrix(as.matrix(sparse_matrix[, -which(names(sparse_matrix) == "drafted")]), label = sparse_matrix$drafted)

# Train an XGBoost model
xgb_model_upperclass <- xgboost(data = dtrain, nrounds = 2000, max_depth = 10, eta = 0.01, subsample = 0.75, colsample_bytree = 0.75, objective = "reg:linear")

# Print the XGBoost model
print(xgb_model_upperclass)


save(xgb_model_upperclass,file = "ncaa_molinar_upperclass_XGB.RData")

categorical_cols <- sapply(popular_test, is.factor)
if (any(categorical_cols)) {
  dummies <- dummyVars(" ~ .", data = popular_test[, categorical_cols])
  sparse_matrix_test <- cbind(popular_test[, !categorical_cols, drop = FALSE], predict(dummies, newdata = popular_test))
}

dtest <- xgb.DMatrix(as.matrix(sparse_matrix_test[, -which(names(sparse_matrix_test) == "drafted")]), label = sparse_matrix_test$drafted)

predictions <- predict(xgb_model_upperclass, dtest)

popular_test$predicted = predictions

drops3 <- c("drafted")
sparse_matrix_new <- sparse_matrix[ , !(names(sparse_matrix) %in% drops3)]

githubURL_upperclass <- "https://raw.githubusercontent.com/nkal22/combine_score/main/ncaa_molinar_upperclass_XGB.RData"

suppressWarnings({
  download.file(githubURL_upperclass, "localfile.RData")
  load(url(githubURL_upperclass))
})

importance_matrix <- xgb.importance(feature_names = colnames(sparse_matrix_new), model = xgb_model_upperclass)

sorted_importance <- importance_matrix[order(-Gain)]

top_10_importance <- head(sorted_importance, 10)

# Plot the top 10 most important variables
barplot(top_10_importance$Gain, names.arg = top_10_importance$Feature, 
        main = "Top 10 Most Important Variables", xlab = "Variables", ylab = "Importance Score")

xgb.plot.importance(importance_matrix)

###CURRENT DRAFT YEAR

names(bart_subset)[names(bart_subset) == "Rec.Rank"] <- "Rec_Rank"

player_names = bart_subset$player_name

drops <- c("player_name","year", "pid", "drafted", "predicted")
bart_2024 <- bart_subset[ , !(names(bart_subset) %in% drops)]

categorical_cols <- sapply(bart_2024, is.factor)
if (any(categorical_cols)) {
  dummies <- dummyVars(" ~ .", data = bart_2024[, categorical_cols])
  sparse_matrix_2024 <- cbind(bart_2024[, !categorical_cols, drop = FALSE], predict(dummies, newdata = bart_2024))
}


sparse_matrix_2024$drafted = 0

d2024 <- xgb.DMatrix(as.matrix(sparse_matrix_2024[, -which(names(sparse_matrix_2024) == "drafted")]), label = sparse_matrix_2024$drafted)

predictions <- predict(xgb_model, d2024)

bart_2024$predicted = abs(predictions * 100)
bart_2024$player_name = player_names

bart_2024 <- bart_2024[, c("player_name", setdiff(names(bart_2024), "player_name"))]

save(xgb_model,file = "ncaa_molinarXGB.RData")

library(openxlsx)

bart_subset2 = bart_subset[(bart_subset$GP > 10),]

xlsx_to_save = bart_subset2[c("player_name", "predicted")]

write.xlsx(xlsx_to_save, file = "~/Downloads/2024_draft.xlsx")



