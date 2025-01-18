bart_data <- read.csv("https://www.barttorvik.com/getadvstats.php?year=all&csv=1", header = FALSE)

colnames(bart_data) = c("player_name",	"team",	"conf",	"GP",	"Min_per",	"ORtg",	"usg",	"eFG",	"TS_per",	"ORB_per",	"DRB_per",	"AST_per",	"TO_per",	"FTM",	"FTA",	"FT_per",	"twoPM",	"twoPA",	"twoP_per",	"TPM",	"TPA",	"TP_per",	"blk_per",	"stl_per",	"ftr",	"yr",	"height",	"num",	"porpag",	"adjoe",	"pfr",	"year",	"pid",	"type",	"Rec_Rank",	 "ast_tov",	 "rimmade",	 "totalrimshots",	 "midmade",	 "totalmidshots",	 "rim_make_percent",	 "mid_make_percent",	 "dunksmade",	 "dunkattempts",	 "dunk_make_percentage", "pick",	 "drtg",	"adrtg",	 "dporpag",	 "stops",	 "bpm",	 "obpm",	 "dbpm",	 "gbpm",	"mp",	"ogbpm",	"dgbpm",	"oreb",	"dreb",	"treb",	"ast",	"stl",	"blk",	"pts",	"position",	"no_idea")

#bart_data1 <- bart_data[which(bart_data$year == 2023),]

#bart_data <- bart_data[bart_data$year != "2023", ]

#bart_data <- rbind(bart_data, bart_data1)

#bart_data$yr <- factor(bart_data$yr,labels = c("Fr", "So", "Jr", "Sr"))

new_bart_data <- bart_data[which(bart_data$yr == 'Fr' | bart_data$yr == 'So' | bart_data$yr == 'Jr' | bart_data$yr == 'Sr'),]

new_bart_data$yr <- factor(new_bart_data$yr,labels = c("Fr", "So", "Jr", "Sr"))

#new_bart_data <- new_bart_data[which(new_bart_data$height != '#VALUE!'),]

new_bart_data[is.na(new_bart_data)] = 0

new_bart_data$drafted <- 0

for(i in 1:nrow(new_bart_data)){
  if(new_bart_data$pick[i] != 0){
    new_bart_data$drafted[i] = 1
  }
  else{
    new_bart_data$drafted[i] = 0
  }
  if(new_bart_data$player_name[i] == "Emoni Bates"){
    new_bart_data$yr[i] = "So"
  }
}

drafted_players = new_bart_data[new_bart_data$drafted == 1,]
undrafted_players = new_bart_data[new_bart_data$drafted == 0,]

drafted_players <- drafted_players %>%
  group_by(player_name) %>%
  mutate(drafted = ifelse(year == max(year), 1, 0))

new_bart_data = rbind(drafted_players, undrafted_players)


new_bart_data$p6 <- NA
new_bart_data$p6 <- 0

for(i in 1:nrow(new_bart_data)){
  if(new_bart_data$conf[i] == "ACC" | new_bart_data$conf[i] == "P10" | new_bart_data$conf[i] == "SEC" | new_bart_data$conf[i] == "B10" | new_bart_data$conf[i] == "BE" | new_bart_data$conf[i] == "B12"){
    new_bart_data$p6[i] = 1
  }
  #else if(new_bart_data$conf[i] == "Amer" | new_bart_data$conf[i] == "A10" | new_bart_data$team[i] == "Gonzaga" ){
  #  new_bart_data$p6[i] = 0.6
  #}
  else{
    new_bart_data$p6[i] = 0
  }
}

feet <- vector()
inches <- vector()
total_height <- vector()
for(i in 1:nrow(new_bart_data)) { 
  feet[i] <- as.numeric(word(new_bart_data$height[i],1,sep = "-")) 
  inches[i] <- as.numeric(word(new_bart_data$height[i],2,sep = "-"))
  total_height[i] <- (feet[i]*12) + inches[i]
}

new_bart_data$height <- total_height

new_bart_data$yr <- factor(new_bart_data$yr,labels = c("Fr", "So", "Jr", "Sr"))

new_bart_data$p6 = as.factor(new_bart_data$p6)

new_bart_data[is.na(new_bart_data)] = 0

drops <- c("ht_DELETE","type", "no_idea", "num", "team", "conf", "pick", "position")
new_bart_data <- new_bart_data[ , !(names(new_bart_data) %in% drops)]

data_with_names <- new_bart_data

important_columns <- c("player_name","year", "Rec.Rank", "porpag", "stops", "dporpag", "p6", "drafted", "yr", "TS_per", "eFG", "usg", "height", "TP_per", "TPA", "blk_per", "AST_per")

similarity_table <- data_with_names[ , names(data_with_names) %in% important_columns]


drops2 <- c("player_name")
new_bart_data <- new_bart_data[ , !(names(new_bart_data) %in% drops2)]

new_bart_data$drafted = as.factor(new_bart_data$drafted)

bart_subset <- data_with_names#[data_with_names$player_name == "Max Abmas", ]

bart_subset <- bart_subset[order(bart_subset$player_name),]

#similarity_2022 <- similarity_table[similarity_table$year == "2023", ]

bart_subset$p6 <- as.numeric(as.character(bart_subset$p6))

names(bart_subset)[names(bart_subset) == "Rec.Rank"] <- "Rec_Rank"

player_names = bart_subset$player_name

years = bart_subset$year

bart_frosh = bart_subset[bart_subset$yr == "Fr", ]
bart_soph = bart_subset[bart_subset$yr == "So", ]
bart_upperclass = bart_subset[(bart_subset$yr != "Fr") & (bart_subset$yr != "So"), ]

frosh_player_names = bart_frosh$player_name
soph_player_names = bart_soph$player_name
upperclass_player_names = bart_upperclass$player_name

frosh_player_years = bart_frosh$year
soph_player_years = bart_soph$year
upperclass_player_years = bart_upperclass$year

frosh_player_pid = bart_frosh$pid
soph_player_pid = bart_soph$pid
upperclass_player_pid = bart_upperclass$pid

drops <- c("player_name","year", "pid", "drafted", "predicted")
bart_2024 <- bart_subset[ , !(names(bart_subset) %in% drops)]
bart_frosh_2024 <- bart_frosh[ , !(names(bart_frosh) %in% drops)]
bart_soph_2024 <- bart_soph[ , !(names(bart_soph) %in% drops)]
bart_upperclass_2024 <- bart_upperclass[ , !(names(bart_upperclass) %in% drops)]


# FRESHMEN
categorical_cols <- sapply(bart_frosh_2024, is.factor)
if (any(categorical_cols)) {
  dummies <- dummyVars(" ~ .", data = bart_frosh_2024[, categorical_cols])
  sparse_matrix_frosh_2024 <- cbind(bart_frosh_2024[, !categorical_cols, drop = FALSE], predict(dummies, newdata = bart_frosh_2024))
}


sparse_matrix_frosh_2024$drafted = 0

dfrosh2024 <- xgb.DMatrix(as.matrix(sparse_matrix_frosh_2024[, -which(names(sparse_matrix_frosh_2024) == "drafted")]), label = sparse_matrix_frosh_2024$drafted)

#SOPHOMORES
categorical_cols <- sapply(bart_soph_2024, is.factor)
if (any(categorical_cols)) {
  dummies <- dummyVars(" ~ .", data = bart_soph_2024[, categorical_cols])
  sparse_matrix_soph_2024 <- cbind(bart_soph_2024[, !categorical_cols, drop = FALSE], predict(dummies, newdata = bart_soph_2024))
}


sparse_matrix_soph_2024$drafted = 0

dsoph2024 <- xgb.DMatrix(as.matrix(sparse_matrix_soph_2024[, -which(names(sparse_matrix_soph_2024) == "drafted")]), label = sparse_matrix_soph_2024$drafted)

#UPPERCLASSMEN

categorical_cols <- sapply(bart_upperclass_2024, is.factor)
if (any(categorical_cols)) {
  dummies <- dummyVars(" ~ .", data = bart_upperclass_2024[, categorical_cols])
  sparse_matrix_upperclass_2024 <- cbind(bart_upperclass_2024[, !categorical_cols, drop = FALSE], predict(dummies, newdata = bart_upperclass_2024))
}


sparse_matrix_upperclass_2024$drafted = 0

dupperclass2024 <- xgb.DMatrix(as.matrix(sparse_matrix_upperclass_2024[, -which(names(sparse_matrix_upperclass_2024) == "drafted")]), label = sparse_matrix_upperclass_2024$drafted)

# FRESHMEN PREDICTIONS
githubURL_frosh <- "https://raw.githubusercontent.com/nkal22/combine_score/main/ncaa_molinar_frosh_XGB.RData"

download.file(githubURL_frosh, "localfile.RData")
load(url(githubURL_frosh))
frosh_predictions_ncaa <- predict(xgb_model_frosh, dfrosh2024)

bart_frosh_2024$predicted = frosh_predictions_ncaa
bart_frosh_2024$player_name = frosh_player_names
bart_frosh_2024$year = frosh_player_years
bart_frosh_2024$pid = frosh_player_pid


bart_frosh_2024 <- bart_frosh_2024[, c("player_name", setdiff(names(bart_frosh_2024), "player_name"))]
bart_frosh$predicted = abs(frosh_predictions_ncaa * 100)

# SOPHOMORE PREDICTIONS

githubURL_soph <- "https://raw.githubusercontent.com/nkal22/combine_score/main/ncaa_molinar_soph_XGB.RData"

download.file(githubURL_soph, "localfile.RData")
load(url(githubURL_soph))
soph_predictions_ncaa <- predict(xgb_model_soph, dsoph2024)

bart_soph_2024$predicted = soph_predictions_ncaa
bart_soph_2024$player_name = soph_player_names
bart_soph_2024$year = soph_player_years
bart_soph_2024$pid = soph_player_pid



bart_soph_2024 <- bart_soph_2024[, c("player_name", setdiff(names(bart_soph_2024), "player_name"))]
bart_soph$predicted = abs(soph_predictions_ncaa * 100)

# UPPERCLASSMAN PREDICTIONS

githubURL_upperclass <- "https://raw.githubusercontent.com/nkal22/combine_score/main/ncaa_molinar_upperclass_XGB.RData"

download.file(githubURL_upperclass, "localfile.RData")
load(url(githubURL_upperclass))
upperclass_predictions_ncaa <- predict(xgb_model_upperclass, dupperclass2024)

bart_upperclass_2024$predicted = upperclass_predictions_ncaa
bart_upperclass_2024$player_name = upperclass_player_names
bart_upperclass_2024$year = upperclass_player_years
bart_upperclass_2024$pid = upperclass_player_pid



bart_upperclass_2024 <- bart_upperclass_2024[, c("player_name", setdiff(names(bart_upperclass_2024), "player_name"))]
bart_upperclass$predicted = abs(upperclass_predictions_ncaa * 100)

bart_subset2 <- rbind(bart_frosh, bart_soph, bart_upperclass)

#bart_2024$predicted = predictions_ncaa * 100
#bart_2024$player_name = player_names
#bart_2024$year = years

#bart_2024 <- bart_2024[, c("player_name", setdiff(names(bart_2024), "player_name"))]

set.seed(123)

bart_filtered <- bart_subset2 %>%
  group_by(pid) %>%
  filter(year == max(year)) %>%
  ungroup()

bart_2024_view = bart_filtered[, c('player_name', 'year', 'yr', 'drafted', 'predicted')]

bart_2024_view$predicted <- ifelse(bart_2024_view$predicted > 100, sample(90:100, sum(bart_2024_view$predicted > 100), replace = TRUE), bart_2024_view$predicted)

write.xlsx(bart_2024_view, file = "~/Downloads/historical_data.xlsx")

