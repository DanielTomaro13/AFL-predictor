library(dplyr)
library(readr)
library(ggplot2)
library(fitzRoy)

# Define the range of years
years <- 2016:2024

# Fetch results for each year using a loop
for (year in years) {
  # Fetch results for the current year
  results <- fetch_results(year)
  
  
  assign(paste0("afl_", year), results)
}

colnames(afl_2016)

# Cleaning and restructre
# Function to restructure a single year's AFL data

restructure_afl_data <- function(afl_data, year) {
  # Create home team perspective rows
  afl_home <- data.frame(
    Date = afl_data$match.date,
    Season = year,  # Using the provided year parameter
    Team = afl_data$match.homeTeam.name,
    Opponent = afl_data$match.awayTeam.name,
    Result = ifelse(afl_data$homeTeamScore.matchScore.totalScore > afl_data$awayTeamScore.matchScore.totalScore, "W", "L"),
    Points_For = afl_data$homeTeamScore.matchScore.totalScore,
    Points_Against = afl_data$awayTeamScore.matchScore.totalScore,
    Spread = afl_data$homeTeamScore.matchScore.totalScore - afl_data$awayTeamScore.matchScore.totalScore,
    Played = TRUE,
    Home = TRUE,
    Game_ID = afl_data$match.matchId,
    ELO = NA,
    Opp_ELO = NA
  )
  
  # Create away team perspective rows
  afl_away <- data.frame(
    Date = afl_data$match.date,
    Season = year,  # Using the provided year parameter
    Team = afl_data$match.awayTeam.name,
    Opponent = afl_data$match.homeTeam.name,
    Result = ifelse(afl_data$awayTeamScore.matchScore.totalScore > afl_data$homeTeamScore.matchScore.totalScore, "W", "L"),
    Points_For = afl_data$awayTeamScore.matchScore.totalScore,
    Points_Against = afl_data$homeTeamScore.matchScore.totalScore,
    Spread = afl_data$awayTeamScore.matchScore.totalScore - afl_data$homeTeamScore.matchScore.totalScore,
    Played = TRUE,
    Home = FALSE,
    Game_ID = afl_data$match.matchId,
    ELO = NA,
    Opp_ELO = NA
  )
  
  # Combine both perspectives
  return(rbind(afl_home, afl_away))
}

# Define the range of years
years <- 2016:2024


restructured_dfs <- list()

# Loop through each year to restructure data
for (year in years) {

  original_df <- get(paste0("afl_", year))
  

  restructured_df <- restructure_afl_data(original_df, year)
  

  restructured_dfs[[as.character(year)]] <- restructured_df
  assign(paste0("afl_", year, "_restructured"), restructured_df)
}

# Combine all restructured data frames into one
afl_all_years <- do.call(rbind, restructured_dfs)

# Check the result
dim(afl_all_years) # Shows number of rows and columns
colnames(afl_all_years) # Shows column names

# Making things numeric
afl_all_years <- afl_all_years %>% mutate(
  ELO = 0,
  Opp_ELO = 0,
  Result = ifelse(Result == "W", 1, Result),
  Result = ifelse(Result == "L", 0, Result),
  Result = ifelse(Result == "T", 0.5, Result),
  Result = as.numeric(Result)
)

# Extract unique teams from the combined dataset
afl_all_years_Teams <- unique(c(afl_all_years$Team))

# Create a dataframe with teams and assign everyone an ELO of 1500
team_elo <- data.frame(
  Team = afl_all_years_Teams,
  ELO = 1500,
  stringsAsFactors = FALSE
)

# Sort by team name alphabetically
team_elo <- team_elo[order(team_elo$Team), ]

# Save the result back to afl_all_years_TEAMS
afl_all_years_Teams <- team_elo

# View the result
head(afl_all_years_Teams)

#### ELO ####

for(i in 1:nrow(afl_all_years)){
  if(i %% 2 != 0){
    # i = 1
    print(i)
    
    
    Team_A <- afl_all_years$Team[i]
    Team_B <- afl_all_years$Team[i+1]
    
    Result_A <- afl_all_years$Result[i]
    Result_B <- afl_all_years$Result[i+1]
    
    ## Get Current ELO ##
    
    ELO_A <- as.numeric(afl_all_years_Teams[afl_all_years_Teams$Team == Team_A, "ELO"])
    ELO_B <- as.numeric(afl_all_years_Teams[afl_all_years_Teams$Team == Team_B, "ELO"])
    
    ## Load current ELO into the main dataset ##
    
    afl_all_years$ELO[i] <- ELO_A
    afl_all_years$Opp_ELO[i] <- ELO_B
    
    afl_all_years$ELO[i+1] <- ELO_B
    afl_all_years$Opp_ELO[i+1] <- ELO_A
    
    ## Update ELOs
    
    R_A <- 10^(ELO_A/400)
    R_B <- 10^(ELO_B/400)
    
    E_A <- R_A/(R_A + R_B)
    E_B <- R_B/(R_A + R_B)
    
    Elo_Updated_A <- ELO_A + 40 * (Result_A - E_A)
    Elo_Updated_B <- ELO_B + 40 * (Result_B - E_B)
    
    ## Update Team ELOs
    
    afl_all_years_Teams[afl_all_years_Teams$Team == Team_A, "ELO"] <- Elo_Updated_A
    afl_all_years_Teams[afl_all_years_Teams$Team == Team_B, "ELO"] <- Elo_Updated_B
    
  }
}

#Naive wins
afl_all_years <- afl_all_years %>% mutate(
  ELO = as.numeric(ELO),
  Opp_ELO = as.numeric(Opp_ELO),
  Elo_Difference = ELO - Opp_ELO,
  Elo_Forecast_Pred = ifelse(ELO > Opp_ELO, 1, 0),
  Elo_Forecast_Result = ifelse(Elo_Forecast_Pred == Result, 1, 0),
)

#### Naive Win Rate ####
# Create a list to store results by year
Results_by_year <- list()
years <- 2016:2024

# Filter data and calculate win rate for each year
for (year in years) {
  # Filter data for the current year
  Results_by_year[[as.character(year)]] <- afl_all_years %>% filter(Season == year)
  
  # Calculate and print naive win rate
  current_results <- Results_by_year[[as.character(year)]]
  win_rate <- sum(current_results$Elo_Forecast_Result)/nrow(current_results)
  print(paste("Win Rate for", year, ":", win_rate))
  
  # Make the filtered dataset available in the global environment
  assign(paste0("Results_", year), current_results)
}

# Calculate overall win rate
Results_all <- afl_all_years %>% filter(Season >= 2016 & Season <= 2024)
overall_win_rate <- sum(Results_all$Elo_Forecast_Result)/nrow(Results_all)
print(paste("Overall Win Rate:", overall_win_rate))

# Add Elo difference column
afl_all_years <- afl_all_years %>% 
  mutate(Elo_Difference = ELO - Opp_ELO)

# Run the GLM model
win_prob_glm_1 <- glm(
  Result ~ Elo_Difference + Home, 
  family = binomial,
  data = afl_all_years %>% filter(Season >= 2016, Season <= 2024)
)

#### Test Model on all years ####
for (year in years) {
  # Get the results dataset for the current year
  current_results <- get(paste0("Results_", year))
  
  # Predict probabilities
  current_results$win_prob_glm_1 <- predict(win_prob_glm_1, 
                                            newdata = current_results, 
                                            type = "response")
  
  # Convert probabilities to predicted outcomes (1/0)
  current_results$win_pred_glm_1 <- ifelse(current_results$win_prob_glm_1 >= 0.5, 1, 0)
  
  # Calculate accuracy
  accuracy <- sum(current_results$win_pred_glm_1 == current_results$Result)/nrow(current_results)
  print(paste("Accuracy for", year, ":", round(accuracy * 100, 2), "%"))
  
  # Update the results in both the list and global environment
  Results_by_year[[as.character(year)]] <- current_results
  assign(paste0("Results_", year), current_results)
}

# Plot actual spread vs predicted probability -- change the year
ggplot(Results_2020) + 
  geom_point(aes(x = Spread, y = win_prob_glm_1)) +
  labs(title = "Actual Spread vs Win Probability",
       x = "Actual Point Spread",
       y = "Predicted Win Probability") +
  theme_minimal()

########### Forecasting ####################
# Get 2025 fixture
afl_2025 <- fetch_fixture(2025)
afl_data <- afl_2025
year <- 2025

restructure_afl_data <- function(afl_data, year) {
  # Create home team perspective rows
  afl_home <- data.frame(
    Date = afl_data$match.date,
    Season = 2025,  # Using the provided year parameter
    Team = afl_data$match.homeTeam.name,
    Opponent = afl_data$match.awayTeam.name,
    status = 'SCHEDULED',
    Home = TRUE,
    ELO = NA,
    Opp_ELO = NA
  )
  
  # Create away team perspective rows
  afl_away <- data.frame(
    Date = afl_data$match.date,
    Season = 2025,  # Using the provided year parameter
    Team = afl_data$match.awayTeam.name,
    Opponent = afl_data$match.homeTeam.name,
    status = 'SCHEDULED',
    Home = FALSE,
    ELO = NA,
    Opp_ELO = NA
  )
  
  # Combine both perspectives
  return(rbind(afl_home, afl_away))
}

restructured_dfs <- list()
restructured_df <- restructure_afl_data(afl_2025, year)
restructured_dfs[[as.character(year)]] <- restructured_df
assign(paste0("afl_", year, "_restructured"), restructured_df)

# Combine all restructured data frames into one
afl_2025 <- do.call(rbind, restructured_dfs)

# Get the most recent ELO for each team from afl_all_years
get_latest_elo_by_team <- function(afl_all_years) {
  # Ensure the data is sorted by date (most recent last)
  afl_all_years <- afl_all_years[order(afl_all_years$Date), ]
  
  # Get the latest entry for each team that has a non-NA ELO value
  latest_elos <- aggregate(
    ELO ~ Team, 
    data = afl_all_years[!is.na(afl_all_years$ELO), ], 
    FUN = function(x) tail(x, 1)
  )
  
  # Convert the result to a named vector for easier use
  elo_vector <- setNames(latest_elos$ELO, latest_elos$Team)
  
  return(elo_vector)
}

# Apply the latest ELO ratings to the 2025 fixture
apply_elo_to_fixture <- function(afl_2025, latest_elos) {
  for (i in 1:nrow(afl_2025)) {
    team <- afl_2025$Team[i]
    opponent <- afl_2025$Opponent[i]
    
    # Assign ELO values if available
    if (team %in% names(latest_elos)) {
      afl_2025$ELO[i] <- latest_elos[team]
    }
    
    if (opponent %in% names(latest_elos)) {
      afl_2025$Opp_ELO[i] <- latest_elos[opponent]
    }
  }
  
  return(afl_2025)
}

# Get latest ELO ratings for each team
latest_elos <- get_latest_elo_by_team(afl_all_years)

# Print the latest ELO ratings for reference
print(latest_elos)

# Apply the ELO ratings to the 2025 fixture
afl_2025 <- apply_elo_to_fixture(afl_2025, latest_elos)

# Check the result
dim(afl_2025) # Shows number of rows and columns
colnames(afl_2025) # Shows column names

afl_2025$Spread_Pred_lm_1 <- predict(spread_lm_1, newdata = afl_2025)
afl_2025$win_prob_glm_1 <- predict(win_prob_glm_1, newdata = afl_2025, type = "response")
View(afl_2025 %>% arrange(desc(win_prob_glm_1)))
