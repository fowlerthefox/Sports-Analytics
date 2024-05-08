########################################################################################

# Clear environment #####
rm(list = ls())

# Read in libraries #####
library(expm)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(kableExtra)
library(formattable)
library(purrr)
library(markovchain)
library(gplots)
library(RColorBrewer)


# Data Transformations ####
# Specify the pattern of the files
pattern <- "^match_file"

# List all .xlsx files in the current directory that start with the pattern
files <- list.files(pattern = paste0(pattern, ".*\\.xls$"))

# list of actions that are not possession ending
not_ending_action = c('Free','Kickout','Hand Pass','Carry','Kick Pass','Sideline','Free 45','Stop')
pos_end_outcome = c('TO','Point','Goal','Wide')
pos_end_action = c('Shot','TO','Shot Point','Shot Goal')


# Read in each file, extract team names and combine them into a single data frame
all_match_data <- map_df(files, ~ {
  file_parts <- strsplit(.x, "_")[[1]]
  team_a <- file_parts[4]
  team_b <- file_parts[6]
  game_id <- as.integer(file_parts[3])
  
  data <- read_excel(.x)
  data <- data %>%
    mutate(game_id = game_id,  # Add game_id column to the data
           minutes = timestamp %/%60,
           seconds = round(timestamp %% 60, 0),
           team_defend_name = case_when(
             team_attack == 'Team A' ~ team_b,
             team_attack == 'Team B' ~ team_a,
             TRUE ~ team_attack
           ),
           team_attack_name = case_when(
             team_attack == 'Team A' ~ team_a,
             team_attack == 'Team B' ~ team_b,
             TRUE ~ team_attack  # Keep original value if it's neither 'Team A' nor 'Team B'
           ))
  
  
  # Create kickout start zone rows
  kickout_starts <- data %>%
    filter(action == "Kickout") %>%
    mutate(zone = 1,  # Starting zone for kickouts
           action = "Kickout Start",
           timestamp = timestamp - 1)  # Adjust timestamp slightly before actual kickout
  
  # Append kickout start rows to the main data
  data <- bind_rows(data, kickout_starts) %>%
    arrange(game_id, timestamp) 
  
  # Resetting ID field for each game
  data <- data %>%
    group_by(game_id) %>%
    mutate(id = row_number()) %>%
    ungroup()
  
  data
})


# replace NA values
all_match_data <- all_match_data %>%
  mutate(across(.cols = c(team_attack, action, outcome, shot_pressure), ~na_if(.x, "NA")))


# add previous & next zones, zone change & team change logic check
all_match_data <- all_match_data %>%
  arrange(game_id, timestamp) %>%  # Ensure data is ordered by game_id and timestamp
  group_by(game_id) %>%
  mutate(score = 
           case_when(
             outcome == 'Point' ~ 1,
             outcome == 'Goal' ~ 3,
             TRUE ~ 0),
         zone_change = coalesce(if_else(lag(zone) != zone,1,0),0),
         team_change = coalesce(if_else(lag(team_attack) != team_attack,1,0),0),
         previous_action = case_when(lag(game_id) == game_id & team_change == 0 ~ lag(action),
                                     TRUE ~ 'NA'),
         next_action = lead(action),
         previous_zone = case_when(lag(game_id) == game_id & team_change == 0 ~ lag(zone),
                                   TRUE ~ 0),
         previous_outcome = lag(outcome),
         next_zone = lead(zone),
         direction = case_when(
           # zone 1 to any other zone is forwards
           previous_zone == 1 ~ "forwards",
           # for zones 2 to 4
           previous_zone %in% 2:4 & zone > previous_zone ~ "forwards",
           previous_zone %in% 2:4 & zone < previous_zone ~ "backwards",
           # for zones 5, 8, 11, 14
           previous_zone %in% c(5, 8, 11, 14) & zone == previous_zone + 1 ~ "right",
           previous_zone %in% c(5, 8, 11, 14) & zone == previous_zone + 2 ~ "right",
           previous_zone %in% c(5, 8, 11, 14) & zone > previous_zone + 2 ~ "forwards",
           previous_zone %in% c(5, 8, 11, 14) & zone < previous_zone ~ "backwards",
           # for zones 6, 9, 12, 15
           previous_zone %in% c(6, 9, 12, 15) & zone == previous_zone - 1 ~ "left",
           previous_zone %in% c(6, 9, 12, 15) & zone == previous_zone + 1 ~ "right",
           previous_zone %in% c(6, 9, 12, 15) & zone > previous_zone + 2 ~ "forwards",
           previous_zone %in% c(6, 9, 12, 15) & zone < previous_zone - 2 ~ "backwards",
           # for zones 5, 8, 11, 14
           previous_zone %in% c(7, 10, 13) & zone == previous_zone - 1 ~ "left",
           previous_zone %in% c(7, 10, 13) & zone == previous_zone - 2 ~ "left",
           previous_zone %in% c(7, 10, 13, 14) & zone > previous_zone ~ "forwards",
           previous_zone %in% c(7, 10, 13, 14) & zone < previous_zone - 2 ~ "backwards",
           # for zone 15, 16
           previous_zone == 16 & zone < previous_zone ~ "backwards",
           # same zone
           previous_zone == zone ~ "same",
           # all other cases
           TRUE ~ "NA"
         ),
         possession_number = 1 + cumsum(team_change),
  ) %>%
  ungroup() %>%
  group_by(game_id, possession_number) %>%
  arrange(game_id, id) %>%
  mutate(action_number = row_number()) %>%
  ungroup()


# Calculate game state and add column to dataset
all_match_data <- all_match_data %>%
  arrange(game_id, timestamp) %>%
  group_by(game_id) %>%
  mutate(
    cum_teamA_score = cumsum(if_else(team_attack == "Team A", score, 0)),
    cum_teamB_score = cumsum(if_else(team_attack == "Team B", score, 0))
  ) %>% 
  rowwise() %>%
  mutate(
    game_state = case_when(
      team_attack == "Team A" & cum_teamA_score > cum_teamB_score ~ "Winning",
      team_attack == "Team A" & cum_teamA_score == cum_teamB_score ~ "Drawing",
      team_attack == "Team A" & cum_teamA_score < cum_teamB_score ~ "Losing",
      team_attack == "Team B" & cum_teamB_score > cum_teamA_score ~ "Winning",
      team_attack == "Team B" & cum_teamB_score == cum_teamA_score ~ "Drawing",
      team_attack == "Team B" & cum_teamB_score < cum_teamA_score ~ "Losing",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()



# MARKOV CHAIN #####

# Model data set ####
# Filter out team changes and shots/TOs which are end and start of moves
match_file_zone_prob <- all_match_data %>%
  filter(team_attack != 'No team',
         zone_change == 1,
         team_change == 0,
         possession_number != 1 & action_number != 1)



#Prob of Shots and TOs for each zone #####
shots <- all_match_data %>%
  filter(action %in% c('Shot Point','Shot Goal')) %>%
  summarise(avg_points = mean(score))

# actions per zone
zone_actions <- all_match_data %>%
  group_by(zone) %>%
  summarise(actions = n())

# shots per zone
zone_shots <- all_match_data %>%
  group_by(zone) %>%
  filter(action %in% c('Shot Point','Shot Goal')) %>%
  summarise(shots = sum(if_else(action %in% c('Shot Point','Shot Goal'), 1, 0)))

# shots scored per zone
zone_score <- all_match_data %>%
  group_by(zone) %>%
  filter(outcome %in% c('Point','Goal')) %>%
  summarise(shot_score = sum(if_else(action %in% c('Shot Point','Shot Goal'), 1, 0)))

# TOs per zone
zone_TO <- all_match_data %>%
  filter(outcome == 'TO') %>%
  filter(!action %in% c('Shot Point','Shot Goal')) %>% # filter out shots that outcome was a TO
  group_by(zone) %>%
  summarise(TO = sum(if_else(outcome == 'TO', 1, 0)))

# join actions, shots, scores & TOs per zone
zone_actions_shots_TO <- zone_actions %>%
  left_join(zone_shots, by = c('zone')) %>%
  left_join(zone_score, by = c('zone')) %>%
  left_join(zone_TO, by = c('zone')) %>%
  mutate(shots = coalesce(shots,0),
         TO = coalesce(TO,0),
         shot_score = coalesce(shot_score, 0),
         Prob_shot = shots/actions,
         Prob_TO = TO/actions,
         Prob_score = coalesce(shot_score/shots, 0)
  )

rm(zone_actions, zone_shots, zone_score, pattern, files)

# zone score probabilities 
scoring_probabilities <- zone_actions_shots_TO$Prob_score

# zone score probabilities 
shooting_probabilities <- zone_actions_shots_TO$Prob_shot

# zone turnover probabilities 
turnover_probabilities <- zone_actions_shots_TO$Prob_TO

# create a data frame from the prob vectors
zone_prob <- data.frame(Zone = 1:16, Shot = shooting_probabilities, Scoring = scoring_probabilities, Turnover = turnover_probabilities)

# melt the data frame to long format
zone_long <- reshape2::melt(zone_prob, id.vars = "Zone", variable.name = "Type", value.name = "Probability")

# remove scoring probabilities
zone_long_shot_TO <- zone_long %>%
  filter(Type != 'Scoring')

# dual bar chart for probability of a shot & probability of a TO
ggplot(zone_long_shot_TO, aes(x = factor(Zone), y = Probability, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Zone", 
       y = "Probability", 
       title = 'Probability of a Shot and a Turnover for each Zone',
       fill = "Event Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 12),
        legend.position = "bottom") + 
  geom_text(aes(label = round(Probability, 2)), vjust = -0.5, position = position_dodge(0.9), size = 3) 

rm(zone_long, zone_long_shot_TO)

# Turnover analysis ####
# every possession that ended up as a TO
to_possession <- all_match_data %>%
  mutate(next_possession_number = lead(possession_number)) %>%
  filter(outcome == 'TO') %>%
  select(game_id, id, TO_zone = zone, action_number, possession_number, next_possession_number) %>%
  mutate(game_id_poss_TO = paste(game_id, next_possession_number, sep = "_"))

# filter to next possessions that followed a TO and had a shot
next_to_possession <- all_match_data %>%
  mutate(game_id_poss = paste(game_id, possession_number, sep = "_")) %>%
  filter(game_id_poss %in% to_possession$game_id_poss_TO) %>%
  filter(action %in% c('Shot Point','Shot Goal')) %>%
  distinct(game_id, half, possession_number, game_id_poss) %>%
  left_join(to_possession %>% select(game_id_poss_TO, TO_zone) , by = c('game_id_poss'='game_id_poss_TO'))

# filter to next possessions that followed a TO and had a score
next_to_possession_score <- all_match_data %>%
  mutate(game_id_poss = paste(game_id, possession_number, sep = "_")) %>%
  filter(game_id_poss %in% to_possession$game_id_poss_TO) %>%
  filter(outcome %in% c('Point','Goal')) %>%
  distinct(game_id, id, half, possession_number, game_id_poss, outcome, score) %>%
  left_join(to_possession %>% select(game_id_poss_TO, TO_zone) , by = c('game_id_poss'='game_id_poss_TO'))

# average score from all possessions that occurred after TO
shot_score_to <- to_possession %>%
  distinct(game_id, next_possession_number, game_id_poss_TO) %>%
  left_join(next_to_possession_score, by = c('game_id_poss_TO'='game_id_poss')) %>%
  mutate(scored = if_else(is.na(score), 0, score)) %>%
  summarise(avg_score = mean(scored))

# number of TOs that the next possession ended in a shot
to_zone_leads_to_shot <- next_to_possession %>%
  group_by(TO_zone) %>%
  summarise(TO_shots = n())

# zone TO to shot %
zone_TO_shots <- zone_TO %>%
  left_join(to_zone_leads_to_shot, by = c('zone'='TO_zone')) %>%
  mutate(perc = TO_shots/TO)

# TO to shot probabilities
turnover_shot_probabilities <- zone_TO_shots$perc

# bar chart for probability of a shot occurring after a TO
ggplot(zone_TO_shots, aes(x = factor(zone), y = perc, fill = perc)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Zone", 
       y = "Probability", 
       title = 'Probability of a Shot occuring in the possesion directly after a Turnover for each Zone',
       fill = "Event Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 12),
        legend.position = "bottom") + 
  geom_text(aes(label = round(perc, 2)), vjust = -0.5, position = position_dodge(0.9), size = 3)

rm(zone_TO, to_zone_leads_to_shot, next_to_possession, to_possession, next_to_possession_score)

# Model Equation #####

# Number of zones plus two for the scoring and turnover states
n_zones <- max(match_file_zone_prob$zone) 
additional_states = 2
n_states = n_zones + additional_states 

# Initialize the transition matrix with zeros
transition_matrix <- matrix(0, nrow = 16, ncol = 16)

# Populate the transition matrix
for (i in 1:n_zones) {
  for (j in 1:n_zones) {
    # Calculate the probability of moving from zone i to zone j
    transition_matrix[i, j] <- nrow(filter(match_file_zone_prob, 
                                           previous_zone == i, 
                                           zone == j)) / nrow(filter(match_file_zone_prob, previous_zone == i))
  }
}


# Normalize rows to ensure they sum to 1, accounting for scoring and turnovers
transition_matrix <- sweep(transition_matrix, 1, rowSums(transition_matrix), FUN="/")


# Generate the heatmap table with value labels for just the transition matrix
heatmap.2(transition_matrix,
          Rowv = FALSE, Colv = FALSE,
          dendrogram = "none",
          col = colorRampPalette(brewer.pal(9, "Blues"))(100),
          scale = "none",
          trace = "none",
          key = TRUE,
          density.info = "none",
          main = "Transition Matrix Heatmap",
          cellnote = round(transition_matrix, 2),
          notecol = "black",
          notecex = 1.0,
          cexCol = 1.0,
          cexRow = 1.0,
          labRow = rownames(transition_matrix),
          labCol = colnames(transition_matrix),
          mar = c(16, 16)) 


# Add scoring and turnover probabilities to the matrix
transition_matrix_prob <- cbind(transition_matrix, scoring_probabilities)
transition_matrix_prob <- cbind(transition_matrix, turnover_probabilities)



n <- nrow(transition_matrix)
transition_matrix[n-1,] <- 0 # Set all probabilities to 0 for the turnover row
transition_matrix[n,] <- 0 # Set all probabilities to 0 for the scoring row
transition_matrix[n-1, n-1] <- 1 # Set turnover state to absorb
transition_matrix[n, n] <- 1 # Set scoring state to absorb


# Generate the heatmap table with value labels for the transition matrix including the probabilities for shooting and TO
heatmap.2(transition_matrix,
          Rowv = FALSE, Colv = FALSE,
          dendrogram = "none",
          col = colorRampPalette(brewer.pal(9, "Blues"))(100),
          scale = "none",
          trace = "none",
          key = TRUE,
          density.info = "none",
          main = "Transition Matrix Heatmap",
          cellnote = round(transition_matrix, 2),
          notecol = "black",
          notecex = 1.0,
          cexCol = 1.0,
          cexRow = 1.0,
          labRow = rownames(transition_matrix),
          labCol = colnames(transition_matrix),
          mar = c(16, 16)) 





# assigning variables for formula
n_zones <- 16  # number of zones
max_transitions <- 20  # maximum number of transitions to consider
TO <- turnover_probabilities
S <- shooting_probabilities
# expected score values from previous research
xG <- c(0, 0, 0, 0, 0.282608696, 0.418604651, 0.282608696, 0.405144695,
        0.455301455, 0.405144695, 0.47362514, 0.670120898, 0.47362514, 0.560344828,
        1.037426901, 0.560344828)


# Initialize xT with zeros
xT <- numeric(n_zones)

# Set a tolerance level for convergence
tolerance <- 1e-4 # can be adjusted

# Variable to track maximum change across zones to check convergence
max_diff <- Inf

# Iteratively update xT until changes are sufficiently small
while (max_diff > tolerance) {
  xT_old <- xT
  for (z in 1:n_zones) {
    m_z <- 1 - S[z] - TO[z] 
    # Compute new xT(z) based on the formula
    xT[z] <- S[z] * xG[z] + m_z * sum(transition_matrix[z, ] * xT_old)
  }
  
  # Calculate the maximum difference to check for convergence
  max_diff <- max(abs(xT - xT_old))
}


rm(shot_score_to, shots)

# Model Validation & Analysis

# Assign expected value to each action based on its starting zone
all_match_data$expected_value <- xT[all_match_data$zone]

# sum the expected threat scores for each team in each game
expected_scores <- all_match_data %>%
  filter(team_attack != 'No Team') %>%
  filter(zone_change == 1) %>%
  filter(outcome != 'TO' | is.na(outcome)) %>%
  group_by(game_id, team_attack_name) %>%
  summarise(expected_threat_value = sum(expected_value)) %>%
  ungroup()

# scores in each match  
match_scores <- all_match_data %>%
  filter(team_attack != 'No Team') %>%
  group_by(game_id, team_attack_name) %>%
  summarise(score = sum(score)) %>%
  ungroup()

# join match scores and expected threat values
scores_and_expected <- match_scores %>%
  left_join(expected_scores, by = c('game_id', 'team_attack_name')) %>%
  mutate(points_diff = score - expected_threat_value)

rm(expected_scores, match_scores)

# summary for differences
summary(scores_and_expected$points_diff)

# histogram to visualize the distribution of differences
hist(scores_and_expected$points_diff, breaks = 10, main = "Distribution of Score vs. Expected Score Differences")

# correlation between actual and expected
cor(scores_and_expected$score, scores_and_expected$expected_threat_value, method = "pearson")

# fit linear model 
lm_result <- lm(score ~ expected_threat_value, data = scores_and_expected)
summary(lm_result)

# create a data frame from your vector
zone_expected_threat <- data.frame(Zone = 1:length(xT), Value = xT)

# table for expected threat for each zone
kable(zone_expected_threat, caption = "Zone XT Value")

# Scatter plot for scores vs expected threat
ggplot(scores_and_expected, aes(x = score, y = expected_threat_value)) +
  geom_point(aes(color = team_attack_name), size = 3) +  # Points differentiated by team
  geom_text(aes(label = game_id), vjust = -1, hjust = 1.5, size = 3) +  # Game ID labels
  geom_smooth(method = "lm", color = "black", se = FALSE) +  # Linear regression line
  labs(title = "Score vs Expected Threat Value by Game",
       x = "Score",
       y = "Expected Threat Value") +
  theme_minimal() 






