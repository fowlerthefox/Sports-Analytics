########################################################################################

# Script runs source data cleaning script to retrieve data set
# Computes shooting, turnover, scoring and transition probabilities 
# Applies markov chain equation and returns expected threat values for each zone

# Clear environment #####
rm(list = ls())

# read in data sets
source('Scripts/01_data_cleaning.R')

#Prob of Shots and TOs for each zone #####
avg_points_scored_from_shot <- all_match_data %>%
  filter(action %in% c('Shot Point','Shot Goal')) %>%
  summarise(avg_points = mean(score))

# actions per zone
zone_actions <- all_match_data %>%
  group_by(zone) %>%
  summarise(actions = n())

# shots per zone
zone_shots <- all_match_data %>%
  group_by(previous_zone) %>%  # Use the previous zone, not the current one
  filter(action %in% c('Shot Point', 'Shot Goal')) %>%
  summarise(shots = sum(if_else(action %in% c('Shot Point', 'Shot Goal'), 1, 0)))

# shots scored per zone
zone_score <- all_match_data %>%
  group_by(zone) %>%
  filter(outcome %in% c('Point','Goal')) %>%
  summarise(shot_score = sum(if_else(action %in% c('Shot Point','Shot Goal'), 1, 0)))

# TOs per last possession zone (use turnover_zone column)
zone_TO <- all_match_data %>%
  filter(outcome == 'TO' & !action %in% c('Shot Point', 'Shot Goal')) %>%
  group_by(turnover_zone) %>%  # Use turnover_zone (last possession before TO)
  summarise(TO = sum(if_else(outcome == 'TO', 1, 0)))

# Join actions, shots, scores & TOs per zone
zone_actions_shots_TO <- zone_actions %>%
  left_join(zone_shots, by = c('zone' = 'previous_zone')) %>%  # Join using previous_zone for shots
  left_join(zone_score, by = c('zone')) %>%  # Join using previous_zone for scores
  left_join(zone_TO, by = c('zone' = 'turnover_zone')) %>%     # Join using turnover_zone for TOs
  mutate(shots = coalesce(shots, 0),     # Fill NA values with 0 for shots
         TO = coalesce(TO, 0),           # Fill NA values with 0 for turnovers
         shot_score = coalesce(shot_score, 0),  # Fill NA values with 0 for shot scores
         Prob_shot = shots / actions,     # Probability of a shot
         Prob_TO = TO / actions,          # Probability of a turnover
         Prob_score = coalesce(shot_score / shots, 0)  # Probability of scoring from a shot
  )

rm(zone_actions, zone_shots, zone_score, zone_TO)

# zone score probabilities 
#scoring_probabilities <- zone_actions_shots_TO$Prob_score

# zone score probabilities 
shooting_probabilities <- zone_actions_shots_TO$Prob_shot

# zone turnover probabilities 
turnover_probabilities <- zone_actions_shots_TO$Prob_TO


# transition matrix ####
match_file_zone_prob_matrix <- match_file_zone_prob %>%
  filter(!outcome == 'TO' | is.na(outcome))
  
# Number of zones plus two for the scoring and turnover states
n_zones <- max(match_file_zone_prob_matrix$zone) 
additional_states = 2
n_states = n_zones + additional_states 

# Initialize the transition matrix with zeros
transition_matrix <- matrix(0, nrow = n_states, ncol = n_states)

# Populate the transition matrix
for (i in 1:n_zones) {
  for (j in 1:n_zones) {
    # Calculate the probability of moving from zone i to zone j
    transition_matrix[i, j] <- nrow(filter(match_file_zone_prob_matrix, 
                                           previous_zone == i, 
                                           zone == j)) / nrow(filter(match_file_zone_prob_matrix, previous_zone == i))
  }
}


# Normalize rows to ensure they sum to 1, accounting for scoring and turnovers
transition_matrix <- sweep(transition_matrix, 1, pmax(rowSums(transition_matrix), 1e-10), FUN="/")


# Model Equation ####
max_transitions <- 20  # maximum number of iterations
S <- shooting_probabilities[1:n_zones]
TO <- turnover_probabilities[1:n_zones]

# expected score values adpated from other research
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

rm(i, j, m_z, max_diff, max_transitions, n_states, n_zones, S, TO, xT_old, z,
   tolerance, additional_states, avg_points_scored_from_shot)

# Assign expected value to each action based on its starting zone
all_match_data$expected_value <- xT[all_match_data$zone]



