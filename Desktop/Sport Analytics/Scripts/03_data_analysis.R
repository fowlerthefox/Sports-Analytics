##################################################################

# Model analysis ####
# Script creates visuals for analysis on transition matrix & probabilites

# Clear environment #####
rm(list = ls())


# read in data sets and model scores
source('Scripts/02_data_processing_modeling.R')


# Transition Analysis ####
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


# Shooting & Turnover Analysis ####
# create a data frame from the prob vectors
zone_prob <- data.frame(Zone = 1:16, Shot = shooting_probabilities, Turnover = turnover_probabilities)

# melt the data frame to long format
zone_long <- reshape2::melt(zone_prob, id.vars = "Zone", variable.name = "Type", value.name = "Probability")

# dual bar chart for probability of a shot & probability of a TO
ggplot(zone_long, aes(x = factor(Zone), y = Probability, fill = Type)) +
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

rm(zone_long, zone_prob)


# Turnover outcome analysis ####
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
zone_TO_shots <- zone_actions_shots_TO %>% 
  select(zone, TO) %>%
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

rm(zone_TO, shot_score_to, to_zone_leads_to_shot, next_to_possession, 
   to_possession, next_to_possession_score)


# Expected Threat analysis
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










