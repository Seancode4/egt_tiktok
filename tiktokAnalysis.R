## Script header 
## TW 5/26/25 

# load libraries --------------------------------------------------------------------

library(tidyverse)
library(ggthemes)

# read data ---------------------------------------------------------------

df_raw <- read_csv("data/df.csv")

colnames(df_raw)

# Top 50 Most-Viewed Hashtags on TikTok ???

# EGT analysis ------------------------------------------------------------

df_clean <- df_raw %>%
  mutate(date = as_datetime(create_time, tz = "America/New_York"),
         date = as.Date(date))

# Define cutoff date - Biden endorses Harris as Dem nominee
cutoff_date <- as.Date("2024-07-21")

# Clean the hashtags
clean_hashtags <- function(tags_str){
  # Remove square brackets and quotes
  tags_str <- str_remove_all(tags_str, "\\[|\\]|'|\"")
  # Split by comma and trim whitespace
  tags <- str_split(tags_str, ",")[[1]]
  tags <- str_trim(tags)
  return(tolower(tags))  # lower-case for uniform matching
}

# Apply to the whole column, will take approx. 2 minutes: 
df_clean$parsed_tags <- lapply(df_clean$hashtag_names, clean_hashtags)
# print(df_clean$parsed_tags[3])

# Robust hashtag-checking function 
has_hashtag <- function(tag_list, tags) {
  # tag_list: character vector of hashtags present (e.g. c("trump", "biden"))
  # tags: character scalar or vector to check (e.g. "trump" or c("trump", "biden"))
  
  # Normalize to lower case for case-insensitive matching
  tag_list <- tolower(tag_list)
  tags <- tolower(tags)
  
  # Check if ANY of the tags are in the tag_list
  return(any(tags %in% tag_list))
}

df_before <- df_clean %>%
  filter(date < cutoff_date) %>%
  filter(mapply(function(tags) has_hashtag(tags, c("trump", "donald", "donald trump")), parsed_tags) &
           mapply(function(tags) has_hashtag(tags, c("biden", "joe", "joe biden")), parsed_tags))

df_after <- df_clean %>%
  filter(date >= cutoff_date) %>%
  filter(mapply(function(tags) has_hashtag(tags, c("trump", "donald", "donald trump")), parsed_tags) &
           mapply(function(tags) has_hashtag(tags, c("kamala", "harris", "kamala harris")), parsed_tags))

# Does not work for multi-strategies: 

# assign_strategy <- function(row){
#   if(row["pro_democrat"] == 1) {
#     return("PD-In")
#   } else if(row["pro_republican"] == 1) {
#     return("PR-In")
#   } else if(row["against_democrat"] == 1) {
#     return("AD-Out")
#   } else if(row["against_republican"] == 1) {
#     return("AR-Out")
#   } else {
#     return(NA)  # no clear strategy
#   }
# }
# 
# df_before$strategy <- apply(df_before, 1, assign_strategy)
# df_after$strategy <- apply(df_after, 1, assign_strategy)
# 
# # Remove rows without assigned strategies:
# df_before <- df_before[!is.na(df_before$strategy), ]
# df_after  <- df_after[!is.na(df_after$strategy), ]


# create payoff matrix ----------------------------------------------------

library(scales)

### Assuming you have your filtered dataframes: df_before and df_after
### Each has columns: pro_democrat, pro_republican, against_democrat, against_republican
### Each is binary (0/1) indicating presence of the strategy in that post

strategies <- c("pro_democrat", "pro_republican", "against_democrat", "against_republican")

# Function to calculate payoffs for multi-label strategies
calculate_payoffs <- function(df) {
  payoff_list <- lapply(strategies, function(strat){
    df %>%
      filter(.data[[strat]] == 1) %>%
      summarise(
        mean_shares = mean(share_count, na.rm=TRUE),
        mean_comments = mean(comment_count, na.rm=TRUE),
        mean_likes = mean(like_count, na.rm=TRUE),
        mean_views = mean(view_count, na.rm=TRUE),
        n_posts = n()
      ) %>%
      mutate(strategy = strat)
  })
  
  payoff_df <- bind_rows(payoff_list)
  
  # Normalize engagement metrics (log-transform and rescale 0-1)
  payoff_df <- payoff_df %>%
    mutate(
      log_shares = log1p(mean_shares),
      log_comments = log1p(mean_comments),
      norm_shares = rescale(log_shares),
      norm_comments = rescale(log_comments),
      log_views = log1p(mean_views),
      log_likes = log1p(mean_likes),
      norm_views = rescale(log_views),
      norm_likes = rescale(log_likes),
      # payoff = 0.7 * norm_shares + 0.3 * norm_comments
      payoff = 0.25 * norm_shares + 0.25 * norm_comments + 0.25 * norm_likes + 0.25 * norm_views
    ) %>%
    arrange(strategy)
  
  return(payoff_df)
}

# Calculate payoffs for before and after datasets
payoff_before <- calculate_payoffs(df_before)
payoff_after  <- calculate_payoffs(df_after)

print(payoff_before)

print(payoff_after)


# create payoff matrix  ---------------------------------------------------

build_payoff_matrix <- function(payoff_df) {
  # Ensure order (matching rownames)
  strategies <- c("pro_democrat", "pro_republican", "against_democrat", "against_republican")
  payoff_vec <- setNames(payoff_df$payoff, payoff_df$strategy)
  
  mat <- matrix(NA, nrow=4, ncol=4, dimnames=list(strategies, strategies))
  
  for(i in strategies) {
    for(j in strategies) {
      if(i == j) {
        mat[i, j] <- payoff_vec[i]
      } else {
        mat[i, j] <- mean(c(payoff_vec[i], payoff_vec[j]))
      }
    }
  }
  
  return(mat)
}


# replicator dynamics simluation ------------------------------------------

replicator_step <- function(freqs, payoff_matrix) {
  fitnesses <- as.numeric(payoff_matrix %*% freqs)
  avg_fitness <- sum(freqs * fitnesses)
  new_freqs <- freqs * fitnesses / avg_fitness
  return(new_freqs / sum(new_freqs))  # Normalize to sum to 1
}


# Construct payoff matrices
payoff_matrix_before <- build_payoff_matrix(payoff_before)
payoff_matrix_after  <- build_payoff_matrix(payoff_after)

# Write payoff matrix before for Dynamo 4S visualization 
write.csv(payoff_matrix_before, "payoff_before.csv", row.names = FALSE)
#

# Choose which matrix to simulate
payoff_matrix <- payoff_matrix_before  # or payoff_matrix_after

# Run simulation (as above)
# ...


# Initialize equal starting frequencies
initial_freqs <- rep(1/4, 4)
strategies <- c("pro_democrat", "pro_republican", "against_democrat", "against_republican")

num_steps <- 100
freqs_over_time <- matrix(NA, nrow=num_steps, ncol=4)
colnames(freqs_over_time) <- strategies

freqs <- initial_freqs

for(t in 1:num_steps) {
  freqs_over_time[t, ] <- freqs
  freqs <- replicator_step(freqs, payoff_matrix)  # Use your matrix here
}

# Plotting dynamics
library(ggplot2)
library(reshape2)

freqs_df <- as.data.frame(freqs_over_time)
freqs_df$time <- 1:num_steps
freqs_long <- melt(freqs_df, id.vars="time", variable.name="Strategy", value.name="Frequency")

ggplot(freqs_long, aes(x=time, y=Frequency, color=Strategy)) +
  geom_line(size=1.2) +
  theme_minimal() +
  labs(title="Replicator Dynamics of Multi-label Strategies", x="Time step", y="Frequency") +
  scale_color_brewer(palette="Set1")


# ESS ---------------------------------------------------------------------

library(pracma)  # for eigenvalue functions

# replicator dynamics vector field: F(x) = x_i * [ (A x)_i - x' A x ]
replicator_vector_field <- function(x, A) {
  fitness <- A %*% x
  avg_fitness <- as.numeric(t(x) %*% fitness)
  dx <- x * (fitness - avg_fitness)
  return(dx)
}

# Jacobian of replicator dynamics at fixed point x*
replicator_jacobian <- function(x, A) {
  n <- length(x)
  J <- matrix(0, n, n)
  Ax <- as.vector(A %*% x)
  avg <- as.numeric(t(x) %*% (A %*% x))
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        J[i,j] <- x[i] * (A[i,j] - Ax[j]) + (Ax[i] - avg)
      } else {
        J[i,j] <- -x[i] * (A[j,j] - Ax[j])
      }
    }
  }
  return(J)
}

# Example: Check stability at equilibrium freq vector freq_eq under payoff matrix A
freq_eq <- c(0, 0, 1, 0)  # from your plot dominant strategy 'against_democrat' fixed at 1
payoff_mat <- payoff_matrix_before  # or your matrix

# Compute Jacobian at freq_eq
J <- replicator_jacobian(freq_eq, payoff_mat)

# Compute eigenvalues
eig_vals <- eigen(J)$values
print(eig_vals)

# Interpretation:
# If all eigenvalues have negative real parts except possibly a zero eigenvalue due to simplex constraint,
# the fixed point is locally stable → candidate ESS.

# If any eigenvalue has positive real part → unstable → no ESS.

# making vizzes -----------------------------------------------------------



df_byday <- df_clean %>% 
  group_by(date) %>% 
  summarise(total_pro_dem = sum(pro_democrat),
            total_pro_rep = sum(pro_republican),
            total_ag_dem = sum(against_democrat),
            total_ag_rep = sum(against_republican),
            total = sum(total_pro_dem, total_pro_rep, total_ag_dem, total_ag_rep),
            prop_pro_dem = (total_pro_dem / total) * 100,
            prop_pro_rep = (total_pro_rep / total) * 100,
            prop_ag_dem = (total_ag_dem / total) * 100,
            prop_ag_rep = (total_ag_rep / total) * 100) %>% 
  ungroup() 

df_new <- df_byday %>% 
  pivot_longer(cols = 7:10,
               names_to = "sentiment",
               values_to = "num")




# make hashtag scatterplot ------------------------------------------------

# scatterplot to visualize engagement vs # hashtags

# make area figures ------------------------------------------------------------

# ggplot(df_new) +
#   geom_area(aes(x = date,
#                 y = num,
#                 fill = sentiment)) +
#   theme_few

ggplot(df_new, aes(x = date, y = num, fill = sentiment)) +
  geom_area(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +  # optional for nice colors
  geom_vline(xintercept = as.Date("2024-7-13"),
             linetype = "dashed",
             color = "black") + 
  geom_vline(xintercept = as.Date("2024-7-22"),
             linetype = "dashed",
             color = "black") + 
  geom_vline(xintercept = as.Date("2024-8-6"),
             linetype = "dashed",
             color = "black") + 
  annotate("text",
           x = as.Date("2024-07-13"), 
           y = 25,
           label = "Attempted Assassination of Donald Trump",
           angle = 90, 
           vjust = 1.5, 
           hjust = 0) + 
  annotate("text",
           x = as.Date("2024-07-22"), 
           y = 25,
           label = "Harris Becomes Democratic Nominee",
           angle = 90, 
           vjust = 1.5, 
           hjust = 0) + 
  annotate("text",
           x = as.Date("2024-08-06"), 
           y = 25,
           label = "Several State Primaries Held",
           angle = 90, 
           vjust = 1.5, 
           hjust = 0) + 
  labs(title = "Sentiment Strategy Over Time",
       x = "Date",
       y = "Percentage of Videos",
       fill = "Strategy") +
  theme_few()

df_byday %>% group_by(date) %>% arrange(desc(prop_pro_dem)) %>% ungroup()
# 7/22/2024 - highest pro-dem on Harris's nomination Trump's assassination attempt 
# 7/13/2024
df_byday %>% group_by(date) %>% arrange(desc(prop_pro_rep)) %>% ungroup()
# 7/13/2024 - Trump's assassination attempt 
# 7/22/2024
df_byday %>% group_by(date) %>% arrange(desc(prop_ag_dem)) %>% ungroup()
# 5/7/2024 - Indiana Primary
# Biden compares Hamas attack to Holocaust in antisemitism warning
# https://www.reuters.com/world/us/biden-condemn-antisemitism-praise-free-speech-holocaust-remembrance-2024-05-07/
# 8/6/2024
df_byday %>% group_by(date) %>% arrange(desc(prop_ag_rep)) %>% ungroup()
# 5/30/2024 Donald Trump found guilty of all 34 charges in hush money trial
# https://www.cnn.com/politics/live-news/trump-hush-money-trial-05-30-24
# 7/13/2024
