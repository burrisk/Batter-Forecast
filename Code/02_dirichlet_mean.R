# Load Packages
list.of.packages <- c("dplyr","ggplot2", "MCMCpack", "mlogit")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)
library(ggplot2)
library(MCMCpack)
library(mlogit)

# Load Data
at_bat <- read.csv("Data/at_bat.csv")

# Number of Unique Batters
length(unique(at_bat$batter))
# [1] 5205

# Define the set of hitters who are also pitchers
pitcher_hit_id <- intersect(at_bat$pitcher, at_bat$batter)
pitcher_hits <- at_bat %>%
  group_by(pitcher) %>%
  summarise(n_faced = n()) %>%
  filter(n_faced >= 50) # Pitchers are those who have faced at least 50 batters

# Only include batters that have at least 50 plate appearances
batter_pa <- at_bat %>%
  group_by(batter) %>%
  summarise(pa = n(), obp = mean(on_base)) %>%
  filter(pa >= 50)

# Create a Binary Variable detailing whether pitcher
at_bat <- at_bat %>%
  mutate(is_pitcher = batter %in% pitcher_hits$pitcher) %>%
  mutate(outcome = ordered(outcome, levels = c("Out", "Walk", "Single", "XBH", "Home Run"))) %>%
  filter(batter %in% batter_pa$batter)


# Plot to demonstrate the difference between pitchers and non-pitchers
# ggplot(batter_pa, aes(x = pa, y = obp, colour = factor(is_pitcher))) +
#   geom_point() +
#   scale_colour_discrete(name="Pitcher",
#                       breaks=c("1", "0"),
#                       labels=c("True", "False")) +
#   xlab("Plate Appearances") +
#   ylab("OBP")

# Get Empirical Prior Distributions for Position Players and Pitchers
empirical.means <- at_bat %>%
  group_by(is_pitcher, outcome) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

prior.prec = 50
prior.batter <- (empirical.means[1:5, 4] * prior.prec)$freq
prior.pitcher <- (empirical.means[6:10, 4] * prior.prec)$freq

# Split data into separate data sets for each batter
player_ab <- split(at_bat, at_bat$batter)
length(player_ab)
# [1] 1668

# First subset the data for speed
player_ab <- player_ab[1:10]



findSmoothedDirichlet <- function(y, gameid, prior, discount = 1){
  # Computes the averaged smoothed posterior means for each category over time for a given
  # set of between-game discount rate and cumulative log-likelihood of data
  #
  # Args:
  #   y: An t x c matrix with y[t,c] = 1 if event c occurred, y[t,c] = 0 otherwise
  #   gameid: A t length vector specifying the game order; should take values 1 to 
  #           the number of games
  #   prior: a c-length vector 
  #   discount: The proportional weight of an at bat from one previous game
  #
  # Returns:
  #   A list with two arguments:
  #     mat: A t x c matrix with the smoothed posterior means from exactly 1:t
  #     log_lik: The cumulative log-likelihood of the observations over the whole data
  
  findMean <- function(i){
    # Computes the averaged smoothed posterior mean at time i for a given
    # discount rate and log-likelihood at time i
    #
    # Args:
    #   i: The time point to calculate the smoothed mean
    #
    # Returns:
    #   A  (c+1)-length vector with the smoothed posterior means at time i (excluding) data
    #   from exactly i and the log-likelihood of that point
    
    game <- gameid[i]
    outcome <- y[i,]
    c <- which(outcome == 1)
    games.away <- abs(game - gameid)
    postDirich <- colSums((y * (discount^games.away))[-i,]) + prior * discount^(game - 1)
    postPrec <- sum(postDirich)
    postMean <- postDirich / postPrec
    alpha <- postDirich[c]
    beta <- postPrec - alpha
    log_lik <- lbeta(alpha + 1, beta) - lbeta(alpha, beta)
    return(c(postMean, log_lik))
  }
  
  mat <- t(apply(matrix(1:nrow(y), ncol = 1), 1, findMean))
  log_lik <- sum(mat[,6])
  mat <- mat[,1:5]
  l <- list(mat, log_lik)
  names(l) <- c("mat", "log_lik")
  return(l)
}

discount = 0.99

# Get posterior means for each player
player_ab <- lapply(player_ab, function(x){
  gameid <- match(x$date, unique(x$date))
  y <- cbind(x$outcome == "Out", x$outcome == "Walk",
                x$outcome == "Single", x$outcome == "XBH",
                x$outcome == "Home Run")
  ispitcher <- x$is_pitcher[1]
  prior <- ispitcher*prior.pitcher + (1-ispitcher)*prior.batter
  post_mat <- findSmoothedDirichlet(y, gameid, prior, discount)[["mat"]]
  colnames(post_mat) <- c("out_mean", "bb_mean", "single_mean","xbh_mean","hr_mean")
  new_mat <- cbind(x, post_mat)
  return(new_mat)
})

ab <- do.call(rbind, player_ab)

save(ab, file = "Data/at_bat_post.Rdata")



