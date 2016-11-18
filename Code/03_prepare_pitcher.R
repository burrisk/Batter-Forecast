# Load Packages
list.of.packages <- c("dplyr")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)


# Load Raw Data
load("Data/at_bat_post.Rdata")
# ab <- read.csv("Data/at_bat.csv")
# 
empirical.means <- ab %>%
  group_by(outcome) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))

prior.prec = 100
prior <- (empirical.means[1:5, 3] * prior.prec)$freq

# Split data into separate data sets for each pitcher
player_ab <- split(ab, ab$pitcher)
length(player_ab)
# [1] 3153

# First subset the data for speed
 player_ab <- player_ab[61:62]



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
    weight = y * discount^games.away
    postDirich <- colSums(weight) - weight[i, ] +
        prior * discount^(game - 1)
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

discount = 0.98

# Get posterior means for each player
player_ab <- lapply(player_ab, function(x){
  gameid <- match(x$date, unique(x$date))
  y <- cbind(x$outcome == "Out", x$outcome == "Walk",
             x$outcome == "Single", x$outcome == "XBH",
             x$outcome == "Home Run")
  post_mat <- matrix(findSmoothedDirichlet(y, gameid, prior, discount)[["mat"]], ncol = 5)
  colnames(post_mat) <- c("out_mean_pitch", "bb_mean_pitch", "single_mean_pitch",
                          "xbh_mean_pitch","hr_mean_pitch")
  new_mat <- cbind(x, post_mat)
  return(new_mat)
})



ab <- do.call(rbind, player_ab)

save(ab, file = "Data/at_bat_post_pitch.Rdata")




