# Load Packages
list.of.packages <- c("dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)
library(ggplot2)
library(mvtnorm)

# Load Data
at_bat <- read.csv("Data/at_bat.csv")

# Number of Unique Batters
length(unique(at_bat$batter))
# [1] 5205

# Define the set of hitters who are also pitchers
pitcher_hit_id <- intersect(at_bat$pitcher, at_bat$batter)
pitcher_hits <- at_bat %>%
  filter(batter %in% pitcher_hit_id) %>%
  group_by(pitcher) %>%
  summarise(n_faced = n()) %>%
  filter(n_faced >= 10) # Pitchers are those who have faced at least 10 batters

# Create a Binary Variable detailing whether pitcher
at_bat$is_pitcher <- at_bat$batter %in% pitcher_hits$pitcher

# Only include batters that have at least 50 plate appearances
batter_pa <- at_bat %>%
  group_by(batter) %>%
  summarise(pa = n(), obp = mean(on_base), is_pitcher = mean(is_pitcher)) %>%
  filter(pa >= 50)

ggplot(batter_pa, aes(x = pa, y = obp, colour = factor(is_pitcher))) +
  geom_point()

at_bat <- at_bat %>%
  filter(batter %in% batter_pa$batter)

# Get Empirical Prior Distributions for Position Players and Pitchers
a.hitter <- mean(at_bat[which(at_bat$is_pitcher == F),"on_base"])
a.pitcher <- mean(at_bat[which(at_bat$is_pitcher ==T),"on_base"])
prior.prec <- 50
prior.hitter <- c(a.hitter*prior.prec, (1-a.hitter)*prior.prec)
prior.pitcher <- c(a.hitter*prior.prec, (1-a.hitter)*prior.prec)

# Split data into separate data sets for each batter
player_ab <- split(at_bat, at_bat$batter)
discounts = seq(0.97, 1, by = 0.003)

# Find posterior probability of discount

# First subset the data for speed
player_ab <- player_ab[1:10]

findSmoothedDirichlet <- function(i, dat, prior, discounts = c(1)){
  mat_list <- lapply(discounts, function(discount){
    index <- dat$gameid[i]
    hit <- dat$on_base[i]
    games.away <- abs(index - dat$gameid)
    success.discount <- dat$on_base*(discount^games.away)
    fail.discount <- (1-dat$on_base)*(discount^games.away)
    alpha <- sum(success.discount) + prior[1]*discount^(index-1)
    beta <- sum(fail.discount) + prior[2]*discount^(index-1)
    log_lik <- lbeta(alpha+hit, beta + 1 - hit) - lbeta(alpha,beta)
    m <- alpha/(alpha + beta)
    vec <- c(alpha, beta, log_lik, m)
    names(vec) <- c(paste("alpha",discount,sep = ""), paste("beta",discount,sep = ""),
                  paste("log_lik",discount,sep = ""), paste("m",discount,sep = ""))
    return(vec)
  })
  mat <- unlist(mat_list)
  return(mat)
}


player_ab <- lapply(player_ab, function(dat){
  N = nrow(dat)
  ispitcher <- dat$is_pitcher[1]
  prior <- ispitcher*prior.pitcher + (1-ispitcher)*prior.hitter
  dat <- transform(dat, gameid=match(dat$date, unique(dat$date)))
  post <- t(sapply(1:N, findSmoothedDirichlet, dat, prior, discounts = discounts))
  dat <- cbind(dat, post)
  return(dat)
})

loglik <- rep(0, length(discounts))
names(loglik) <- paste("log_lik",discounts,sep = "")

for (i in 1:length(player_ab)){
  loglik = loglik + colSums(player_ab[[i]][,names(loglik)])
}

odds <- exp(loglik - loglik[1])
postProbs <- odds/sum(odds)
names(postProbs) <- paste("m",discounts,sep = '')
plot(discounts, postProbs, pch = 19)
lines(discounts,postProbs)

player_ab_mean <- lapply(player_ab, function(dat){
  means <- dat[,names(postProbs)]
  dat$postMean <- as.vector(as.matrix(means) %*% as.matrix(postProbs,ncol = 1))
  dat <- dat %>%
    dplyr::select(gameday_link, date, batter_name, pitcher_name, stand, p_throws, batter, pitcher,
           event, is_pitcher, outcome, on_base, gameid, postMean)
}) 

for(i in 1:10){
  d <- player_ab_mean[[i]]
  plot(1:nrow(d), d$postMean, type = 'l', xlab = d$batter_name[1], ylab = "On Base")
}

abreu <- player_ab[[1]]
abreu$pm <- abreu$m0.991
abreu$lwr <- qbeta(0.10, abreu$alpha0.991, abreu$beta0.991)
abreu$upr <- qbeta(0.90, abreu$alpha0.991, abreu$beta0.991)
abreu$date <- as.Date(abreu$date)

plot(abreu$date, abreu$pm, type = 'l', ylim = c(0.30,0.45))
lines(abreu$date, abreu$lwr, col = "red")
lines(abreu$date, abreu$upr, col = "red")

# One Time Series Implementation
y = abreu$pm
abreu$y = y
abreu$x = (abreu$p_throws == abreu$stand)
summary(glm(on_base~ x, data = abreu, family = "binomial"))

y = cbind(abreu$outcome == "Out", abreu$outcome == "Walk",
          abreu$outcome == "Single", abreu$outcome == "XBH",
          abreu$outcome == "Home Run")
y = apply(y, 2, as.numeric)
colnames(y) = c("Out", "Walk", "Single", "XBH", "HR")
t = nrow(y)
overall_means = apply(y,2,mean)
m0 = log(overall_means[2:5]/overall_means[1])

priors <- list(
  W.phi = 0.1*diag(8),
  W.df = 9,
  V.phi = 0.1*diag(4),
  V.df = 5,
  m0 = c(m0, rep(0, 4)),
  C0 = diag(8)
)

F_mat = cbind(diag(4), matrix(0, nrow = 4, ncol = 4))
H_mat = diag(8) + cbind(matrix(0, nrow = 8, ncol = 4), rbind(diag(4), matrix(0, nrow = 4,
                                                                             ncol = 4)))

set.seed(314)

library(MASS)
library(MCMCpack)
library(dlm)

y_disc = sapply(1:nrow(y), function(i){
  ab_away = abs(1:nrow(y) - i)
  discount = 0.995
  c = t(colSums(y*discount^ab_away))
  c = c/sum(c)
})

y_mean = apply(y,2,mean)

ind_prop_mean = log(y_mean/y_mean[1])[2:5]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

inits <- list(
  W = riwish(priors[["W.df"]], priors[["W.phi"]]),
  V = riwish(priors[["V.df"]], priors[["V.phi"]]),
  theta = cbind(rep.row(ind_prop_mean,nrow(y)), matrix(0, nrow = nrow(y), ncol = 4)),
  eta = t(apply(y_disc, 2, function(x){
    log(x/x[1])[2:5]
  }))
)

updateTheta <- function(y, eta, W, V, F_mat, H_mat, n){
  m = priors[["m0"]]
  C = priors[["C0"]]
  model.theta = dlm(m0 = m, C0 = C, FF = F_mat, V = V, W = W, GG = H_mat)
  model.filter = dlmFilter(eta,model.theta)
  model.smooth = dlmSmooth(model.filter)
  var = dlmSvd2var(model.smooth$U.S, model.smooth$D.S)
  theta = do.call(rbind,lapply(1:n, function(i){
    mvrnorm(1, model.smooth[["s"]][i,1:8], var[[i]])
  }))
  theta
}

updateVar <- function(y, eta, F_mat, H_mat, theta, n){
  # Update W
  W.phi.prior = priors[["W.phi"]]
  W.df.prior = priors[["W.df"]]
  
  predW = H_mat %*% t(theta[1:(n-1), ])
  actW = t(theta[2:n, ])
  missW = t(actW - predW)
  mat_W = Reduce('+', lapply(1:nrow(missW), function(i){
    matrix(missW[i,], ncol = 1) %*% matrix(missW[i,], nrow = 1)
  }))
  W = riwish(W.df.prior + n - 1, solve(solve(W.phi.prior + mat_W)))
  
  V.phi.prior = priors[["V.phi"]]
  V.df.prior = priors[["V.df"]]
  
  predV = t(F_mat %*% t(theta))
  missV = eta - predV
  mat_V = Reduce('+', lapply(1:nrow(missV), function(i){
    matrix(missV[i,], ncol = 1) %*% matrix(missV[i,], nrow = 1)
  }))
  V = riwish(V.df.prior + n - 1, solve(solve(V.phi.prior + mat_V)))
  
  l = list(W, V)
  names(l) = c("W", "V")
  return(l)
}



ind_prop_cov = 0.09*diag(4)
ind_prop_cov_inv = solve(ind_prop_cov)



updateEta = function(theta, y, V, F_mat, eta){
  logliky = function(y, eta){
    etaexp = cbind(rep(1,nrow(eta)),exp(eta))
    etaexp = etaexp/rowSums(etaexp)
    lik = diag(y %*% t(etaexp))
    return(sum(log(lik)))
  }
  
  v_inv = solve(V)
  pred = F_mat %*% t(theta)
  
  #Propose eta
  eta_prop = apply(t(pred), 1, function(x){
    mvrnorm(1, mu = x, Sigma = V)
  })
  
  eta_prop = t(eta_prop)
  e = rep(0, nrow(eta))
  # Choose whether to accept or reject
  for (i in 1:nrow(eta)){
    log_p = min(0, log(runif(1)))
    lp_current = log(dmvnorm(eta[i,], t(pred)[i,], V)) + logliky(matrix(y[i,],nrow = 1), matrix(eta[i,],
                                                                                  nrow = 1))
    lp_prop = log(dmvnorm(eta_prop[i,], t(pred)[i,], V)) +  logliky(matrix(y[i,],nrow = 1), matrix(eta_prop[i,],
                                                                                                   nrow = 1))
    if (lp_prop - lp_current > log_p){
      eta_prop[i,] = eta_prop[i,]
      e[i] = 1
    }
    else{
      eta_prop[i,] = eta[i,]
      e[i] = 0
    }
  }
  
  return(eta_prop)
}


W = inits[["W"]]
V = inits[["V"]]
theta = inits[["theta"]]
eta = inits[["eta"]]
n = nrow(y)

eta_list = list()
for (i in 1:40){
  theta = updateTheta(y = y, eta = eta, W = W, V = V, F_mat = F_mat, H_mat = H_mat, n=n)
  l = updateVar(y=y, eta=eta, F_mat=F_mat, H_mat=H_mat, theta=theta, n=n)
  W = l[["W"]]
  V = l[["V"]]
  eta = updateEta(theta = theta, y= y, V=V, F_mat=F_mat, eta=eta)
  eta_list[[i]] = eta
}


postmean = Reduce("+", eta_list)/length(eta_list)
outexp = cbind(rep(1,nrow(postmean)),exp(postmean))
out = outexp/rowSums(outexp)

plot(out[,1], type='l', xlab = "Game in Career", ylab = "Out Probability", main = "Bobby Abreu")

# Try some dynamic ordered probit
# First binary outcomes
y = abreu$on_base

y_disc
