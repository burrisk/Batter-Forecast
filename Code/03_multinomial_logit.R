# Load Packages
list.of.packages <- c("dplyr", "nnet", "MCMCpack","coda","LaplacesDemon")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)
library(nnet)
library(MCMCpack)
library(coda)
library(LaplacesDemon)

load("Data/at_bat_post_pitch.Rdata")

# Only use Indians 2016 World Series Position Players
indians.players = c("Carlos Santana", "Jason Kipnis", "Yan Gomes", "Francisco Lindor",
                    "Michael Martinez", "Mike Napoli", "Roberto Perez", "Jose Ramirez",
                    "Lonnie Chisenhall", "Coco Crisp", "Rajai Davis", "Brandon Guyer",
                    "Tyler Naquin")

ab1 <- ab %>%
  filter(batter_name %in% indians.players)

pa_indians <- ab1 %>%
  group_by(batter) %>%
  summarise(n = n())

latest <- as.Date("2016-10-01", format = "%Y-%m-%d")

# model.1 <- MCMCmnl(outcome ~ out_mean  +
#                      (p_throws==stand),
#                    data = ab1, baseline = "Out", 
#                    mcmc.method = "slice")
# 
model.2 <- multinom(outcome ~  bb_mean + bb_mean_pitch +
                      single_mean + single_mean_pitch + 
                      xbh_mean + xbh_mean_pitch + 
                      hr_mean + hr_mean_pitch + 
                      (p_throws == stand), data = ab)


model.2.sum <- summary(model.2)

# Penalized Maximum Likelihood
library(pmlr)
# model.3 <- multinom(outcome ~ factor(batter)*(p_throws == stand) + out_mean +
#                     out_mean_pitch +
#                     bb_mean +
#                     bb_mean_pitch + 
#                     hr_mean +
#                     hr_mean_pitch, data = ab1, maxiter = 1000)
# 
# 
# model.3.sum <- summary(model.3)

# Get World Series Game 1 Traits
ws.batters <- ab1 %>%
  arrange(desc(date)) %>%
  group_by(batter_name, batter) %>%
  filter(row_number() == 1) %>%
  dplyr::select(batter, batter_name, stand, out_mean, bb_mean, single_mean, xbh_mean, hr_mean)

lester <- ab %>%
  filter(pitcher_name == "Jon Lester") %>%
  arrange(desc(date)) %>%
  filter(row_number() == 1) %>%
  dplyr::select(pitcher_name, p_throws, out_mean_pitch, bb_mean_pitch, single_mean_pitch, 
                xbh_mean_pitch, hr_mean_pitch)

hendricks <- ab %>%
  filter(pitcher_name == "Kyle Hendricks") %>%
  arrange(desc(date)) %>%
  filter(row_number() == 1) %>%
  dplyr::select(pitcher_name, p_throws, out_mean_pitch, bb_mean_pitch, single_mean_pitch, 
                xbh_mean_pitch, hr_mean_pitch)

ws.g1 <- merge(ws.batters, lester)
ws.g2 <- merge(ws.batters, hendricks)

ws.g1_predict <- predict(model.2, ws.g1, type = "probs")
ws.g2_predict <- predict(model.2, ws.g2, type = "probs")
colnames(ws.g1_predict) <- colnames(ws.g2_predict) <- c("pred_out_p", "pred_bb_p", 
                                                        "pred_single_p", "pred_xbh_p",
                                                        "pred_hr_p")

ws.g1 <- cbind(ws.g1, ws.g1_predict)
ws.g2 <- cbind(ws.g2, ws.g2_predict)

g1.batters <- c("Rajai Davis", "Jason Kipnis", "Francisco Lindor", 
                "Mike Napoli", "Carlos Santana", "Jose Ramirez", "Brandon Guyer",
                "Lonnie Chisenhall", "Roberto Perez")

g2.batters <- c("Carlos Santana", "Jason Kipnis", "Francisco Lindor", 
                "Mike Napoli", "Jose Ramirez", "Lonnie Chisenhall", "Coco Crisp",
                "Tyler Naquin", "Roberto Perez")

ws.g1 <- ws.g1 %>%
  slice(match(g1.batters, batter_name))

ws.g2 <- ws.g2 %>%
  slice(match(g2.batters, batter_name))

library(ggplot2)
# Make some plots
suzuki <- ab %>%
  filter(batter_name %in% c("Ichiro Suzuki")) %>%
  arrange(date)
pdf("Output/Suzuki.pdf")
ggplot(suzuki, aes(x = 1:nrow(suzuki), y = 1 - out_mean)) + geom_line(color = "orange") +
  xlab("At-Bat") + 
  ylab("Mean OBP") +
  ggtitle("Ichiro Suzuki") +
  theme(plot.title = element_text(size=22))
dev.off()


burnett <- ab %>%
  filter(pitcher_name %in% c("Clayton Kershaw")) %>%
  arrange(date,inning,o)
pdf("Output/kershaw.pdf")
ggplot(burnett, aes(x = 1:nrow(burnett), y = out_mean_pitch)) + geom_line(color = "purple") +
  xlab("At-Bat") + 
  ylab("Mean Out Rate") +
  ggtitle("Clayton Kershaw") +
  theme(plot.title = element_text(size=22))
dev.off()

# Plot Home Run probabilities
pdf("Output/HRProb_g1.pdf")
par(mar = c(8.1,4.1,4.1,2.1))
barplot(ws.g1$pred_hr_p, ylab = "Probability", col = rainbow(13),
        names.arg = ws.g1$batter_name, las = 2, main = "Home Runs (Game 1)")
dev.off()

pdf("Output/HRProb_g2.pdf")
par(mar = c(8.1,4.1,4.1,2.1))
barplot(ws.g2$pred_hr_p, ylab = "Probability", col = rainbow(13),
        names.arg = ws.g2$batter_name, las = 2, main = "Home Runs (Game 2)")
dev.off()



