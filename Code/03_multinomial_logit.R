model.1 <- MCMCmnl(outcome ~ out_mean + single_mean + hr_mean + xbh_mean +
                     (p_throws == stand), data = ab, baseline = "Out", 
                   mcmc.method = "RWM")

model.2 <- multinom(outcome ~ out_mean + single_mean + hr_mean + xbh_mean +
                      p_throws*factor(batter), data = ab)