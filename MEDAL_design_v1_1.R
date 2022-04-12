## MEDAL: analgesics for acute back pain trial
## program to produce graphs for design report

## March 2022

## weeks has 1 line per interim
## summary has 1 line per scenario
## patients has 1 line per patient
## simulations has 1 line per simulation

library(ggplot2)
library(dplyr)
setwd("C:/simon/research/trials/BCTU/back_pain")

## read in data - aggregated simulation data from
## MEDAL_TTE_1320.FACTS

sims <- read.csv("C:/simon/research/trials/BCTU/Back_pain/sim_data/agg_MEDAL_TTE_1320_simulations.csv", 
                 skip = 2)

#simulations.df$odds1 <- simulations.df$Mean.resp.1/(1-simulations.df$Mean.resp.1)
#simulations.df$odds2 <- simulations.df$Mean.resp.2/(1-simulations.df$Mean.resp.2)
#simulations.df$fOR <- simulations.df$odds2/simulations.df$odds1

graphlabs <- data.frame("labels" = c("A: null", "B: two good", "C: one good", "D: moderate & good",
                   "E: two moderate", "F: one moderate"), "Dose.Response.Profile" = c("null", "2_good", "1_good",
                    "mod&good", "2_mod", "1_mod"))

sim2 <- left_join(graphlabs, sims, by = "Dose.Response.Profile")
sim2$totss <- sim2$Alloc.2 + sim2$Alloc.3 + sim2$Alloc.4 + sim2$Alloc.5 +
  sim2$Alloc.6 + sim2$Alloc.7

#################################################################################################

## histograms sample size

ggplot(sim2, aes(x=totss)) +
  facet_wrap(~labels, ncol=2) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=20) +
  theme_minimal() +
  labs(x="sample size") +
  ggtitle("Overall sample size") +
  ylim(0, 1000) +
  xlim(400, 1600) +
  scale_x_continuous(breaks=c(500, 1000, 1500))

##########################################################################################

## histograms probability of being best arm

ggplot(sim2, aes(x=Pr.PBO_.2)) +
  facet_wrap(~labels, ncol=2) +
  geom_histogram(fill = "darkgreen", alpha=0.4, binwidth=0.025) +
  theme_minimal() +
  labs(x="probability of benefit", subtitle = "red line = probability of benefit of 0.975") +
  geom_vline(xintercept=0.975, color="red") +
  ggtitle("Probability of treatment effect > 0 (benefit)")

####################################################################################################

## histogram of 5% difference 

ggplot(simulations.df, aes(x=simulations.df$Pr.CSD.2)) +
  facet_wrap(~labels, ncol=2) +
  geom_histogram(fill = "brown", alpha=0.4, binwidth=0.025) +
  theme_minimal() +
  labs(x="probability of >5% benefit", subtitle = "red line = probability of 0.9") +
  geom_vline(xintercept=0.9, color="red") +
  ggtitle("Probability of benefit > 5%")

#################################################################################################

## histograms of final odds ratio

ggplot(simulations.df, aes(x=simulations.df$fOR)) +
  facet_wrap(~labels, ncol=2) +
  geom_histogram(fill = "darkred", alpha=0.4, binwidth=0.025) +
  theme_minimal() +
  labs(x="odds ratio", subtitle = "red line = odds ratio of 1") +
  geom_vline(xintercept=1, color="red") +
  ggtitle("Final odds ratios") +
  xlim(0, 2.5)



## histogram of number of trials with each type of outcome
## in each scenario

subA <- subset(simulations.df, simulations.df$Outcome == 4)

ggplot(data=subA, aes(x=subA$Response.Profile)) + 
  geom_bar(stat = "count", fill = "light blue", width=0.5) + 
  theme_minimal() +
  ggtitle("Stopped early for lack of effectiveness 5000 simulations") +
  labs(x="Scenario") +
  ylim(0, 5000)

subB <- subset(simulations.df, simulations.df$Outcome == 1)

ggplot(data=subB, aes(x=subB$Response.Profile)) + 
  geom_bar(stat = "count", fill = "light blue", width=0.5) + 
  theme_minimal() +
  ggtitle("Stopped early for success 5000 simulations") +
  labs(x="Scenario") +
  ylim(0,5000)

###############################################################

## plot of power
## power being proportion of trials that have a "positive" outcome
## or stop early for success

px <- as.data.frame(table(simulations.df$Response.Profile, simulations.df$Success.Pr.PBO__at_abx))
px2 <- px[c(14, 15, 16, 17, 18, 19), ]
px2$power <- px2$Freq/5000

ggplot(px2, aes(x = Var1, y = power, group = 1)) +
  geom_point(size=2) +
  labs(x = "scenario", y="prob positive") +
  geom_line()

###############################################################

## need to sort out criterion for "success" and add up number satisfying that
## don't think this is quite right yet
## also plots of mean sample size and error bars

###############################################################

subpat <- subset(patients.df, patients.df$Scenario.ID == 1)
table(subpat$Visit.1)
table(subpat$Visit.2)
table(subpat$Visit.3)
table(subpat$Visit.4)
table(subpat$Visit.5)
table(subpat$Visit.6)

###############################################################
## this is just some code for helping with plotting
## from EOLIA profram which is why it isn't relevant here

ggplot(data=xy, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", aes(fill=Reason), width=0.8, position=position_dodge(0.5)) + 
  theme_minimal() +
  ggtitle("Control 60%, ECMO 40%") +
  labs(x="interim at which trial stopped") +
  scale_fill_brewer(palette="Paired") +
  scale_x_discrete(labels=c("Interim 1", "Interim 2", "Interim 3", "Interim 4", 
                            "Interim 5", "6" = "Final analysis"))

# histogram of odds ratio
ggplot(stoptrial, aes(x=stoptrial$OR)) + geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="odds ratio") +
  geom_vline(xintercept=1.0, color="red") +
  ggtitle("Control 60%, ECMO 40%") +
  
  
  # tables
  table(stoptrial$stoptype)
table(stoptrial$stop)

# subset data to get sims stopped at each interim
# and plot OR

sub1 <- subset(stoptrial, stoptrial$InterimNumber == 1)
ggplot(sub1, aes(x=sub1$OR)) + geom_histogram(fill = "blue", alpha=0.4, binwidth=0.05) +
  theme_minimal() +
  labs(x="odds ratio") +
  geom_vline(xintercept=2.25, color="red") +
  ggtitle("Stopped at interim 1")

ggplot(subsim, aes(x = V_val, y = Z_val)) +
  xlim(0, 25) + ylim(-10, 25) +
  geom_point(size=2) +
  geom_abline(intercept = -5.54, slope = 0.81, colour="red") +
  geom_abline(intercept = 5.54, slope = 0.27, colour="red") +
  geom_line(data = subsim[subsim$Sim == 1, ]) +
  geom_line(data = subsim[subsim$Sim == 2, ]) +
  geom_line(data = subsim[subsim$Sim == 3, ]) +
  geom_line(data = subsim[subsim$Sim == 4, ]) +
  geom_line(data = subsim[subsim$Sim == 5, ])

## plot selected simulations in f8

plotline1 <- data.frame(x1 = 0, x2 = 20.5185, y1 = 5.54, y2 = 11.08)
plotline2 <- data.frame(x1 = 0, x2 = 20.5185, y1 = -5.54, y2 = 11.08)

f9 <- subset(f8, f8$InterimNumber == f8$stopat)

ggplot(f8, aes(x = V_val, y = Z_val)) +
  xlim(0, 25) + ylim(-10, 15) +
  geom_point(size=2) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour="red"), data = plotline1) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour="red"), data = plotline2) +
  geom_line(data = f8[f8$Sim == 48, ]) +
  geom_line(data = f8[f8$Sim == 49, ]) +
  geom_line(data = f8[f8$Sim == 50, ]) +
  geom_line(data = f8[f8$Sim == 51, ]) +
  geom_point(aes(x=fV_val, y=fZ_val, colour="red"), data=f9)


## this is trying to do a facet plot which would be easier

subX <- subset(simulations.df, simulations.df$Scenario.ID == c(1, 2, 3, 4))

ggplot(subX, aes(x=subX$Subjects)) +
  facet_grid(rows = vars(subX$Response.Profile)) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=20) +
  theme_minimal() +
  labs(x="sample size") +
  ggtitle("Overall sample size")
ylim(0, 4000) 


## just messing around with this - seems to work now

ggplot(simulations.df, aes(x=simulations.df$Subjects)) +
  facet_wrap(~simulations.df$Response.Profile, ncol=2) +
  geom_histogram(fill = "blue", alpha=0.4, binwidth=20) +
  theme_minimal() +
  labs(x="sample size") +
  ggtitle("Overall sample size") +
  ylim(0, 4000) +
  xlim(400, 1600) +
  scale_x_continuous(breaks=c(500, 1000, 1500))
