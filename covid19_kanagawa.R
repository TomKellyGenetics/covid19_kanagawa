# get open data

system("curl https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv -o 130001_tokyo_covid19_patients.csv")

library("vroom")
tokyo_data <- vroom::vroom("130001_tokyo_covid19_patients.csv")
tokyo_data <- as.data.frame(tokyo_data )
dim(tokyo_data)

system("curl https://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv -o kanagawa_covid19_patients.csv")
system("wget https://www.pref.kanagawa.jp/osirase/1369/data/csv/patient.csv")

library("vroom")
locale_jp <- locale(encoding = "CP932")
kanagawa_data <- vroom::vroom("kanagawa_covid19_patients.csv", locale = locale_jp )
kanagawa_data <- as.data.frame(kanagawa_data )
dim(kanagawa_data)

#2957 cases reported in kanagawa (to kanagawa_data 2020/08/07)
head(kanagawa_data)
#all in kanagawa (687  in Kawasaki, 1214 in Yokohama)
table(kanagawa_data$居住地)

#day of the week
table(tokyo_data$曜日)
kanagawa_data$曜日 <- tokyo_data$曜日[match(kanagawa_data$発表日, tokyo_data$公表_年月日)]
head(kanagawa_data, 25)
kanagawa_data$曜日[1:10] <- c("木", "火", "木", "金", "月", "火", "水", "水", "木", "木")
kanagawa_data$曜日[23:24] <- "金"
table(kanagawa_data$曜日)
pdf("cases_by_day.pdf", width = 6, height = 4)
barplot(table(kanagawa_data$曜日)[c(3, 6, 5, 4, 7, 1:2)], names = c( "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
abline(h = 0)
title(main = "Cases in Kanagawa by day of the week (reported)",
      ylab = "Cases (to date 2020/08/07)", 
      sub = "Day reported \nSource: Kanagawa Prefectural Government Health and Welfare Bureau\n https://www.pref.kanagawa.jp/osirase/1369/")
dev.off()
head(kanagawa_data$曜日)
table(kanagawa_data$曜日)[c(3, 6, 5, 4, 7, 1:2)]

#kanagawa_data reported
cases_per_day <- table(c(kanagawa_data$発表日, rep("2020-08-07", 107)))
pdf("cases_per_day.pdf", width = 6, height = 4)
plot(cases_per_day, 
     col = "grey35",
     main = "Cases in Kanagawa by day of the week (reported)",
     ylab = "Cases (to date 2020/08/07)", 
     sub = "Day reported \nSource: Kanagawa Prefectural Government Health and Welfare Bureau\n https://www.pref.kanagawa.jp/osirase/1369/")
abline(h = 0)
dev.off()

library("xts")
#weekly average (rolling)
cases_mat <- cbind(names(cases_per_day), cases_per_day)
cases_mat[,1] <- as.Date(cases_mat[,1],format='%y-%m-%d')
# ex2= xts(cases_mat[,2],order.by=cases_mat[,1])
# xts::apply.weekly(ex2, mean)

length(cases_per_day)
rolling_mean <- rep(NA, length(cases_per_day))
for(ii in 4:(length(cases_per_day)-3)){
  rolling_mean[ii] <- mean(cases_per_day[(ii-3):(ii+3)])
}

pdf("cases_per_day_mean.pdf", width = 6, height = 4)
plot(cases_per_day, 
     col = "grey35",
     main = "Cases in Kanagawa by day of the week (reported)",
     ylab = "Cases (to date 2020/08/07)", 
     sub = "Day reported \nSource: Kanagawa Prefectural Government Health and Welfare Bureau\n https://www.pref.kanagawa.jp/osirase/1369/")
abline(h = 0)
lines(rolling_mean, col = "red", lty=1, cex = 2.5)
legend("top", lty = 1, legend = "7-day mean (rolling)", col = "red")
dev.off()

library("scales")
library("RColorBrewer")
pal = sapply(RColorBrewer::brewer.pal(9, "Set1"), alpha, 0.5)[c(2:5, 7:8, 1)]
days <- rle(kanagawa_data$曜日)$values
days <- c(days[1:3], days[3], days[4:length(days)])  
days <- factor(days, levels = c("月", "火",  "水", "木", "金", "土", "日"))

pdf("cases_per_day_mean_weekly.pdf", width = 6, height = 4)
plot(cases_per_day, 
     col = pal[as.numeric(days)],
     main = "Cases in Kanagawa by day of the week (reported)",
     ylab = "Cases (to date 2020/08/07)", 
     sub = "Day reported \nSource: Kanagawa Prefectural Government Health and Welfare Bureau\n https://www.pref.kanagawa.jp/osirase/1369/")
abline(h = 0)
lines(rolling_mean, col = "red", lty=1, cex = 2.5)
legend("top", lty = 1, legend = "7-day mean (rolling)", col = "red")
legend("topleft", fill = pal, legend = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
dev.off()

kanagawa_testing <- openxlsx::read.xlsx("20200807_kanagawa_testing.xlsx", colNames = TRUE)
kanagawa_testing <- as.data.frame(kanagawa_testing)
kanagawa_testing[,1] <- openxlsx::convertToDate(kanagawa_testing[,1])
colnames(kanagawa_testing)[1:7] <- c("Date", "PosPCR", "PosAntigen", "NegPCR", "NegAntigen", "Testing(7-day-mean)", "PosRate")

head(kanagawa_testing)
tail(kanagawa_testing)
head(cases_per_day)
tail(cases_per_day)

# names(tests_per_day) <- rev(kanagawa_testing$Date)
# tests_per_day <- rev(kanagawa_testing$`Testing(7-day-mean)`)
# head(tests_per_day, 12)
# tail(tests_per_day)
## tests usually reported with 2-3 day with weekly-averages 
## no longer needed as latest figures used from NHK broadcast
#tests_per_day <- c(tests_per_day[10:length(tests_per_day)], NA, NA)

total_pos <- rev(kanagawa_testing$PosPCR + kanagawa_testing$PosAntigen)
total_neg <- rev(kanagawa_testing$NegPCR + kanagawa_testing$NegAntigen)
pos_rate <- rev(kanagawa_testing$PosRate)
total_tests <- total_pos+total_neg
testing <- rev(kanagawa_testing$`Testing(7-day-mean)`)
pos_known <- rev(kanagawa_testing$PosRate)
pos_community <- rev(kanagawa_testing$pos_community)
pos_community_weekly <- rev(kanagawa_testing$pos_community_weekly)
community_rate_change <- rev(kanagawa_testing$rate_change_community)
tot_tests <- rev(kanagawa_testing$tot_tests)
tests_summary <- cbind(total_pos, total_neg, total_tests, pos_rate, testing, pos_known, pos_community, pos_community_weekly, community_rate_change, tot_tests)
tests_summary <- as.data.frame(tests_summary)
tests_summary$Date <- rev(kanagawa_testing$Date)
# tests_summary <- tests_summary[,c(6, 1:6)]
# tests_summary <- tests_summary[1:nrow(tests_summary),]
## tests usually reported with 2-3 day with weekly-averages 
## no longer needed as latest figures used from NHK broadcast
#tests_summary[nrow(tests_summary)+1:2,] <- NA
#tests_summary[nrow(tests_summary)-1:0,1] <- tests_summary$Date[nrow(tests_summary)-2]+1:2
## adjust for initial days with few tests / cases (on different days)
# tests_summary2 <- as.data.frame(matrix(NA, nrow(tests_summary)+5, ncol(tests_summary)))
# tests_summary2[1:nrow(tests_summary)+5,] <- tests_summary
# tests_summary2[,1] <- as.Date(tests_summary2[,1])
# tests_summary2[1:5,1] <- as.Date(names(cases_per_day)[1:5])
# tail(tests_summary2)
# tests_summary2 <- tests_summary
# rm(tests_summary2)


tests_summary$Date[match(as.Date(names(cases_per_day)), tests_summary$Date)]
tests_per_day_weekly <- tests_summary$testing[match(as.Date(names(cases_per_day)), tests_summary$Date)]
tests_per_day_weekly[1:6]
tail(tests_per_day)
tests_per_day <- tests_summary$tot_tests

pdf("cases_per_day_mean_weekly_tests.pdf", width = 6, height = 4)
plot(cases_per_day, 
     ylim = c(0, max(tests_per_day, na.rm = T)),
     col = pal[as.numeric(days)],
     main = "Cases in Kanagawa by day of the week (reported)",
     ylab = "Cases (to date 2020/08/07)", 
     sub = "Day reported \nSource: Kanagawa Prefectural Government Health and Welfare Bureau\n https://www.pref.kanagawa.jp/osirase/1369/")
abline(h = 0)
lines(rolling_mean, col = "red", lty=1, cex = 2.5)
lines(tests_per_day_weekly, col = "blue",  lty=1, cex = 2.5)
lines(tests_per_day, col = "blue",  lty=2, cex = 2.5)
legend("top", lty = c(1, 1, 2), legend = c("cases (7-day mean)", "tests (7-day mean)",  "tests (daily)"), col = c("red", "blue", "blue"))
legend("topleft", fill = pal, legend = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
dev.off()

tests_pos_rate <- tests_summary$pos_rate[match(as.Date(names(cases_per_day)), tests_summary$Date)]

length(tests_pos_rate)
tests_pos_rate_weekly <- rep(NA, length(tests_pos_rate))
for(ii in 4:(length(tests_pos_rate)-3)){
  tests_pos_rate_weekly[ii] <- mean(tests_pos_rate[(ii-3):(ii+3)], na.rm = TRUE)
}
tests_pos_rate_weekly[which(is.na(tests_pos_rate))] <- NA
tests_pos_rate_weekly[which(is.na(tests_pos_rate))+2] <- NA
#tests_pos_rate_weekly[which(is.na(tests_pos_rate))-2] <- NA

pdf("cases_per_day_mean_weekly_tests_pos.pdf", width = 6, height = 4)
par(mar = c(5.1, 4.1, 4.1, 5.1))
plot(cases_per_day, 
     #ylim = c(0, max(tests_per_day, na.rm = T)),
     col = pal[as.numeric(days)],
     main = "Cases in Kanagawa by day of the week (reported)",
     ylab = "Cases (to date 2020/08/07)", 
     sub = "Day reported \nSource: Kanagawa Prefectural Government Health and Welfare Bureau\n https://www.pref.kanagawa.jp/osirase/1369/")
abline(h = 0)
lines(rolling_mean, col = "red", lty=1, cex = 2.5)
lines(tests_pos_rate_weekly * 5, col = "blue",  lty=1, cex = 2.5)
lines(tests_pos_rate * 5, col = "blue",  lty=2, cex = 2.5)
axis(4, at = seq(0, 300, 25), labels = seq(0, 300, 25)/5)
mtext("Positive rate (%)", 4, line = 3)
legend("top", lty = c(1, 1, 2), legend = c("cases (7-day mean)", "pos rate (7-day mean)", "pos rate (daily"), col = c("red", "blue", "blue"))
legend("topleft", fill = pal, legend = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"), cex = 0.75)
dev.off()

pos_community <- tests_summary$pos_community[match(as.Date(names(cases_per_day)), tests_summary$Date)]
pos_community_weekly <- tests_summary$pos_community_weekly[match(as.Date(names(cases_per_day)), tests_summary$Date)]
pos_community_weekly <- pos_community_weekly[5:length(pos_community_weekly)]

pdf("cases_per_day_mean_weekly_community.pdf", width = 6, height = 4)
plot(cases_per_day, 
     col = pal[as.numeric(days)],
     main = "Cases in Kanagawa by day of the week (reported)",
     ylab = "Cases (to date 2020/08/07)", 
     sub = "Day reported \nSource: Kanagawa Prefectural Government Health and Welfare Bureau\n https://www.pref.kanagawa.jp/osirase/1369/")
abline(h = 0)
lines(rolling_mean, col = "red", lty=1, cex = 2.5)
lines(pos_community_weekly, col = "blue",  lty=1, cex = 2.5)
lines(pos_community, col = "blue",  lty=2, cex = 2.5)
legend("top", lty = c(1, 1, 2), legend = c("cases (7-day mean)", "unknown origin (7-day mean)", "unknown origin"), col = c("red", "blue", "blue"))
legend("topleft", fill = pal, legend = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
dev.off()



##########
# modified from: http://freerangestats.info/blog/2020/07/18/victoria-r-convolution
#credit Peter Ellis @ellis2013nz
##########

library(tidyverse)
library(googlesheets4)
library(janitor)
library(scales)
library(mgcv)
library("EpiNow2") # remotes::install_github("epiforecasts/EpiNow2")
library(frs)     # remotes::install_github("ellisp/frs-r-package/pkg")
library(patchwork)
library(glue)
library(surveillance) # for backprojNP()
library("dplyr")
library("ggplot2")
library("lubridate")
detach("package:plyr")
library("dplyr")
library("purrr")
library("tidyr")
library("surveillance")
library("stringr")

#-----------------the Victoria data--------------

url <- "https://docs.google.com/spreadsheets/d/1q5gdePANXci8enuiS4oHUJxcxC13d6bjMRSicakychE/edit#gid=1437767505"

gd_orig <- read_sheet(url) 

state <- gd_orig$State

d <- gd_orig %>%
  clean_names() %>% 
  filter(state == "VIC") %>%
  # deal with problem of multiple observations some days:
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(tests_conducted_total = max(tests_conducted_total, na.rm = TRUE),
            cumulative_case_count = max(cumulative_case_count, na.rm = TRUE)) %>%
  mutate(tests_conducted_total  = ifelse(tests_conducted_total < 0, NA, tests_conducted_total),
         cumulative_case_count = ifelse(cumulative_case_count < 0, NA, cumulative_case_count)) %>%
  ungroup() %>%
  # correct one typo, missing a zero
  mutate(tests_conducted_total = ifelse(date == as.Date("2020-07-10"), 1068000, tests_conducted_total)) %>%
  # remove two bad dates 
  filter(!date %in% as.Date(c("2020-06-06", "2020-06-07"))) %>%
  mutate(test_increase = c(tests_conducted_total[1], diff(tests_conducted_total)),
         confirm = c(cumulative_case_count[1], diff(cumulative_case_count)),
         pos_raw = pmin(1, confirm / test_increase)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), 
           fill = list(confirm = 0)) %>%
  mutate(numeric_date = as.numeric(date),
         positivity = pos_raw) %>%
  filter(date > as.Date("2020-02-01")) %>%
  fill(positivity, .direction = "downup") %>%
  # I don't believe the sqrt "corrected" cases helped here so have a much more modest 0.1.
  # But first we need to model positivity to smooth it, as it's far too spiky otherwise:
  mutate(ps1 = fitted(gam(positivity ~ s(numeric_date), data = ., family = "quasipoisson")),
         ps2 = fitted(loess(positivity ~ numeric_date, data = ., span = 0.1)),
         cases_corrected = confirm * ps1 ^ 0.1 / min(ps1 ^ 0.1)) %>%
  ungroup() %>%
  mutate(smoothed_confirm = fitted(loess(confirm ~ numeric_date, data = ., span = 0.1)))


the_caption <- "Data gathered by The Guardian; analysis by http://freerangestats.info"  

# Positivity plot:
d %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = pos_raw)) +
  geom_line(aes(y = ps2)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  labs(x = "", y = "Test positivity",
       title = "Positive test rates for COVID-19 in Melbourne, Victoria",
       caption = the_caption)

# Case numbers plot
d %>%
  select(date, cases_corrected, confirm) %>%
  gather(variable, value, -date) %>%
  mutate(variable = case_when(
    variable == "confirm" ~ "Recorded cases",
    variable == "cases_corrected" ~ "With small adjustment for test positivity"
  )) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_point()  +
  geom_smooth(se = FALSE, span = 0.07) +
  labs(x = "", y = "Number of new cases per day",
       colour = "",
       caption = the_caption,
       title = "Covid-19 cases per day in Melbourne, Victoria",
       subtitle = "With and without a small adjustment for test positivity. No adjustment for delay.")

pmf_covid <- c(0, dpois(0:20, lambda = 9))

# takes a few minutes
bp_covid <- backprojNP(sts(d$confirm), pmf_covid)

sharpened <- tibble(recovered_x = bp_covid@upperbound) %>%
  mutate(position = 1:n())

p_adj_conv <- d %>%
  mutate(position = 1:n()) %>%
  left_join(sharpened, by = "position") %>%
  select(date, recovered_x, confirm)  %>%
  mutate(recovered_x = replace_na(recovered_x, 0)) %>%
  gather(variable, value, -date) %>%
  mutate(variable = case_when(
    variable == "confirm" ~ "Confirmed cases",
    variable == "recovered_x" ~ "Estimated original cases accounting for delay"
  )) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line(size = 1.5) +
  labs(title = "Back-projection of Victorian Covid-19 infections",
       subtitle = str_wrap("Non-parametric back-projection of incidence cases assuming 
       average delay of 10 days between infection and observation, using methods in 
                           Becker et al (1991). No correction for right truncation of data,
                           so the last 15 days will be badly biased downwards.", 100),
       x = "",
       y = "Number of infections",
       colour = "")

print(p_adj_conv1)

#------------Estimating R with EpiNow2---------------------
# Various delay/lag distributions as per the Covid-19 examples in the EpiNow2 documentation.

reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 30)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 30)


#---------Based on original data-------------

estimates <- EpiNow2::epinow(reported_cases = d, 
                             generation_time = generation_time,
                             delays = list(incubation_period, reporting_delay),
                             horizon = 7, samples = 3000, warmup = 600, 
                             cores = 4, chains = 4, verbose = TRUE, 
                             adapt_delta = 0.95)

# Function for doing some mild polishing to the default plot from epinow();
# assumes existence in global environment of a ggplot2 theme called my_theme, 
# defined previously, and takes the output of epinow() as its main argument:
my_plot_estimates <- function(estimates, extra_title = ""){
  my_theme <- my_theme +
    theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) 
  
  p <- estimates$plots$summary
  
  p1 <- p$patches$plots[[1]] +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    my_theme +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, 1000)) +
    labs(title = glue("Estimated infections based on confirmed cases{extra_title}"),
         x = "") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  p2 <- p$patches$plots[[2]] +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    my_theme +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, 1000)) +
    labs(title = glue("Estimated infections taking delay{extra_title} into account"),
         x = "") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  p3 <- p$data %>% 
    filter(date > as.Date("2020-04-20")) %>%
    ggplot(aes(x = date, y = median, fill = type)) +
    my_theme +
    geom_hline(yintercept = 1, colour = "steelblue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_ribbon(aes(ymin = bottom, ymax = top), alpha = 0.1) +
    geom_line(aes(colour = type)) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    labs(title = glue("Effective Reproduction Number, correcting for both delay and right truncation{extra_title}"),
         y = bquote("Estimated" ~ R[t]),
         x = "")  +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  pc <- p1 + p2 + p3 + plot_layout(ncol = 1)
  
  return(pc)
}


my_plot_estimates(estimates)  

#---------Based on positivity-adjusted-------------

d2 <- select(d, date, cases_corrected) %>%
  mutate(confirm = round(cases_corrected) )

estimates2 <- EpiNow2::epinow(reported_cases = d2, 
                              generation_time = generation_time,
                              delays = list(incubation_period, reporting_delay),
                              horizon = 7, samples = 3000, warmup = 600, 
                              cores = 4, chains = 4, verbose = TRUE, 
                              adapt_delta = 0.95)


my_plot_estimates(estimates2, extra_title = " and positivity")

#-----------------the kanagawa data--------------

head(as.data.frame(gd_orig))
tail(as.data.frame(gd_orig))

dim(kanagawa_data)
dim(kanagawa_testing)
dim(cases_mat)
dim(gd_orig)
kd_orig <- matrix(NA, length(cases_per_day), 13)
colnames(kd_orig) <- colnames(gd_orig)
kd_orig <- as.data.frame(kd_orig)
kd_orig$State <- kanagawa_data$居住地[1]
kd_orig$Date <- as.Date(names(cases_per_day))
kd_orig$Time <- rep(NA, nrow(kd_orig))
kd_orig$`Cumulative case count` <- cumsum(cases_per_day)
kd_orig$`Update Source` <- rep("kanagawa.Metro.Govt", nrow(kd_orig))
kd_orig$`Tests conducted (negative)` <- tests_summary$total_neg[match(as.Date(names(cases_per_day)), tests_summary$Date)]
kd_orig$`Tests conducted (total)` <- tests_summary$total_tests[match(as.Date(names(cases_per_day)), tests_summary$Date)]
kd_orig$`Tests conducted (total)`[is.na(kd_orig$`Tests conducted (total)`)] <- 0
kd_orig$`Tests conducted (total)` <- cumsum(kd_orig$`Tests conducted (total)`)
head(kd_orig)
tail(kd_orig)

d <- kd_orig %>%
  clean_names() %>% 
  #filter(state == "VIC") %>%
  # deal with problem of multiple observations some days:
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(tests_conducted_total = max(tests_conducted_total, na.rm = TRUE),
            cumulative_case_count = max(cumulative_case_count, na.rm = TRUE)) %>%
  mutate(tests_conducted_total  = ifelse(tests_conducted_total <= 0, NA, tests_conducted_total),
         cumulative_case_count = ifelse(cumulative_case_count <= 0, NA, cumulative_case_count)) %>%
  ungroup() %>%
  ## these errors are specific to the Melbourne, Victoria  data 
  # correct one typo, missing a zero
  #mutate(tests_conducted_total = ifelse(date == as.Date("2020-07-10"), 1068000, tests_conducted_total)) %>%
  # remove two bad dates 
  #filter(!date %in% as.Date(c("2020-06-06", "2020-06-07"))) %>%
  mutate(test_increase = c(tests_conducted_total[1], diff(tests_conducted_total)),
         confirm = c(cumulative_case_count[1], diff(cumulative_case_count)),
         pos_raw = pmin(1, confirm / test_increase)) %>%
  complete(date = seq.Date(min(date), max(date), by="day"), 
           fill = list(confirm = 0)) %>%
  mutate(numeric_date = as.numeric(date),
         positivity = pos_raw) %>%
  filter(date > as.Date("2020-02-01")) %>%
  fill(positivity, .direction = "downup") %>%
  # I don't believe the sqrt "corrected" cases helped here so have a much more modest 0.1.
  # But first we need to model positivity to smooth it, as it's far too spiky otherwise:
  mutate(ps1 = fitted(gam(positivity ~ s(numeric_date), data = ., family = "quasipoisson")),
         ps2 = fitted(loess(positivity ~ numeric_date, data = ., span = 0.1)),
         cases_corrected = confirm * ps1 ^ 0.1 / min(ps1 ^ 0.1)) %>%
  ungroup() %>%
  mutate(smoothed_confirm = fitted(loess(confirm ~ numeric_date, data = ., span = 0.1)))


the_caption <- "Data gathered from Kanagawa Prefectural Government https://www.pref.kanagawa.jp/osirase/1369/\nAnalysis based on http://freerangestats.info"

# Positivity plot:
pdf("positivity.pdf", width = 6, height = 4)
d %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = pos_raw)) +
  geom_line(aes(y = ps2)) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  labs(x = "", y = "Test positivity",
       title = "Positive test rates for COVID-19 in Kanagawa, Japan",
       caption = the_caption)
dev.off()

# Case numbers plot
pdf("case_numbers.pdf", width = 6, height = 4)
d %>%
  select(date, cases_corrected, confirm) %>%
  gather(variable, value, -date) %>%
  mutate(variable = case_when(
    variable == "confirm" ~ "Recorded cases",
    variable == "cases_corrected" ~ "With small adjustment for test positivity"
  )) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_point()  +
  geom_smooth(se = FALSE, span = 0.07) +
  labs(x = "", y = "Number of new cases per day",
       colour = "",
       caption = the_caption,
       title = "Covid-19 cases per day in Kanagawa, Japan",
       subtitle = "With and without a small adjustment for test positivity. No adjustment for delay.")
dev.off()

pmf_covid <- c(0, dpois(0:20, lambda = 9))

# takes a few minutes
bp_covid <- backprojNP(sts(d$confirm), pmf_covid)

sharpened <- tibble(recovered_x = bp_covid@upperbound) %>%
  mutate(position = 1:n())

p_adj_conv <- d %>%
  mutate(position = 1:n()) %>%
  left_join(sharpened, by = "position") %>%
  select(date, recovered_x, confirm)  %>%
  mutate(recovered_x = replace_na(recovered_x, 0)) %>%
  gather(variable, value, -date) %>%
  mutate(variable = case_when(
    variable == "confirm" ~ "Confirmed cases",
    variable == "recovered_x" ~ "Estimated original cases accounting for delay"
  )) %>%
  ggplot(aes(x = date, y = value, colour = variable)) +
  geom_line(size = 1.5) +
  labs(title = "Back-projection of kanagawa Covid-19 infections",
       subtitle = str_wrap("Non-parametric back-projection of incidence cases assuming 
       average delay of 10 days between infection and observation, using methods in 
                           Becker et al (1991). No correction for right truncation of data,
                           so the last 15 days will be badly biased downwards.", 100),
       x = "",
       y = "Number of infections",
       colour = "")

pdf("backprojection.pdf", width = 8, height = 16/3)
print(p_adj_conv) + labs(caption = the_caption)
dev.off()

#------------Estimating R with EpiNow2---------------------
# Various delay/lag distributions as per the Covid-19 examples in the EpiNow2 documentation.

reporting_delay <- EpiNow2::bootstrapped_dist_fit(rlnorm(100, log(6), 1))
## Set max allowed delay to 30 days to truncate computation
reporting_delay$max <- 30

generation_time <- list(mean = EpiNow2::covid_generation_times[1, ]$mean,
                        mean_sd = EpiNow2::covid_generation_times[1, ]$mean_sd,
                        sd = EpiNow2::covid_generation_times[1, ]$sd,
                        sd_sd = EpiNow2::covid_generation_times[1, ]$sd_sd,
                        max = 30)

incubation_period <- list(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                          mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                          sd = EpiNow2::covid_incubation_period[1, ]$sd,
                          sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                          max = 30)


#---------Based on original data-------------

estimates <- EpiNow2::epinow(reported_cases = d, 
                             generation_time = generation_time,
                             delays = list(incubation_period, reporting_delay),
                             horizon = 7, samples = 3000, warmup = 600, 
                             cores = 4, chains = 4, verbose = TRUE, 
                             adapt_delta = 0.95)

# Function for doing some mild polishing to the default plot from epinow();
# assumes existence in global environment of a ggplot2 theme called my_theme, 
# defined previously, and takes the output of epinow() as its main argument:
my_theme <- ggplot2::theme_minimal()

my_plot_estimates <- function(estimates, extra_title = ""){
  my_theme <- my_theme +
    theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) 
  
  p <- estimates$plots$summary
  
  p1 <- p$patches$plots[[1]] +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    my_theme +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, 1000)) +
    labs(title = glue("Estimated infections based on confirmed cases{extra_title}"),
         x = "") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  p2 <- p$patches$plots[[2]] +
    scale_y_continuous(label = comma_format(accuracy = 1)) +
    my_theme +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    coord_cartesian(ylim = c(0, 1000)) +
    labs(title = glue("Estimated infections taking delay{extra_title} into account"),
         x = "") +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  p3 <- p$data %>% 
    filter(date > as.Date("2020-04-20")) %>%
    ggplot(aes(x = date, y = median, fill = type)) +
    my_theme +
    geom_hline(yintercept = 1, colour = "steelblue") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
    geom_ribbon(aes(ymin = bottom, ymax = top), alpha = 0.1) +
    geom_line(aes(colour = type)) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    labs(title = glue("Effective Reproduction Number, correcting for both delay{extra_title} and right truncation"),
         y = bquote("Estimated" ~ R[t]),
         x = "")  +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b", limits = range(p$data$date))
  
  pc <- p1 + p2 + p3 + plot_layout(ncol = 1)
  
  return(pc)
}

pdf("estimates.pdf", width = 10, height = 20)
my_plot_estimates(estimates)   +
  labs(caption = the_caption)
dev.off()

#---------Based on positivity-adjusted-------------

d2 <- select(d, date, cases_corrected) %>%
  mutate(confirm = round(cases_corrected) )

estimates2 <- EpiNow2::epinow(reported_cases = d2, 
                              generation_time = generation_time,
                              delays = list(incubation_period, reporting_delay),
                              horizon = 7, samples = 3000, warmup = 600, 
                              cores = 4, chains = 4, verbose = TRUE, 
                              adapt_delta = 0.95)

pdf("estimates_pos.pdf", width = 10, height = 20)
my_plot_estimates(estimates2, extra_title = " and positivity") +
  labs(caption = the_caption)
dev.off()

#---------Based on weekly-adjusted-------------

weekly <- table(kanagawa_data$曜日)[c(3, 6, 5, 4, 7, 1:2)]
testing_rate <- weekly / mean(weekly)

d3 <- d2
colnames(d3)[2] <- "cases"
library("data.table")
## Define a single report delay distribution
delay_def <- EpiNow2::lognorm_dist_def(mean = 5, 
                                       mean_sd = 1,
                                       sd = 3,
                                       sd_sd = 1,
                                       max_value = 30,
                                       samples = 1,
                                       to_log = TRUE)

## Define a single incubation period
incubation_def <- EpiNow2::lognorm_dist_def(mean = EpiNow2::covid_incubation_period[1, ]$mean,
                                            mean_sd = EpiNow2::covid_incubation_period[1, ]$mean_sd,
                                            sd = EpiNow2::covid_incubation_period[1, ]$sd,
                                            sd_sd = EpiNow2::covid_incubation_period[1, ]$sd_sd,
                                            max_value = 30, samples = 1)

d3 <- d2
d3[,2]
colnames(d2)[2] <- "cases"


d3 <- as.data.table(d3)
d3 <- EpiNow2::adjust_infection_to_report(d , 
                                          delay_defs = list(incubation_def, delay_def), 
                                          reporting_effect = testing_rate)

estimates3 <- EpiNow2::adjust_infection_to_report(reported_cases = d3, 
                                                  generation_time = generation_time,
                                                  delays = list(incubation_period, reporting_delay),
                                                  horizon = 7, samples = 3000, warmup = 600, 
                                                  cores = 4, chains = 1, verbose = TRUE, 
                                                  adapt_delta = 0.99)

d3 <- d
d3[,"cases"] <- d3[,"cases"] / testing_rate[as.numeric(days)]
d3[, "cumulative_case_count"] <- cumsum(d3[,"cases"])
colnames(d3)[11] <- "corrected_cases"

estimates3 <- EpiNow2::epinow(reported_cases = d3, 
                              generation_time = generation_time,
                              delays = list(incubation_period, reporting_delay),
                              horizon = 7, samples = 3000, warmup = 600, 
                              cores = 4, chains = 4, verbose = TRUE, 
                              adapt_delta = 0.95)

pdf("estimates_weekly.pdf", width = 10, height = 20)
my_plot_estimates(estimates3, extra_title = " and seasonally adjusted") +
  labs(caption = the_caption)
dev.off()

d4 <- d2
d4[,"cases"] <- d4[,"cases"] / testing_rate[as.numeric(days)]

estimates4 <- EpiNow2::epinow(reported_cases = d4, 
                              generation_time = generation_time,
                              delays = list(incubation_period, reporting_delay),
                              horizon = 7, samples = 3000, warmup = 600, 
                              cores = 4, chains = 4, verbose = TRUE, 
                              adapt_delta = 0.95)

pdf("estimates_weekly_pos.pdf", width = 10, height = 20)
my_plot_estimates(estimates4, extra_title = " positivity and seasonally adjusted") +
  labs(caption = the_caption)
dev.off()

png("estimates_weekly_pos.png", width = 10*75, height = 20*75)
my_plot_estimates(estimates4, extra_title = " positivity and seasonally adjusted") +
  labs(caption = the_caption)
dev.off()

