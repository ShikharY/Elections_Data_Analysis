library(haven)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)
library(reshape2)
library(forecast)

elections <-
  read_dta(
    "Data/dataverse_files/Bhavnani India State Election Dataset v 3.0.dta"
  )

barplot(table(elections$cand_sex))

#Function to calculate gender ratio of a particular year

calc_gender_ratio_per_year <- function(year) {
  sum(elections$year == year & elections$cand_sex == 'F') /
    sum(elections$year == year  & elections$cand_sex == 'M')
}

#function to calculate number of years

number_of_years <-
  function(st_name) {
    #CAN USE LENGTH FUNCTION FOR THIS.
    years <- (sum(table(unique(st_name))))
    return(years)
  }


#calculating gender ratios over the years

gender_ratio <- numeric()
year <- unique(elections$year)

for (i in 1:number_of_years(elections$year)) {
  gender_ratio[i] <- calc_gender_ratio_per_year(year[i])
  
}

#storing gender ratio values in a new dataset,
gender_ratios_by_year <-
  data.frame("year" = year,
             "ratios" = gender_ratio,
             stringsAsFactors = FALSE)


#Plotting the gender ratios vs year using ggplot2, adding a red line to visualize a g perfect gender ratio of 1 and
#a green line to visualize India's gender ratio in 2015.

ggplot(gender_ratios_by_year, aes(x = year, y = ratios*100), color = "steelblue") +
  geom_point() + geom_smooth() +
  labs(title = "Election Data (1977 - 2015)", x = "Year", y = "Gender Ratio of Cnadidates (Females per 100 Males)") +
  geom_hline(yintercept = 100,
             color = "red",
             size = 2) +
  geom_hline(yintercept = 91.8,
             color = "green",
             size = 2)
ggsave("National_Average_Gender_Ratio_Trend.png")


#statistical significance by stimulating and sampling
simulation <- gender_ratios_by_year
trials <- 100000
counts <- numeric(100000)

for (i in 1:trials) {
  simulation$ratios <- sample(simulation$ratios)
  
  reg_stat_significance <-
    lm(simulation$ratios ~ simulation$X.year.....year)
  
  counts[i] <- reg_stat_significance$coefficients[2]
  
}

hist(counts, xlim = c(-0.0019, 0.0019))
#creating a linear regression model for actual data and displaying the slope of actual regression as a vertical red line.
reg_gender_ratios_by_year <-
  lm(gender_ratios_by_year$ratios ~ gender_ratios_by_year$X.year.....year)
abline(v = reg_gender_ratios_by_year$coefficients[2], col = "red")

#even with 1 lakh random trials it is near impossible to get the positive trend that we see. hence it is statistically significant.
#it also has a very low p value.



##looping state wise data

election_split <- split(elections, elections$st_name)
states_data_list <- list()
for (n in names(election_split)) {
  dat <- election_split[[n]]   #Extract the data from the list
  states_data_list[[n]] <- data.frame(
    st_name = n,
    year = dat$year,
    ac_no = dat$ac_no,
    ac_name = dat$ac_name,
    ac_type = dat$ac_type,
    cand_name = dat$cand_name,
    cand_sex = dat$cand_sex,
    partyname = dat$partyname,
    partyabbre = dat$partyabbre,
    totvotpoll = dat$totvotpoll,
    electors = dat$electors
  )
  
}



dat_ratio <- numeric()
states_gen_ratio_list <- list()
gen_ratio_dat <- list()
for (s in names(states_data_list)) {
  state_years <- unique(states_data_list[[s]]$year)
  dat_ratio <- numeric()
  
  for (i in 1:length(state_years)) {
    dat_ratio[i] <-
      sum(states_data_list[[s]]$year == state_years[i] &
            states_data_list[[s]]$cand_sex == 'F') /
      sum(states_data_list[[s]]$year == state_years[i] &
            states_data_list[[s]]$cand_sex == 'M')
    
  }
  states_gen_ratio_list[[s]] <- data.frame(year = state_years,
                                           gen_ratio = dat_ratio,
                                           st_name = unique(states_data_list[[s]]$st_name))
}



states_gen_ratio_data <- do.call(rbind, states_gen_ratio_list)

by_state <- states_gen_ratio_data %>% group_by(st_name)

ggplot(data=by_state, aes(x = year, y = gen_ratio, group = st_name)) + 
  coord_cartesian(ylim = c(0, 1)) + geom_point(aes(color = st_name)) + 
  geom_line(aes(color = st_name))









ggplot(states_gen_ratio_data, aes(x = year, y = gen_ratios)) +
  geom_point(aes(color = st_name))  + geom_abline(
    slope = reg$coefficients[2],
    intercept = reg$coefficients[1],
    col = "blue",
    size = 1
  ) +
  coord_cartesian(ylim = c(0, 0.2)) +
  
  geom_hline(yintercept = 1,
             color = "red",
             size = 2) +
  geom_hline(yintercept = 0.918,
             color = "green",
             size = 2) +
  
  geom_point(data = kerala_plot, aes(y = g_ratio, x = year, color = "Kerala")) +
  geom_abline(
    slope = reg_kerala$coefficients[2],
    intercept = reg_kerala$coefficients[1],
    col = "red",
    size = 1
  ) +
  geom_point(data = Sikkim_plot, aes(x = year, y = g_ratio, color = "Sikkim")) +
  geom_abline(
    slope = reg_sikkim$coefficients[2],
    intercept = reg_sikkim$coefficients[1],
    color = "orange",
    size = 1
  ) +
  labs(
    title = "Election Data (1978 - 2015)",
    x = "Year",
    y = "Gender Ratio of Cnadidates (Females per Males)",
    colour = c("National", "Sikkim", "Delhi", "Kerala", "Bihar")
  ) +
  geom_point(data = Delhi_plot, aes(x = year, y = g_ratio, color = "Delhi")) + geom_abline(
    slope = reg_delhi$coefficients[2],
    intercept = reg_delhi$coefficients[1],
    col = "pink",
    size = 1
  ) +
  geom_point(data = Bihar_plot, aes(x = year, y = g_ratio, color = "Bihar")) +
  geom_abline(
    slope = reg_bihar$coefficients[2],
    intercept = reg_bihar$coefficients[1],
    col = "yellow",
    size = 1
  ) + theme_dark() +
  scale_color_manual(
    name = "States",
    values = c(
      "National" = "blue",
      "Kerala" = "red",
      "Sikkim" = "orange",
      "Delhi" = "Pink",
      "Bihar" = "yellow"
    ),
    labels = c("Bihar", "Delhi", "Kerala", "National", "Sikkim")
  ) + theme(legend.position = c(0.90, 0.80))



#looking at gender ratio in major political parties

unique(elections$partyname)


gender_ratio_party <- function (party) {
  g_ratio_party <-
    sum(elections$partyname == party & elections$cand_sex == 'F') /
    sum(elections$partyname == party  & elections$cand_sex == 'M')
  return(g_ratio_party)
}

Independent <- gender_ratio_party ("Independent")
BJP <- gender_ratio_party("Bhartiya Janata Party")
INC <- gender_ratio_party("Indian National Congress")
DMK <- gender_ratio_party("Dravida Munnetra Kazhagam")
CPIM <- gender_ratio_party("Communist Party Of India (Marxist)")

party_ratio <- c(Independent, BJP, INC, DMK, CPIM)
party_name <- c("Independent", "BJP", "INC", "DMK", "CPIM")

paty_data <-
  data.frame("gender_ratio" = party_ratio, "party" = party_name)

plot(party_name ~ party_ratio)



######TRYING OUT ANIMATION

gen_elec_years <- c(1977, 1980, 1989, 1991, 1996, 2009, 2014)


calc_males <- function(year) {
  sum(elections$year == year & elections$cand_sex == "M")
}

calc_females <- function(year) {
  sum(elections$year == year & elections$cand_sex == "F")
}

male <- numeric()
year_ani <- gen_elec_years

for (i in 1:number_of_years(gen_elec_years)) {
  male[i] <- calc_males(year_ani[i])
  
}

female <- numeric()

for (i in 1:number_of_years(gen_elec_years)) {
  female[i] <- calc_females(year_ani[i])
  
}


animations_data <-
  data.frame("year" = year_ani,
             "males" = male,
             "females" = female)

animations_data <- animations_data %>% arrange(year)




test_data_long <-
  melt(animations_data, id = "year")  # convert to long format

p <- ggplot(data = test_data_long,
            aes(x = year, y = value, colour = variable)) +
  geom_point() + geom_smooth(se = FALSE)

p

p +
  geom_point() +
  transition_reveal(year)

#NOTE: try using male or female candidates per 10,000 voters to get a more standardised idea over state and general electsions

calc_male_cand_per_voters <- function(year) {
  sum(elections$cand_sex == "M" & elections$year == year) /
    sum(distinct_electors$electors[distinct_electors$year == year])
}

calc_female_cand_per_voters <- function(year) {
  sum(elections$year == year & elections$cand_sex == "F") /
    sum(distinct_electors$electors[distinct_electors$year == year])
}


male_per_voters <- numeric()
year_per_voters <- unique(elections$year)

for (i in 1:number_of_years(elections$year)) {
  male_per_voters[i] <- calc_male_cand_per_voters(year_per_voters[i])
}


female_per_voters <- numeric()

for (i in 1:number_of_years(elections$year)) {
  female_per_voters[i] <-
    calc_female_cand_per_voters(year_per_voters[i])
}


cands_per_voter <- data.frame(
  "year" = year_per_voters,
  "Males_per_voters" = male_per_voters * 100,
  "Females_per_voters" = female_per_voters *
    100
)

cands_per_voter <- cands_per_voter %>% arrange(year)

p2 <- ggplot(cands_per_voter) +
  geom_line(aes(x = year, y = Males_per_voters, colour = "males")) +
  geom_line(aes(x = year, y = Females_per_voters, colour = "females"))
p2





p2 +
  transition_reveal(year)


#the ratio of male candidates per voters seems too high, closing in on one, suggesting that almost all electors were candidates

distinct_electors <-
  elections %>% distinct(electors, .keep_all = TRUE)
sum(distinct_electors$electors[distinct_electors$year == 1978])


#1993 spike problem

cands_per_voter_long <-
  melt(cands_per_voter, id = "year")  # convert to long format
cands_per_voter_long %>% filter(year != 1979) -> cands_per_voter_long


p3 <-
  ggplot(cands_per_voter_long, aes(x = year, y = value, colour = variable)) +
  geom_point() + geom_smooth(se = FALSE)

p3




p3 +
  transition_reveal(year) + geom_

#### forecasting and animating when gender ration will be equal to 1



gender_ratio_ts <- ts(gender_ratios_by_year[, 2], start = 1978)
fc <- naive(gender_ratio_ts, h = 50)
autoplot(gender_ratio_ts)
checkresiduals(fc)
autoplot(fc, PI = FALSE)

# forecasting using regression
