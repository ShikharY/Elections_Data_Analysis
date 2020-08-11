library(shiny)
library(rsconnect)
library(haven)
library(tidyverse)
library(ggthemes)

shinyServer(function(input, output){
  



elections <-
  read_dta(
    "Bhavnani India State Election Dataset v 3.0.dta", encoding = "latin1"
  )

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
      (sum(states_data_list[[s]]$year == state_years[i] &
            states_data_list[[s]]$cand_sex == 'F') /
      sum(states_data_list[[s]]$year == state_years[i] &
            states_data_list[[s]]$cand_sex == 'M'))*100
    
  }
  states_gen_ratio_list[[s]] <- data.frame(year = state_years,
                                           gen_ratio = dat_ratio,
                                           st_name = unique(states_data_list[[s]]$st_name))
}



states_gen_ratio_data <- do.call(rbind, states_gen_ratio_list)

  output$plot <- renderPlot({
    states_gen_ratio_data %>% filter(st_name %in% input$state) %>%
      ggplot() + 
      coord_cartesian(ylim = c(0, input$y_axis), xlim = c(input$year)) + 
      geom_point(aes(size = gen_ratio, col = st_name, group = st_name, x = year, y = gen_ratio)) + 
      geom_smooth(aes(col = st_name, x = year, y= gen_ratio),se = FALSE, method = "lm") + 
      labs( x = "Year", y = "Gender Ratio (Female Candidates per (0-100) Male Candidates)") +
      guides(col=guide_legend("State"),
             size=guide_legend("Women per 100 Men"))
  })

} )

