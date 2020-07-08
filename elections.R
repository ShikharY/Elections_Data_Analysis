library(haven)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(av)
library(png)
library(reshape2)
Bhavnani_India_State_Election_Dataset_v_3_0 <- read_dta("C:/Users/home/Desktop/dataverse_files (1)/Bhavnani India State Election Dataset v 3.0.dta")

#Inspecting data
elections <- Bhavnani_India_State_Election_Dataset_v_3_0
barplot(table(elections$cand_sex))

#Function to calculate gender ratio of a particular year

calc_gender_ratio_per_year <- function(year) {
  
  sum(elections$year == year & elections$cand_sex == 'F') / 
    sum(elections$year == year  & elections$cand_sex == 'M')
}
 
#function to calculate number of years 

number_of_years <- function(st_name){
  years <- (sum(table(unique(st_name))))
  return(years)
}


#calculating gender ratios over the years

gender_ratio <- numeric()
year <- unique(elections$year)

for (i in 1:number_of_years(elections$year)) {
  gender_ratio[i] <- calc_gender_ratio_per_year(year[i])
  
  }

#storing gender ratio values in a new dataset, gender_ratios_by_year and arranging them in chronological order.
gender_ratios_by_year <- data.frame("year" <- year, "ratios" = gender_ratio, 
                                    stringsAsFactors = FALSE )

gender_ratios_by_year%>% arrange(year)

#Plotting the gender ratios vs year using ggplot2, adding a red line to visualize a g perfect gender ratio of 1 and
#a green line to visualize India's gender ratio in 2015.

ggplot(gender_ratios_by_year, aes(x=year, y= ratios), color="steelblue") + 
  geom_point() + geom_smooth() +
  labs(title="Election Data (1977 - 2015)", x="Year", y="Gender Ratio of Cnadidates (Females per Males)") + 
  geom_hline(yintercept = 1, color = "red", size = 2 ) +
  geom_hline(yintercept = 0.918, color = "green", size = 2 )
  

 
#statistical siginificance by stimulating and sampling
simulation <- gender_ratios_by_year
trials <- 100000
counts <- numeric(100000)

for ( i in 1:trials) { 
  
  simulation$ratios <- sample(simulation$ratios)
  
  reg_stat_significance <- lm(simulation$ratios~simulation$X.year.....year)
   
  counts[i] <- reg_stat_significance$coefficients[2]
  
}

hist(counts, xlim = c(-0.0019, 0.0019))
#creating a linear regression model for actual data and displaying the slope of actual regression as a vertical red line.
reg_gender_ratios_by_year <-  lm(gender_ratios_by_year$ratios~gender_ratios_by_year$X.year.....year)
abline(v=reg_gender_ratios_by_year$coefficients[2], col = "red")

#even with 1 lakh random trials it is near impossible to get the postive trend that we see. hence it is statistically sigificant.
#it also has a very low p value.



#########looping state wise data

states <- unique(elections$st_name)

for (i in number_of_years(elections$st_name)) {

  states[i] <- data.frame(subset(elections, st_name == states[i] ))


  gender_ratio <- numeric()
  year <- unique(elections$year)
  
  for (i in 1:number_of_years(elections$year)) {
    gender_ratio[i] <- calc_gender_ratio_per_year(year[i])
    
  }
  
  #storing gender ratio values in a new dataset, gender_ratios_by_year and arranging them in chronological order.
  gender_ratios_by_year <- data.frame("year" <- year, "ratios" = gender_ratio, 
                                      stringsAsFactors = FALSE )
  
    }


#storing gender ratio values in a new dataset, gender_ratios_by_year and arranging them in chronological order.
gender_ratios_by_year <- data.frame("year" <- year, "ratios" = gender_ratio, 
                                    stringsAsFactors = FALSE )


#trying for loop

ratio_k <- numeric(number_of_years(elections$st_name == "kerala"))
kkk <- unique(kerala$year)

for (i in 1:9) {
  
  ratio_k[i] <- sum(kerala$year == kkk[i] & kerala$cand_sex == 'F') / 
    sum(kerala$year == kkk[i] & kerala$cand_sex == 'M')
  
} 

kerala_plot <- data.frame("year" = unique(kerala$year), "g_ratio" = ratio_k)


andhra_Pradesh <- subset(elections, st_name == "Andhra Pradesh" )

#loop

ratio_ap <- numeric(9)
ap <- unique(andhra_Pradesh$year)

for (i in 1:9) {
  
  ratio_ap[i] <- sum(andhra_Pradesh$year == ap[i] & andhra_Pradesh$cand_sex == 'F') / 
    sum(andhra_Pradesh$year == ap[i] & andhra_Pradesh$cand_sex == 'M')
  
} 

andhra_pradesh_plot <- data.frame("year" = unique(andhra_Pradesh$year), "g_ratio" = ratio_ap)

#

assam <- subset(elections, st_name == "Assam" )

#loop

ratio_ass <- numeric(8)
ass <- unique(assam$year)

for (i in 1:8) {
  
  ratio_ass[i] <- sum(assam$year == ass[i] & assam$cand_sex == 'F') / 
    sum(assam$year == ass[i] & assam$cand_sex == 'M')
  
} 

assam_plot <- data.frame("year" = unique(assam$year), "g_ratio" = ratio_ass)
##


chhattisgarh <- subset(elections, st_name == "Chhattisgarh" )

#loop

ratio_chh <- numeric(3)
chh <- unique(chhattisgarh$year)

for (i in 1:3) {
  
  ratio_chh[i] <- sum(chhattisgarh$year == chh[i] & chhattisgarh$cand_sex == 'F') / 
    sum(chhattisgarh$year == chh[i] & chhattisgarh$cand_sex == 'M')
  
} 

chhattisgarh_plot <- data.frame("year" = unique(chhattisgarh$year), "g_ratio" = ratio_chh)
##



gujarat <- subset(elections, st_name == "Gujarat" )

#loop

ratio_guj <- numeric(number_of_years(gujarat$year))
guj <- unique(gujarat$year)

for (i in 1:number_of_years(gujarat$year)) {
  
  ratio_guj[i] <- sum(gujarat$year == guj[i] & gujarat$cand_sex == 'F') / 
    sum(gujarat$year == guj[i] & gujarat$cand_sex == 'M')
  
} 

gujarat_plot <- data.frame("year" = unique(gujarat$year), "g_ratio" = ratio_guj)

##

 himachal_pradesh <- subset(elections, st_name == "Himachal Pradesh" )
 
 #loop
 
 ratio_hp <- numeric(number_of_years(himachal_pradesh$year))
 hp <- unique(himachal_pradesh$year)
 
 for (i in 1:number_of_years(himachal_pradesh$year)) {
   
   ratio_hp[i] <- sum(himachal_pradesh$year == hp[i] & himachal_pradesh$cand_sex == 'F') / 
     sum(himachal_pradesh$year == hp[i] & himachal_pradesh$cand_sex == 'M')
   
 } 
 
himachal_pradesh_plot <- data.frame("year" = unique(himachal_pradesh$year), "g_ratio" = ratio_hp)
 ##

############ 

Jharkhand <- subset(elections, st_name == "Jharkhand" )

#loop

ratio_jh <- numeric(number_of_years(Jharkhand$year))
jh <- unique(Jharkhand$year)

for (i in 1:number_of_years(Jharkhand$year)) {
  
  ratio_jh[i] <- sum(Jharkhand$year == jh[i] & Jharkhand$cand_sex == 'F') / 
    sum(Jharkhand$year == jh[i] & Jharkhand$cand_sex == 'M')
  
} 

Jharkhand_plot <- data.frame("year" = unique(Jharkhand$year), "g_ratio" = ratio_jh)
##

############ 

Maharashtra <- subset(elections, st_name == "Maharashtra" )

#loop

ratio_mh <- numeric(number_of_years(Maharashtra$year))
mh <- unique(Maharashtra$year)

for (i in 1:number_of_years(Maharashtra$year)) {
  
  ratio_mh[i] <- sum(Maharashtra$year == mh[i] & Maharashtra$cand_sex == 'F') / 
    sum(Maharashtra$year == mh[i] & Maharashtra$cand_sex == 'M')
  
} 

Maharashtra_plot <- data.frame("year" = unique(Maharashtra$year), "g_ratio" = ratio_mh)
##
############ 

Meghalaya <- subset(elections, st_name == "Meghalaya" )

#loop

ratio_mg <- numeric(number_of_years(Meghalaya$year))
mg <- unique(Meghalaya$year)

for (i in 1:number_of_years(Meghalaya$year)) {
  
  ratio_mg[i] <- sum(Meghalaya$year == mg[i] & Meghalaya$cand_sex == 'F') / 
    sum(Meghalaya$year == mg[i] & Meghalaya$cand_sex == 'M')
  
} 

Meghalaya_plot <- data.frame("year" = unique(Meghalaya$year), "g_ratio" = ratio_mg)
##
############ 

Nagaland <- subset(elections, st_name == "Nagaland" )

#loop

ratio_ng <- numeric(number_of_years(Nagaland$year))
ng <- unique(Nagaland$year)

for (i in 1:number_of_years(Nagaland$year)) {
  
  ratio_ng[i] <- sum(Nagaland$year == ng[i] & Nagaland$cand_sex == 'F') / 
    sum(Nagaland$year == ng[i] & Nagaland$cand_sex == 'M')
  
} 

Nagaland_plot <- data.frame("year" = unique(Nagaland$year), "g_ratio" = ratio_ng)
##


############ 

Odisha <- subset(elections, st_name == "Odisha" )

#loop

ratio_od <- numeric(number_of_years(Odisha$year))
od <- unique(Odisha$year)

for (i in 1:number_of_years(Odisha$year)) {
  
  ratio_od[i] <- sum(Odisha$year == od[i] & Odisha$cand_sex == 'F') / 
    sum(Odisha$year == od[i] & Odisha$cand_sex == 'M')
  
} 

odisha_plot <- data.frame("year" = unique(Odisha$year), "g_ratio" = ratio_od)
##

############ 

Punjab <- subset(elections, st_name == "Punjab" )

#loop

ratio_pj <- numeric(number_of_years(Punjab$year))
pj <- unique(Punjab$year)

for (i in 1:number_of_years(Punjab$year)) {
  
  ratio_pj[i] <- sum(Punjab$year == pj[i] & Punjab$cand_sex == 'F') / 
    sum(Punjab$year == pj[i] & Punjab$cand_sex == 'M')
  
} 

Punjab_plot <- data.frame("year" = unique(Punjab$year), "g_ratio" = ratio_pj)
##


############ 

Sikkim <- subset(elections, st_name == "Sikkim" )

#loop

ratio_sk <- numeric(number_of_years(Sikkim$year))
sk <- unique(Sikkim$year)

for (i in 1:number_of_years(Sikkim$year)) {
  
  ratio_sk[i] <- sum(Sikkim$year == sk[i] & Sikkim$cand_sex == 'F') / 
    sum(Sikkim$year == sk[i] & Sikkim$cand_sex == 'M')
  
} 

Sikkim_plot <- data.frame("year" = unique(Sikkim$year), "g_ratio" = ratio_sk)
##

############ 

Tripura <- subset(elections, st_name == "Tripura" )

#loop

ratio_tr <- numeric(number_of_years(Tripura$year))
tr <- unique(Tripura$year)

for (i in 1:number_of_years(Tripura$year)) {
  
  ratio_tr[i] <- sum(Tripura$year == tr[i] & Tripura$cand_sex == 'F') / 
    sum(Tripura$year == tr[i] & Tripura$cand_sex == 'M')
  
} 

Tripura_plot <- data.frame("year" = unique(Tripura$year), "g_ratio" = ratio_tr)
##

############ 

Uttarakhand <- subset(elections, st_name == "Uttarakhand" )

#loop

ratio_ut <- numeric(number_of_years(Uttarakhand$year))
ut <- unique(Uttarakhand$year)

for (i in 1:number_of_years(Uttarakhand$year)) {
  
  ratio_ut[i] <- sum(Uttarakhand$year == ut[i] & Uttarakhand$cand_sex == 'F') / 
    sum(Uttarakhand$year == ut[i] & Uttarakhand$cand_sex == 'M')
  
} 

Uttarakhand_plot <- data.frame("year" = unique(Uttarakhand$year), "g_ratio" = ratio_ut)
##

############ 

Arunachal_Pradesh <- subset(elections, st_name == "Arunachal Pradesh" )

#loop

ratio_ap <- numeric(number_of_years(Arunachal_Pradesh$year))
ap <- unique(Arunachal_Pradesh$year)

for (i in 1:number_of_years(Arunachal_Pradesh$year)) {
  
  ratio_ap[i] <- sum(Arunachal_Pradesh$year == ap[i] & Arunachal_Pradesh$cand_sex == 'F') / 
    sum(Arunachal_Pradesh$year == ap[i] & Arunachal_Pradesh$cand_sex == 'M')
  
} 

Arunachal_Pradesh_plot <- data.frame("year" = unique(Arunachal_Pradesh$year), "g_ratio" = ratio_ap)
##


############ 

Bihar <- subset(elections, st_name == "Bihar" )

#loop

ratio_bh <- numeric(number_of_years(Bihar$year))
bh <- unique(Bihar$year)

for (i in 1:number_of_years(Bihar$year)) {
  
  ratio_bh[i] <- sum(Bihar$year == bh[i] & Bihar$cand_sex == 'F') / 
    sum(Bihar$year == bh[i] & Bihar$cand_sex == 'M')
  
} 

Bihar_plot <- data.frame("year" = unique(Bihar$year), "g_ratio" = ratio_bh)
##

############ 

Goa <- subset(elections, st_name == "Goa" )

#loop

ratio_go <- numeric(number_of_years(Goa$year))
go <- unique(Goa$year)

for (i in 1:number_of_years(Goa$year)) {
  
  ratio_go[i] <- sum(Goa$year == go[i] & Goa$cand_sex == 'F') / 
    sum(Goa$year == go[i] & Goa$cand_sex == 'M')
  
} 

Goa_plot <- data.frame("year" = unique(Goa$year), "g_ratio" = ratio_go)
##

############ 

Haryana <- subset(elections, st_name == "Haryana" )

#loop

ratio_hr <- numeric(number_of_years(Haryana$year))
hr <- unique(Haryana$year)

for (i in 1:number_of_years(Haryana$year)) {
  
  ratio_hr[i] <- sum(Haryana$year == hr[i] & Haryana$cand_sex == 'F') / 
    sum(Haryana$year == hr[i] & Haryana$cand_sex == 'M')
  
} 

Haryana_plot <- data.frame("year" = unique(Haryana$year), "g_ratio" = ratio_hr)
##

############ 

Jammu_Kashmir <- subset(elections, st_name == "Jammu & Kashmir" )

#loop

ratio_jk <- numeric(number_of_years(Jammu_Kashmir$year))
jk <- unique(Jammu_Kashmir$year)

for (i in 1:number_of_years(Jammu_Kashmir$year)) {
  
  ratio_jk[i] <- sum(Jammu_Kashmir$year == jk[i] & Jammu_Kashmir$cand_sex == 'F') / 
    sum(Jammu_Kashmir$year == jk[i] & Jammu_Kashmir$cand_sex == 'M')
  
} 

Jammu_Kashmir_plot <- data.frame("year" = unique(Jammu_Kashmir$year), "g_ratio" = ratio_jk)
##

############ 

Karnataka <- subset(elections, st_name == "Karnataka" )

ratio_ka <- numeric(number_of_years(Karnataka$year))
ka <- unique(Karnataka$year)

for (i in 1:number_of_years(Karnataka$year)) {
  
  ratio_ka[i] <- sum(Karnataka$year == ka[i] & Karnataka$cand_sex == 'F') / 
    sum(Karnataka$year == ka[i] & Karnataka$cand_sex == 'M')
  
} 

Karnataka_plot <- data.frame("year" = unique(Karnataka$year), "g_ratio" = ratio_ka)
##

############ 

Madhya_Pradesh <- subset(elections, st_name == "Madhya Pradesh" )

ratio_mp <- numeric(number_of_years(Madhya_Pradesh$year))
mp <- unique(Madhya_Pradesh$year)

for (i in 1:number_of_years(Madhya_Pradesh$year)) {
  
  ratio_mp[i] <- sum(Madhya_Pradesh$year == mp[i] & Madhya_Pradesh$cand_sex == 'F') / 
    sum(Madhya_Pradesh$year == mp[i] & Madhya_Pradesh$cand_sex == 'M')
  
} 

Madhya_Pradesh_plot <- data.frame("year" = unique(Madhya_Pradesh$year), "g_ratio" = ratio_mp)
##


############ 

Manipur <- subset(elections, st_name == "Manipur" )

ratio_mr <- numeric(number_of_years(Manipur$year))
mr <- unique(Manipur$year)

for (i in 1:number_of_years(Manipur$year)) {
  
  ratio_mr[i] <- sum(Manipur$year == mr[i] & Manipur$cand_sex == 'F') / 
    sum(Manipur$year == mr[i] & Manipur$cand_sex == 'M')
  
} 

Manipur_plot <- data.frame("year" = unique(Madhya_Pradesh$year), "g_ratio" = ratio_mp)
##

############ 

Mizoram <- subset(elections, st_name == "Mizoram" )

ratio_mz <- numeric(number_of_years(Mizoram$year))
mz <- unique(Mizoram$year)

for (i in 1:number_of_years(Mizoram$year)) {
  
  ratio_mz[i] <- sum(Mizoram$year == mz[i] & Mizoram$cand_sex == 'F') / 
    sum(Mizoram$year == mz[i] & Mizoram$cand_sex == 'M')
  
} 

Mizoram_plot <- data.frame("year" = unique(Mizoram$year), "g_ratio" = ratio_mz)
##

############ 

Delhi <- subset(elections, st_name == "National Capital Territory Of Delhi" )

ratio_dl <- numeric(number_of_years(Delhi$year))
dl <- unique(Delhi$year)

for (i in 1:number_of_years(Delhi$year)) {
  
  ratio_dl[i] <- sum(Delhi$year == dl[i] & Delhi$cand_sex == 'F') / 
    sum(Delhi$year == dl[i] & Delhi$cand_sex == 'M')
  
} 

Delhi_plot <- data.frame("year" = unique(Delhi$year), "g_ratio" = ratio_dl)
##


############ 

Puducherry <- subset(elections, st_name == "Puducherry" )

ratio_pc <- numeric(number_of_years(Puducherry$year))
pc <- unique(Puducherry$year)

for (i in 1:number_of_years(Puducherry$year)) {
  
  ratio_pc[i] <- sum(Puducherry$year == pc[i] & Puducherry$cand_sex == 'F') / 
    sum(Puducherry$year == pc[i] & Puducherry$cand_sex == 'M')
  
} 

Puducherry_plot <- data.frame("year" = unique(Puducherry$year), "g_ratio" = ratio_pc)
##

############ 

Rajasthan <- subset(elections, st_name == "Rajasthan" )

ratio_rj <- numeric(number_of_years(Rajasthan$year))
rj <- unique(Rajasthan$year)

for (i in 1:number_of_years(Rajasthan$year)) {
  
  ratio_rj[i] <- sum(Rajasthan$year == rj[i] & Rajasthan$cand_sex == 'F') / 
    sum(Rajasthan$year == rj[i] & Rajasthan$cand_sex == 'M')
  
} 

Rajasthan_plot <- data.frame("year" = unique(Rajasthan$year), "g_ratio" = ratio_rj)
##

############ 

Tamil_Nadu <- subset(elections, st_name == "Tamil Nadu" )

ratio_tn <- numeric(number_of_years(Tamil_Nadu$year))
tn <- unique(Tamil_Nadu$year)

for (i in 1:number_of_years(Tamil_Nadu$year)) {
  
  ratio_tn[i] <- sum(Tamil_Nadu$year == tn[i] & Tamil_Nadu$cand_sex == 'F') / 
    sum(Tamil_Nadu$year == tn[i] & Tamil_Nadu$cand_sex == 'M')
  
} 

Tamil_Nadu_plot <- data.frame("year" = unique(Tamil_Nadu$year), "g_ratio" = ratio_tn)
##

############ 

Uttar_Pradesh <- subset(elections, st_name == "Uttar Pradesh" )

ratio_up <- numeric(number_of_years(Uttar_Pradesh$year))
up <- unique(Uttar_Pradesh$year)

for (i in 1:number_of_years(Uttar_Pradesh$year)) {
  
  ratio_up[i] <- sum(Uttar_Pradesh$year == up[i] & Uttar_Pradesh$cand_sex == 'F') / 
    sum(Uttar_Pradesh$year == up[i] & Uttar_Pradesh$cand_sex == 'M')
  
} 

Uttar_Pradesh_plot <- data.frame("year" = unique(Uttar_Pradesh$year), "g_ratio" = ratio_up)
##

############ 

West_Bengal <- subset(elections, st_name == "West Bengal" )

ratio_wb <- numeric(number_of_years(West_Bengal$year))
wb <- unique(West_Bengal$year)

for (i in 1:number_of_years(West_Bengal$year)) {
  
  ratio_wb[i] <- sum(West_Bengal$year == wb[i] & West_Bengal$cand_sex == 'F') / 
    sum(West_Bengal$year == wb[i] & West_Bengal$cand_sex == 'M')
  
} 

West_Bengal_plot <- data.frame("year" = unique(West_Bengal$year), "g_ratio" = ratio_wb)
##

plot(ratio_plot_data$gen_ratios~ratio_plot_data$year_ratio, ylim = c(0,1), xlab = " Election Year", ylab = "Gender Ratio (Females per Males" )
reg <- lm(ratio_plot_data$gen_ratios~ratio_plot_data$year_ratio)
abline(reg, col =  "blue")
reg$coefficients[2]

abline(h = 1, col = "red") 

#actual indian gender ratio


abline (h = 0.9, col = "green")

points(kerala_plot$g_ratio~kerala_plot$year, col = "red")
reg_kerala <- lm(kerala_plot$g_ratio~kerala_plot$year)
abline(reg_kerala, col = "red")

points(assam_plot$g_ratio~assam_plot$year, col = "orange")
reg_assam <- lm(assam_plot$g_ratio~assam_plot$year)
abline(reg_assam, col = "pink")

points(chhattisgarh_plot$g_ratio~chhattisgarh_plot$year, col = "green")
reg_chhatisgarh <- lm(chhattisgarh_plot$g_ratio~chhattisgarh_plot$year)
abline(reg_chhatisgarh, col = "green")

  
points(gujarat_plot$g_ratio~gujarat_plot$year, col = "blueviolet")
reg_gujarat <- lm(gujarat_plot$g_ratio~gujarat_plot$year)
abline(reg_gujarat, col = "blueviolet")

points(himachal_pradesh_plot$g_ratio~himachal_pradesh_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_himachal_pradesh <- lm(himachal_pradesh_plot$g_ratio~himachal_pradesh_plot$year)
abline(reg_himachal_pradesh, col = rand_color(30, hue = NULL, transparency = 0))

points(Jharkhand_plot$g_ratio~Jharkhand_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_jharkhand <- lm(Jharkhand_plot$g_ratio~Jharkhand_plot$year)
abline(reg_jharkhand, col = rand_color(30, hue = NULL, transparency = 0))

points(Maharashtra_plot$g_ratio~Maharashtra_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_maharashtra <- lm(Maharashtra_plot$g_ratio~Maharashtra_plot$year)
abline(reg_maharashtra, col = rand_color(30, hue = NULL, transparency = 0))

points(Meghalaya_plot$g_ratio~Meghalaya_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_meghalaya <- lm(Meghalaya_plot$g_ratio~Meghalaya_plot$year)
abline(reg_meghalaya, col = rand_color(30, hue = NULL, transparency = 0))

points(Nagaland_plot$g_ratio~Nagaland_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_nagaland <- lm(Nagaland_plot$g_ratio~Nagaland_plot$year)
abline(reg_nagaland, col = rand_color(30, hue = NULL, transparency = 0))

points(odisha_plot$g_ratio~odisha_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_odisha <- lm(odisha_plot$g_ratio~odisha_plot$year)
abline(reg_odisha, col = rand_color(30, hue = NULL, transparency = 0))

points(Punjab_plot$g_ratio~Punjab_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_punjab <- lm(Punjab_plot$g_ratio~Punjab_plot$year)
abline(reg_punjab, col = rand_color(30, hue = NULL, transparency = 0))

points(Sikkim_plot$g_ratio~Sikkim_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_sikkim <- lm(Sikkim_plot$g_ratio~Sikkim_plot$year)
abline(reg_sikkim, col = rand_color(30, hue = NULL, transparency = 0))

points(Tripura_plot$g_ratio~Tripura_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_tripura <- lm(Tripura_plot$g_ratio~Tripura_plot$year)
abline(reg_tripura, col = rand_color(30, hue = NULL, transparency = 0))

points(Uttarakhand_plot$g_ratio~Uttarakhand_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_uttarakhand <- lm(Uttarakhand_plot$g_ratio~Uttarakhand_plot$year)
abline(reg_uttarakhand, col = rand_color(30, hue = NULL, transparency = 0))

points(Arunachal_Pradesh_plot$g_ratio~Arunachal_Pradesh_plot$year, col = rand_color(30, hue = NULL, transparency = 0))
reg_arunachal_pradesh <- lm(Arunachal_Pradesh_plot$g_ratio~Arunachal_Pradesh_plot$year)
abline(reg_arunachal_pradesh, col = rand_color(30, hue = NULL, transparency = 0))

points(Bihar_plot$g_ratio~Bihar_plot$year, col = rand_color(30, transparency = 0))
reg_bihar <- lm(Bihar_plot$g_ratio~Bihar_plot$year)
abline(reg_bihar, col = rand_color(30, hue = NULL, transparency = 0))

points(Goa_plot$g_ratio~Goa_plot$year, col = rand_color(30, transparency = 0))
reg_goa <- lm(Goa_plot$g_ratio~Goa_plot$year)
abline(reg_goa, col = rand_color(30, transparency = 0))

points(Haryana_plot$g_ratio~Haryana_plot$year, col = rand_color(30, transparency = 0))
reg_haryana <- lm(Haryana_plot$g_ratio~Haryana_plot$year)
abline(reg_haryana, col = rand_color(30, transparency = 0))

points(Jammu_Kashmir_plot$g_ratio~Jammu_Kashmir_plot$year, col = rand_color(30, transparency = 0))
reg_jammu_kashmir <- lm(Jammu_Kashmir_plot$g_ratio~Jammu_Kashmir_plot$year)
abline(reg_jammu_kashmir, col = rand_color(30, transparency = 0))

points(Karnataka_plot$g_ratio~Karnataka_plot$year, col = rand_color(30, transparency = 0))
reg_karnataka <- lm(Karnataka_plot$g_ratio~Karnataka_plot$year)
abline(reg_karnataka, col = rand_color(30, transparency = 0))

points(Madhya_Pradesh_plot$g_ratio~Madhya_Pradesh_plot$year, col = rand_color(30, transparency = 0))
reg_madhya_pradesh <- lm(Madhya_Pradesh_plot$g_ratio~Madhya_Pradesh_plot$year)
abline(reg_madhya_pradesh, col = rand_color(30, transparency = 0))

points(Manipur_plot$g_ratio~Manipur_plot$year, col = rand_color(30, transparency = 0))
reg_manipur <- lm(Manipur_plot$g_ratio~Manipur_plot$year)
abline(reg_manipur, col = rand_color(30, transparency = 0))

points(Mizoram_plot$g_ratio~Mizoram_plot$year, col = rand_color(30, transparency = 0))
reg_mizoram <- lm(Mizoram_plot$g_ratio~Mizoram_plot$year)
abline(reg_mizoram, col = rand_color(30, transparency = 0))

points(Delhi_plot$g_ratio~Delhi_plot$year, col = rand_color(30, transparency = 0))
reg_delhi <- lm(Delhi_plot$g_ratio~Delhi_plot$year)
abline(reg_delhi, col = rand_color(30, transparency = 0))

points(Rajasthan_plot$g_ratio~Rajasthan_plot$year, col = rand_color(30, transparency = 0))
reg_rajasthan <- lm(Rajasthan_plot$g_ratio~Rajasthan_plot$year)
abline(reg_rajasthan, col = rand_color(30, transparency = 0))

points(Tamil_Nadu_plot$g_ratio~Tamil_Nadu_plot$year, col = rand_color(30, transparency = 0))
reg_tamil_nadu <- lm(Tamil_Nadu_plot$g_ratio~Tamil_Nadu_plot$year)
abline(reg_tamil_nadu, col = rand_color(30, transparency = 0))

points(Uttar_Pradesh_plot$g_ratio~Uttar_Pradesh_plot$year, col = rand_color(30, transparency = 0))
reg_uttar_pradesh <- lm(Uttar_Pradesh_plot$g_ratio~Uttar_Pradesh_plot$year)
abline(reg_uttar_pradesh, col = rand_color(30, transparency = 0))

points(West_Bengal_plot$g_ratio~West_Bengal_plot$year, col = rand_color(30, transparency = 0))
reg_west_bengal <- lm(West_Bengal_plot$g_ratio~West_Bengal_plot$year)
abline(reg_west_bengal, col = rand_color(30, transparency = 0))

###########

#gplot

library(ggplot2)
ggplot(ratio_plot_data, aes(x=year_ratio, y= gen_ratios)) + 
  geom_point(aes(color = "National"))  + geom_abline(slope = reg$coefficients[2], intercept = reg$coefficients[1],  col = "blue", size = 1 ) +
  coord_cartesian(ylim=c(0, 0.2)) + 
 
   geom_hline(yintercept = 1, color = "red", size = 2 ) +
  geom_hline(yintercept = 0.918, color = "green", size = 2 ) + 
  
  geom_point(data = kerala_plot, aes(y = g_ratio, x = year, color = "Kerala")) +
   geom_abline(slope = reg_kerala$coefficients[2], intercept = reg_kerala$coefficients[1], col = "red", size = 1) + 
  geom_point(data = Sikkim_plot, aes(x=year, y= g_ratio, color = "Sikkim")) +
  geom_abline(slope = reg_sikkim$coefficients[2], intercept = reg_sikkim$coefficients[1], color = "orange", size = 1) +
  labs(title="Election Data (1978 - 2015)", x="Year", y="Gender Ratio of Cnadidates (Females per Males)", colour = c("National", "Sikkim", "Delhi", "Kerala", "Bihar")) +
  geom_point(data=Delhi_plot, aes(x=year,y=g_ratio, color = "Delhi" )) + geom_abline(slope = reg_delhi$coefficients[2], intercept = reg_delhi$coefficients[1], col = "pink", size = 1)+
  geom_point(data=Bihar_plot,aes(x=year,y=g_ratio, color = "Bihar")) +
  geom_abline(slope = reg_bihar$coefficients[2], intercept = reg_bihar$coefficients[1], col = "yellow", size = 1) +theme_dark()+
  scale_color_manual(name = "States",
                     values = c( "National" = "blue", "Kerala" = "red", "Sikkim" = "orange", "Delhi" = "Pink", "Bihar"="yellow"),
                     labels = c("Bihar", "Delhi", "Kerala", "National", "Sikkim")) + theme(legend.position =c(0.90,0.80))



#looking at gender ratio in major political parties

unique(elections$partyname)


gender_ratio_party <- function (party) {
  g_ratio_party <- sum(elections$partyname == party & elections$cand_sex == 'F') / 
    sum(elections$partyname == party  & elections$cand_sex == 'M')
  return(g_ratio_party)
}

Independent <- gender_ratio_party ("Independent")
BJP <- gender_ratio_party("Bhartiya Janata Party")
INC <- gender_ratio_party("Indian National Congress")
DMK <- gender_ratio_party("Dravida Munnetra Kazhagam")
CPIM<-gender_ratio_party("Communist Party Of India (Marxist)")

party_ratio <- c(Independent,BJP,INC,DMK,CPIM)
party_name <- c("Independent","BJP","INC","DMK","CPIM")

paty_data <- data.frame("gender_ratio" = party_ratio, "party" = party_name)

plot(party_name~party_ratio)



######TRYING OUT ANIMATION

gen_elec_years <- c(1977,1980,1989,1991,1996,2009,2014)


calc_males <- function(year) {
  sum(elections$year== year & elections$cand_sex == "M")
}

calc_females <- function(year) {
  sum(elections$year== year & elections$cand_sex == "F")
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


animations_data <- data.frame("year" = year_ani, "males" = male, "females" = female)

animations_data <- animations_data %>% arrange(year)




test_data_long <- melt(animations_data, id="year")  # convert to long format

p <- ggplot(data=test_data_long,
       aes(x=year, y=value, colour=variable)) +
  geom_line()

p

p + 
  geom_point() +
  transition_reveal(year)
