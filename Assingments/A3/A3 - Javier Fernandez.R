# ------------------------------------------------------
#  Econ 613 - Assignment 2 - Javier Fernandez
# ------------------------------------------------------

## Preliminaries -----
rm(list=ls())

# Libraries
library(data.table)
library(moments)
library(stargazer)
library(tidyverse)

# Set WD
setwd("C:/Users/javie/OneDrive/Documents/GitHub/ECON613/Assingments/A3")


# -----------------------------------
# ------ 2 - Data Manipulation -------
# -----------------------------------

#Load data
population <- read.csv("population.csv")
crime <- read.csv("crime_long.csv")

# 2.1 Calculate total crime per month and plot the time series of crime. ----

  # a. Total crime overall
total_a <- crime %>% group_by(crime_month) %>% summarize(total_crimes=sum(crimes))
ggplot(data = total_a,aes(x=crime_month,y=total_crimes,group=1))+geom_line()


  # b. Total crime by district
total_b <- crime %>% group_by(crime_month,district) %>% summarize(total_crimes=sum(crimes))
ggplot(data=total_b,aes(x=crime_month,y=total_crimes,
                             colour=district,group=district)) +
  geom_line()

crime <- left_join(crime,total_b)

# 2.2 Merge the two datasets by districts-units and period. ----
crime  <- crime %>% pivot_wider(names_from=crime_type,values_from=crimes,
values_fn=sum) # As there are some rows with the same months and district and different values, 
                # we use values_fn
population <- population %>% rename(crime_month=month)
pop_crime <- left_join(crime,population)

# 3. Construct a panel data of unit over time with the following variables ----
    # Total crimes per resident 
    # Violent crimes per resident 
    # Property crimes per resident
    # Median income
    # Share of black, hispanic and white residents

# Creating variables
Variables <-pop_crime %>% group_by(crime_month,district) %>% 
  summarise(tot_crim_per_res=total_crimes/tot_pop,
            viol_crim_per_res=violent/tot_pop,
            prop_crim_per_res=property/tot_pop,
            share_black=tot_black/tot_pop,
            share_hisp=tot_hisp/tot_pop,
            share_white=tot_white/tot_pop)

# Creating panel data
panel_crime <- left_join(pop_crime,Variables)


# -------------------------------------------
# ------ 3 - Panel Data: Introduction -------
# -------------------------------------------

#Load data
officers <- read.csv("officers.csv")

officers <- officers %>% rename(district=unit,crime_month=month)

final_panel <- left_join(officers,panel_crime)


model1 <- lm(data=final_panel,
             arrest~tenure+tot_crim_per_res+p50_inc+share_white+share_hisp+share_black) 
 
summary(model1)


# -------------------------------------------
# ------ 4 - Panel Data: More Controls ------
# -------------------------------------------

model2 <- lm(data=final_panel,
             arrest~tenure+tot_crim_per_res+p50_inc+share_white+share_hisp+share_black+
     factor(crime_month)+factor(district))

summary(model2)

# -------------------------------------------------------
# ------ 5 - Panel Data: Individual Fixed Effects -------
# -------------------------------------------------------

# 5.1 Between estimation

between_data <- final_panel %>% as.data.table()
between_data <- between_data[,paste('avg', names(between_data)[4:22], sep="_") :=lapply(.SD,mean),
                             by=NUID,.SDcols=4:22] %>% as.data.frame()
bet_model <- lm(data=between_data,avg_arrest~avg_tenure+avg_tot_crim_per_res+avg_p50_inc+
     avg_share_white+avg_share_hisp+avg_share_black) 



# 5.2 Within estimation

within_data <- left_join(final_panel %>% 
                           dplyr::select(NUID,crime_month,arrest,tenure,tot_crim_per_res,
                                  p50_inc,tot_white,tot_hisp,tot_black),between_data)
within_data <- within_data %>% mutate(
  w_arrest=arrest-avg_arrest,
  w_tenure=tenure-avg_tenure,
  w_tot_crim_per_res=tot_crim_per_res-avg_tot_crim_per_res,
  w_p50_inc=p50_inc-avg_p50_inc,
  w_share_white=share_white-avg_share_white,
  w_share_hisp=share_hisp-avg_share_hisp,
  w_share_black=share_black-avg_share_black
)

# within_model <- lm(data=within_data,w_arrest~w_tenure+w_tot_crim_per_res+w_p50_inc+
     # w_tot_white+w_tot_hisp+w_tot_black) 

# 5.3 First Difference
subset_panel<- final_panel %>% dplyr::select(NUID,crime_month,arrest,tenure,tot_crim_per_res,
                       p50_inc,share_white,share_hisp,share_black) %>% 
              mutate(NUID=as.numeric(NUID))
ordered_panel <- subset_panel[order(subset_panel$NUID,subset_panel$crime_month),]
diff_panel <- NULL


min_month <- ordered_panel %>% group_by(NUID) %>% summarise(crime_month=min(crime_month))
min_month <- min_month %>% mutate(drop=1)

diff_panel <- ordered_panel[,3:9]-dplyr::lag(ordered_panel[,3:9])
diff_panel <- cbind(ordered_panel[,1:2],diff_panel)
diff_panel <- left_join(diff_panel,min_month)
diff_panel <- diff_panel %>% filter(is.na(drop))
diff_panel <- diff_panel %>% dplyr::select(-drop)

 firstdiff_model <- lm(data=diff_panel, arrest~tenure+tot_crim_per_res+p50_inc+
                        share_white+share_hisp+share_black)

stargazer(bet_model,within_model,firstdiff_model)

### 5.4 GMM estimation ----

# To construct parameter vectors we first have to rescale UNID and months so that
# they are ordered from 1 to # of options.

officers <- final_panel %>% dplyr::select(NUID) %>% unique()
officers <- officers %>% mutate(id_off=1:nrow(officers))

months <- final_panel %>% dplyr::select(crime_month) %>% unique()
months <- months %>% mutate(id_month=1:nrow(months))

gmm_panel <- left_join(final_panel,officers)
gmm_panel <- left_join(gmm_panel,months)



GMM = function(param,gmm_panel){
  gmm_panel <- gmm_panel %>% drop_na()
  #Coefficients and preliminaries
  ni = max(gmm_panel$id_off)
  alpha = param[1:ni]
  beta = param[ni+1]
  gamma = param[(ni+2):(ni+6)]
  phi = param[(ni+7):(ni+31)]
  kappa = param[(ni+32):(ni+31+max(gmm_panel$id_month))]
  
  #Variables
  arrest = gmm_panel$arrest
  tenure = gmm_panel$tenure
  tot_crimes_pr = gmm_panel$tot_crim_per_res
  median_income = gmm_panel$p50_inc
  s_black= gmm_panel$share_black
  s_white= gmm_panel$share_white
  s_hisp= gmm_panel$share_hisp
  
  # Loop to compute estimates
  est=mat.or.vec(nrow(gmm_panel),1)
    for (ii in 1:nrow(gmm_panel)){
    est[ii] = alpha[gmm_panel$id_off[ii]]+beta*tenure[ii]+
      gamma[1]*tot_crimes_pr[ii] + gamma[2]*median_income[ii]+
      gamma[3]*s_black[ii]+gamma[4]*s_white[ii]+gamma[5]*s_hisp[ii]
    + phi[gmm_panel$district[ii]]+kappa[gmm_panel$id_month[ii]]
  }

  moments_est=all.moments(est,order.max=2)[-1]
  moments_arrest=all.moments(arrest,order.max=2)[-1]
  
  like = sum((moments_est - moments_arrest)^2)
  return(like)
}

set.seed(123456)
subset_gmm_panel <- gmm_panel[1:50000,]  
param=runif(max(subset_gmm_panel$id_off)+6+max(subset_gmm_panel$id_month))/10000
GMM(param,subset_gmm_panel)


res  = optim(param,fn=GMM,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),
             gmm_panel=subset_gmm_panel)

# Histogram of individual fixed effects 
hist(res$par[1:623], main = "Individual Fixed Effects")

# Coefficients
gmm_coef <- res$par[624:629]
names(gmm_coef) <- c("Tenure","Crimes per resident","Median Income","Share of White","Share of Hispanic","Share of Black")
gmm_coef
# Histogram of district effects
hist(res$par[630:654], main = "District Effects")

# Histogram of time effects
hist(res$par[655:761], main = "Time Effects")



