# ------------------------------------------------------
#  Econ 613 - Assignment 2 - Javier Fernandez
# ------------------------------------------------------

## Preliminaries -----

# Libraries
library(tidyverse)
library(bayesm)
library(nloptr)
library(mlogit)


# Set WD
setwd("C:/Users/javie/OneDrive/Documents/GitHub/ECON613/Assingments/A2")


# -----------------------------------
# ------ 1 - Data Description -------
# -----------------------------------

# Invoke dataset
data("margarine")

# a. Average and dispersion of prices

# By product
avg_price <- apply(margarine$choicePrice[,3:12], 2, mean)
sd_price <- apply(margarine$choicePrice[,3:12], 2, sd)
price_table <- t(rbind(avg_price,sd_price))
price_table

# b. Market share (choice frequency) and market share by product characteristics (choice frequency by
# price bins: below average, over average)

## Market share
mkt_share <- 100*table(margarine$choicePrice[,2])/nrow(margarine$choicePrice)
names(mkt_share) <- names(margarine$choicePrice[,3:12])

## Market share by product characteristics
# Generate a dummy variable indicating if the product was bought below or above price 
  avg_price <- data.frame("product"=names(avg_price),"avg_price"=avg_price)
    temp=NULL
    chose=NULL
    price=NULL
    avg_price_of_choice=NULL
  
  for(i in 1:nrow(margarine$choicePrice)){
    chose[i]=margarine$choicePrice$choice[i]
    price[i]=margarine$choicePrice[i,chose[i]+2]
    avg_price_of_choice[i]=avg_price[chose[i],2]
    temp=rbind(temp,c(chose[i],price[i],avg_price_of_choice[i]))
  } 
  colnames(temp) <- c("Choice","Price","Avg_Price") 
  temp <- temp %>% as.data.frame() %>% 
  mutate(below_above_avg=ifelse(Price>Avg_Price,"Above","Below"))
  
# By price bins  
table(temp$Choice,temp$below_above_avg) %>% prop.table(.,margin = 2)

# c. Illustrate the mapping between observed attributes and choices. (Which customers are choosing
# which products?)
merged_data <- left_join(margarine$choicePrice,margarine$demos)
merged_data <- merged_data %>% mutate(Income_bins=case_when(
              Income<20~"< 20",
              Income>=20 & Income<40 ~ "20-39",
              Income>=40 & Income<60 ~ "40-59",
              Income>=60~"> 60"
))
merged_data$Income_bins <- factor(merged_data$Income_bins, levels = c("< 20","20-39","40-59","> 60"))
choice_by_incomebins <- 100*prop.table(table(merged_data$choice,merged_data$Income_bins),2)
print(choice_by_incomebins)

# Fam size
100*prop.table(table(merged_data$choice,merged_data$Fam_Size),1)
# College
100*prop.table(table(merged_data$choice,merged_data$college),1)
# Retired
100*prop.table(table(merged_data$choice,merged_data$retired),1)
# White collar
100*prop.table(table(merged_data$choice,merged_data$whtcollar),1)

# -----------------------------------
# -------- 2 - First Model ----------
# -----------------------------------

# Conditional Logit Function
conditional_logit = function(parameters,choice,x1){
  ni = nrow(x1)
  nj = 10
  ut = mat.or.vec(ni,nj)
  
  ut[,1] = parameters[10]*x1[,1] # intercept=0
  for (j in 2:nj)
  {
    # conditional logit
    ut[,j] = parameters[j-1] + parameters[10]*x1[,j]
  }
  prob   = exp(ut)          
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) 

  # Probabilities times Indicator of choice
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

# Optimizing to get the results
npar=10
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar)

choice=margarine$choicePrice[,2]
x1=margarine$choicePrice[,3:12]

res_cond_logit = optim(start,fn=conditional_logit,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),
                        choice=choice,x1=x1,)


# ------------------------------------
# -------- 3 - Second Model ----------
# ------------------------------------

# Multinomial Logit Function
multinomial_logit = function(param,choice,X,n_alternatives){
  #Coefficients and preliminaries
  ni = nrow(X)
  nj = n_alternatives
  intercepts = c(0,param[1:(nj- 1)])
  slopes1 = c(0,param[nj:(2*(nj-1))])
  slopes2 = c(0,param[(2*(nj-1)+1):(3*(nj-1))])
  slopes3 = c(0,param[(3*(nj-1)+1):(4*(nj-1))])
  slopes4 = c(0,param[(4*(nj-1)+1):(5*(nj-1))])
  slopes5 = c(0,param[(5*(nj-1)+1):length(param)])
  ut = mat.or.vec(ni,nj)
  
  #Variables
  Income = X[,1]
  Fam_size = X[,2]
  college = X[,3]
  whtcollar = X[,4]
  retired = X[,5]
  
  # Loop to compute probabilites
  ut[,1] = 0 # intercept =0 and slopes=0
  for (j in 2:nj){
    # multinomial logit
    ut[,j] = intercepts[j] + slopes1[j]*Income + slopes2[j]*Fam_size + 
      slopes3[j]*college + slopes4[j]*whtcollar + slopes5[j]*retired
  }
  prob   = exp(ut)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
    if(is.na(probc[i])){
      break 
      }
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

# Dataset
X <- merged_data %>% select(Income,Fam_Size,college,whtcollar,retired)

# Initial values for model estimation
set.seed(12)
n_alternatives=10
npar=(n_alternatives-1)*6
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar)


res_multlogit=optim(start,fn=multinomial_logit,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),
                 choice=choice,X=X,n_alternatives=n_alternatives,hessian=FALSE)

# ----------------------------------------
# -------- 4 - Marginal Effects ----------
# ----------------------------------------

# 4.1 Average Marginal Effect for the First Model: Conditional Logit ----

  # Coefficients
  Coef_CL=res_cond_logit$par
  
  # Compute probability matrix
  x1=margarine$choicePrice[,3:12] 
    
  ut=mat.or.vec(nrow(x1),ncol(x1))
  ut[,1] = Coef_CL[10]*x1[,1]
  for (j in 2:ncol(x1))
  {
    # conditional logit
    ut[,j] = Coef_CL[j-1] + Coef_CL[10]*x1[,j]
  }
  prob   = exp(ut)          
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) 
  
  # Computing marginal effects
  avg_mgl_effects_CL <- NULL
  
  for(j in 1:10){
    mgl_effects_1=mat.or.vec(nrow(x1),ncol(x1))
    dummy_reference_option <- rep(0,10)
    dummy_reference_option[j] <- 1
    for(jj in 1:10){
      mgl_effects_1[,jj]=prob[,jj]*(dummy_reference_option[jj]-prob[,1])*Coef_CL[10]
    }
    temp_avg_mgl_effects_CL=colMeans(mgl_effects_1)
    avg_mgl_effects_CL <- cbind(avg_mgl_effects_CL,temp_avg_mgl_effects_CL)
  }
  colnames(avg_mgl_effects_CL) <- colnames(x1)
  rownames(avg_mgl_effects_CL) <- colnames(x1)
  avg_mgl_effects_CL

# 4.2 Average Marginal Effect for the Second Model: Multinomial Logit ----

  # Coefficients
  Coef_Mult=res_multlogit$par
  
  # Data
  X <- merged_data %>% select(Income,Fam_Size,college,whtcollar,retired)
  n_alternatives=10
  
  # Compute probability matrix  
  ut=mat.or.vec(nrow(X),n_alternatives)
  ut[,1] = 0
  for (j in 2:n_alternatives){
    # multinomial logit
    ut[,j] = Coef_Mult[j-1] + Coef_Mult[j+8]*X$Income + Coef_Mult[j+17]*X$Fam_Size + 
      Coef_Mult[j+26]*X$college + Coef_Mult[j+35]*X$whtcollar + Coef_Mult[j+44]*X$retired
  }
  prob   = exp(ut)          
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) 
  
  # Computing marginal effects
    ### a. For Income
  mgl_effects_Income=mat.or.vec(nrow(X),n_alternatives)
  Income_coefs=c(0,Coef_Mult[10:18])
  wgt_avg_Income=prob%*%Income_coefs
  
  for(j in 1:10){
    mgl_effects_Income[,j]=prob[,j]*(Income_coefs[j]-wgt_avg_Income)
  }
  
  avg_mgl_effects_Income=colMeans(mgl_effects_Income)

  ### b. For Fam_Size
  mgl_effects_Fam_Size=mat.or.vec(nrow(X),n_alternatives)
  Fam_Size_coefs=c(0,Coef_Mult[19:27])
  wgt_avg_Fam_Size=prob%*%Fam_Size_coefs
  
  for(j in 1:10){
    mgl_effects_Fam_Size[,j]=prob[,j]*(Fam_Size_coefs[j]-wgt_avg_Fam_Size)
  }
  
  avg_mgl_effects_Fam_Size=colMeans(mgl_effects_Fam_Size)
  
  ### c. For college
  
  mgl_effects_college=mat.or.vec(nrow(X),n_alternatives)
  college_coefs=c(0,Coef_Mult[28:36])
  wgt_avg_college=prob%*%college_coefs
  
  for(j in 1:10){
    mgl_effects_college[,j]=prob[,j]*(college_coefs[j]-wgt_avg_college)
  }
  
  avg_mgl_effects_college=colMeans(mgl_effects_college)
  
  ### d. For whtcollar
  
  mgl_effects_whtcollar=mat.or.vec(nrow(X),n_alternatives)
  whtcollar_coefs=c(0,Coef_Mult[37:45])
  wgt_avg_whtcollar=prob%*%whtcollar_coefs
  
  for(j in 1:10){
    mgl_effects_whtcollar[,j]=prob[,j]*(whtcollar_coefs[j]-wgt_avg_whtcollar)
  }
  
  avg_mgl_effects_whtcollar=colMeans(mgl_effects_whtcollar)
  
  ### e. For retired
  
  mgl_effects_retired=mat.or.vec(nrow(X),n_alternatives)
  retired_coefs=c(0,Coef_Mult[46:54])
  wgt_avg_retired=prob%*%retired_coefs
  
  for(j in 1:10){
    mgl_effects_retired[,j]=prob[,j]*(retired_coefs[j]-wgt_avg_retired)
  }
  
  avg_mgl_effects_retired=colMeans(mgl_effects_retired)

  results = cbind(avg_mgl_effects_Income,avg_mgl_effects_Fam_Size,avg_mgl_effects_college,
                  avg_mgl_effects_whtcollar,avg_mgl_effects_retired)
  colnames(results) = colnames(X)  
  rownames(results) = names(merged_data)[3:12]
  results  
  
  

  
# ---------------------------------------------------------------
# -------- 5 - Independence of Irrelevant Alternatives ----------
# ---------------------------------------------------------------
  
  ## Mixed logit model
  Mixed_logit = function(param,choice,Product_X,Individuals_X,n_alternatives){
    #Coefficients and preliminaries
    ni = length(choice)
    nj = n_alternatives
    intercepts = c(0,param[1:(nj- 1)])
    slopes1 = c(0,param[nj:(2*(nj-1))])
    slopes2 = c(0,param[(2*(nj-1)+1):(3*(nj-1))])
    slopes3 = c(0,param[(3*(nj-1)+1):(4*(nj-1))])
    slopes4 = c(0,param[(4*(nj-1)+1):(5*(nj-1))])
    slopes5 = c(0,param[(5*(nj-1)+1):(6*(nj-1))])
    slope6 = param[length(param)]
    ut = mat.or.vec(ni,nj)
    
    #Variables
    Income = Individuals_X[,1]
    Fam_size = Individuals_X[,2]
    college = Individuals_X[,3]
    whtcollar = Individuals_X[,4]
    retired = Individuals_X[,5]
    price = Product_X
    # Loop to compute probabilites
    ut[,1] = slope6*price[,1] # intercept =0 and slopes=0
    for (j in 2:nj){
      # multinomial logit
      ut[,j] = intercepts[j] + slopes1[j]*Income + slopes2[j]*Fam_size + 
        slopes3[j]*college + slopes4[j]*whtcollar + slopes5[j]*retired + slope6*price[,j]
    }
    prob   = exp(ut)
    prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
    probc = NULL
    for (i in 1:ni){
      probc[i] = prob[i,choice[i]]
    }
    probc[probc>0.999999] = 0.999999
    probc[probc<0.000001] = 0.000001
    like = sum(log(probc))
    return(-like)
  }

  # Dataset
  Individuals_X <- merged_data %>% select(Income,Fam_Size,college,whtcollar,retired)
  Product_X <- merged_data %>% select(3:12) #prices
  
  # Initial values for model estimation
  set.seed(1234)
  n_alternatives=10
  npar=(n_alternatives-1)*6 +1
  lower  = rep(-10,npar)
  upper  = rep(10,npar)
  start  = runif(npar)
  
  
  res_mixed=optim(start,fn=Mixed_logit,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),
                      choice=choice,Product_X=Product_X,Individuals_X=Individuals_X,n_alternatives=n_alternatives,hessian=FALSE)
  beta_f=res_mixed$par # Coefficients
  like_f=res_mixed$value # Likelihood
  
  ## Mixed logit removing option 10 -------
  merged_data_iia <- merged_data %>% filter(choice!=10) %>% select(-PHse_Tub)
  
  Mixed_logit_2 = function(param,choice,Product_X,Individuals_X,n_alternatives){
    #Coefficients and preliminaries
    ni = length(choice)
    nj = n_alternatives
    intercepts = c(0,param[1:(nj- 1)])
    slopes1 = c(0,param[nj:(2*(nj-1))])
    slopes2 = c(0,param[(2*(nj-1)+1):(3*(nj-1))])
    slopes3 = c(0,param[(3*(nj-1)+1):(4*(nj-1))])
    slopes4 = c(0,param[(4*(nj-1)+1):(5*(nj-1))])
    slopes5 = c(0,param[(5*(nj-1)+1):(6*(nj-1))])
    slope6 = param[length(param)]
    ut = mat.or.vec(ni,nj)
    
    #Variables
    Income = Individuals_X[,1]
    Fam_size = Individuals_X[,2]
    college = Individuals_X[,3]
    whtcollar = Individuals_X[,4]
    retired = Individuals_X[,5]
    price = Product_X
    # Loop to compute probabilites
    ut[,1] = slope6*price[,1] # intercept =0 and slopes=0
    for (j in 2:nj){
      # multinomial logit
      ut[,j] = intercepts[j] + slopes1[j]*Income + slopes2[j]*Fam_size + 
        slopes3[j]*college + slopes4[j]*whtcollar + slopes5[j]*retired + slope6*price[,j]
    }
    prob   = exp(ut)
    prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
    probc = NULL
    for (i in 1:ni){
      probc[i] = prob[i,choice[i]]
    }
    probc[probc>0.999999] = 0.999999
    probc[probc<0.000001] = 0.000001
    like = sum(log(probc))
    return(-like)
  }
  
  # Dataset
  Individuals_X_iia <- merged_data_iia %>% select(Income,Fam_Size,college,whtcollar,retired)
  Product_X_iia <- merged_data_iia %>% select(3:12) #prices
  choice_iia <- merged_data_iia$choice

  # Initial values for model estimation
  set.seed(1234)
  n_alternatives_iia=9
  npar_iia=(n_alternatives_iia-1)*6 +1
  lower  = rep(-10,npar_iia)
  upper  = rep(10,npar_iia)
  start  = runif(npar_iia)
  
  
  
  res_mixed_iia=optim(start,fn=Mixed_logit_2,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),
                  choice=choice_iia,Product_X=Product_X_iia,Individuals_X=Individuals_X_iia,
                  n_alternatives=n_alternatives_iia,hessian=FALSE)
  beta_r=res_mixed_iia$par # Coefficients
  like_r=res_mixed_iia$value # Likelihood
  
  ## Testing for IIA
  # To compute the MTT statistics we have to compute the likelihood of the restricted model with
  # the coefficients of the free model and the restricted model
  
  ## For the Lr using the unrestricted coefficients: Lr_beta_f
  
  # Observations are the ones from the restricted model
  Individuals_X_iia <- merged_data_iia %>% select(Income,Fam_Size,college,whtcollar,retired)
  Product_X_iia <- merged_data_iia %>% select(3:12) #prices
  choice_iia <- merged_data_iia$choice
  
  # Loop to compute probabilites
  # Coefficiens
  intercepts = beta_f[1:8]
  slopes1 = beta_f[10:17]
  slopes2 = beta_f[19:26]
  slopes3 = beta_f[28:35]
  slopes4 = beta_f[37:44]
  slopes5 = beta_f[46:53]
  slope6 = beta_f[length(beta_f)]
  param_beta_f_for_iia=c(intercepts,slopes1,slopes2,slopes3,slopes4,slopes5,slope6)
  
  #Variables
  Income = Individuals_X_iia[,1]
  Fam_size = Individuals_X_iia[,2]
  college = Individuals_X_iia[,3]
  whtcollar = Individuals_X_iia[,4]
  retired = Individuals_X_iia[,5]
  price = Product_X_iia

  # Loop to compute probabilites
  ni = length(choice_iia)
  nj = 9
  ut = mat.or.vec(ni,nj)
  ut[,1] = slope6*price[,1] # intercept =0 and slopes=0
  for (j in 2:nj){
    # mixed logit
    ut[,j] = intercepts[j-1] + slopes1[j-1]*Income + slopes2[j-1]*Fam_size + 
      slopes3[j-1]*college + slopes4[j-1]*whtcollar + slopes5[j-1]*retired + slope6*price[,j-1]
  }
  prob   = exp(ut)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  for (i in 1:ni){
    probc[i] = prob[i,choice_iia[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  Lr_beta_f = sum(log(probc))
  
  # Computing the MTT statistic
  mtt=-2*(Lr_beta_f-(-like_r))
  chi_95=qchisq(0.95,df=length(beta_r))
  mtt>chi_95
  