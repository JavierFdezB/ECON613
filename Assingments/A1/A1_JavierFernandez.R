# ------------------------------------------------------
#  Econ 613 - Assignment 1 - Javier Fernandez
# ------------------------------------------------------

## 0. Preliminaries -----

# Libraries
library(tidyverse)
# install.packages("matlib")
library(matlib)

# Set WD
setwd("C://Users//javie//OneDrive//Documents//Duke - MAE//Academic//Spring 2021//ECON 613 - Applied Metrics (Micro)//Assingments//A1")

# -------------------
# ----- PART 1 ------
# -------------------

# Load Data
dat_student <- read.csv("datstu.csv")
dat_school <- read.csv("datsss.csv")
dat_position <- read.csv("datjss.csv")

## 1. Missing data -----

# For easier calculations I am
# Gathering information to show one choice per row (six rows per student)
programs <- dat_student %>% select(1:4,11:18) %>% 
  pivot_longer(cols=c("choicepgm1","choicepgm2","choicepgm3","choicepgm4",
                      "choicepgm5","choicepgm6"),names_to="Choice_num",
               names_prefix = "choicepgm",
               values_to="Program")

schools <- dat_student %>% select(1:10,17:18) %>% 
  pivot_longer(cols=c("schoolcode1","schoolcode2","schoolcode3","schoolcode4",
                      "schoolcode5","schoolcode6"),names_to="Choice_num",
               names_prefix = "schoolcode",
               values_to="School")
dat_student_clean <- merge(programs,schools)

# a. Number of students
nrow(dat_student) # max(dat_student$X)

# answer: the data contains 340,823 distinct students


# b. Number of schools
dat_student_clean %>% group_by(School) %>% summarise(Count=n()) %>% 
  filter(School!="" | !is.na(School)) %>% nrow()

# answer: the data contains 640 different schools


# c. Number of programs
dat_student_clean %>% group_by(Program) %>% summarise(Count=n()) %>% 
  filter(Program!="" | !is.na(Program)) %>% nrow()


# answer: the data contains 33 distinct programs


# d. Number of choices

# Number of distinct choices: 
dat_student_clean %>% group_by(School,Program) %>% summarise(Count=n()) %>% 
  filter(Program!="" | School!="") %>% nrow()

# answer: There are 3,085 distinct choices: combinations of schools and programs


# e. Missing test scores
sum(is.na(dat_student$score))

# answer: There are 179,887 students missing test scores


# f. Apply to the same school

dat_student_clean %>% group_by(X) %>% filter(Program!="") %>%
    summarise(Number_of_schools=n_distinct(School)) %>% filter(Number_of_schools==1) %>% nrow()

# answer: 663 students applied to the same school in all their cases, irrespective to the
#         number of programs they applied to.


# g. Apply to less than 6 choices

dat_student_clean %>% group_by(X) %>% filter(Program=="") %>%
  summarise(Number_of_Programs=n())  %>%  nrow()

# answer: 20,988 students applied to less than 6 choices.


## 2. Data -----

# To do this we first have to filter the student data by only keeping the
# the student was admitted to.

program_admitted <-   dat_student_clean %>% filter(Choice_num==rankplace)

admission_stats <- program_admitted %>% group_by(School,Program) %>% 
  summarize(cutoff=min(score),quality=mean(score),size=n())

### erasing duplicates in dat_school
dat_school_clean <- dat_school[!duplicated(dat_school$schoolcode),]

choice_lvl_data <- left_join(admission_stats,dat_school_clean[,-1],by= c("School"="schoolcode"))


## 3. Distance -----

# Getting the coordinates for the junior high school
program_admitted_location <- left_join(program_admitted,dat_position[,-1],by= c("jssdistrict"="jssdistrict"))

program_admitted_location <- left_join(program_admitted_location[,-1],choice_lvl_data,
                                       by=c("School"="School","Program"="Program"))

# Renaming variables for simplicity
program_admitted_location <- program_admitted_location %>% 
                              rename(jsslong=point_x,jsslat=point_y,
                                     sss_code=School)

# Constructing distance variable
program_admitted_location <- program_admitted_location %>% 
                    mutate(distance=sqrt((69.172*(ssslong-jsslong)*cos(jsslat/57.3))^2 +
                                           (69.172*(ssslat-jsslat))^2))

## 4. Descriptive Characteristics -----

# Total sample
program_admitted_location %>%  
  summarise(Mean_cutoff=mean(cutoff),
            Stdev_cutoff=sd(cutoff),
            Mean_quality=mean(quality),
            Stdev_quality=sd(quality),
            Mean_distance=mean(distance,na.rm = TRUE),
            Stdev_distance=sd(distance,na.rm = TRUE))

# By School 
program_admitted_location %>% group_by(schoolname) %>%  
                          summarise(Mean_cutoff=mean(cutoff),
                          Stdev_cutoff=sd(cutoff),
                          Mean_quality=mean(quality),
                          Stdev_quality=sd(quality),
                          Mean_distance=mean(distance,na.rm = TRUE),
                          Stdev_distance=sd(distance,na.rm = TRUE))

# By Program 
program_admitted_location %>% group_by(Program) %>%  
  summarise(Mean_cutoff=mean(cutoff),
            Stdev_cutoff=sd(cutoff),
            Mean_quality=mean(quality),
            Stdev_quality=sd(quality),
            Mean_distance=mean(distance,na.rm = TRUE),
            Stdev_distance=sd(distance,na.rm = TRUE))

# Differentiated by quantiles (This is to be interpreted as the mean cutoff
# of the schools quantile x students will go to.)

program_admitted_location %>% mutate(quantile=case_when(
  score<quantile(score,0.1)~1,
  score>=quantile(score,0.1) & score<quantile(score,0.2)~2,
  score>=quantile(score,0.2) & score<quantile(score,0.3)~3,
  score>=quantile(score,0.3) & score<quantile(score,0.4)~4,
  score>=quantile(score,0.4) & score<quantile(score,0.5)~5,
  score>=quantile(score,0.5) & score<quantile(score,0.6)~6,
  score>=quantile(score,0.6) & score<quantile(score,0.7)~7,
  score>=quantile(score,0.7) & score<quantile(score,0.8)~8,
  score>=quantile(score,0.8) & score<quantile(score,0.9)~9,
  score>=quantile(score,0.9) ~10,
)) %>% group_by(quantile) %>%  summarise(Mean_cutoff=mean(cutoff),
                                        Stdev_cutoff=sd(cutoff),
                                        Mean_quality=mean(quality),
                                        Stdev_quality=sd(quality),
                                        Mean_distance=mean(distance,na.rm = TRUE),
                                        Stdev_distance=sd(distance,na.rm = TRUE))



# -------------------
# ----- PART 2 ------
# -------------------

## 5. Data creation -----

set.seed(123)

# X1
X1 <- runif(10000,min=1,max=3)

# X2
X2 <- rgamma(10000,shape = 3,scale = 2)

# X3
X3 <- rbinom(10000,size=1,prob = 0.3)

# Error term
error <- rnorm(10000,mean=2,sd=1)

# Create Y and Ydum

# Y
par <- c(0.5,1.2,-0.9,0.1)
Y <- par[1] + par[2]*X1 + par[3]*X2 + par[4]*X3 + error

# Ydum
Ydum <- as.numeric(Y>mean(Y))




## 6. OLS -----

# Correlation Y and X1. How different is it from 1.2?

cor(Y,X1)

# answer: the result is 0.21, Being Y a linear function of X1 we would have expected the correlation
# to be larger.


# Calculate the coefficients

# For validation: summary(lm(Y~X1+X2+X3))

regressors <- as.matrix(t(rbind(rep(1,10000),X1,X2,X3)),ncol=4)

betas <-inv(t(regressors)%*%regressors)%*%(t(regressors)%*%Y)
# betas are the coefficients for the OLS estimation 

resids <- Y-regressors%*%betas
sigma_2 <- as.numeric(t(resids)%*%resids/(10000-4))
var_cov_matrix <- sigma_2*inv(t(regressors)%*%regressors)
std_errors <- sqrt(diag(var_cov_matrix))
# std_errors are the standard errors of the coefficients
coef_stderrors <- cbind(betas,std_errors)
colnames(coef_stderrors) <- c("Coefs","Std. Errors")

print(coef_stderrors) # Answer



## 7. Discrete choice -----
####   7.a. Linear probability model ----

  # The linear probability model can be estimated by OLS
  # Function:
    linear_prob_model <- function(Y,regressors){
        betas <-inv(t(regressors)%*%regressors)%*%(t(regressors)%*%Y)
        resids <- Y-regressors%*%betas
        sigma_2 <- as.numeric(t(resids)%*%resids/(nrow(regressors)-ncol(regressors)))
        var_cov_matrix <- sigma_2*inv(t(regressors)%*%regressors)
        std_errors <- sqrt(diag(var_cov_matrix))
        coef_stderrors <- cbind(betas,std_errors)
        colnames(coef_stderrors) <- c("Coefs","Std. Errors")
        return(coef_stderrors) # Answer
    }

  # Estimation
regressors <- as.matrix(t(rbind(rep(1,10000),X1,X2,X3)),ncol=4)

# Results of the linear probability model (Coefs and regressors)
results_lpm <- linear_prob_model(Ydum,regressors)  

####   7.b  Probit ----

      # Probit function
      probit_likelihood = function(coefs,x1,x2,x3,y)
      {
        xbeta           = coefs[1] + coefs[2]*x1 + coefs[3]*x2 + coefs[4]*x3
        pr              = pnorm(xbeta)
        pr[pr>0.999999] = 0.999999
        pr[pr<0.000001] = 0.000001
        like            = y*log(pr) + (1-y)*log(1-pr)
        return(-sum(like))
      }

### Estimation

    # Trying random starting points to check convergence ----
# tries = 100
# out_probit = mat.or.vec(tries,4)
# for (i in 1:tries)
# {
#   start    = runif(4,-10,10)
#   #res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
#   res      = optim(start,fn=probit_likelihood,method="BFGS",control=list(trace=6,maxit=1000),x1=X1,x2=X2,x3=X3,y=Ydum)
#   out_probit[i,] = res$par
# }
# # result: Using random starting points between -10 and 10 yields a wide range of parameter estimates

    # Result Probit----
start = runif(4)
res_probit = optim(start,fn=probit_likelihood,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),x1=X1,x2=X2,x3=X3,y=Ydum,hessian=TRUE)

fisher_info_probit = solve(res_probit$hessian)       
prop_sigma_probit  = sqrt(diag(fisher_info_probit))


####   7.c  Logit ----

      # Logit function
      logit_likelihood = function(y,x1,x2,x3,coefs)
      {
        xbeta           = coefs[1] + coefs[2]*x1 + coefs[3]*x2 + coefs[4]*x3
        pr              = exp(xbeta)/(1+exp(xbeta))
        pr[pr>0.999999] = 0.999999
        pr[pr<0.000001] = 0.000001
        like           = y*log(pr) + (1-y)*log(1-pr)
        return(-sum(like))
      }

### Estimation

    # Trying random starting points to check convergence ----
# tries = 100
# out_logit = mat.or.vec(tries,4)
# for (i in 1:tries)
# {
#   start    = runif(4,-10,10)
#   res_logit      = optim(start,fn=logit_likelihood,method="BFGS",control=list(trace=6,maxit=1000),x1=X1,x2=X2,x3=X3,y=Ydum)
#   out_logit[i,] = res_logit$par
# }
# # result: Using random starting points between -10 and 10 yields a wide range of parameter estimates

    # Result Logit ----
start = runif(4)
res_logit = optim(start,fn=logit_likelihood,method="BFGS",control=list(trace=6,REPORT=1,maxit=10000),
                  x1=X1,x2=X2,x3=X3,y=Ydum,hessian=TRUE)

fisher_info_logit = solve(res_logit$hessian)       
prop_sigma_logit  = sqrt(diag(fisher_info_logit))


  # Final Results ----
results = cbind(par,results_lpm[,1],results_lpm[,2],
                res_probit$par,prop_sigma_probit,res_logit$par,prop_sigma_logit)
colnames(results) = c("True parameter","LPM: est","LPM :se","Probit: est","Probit: :se",
                      "Logit: est","Logit: :se")
results
  # Answer
# 1. The LPM, which is the only comparable model in terms of coefficients, produces estimates
# really different from the true parameters. At least they are all un the correct direction.
# Using a t-test with 95% confidence, the intercept, X1 and X2 are significant. This is not the case for X3.  
abs(results_lpm[,1]/results_lpm[,2])>1.96

# 2. The Probit model coefficients are not directly comparable with the true parameters. 
# We can observe that the sign of the estimates is correct for all but X3. 
# Using a t-test with 95% confidence, the intercept, X1 and X2 are significant. This is not the case for X3.  
abs(res_probit$par/prop_sigma_probit)>1.96

# 3. The Logit model coefficients are not directly comparable with the true parameters. 
# We can observe that the sign of the estimates is correct for all but X3. 
# Using a t-test with 95% confidence, the intercept, X1 and X2 are significant. This is not the case for X3.  
abs(res_logit$par/prop_sigma_logit)>1.96


## 8. Marginal effects -----
