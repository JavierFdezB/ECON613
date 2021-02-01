#--------------------------------
# Assingment 0 ------------------
#--------------------------------

# Practice code

# Excercise 1: Introduction ----
# 1.Create a directory for this class and store your script \a0.R"
# 2. Install the packages, Hmisc, gdata,boot,xtable,MASS,moments,snow,mvtnorm
packages <- c("Hmisc","gdata","boot","xtable","MASS","moments","snow","mvtnorm")
# sapply(packages,install.packages)

# 3. Set your working directory
setwd("C://Users//javie//OneDrive//Documents//GitHub//ECON613//Assingments")

# 4. List the content of your directory and the content of your environment
list.files()
ls()

# 5. Check whether 678 is a multiple of 9
678%%9==0

# 6. Save your environment
save.image("misc.RDATA")

# 7. Find help on the function mean, cut2
help(mean)
?mean
??cut2

# 8. Find an operation that returns NaN (Not A Number)
0/0

# Excercise 2: Object Manipulation ----
# 1. Print Titanic, and write the code to answer these questions (one function (sum) , one operation)
titanic.df <- as.data.frame(Titanic)

# (a) Total population
sum(titanic.df$Freq)
# (b) Total adults
sum(titanic.df[titanic.df$Age == "Adult",]$Freq)
# (c) Total crew
sum(titanic.df[titanic.df$Class == "Crew",]$Freq)
# (d) 3rd class children
sum(titanic.df[titanic.df$Class == "3rd" & titanic.df$Age=="Child",]$Freq)
# (e) 2nd class adult female
sum(titanic.df[titanic.df$Class == "2nd" & titanic.df$Age=="Adult" & titanic.df$Sex=="Female",]$Freq)
# (f) 1st class children male
sum(titanic.df[titanic.df$Class == "1st" & titanic.df$Age=="Child" & titanic.df$Sex=="Male",]$Freq)
# (g) Female Crew survivor
sum(titanic.df[titanic.df$Class == "Crew" & titanic.df$Survived=="Yes" & titanic.df$Sex=="Female",]$Freq)
# (h) 1st class adult male survivor
sum(titanic.df[titanic.df$Class == "1st" & titanic.df$Survived=="Yes" & titanic.df$Sex=="Male" & titanic.df$Age=="Adult",]$Freq)

# 2. Using the function prop.table, find
# (a) The proportion of survivors among first class, male, adult
table_titanic <- xtabs(Freq~Survived,data = titanic.df[titanic.df$Class == "1st" & 
                         titanic.df$Age=="Adult" & titanic.df$Sex=="Male",])
prop.table(table_titanic)
# (b) The proportion of survivors among first class, female, adult
table_titanic <- xtabs(Freq~Survived,data = titanic.df[titanic.df$Class == "1st" & 
                                                         titanic.df$Age=="Adult" & titanic.df$Sex=="Female",])
prop.table(table_titanic)

# (c) The proportion of survivors among first class, male, children
table_titanic <- xtabs(Freq~Survived,data = titanic.df[titanic.df$Class == "1st" & 
                                                         titanic.df$Age=="Child" & titanic.df$Sex=="Male",])
prop.table(table_titanic)
# (d) The proportion of survivors among third class, female, adult
table_titanic <- xtabs(Freq~Survived,data = titanic.df[titanic.df$Class == "3rd" & 
                                                         titanic.df$Age=="Adult" & titanic.df$Sex=="Female",])
prop.table(table_titanic)
# Excercise 3: Vector Introduction ----

# 1. Use three different ways, to create the vectors
# (a) a = 1, 2,...  50
1:50
seq(1,50,1)
c(1,2,3,4,5,6,7,8,9,10:50)

# (b) b = 50; 49; : : : ; 1
rev(1:50)
seq(50,1,-1)
rev(c(1,2,3,4,5,6,7,8,9,10:50))

# 2. Create the vectors
# (a) a = 10; 19; 7; 10; 19; 7; : : : ; 10; 19; 7 with 15 occurrences of 10,19,7
rep(c(10,9,7),15)
# (b) b = 1; 2; 5; 6; : : : ; 1; 2; 5; 6 with 8 occurrences of 1,2,5,6
rep(c(1,2,5,6),8)
# 3. Create a vector of the values of log(x)sin(x) at x = 3:1; 3:2; : : : ; 6
x <- seq(3.1,6,0.1)
log(x)*sin(x)
# 4. Using the function sample, draw 90 values between (0,100) and calculate the mean. Re-do
# the same operation allowing for replacement.
mean(sample(0:100,90, replace = FALSE))
mean(sample(0:100,90, replace = TRUE))
# 5. Calculate
# a)
a <- matrix(1:20,ncol=1)
b <- matrix(1:15,ncol=1)
vec_results <- NULL
for(i in a){
  num <- exp(sqrt(i))*log(i^5)
  denom <- 5+cos(i)*sin(b)
res <- num/denom
vec_results <- cbind(vec_results,sum(res))
}
sum(vec_results)
# b)
vec_results_b <- NULL
for(a in 1:20){
  for(b in 1:a){
    num <- exp(sqrt(a))*log(a^5)
    denom <- 5+exp(a*b)*cos(a)*sin(b)
    res <- num/denom
    vec_results_b <- cbind(vec_results_b,res) 
  }
}
sum(vec_results_b)

# 6. Create a vector of the values of exp(x) cos(x) at x = 3, 3.1, ...6.
x <- seq(3,6,0.1)
exp(x)*cos(x)
