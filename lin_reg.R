# Data exploration and linear regression (R)


df <- read.csv(file = "C:/Users/jaska/Desktop/data/data.csv", header=T)
head(df) # checking

# converting appropriate columns to factor types
df$Sex <- as.factor(df$Sex)
df$City <- as.factor(df$City)
df$Profession <- as.factor(df$Profession)
df$Brand <- as.factor(df$Brand)
df$Location <- as.factor(df$Location)

# univariate analysis
# performing linear regression for various variables
# default is family = "gaussian", i.e. normal distribution is assumed
m1 <- glm(Score~Brand, data=df)
summary(m1)

m2 <- glm(Score~Sex, data=df)
summary(m2)

m3 <- glm(Score~City, data=df)
summary(m3)

m4 <- glm(Score~Profession, data=df)
summary(m4)

m5 <- glm(Score~Age, data=df)
summary(m5)

m6 <- glm(Score~Location, data=df)
summary(m6)

m7 <- glm(Score~N_dose, data=df)
summary(m7)

# finding overall table one for analysis
library(tableone)
vars <- c("Sex", "City", "Profession", "Age", "Brand", "Location", "Score")
table1 <- CreateTableOne(vars=vars, data=df)
table1

vars <- c("Score")
strat <- c("Sex")
table1_2 <- CreateTableOne(vars=vars, strata=strat, data=df)
table1_2

strat <- c("City")
table1_3 <- CreateTableOne(vars=vars, strata=strat, data=df)
table1_3

strat <- c("Profession")
table1_4 <- CreateTableOne(vars=vars, strata=strat, data=df)
table1_4

strat <- c("Brand")
table1_5 <- CreateTableOne(vars=vars, strata=strat, data=df)
table1_5

strat <- c("Location")
table1_6 <- CreateTableOne(vars=vars, strata=strat, data=df)
table1_6

# conducting multiple linear regression analysis
m8 <- glm(Score~Brand+Age+Brand*Age, data=df)
summary(m8)

# conducting stepwise regression model development
library(MASS)
m9 <- glm(Score~Sex+City+Profession+Age+Brand+Location+Brand*Age, data=df)
stepAIC(object = m9, direction = "backward")

# model selection based on stepwise selection
m10 <- glm(Score~Brand+Age, data=df)
summary(m10)