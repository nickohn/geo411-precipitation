#Geography411 - Homework 1
#Nick Ohnmeiss
#Description: Analysis of annual precipitation data for Buffalo and San Diego

getwd()
dir()

#Step 4 
precip <- read.csv("precip .csv")
precip

#Step 5
dim(precip)
length(precip$Buffalo)
summary(precip)
mean(precip$Buffalo)
mean(precip$SanDiego)
sd(precip$Buffalo)
sd(precip$SanDiego)

#Step 6, histogram creation
hist(precip$Buffalo)

hist(precip$Buffalo, freq = FALSE)
curve(dnorm(x, mean=mean(precip$Buffalo), sd=sd(precip$Buffalo)), add=TRUE)

hist(precip$SanDiego, freq = FALSE)
curve(dnorm(x, mean=mean(precip$SanDiego), sd=sd(precip$SanDiego)), add=TRUE)

#Step 7, subset creation
precip$late <- ifelse(precip$Year >= 1983, 1, 0)
precip
precip$late == 1
precip$Year[precip$late == 1]

#Buffalo subsets
bfloLate <- precip$Buffalo[precip$late == 1]
bfloEarly <- precip$Buffalo[precip$late == 0]

#San Diego subsets
SDLate <- precip$SanDiego[precip$late == 1]
SDEarly <- precip$SanDiego[precip$late == 0]


#Step 8, t-test and vectors

#Unequal Variance Assumed, two-sided test
t.test(bfloLate, bfloEarly)

#Unequal Variance Assumed, one-sided test
t.test(bfloLate, bfloEarly, alternative = "greater")

#Equal Variance Assumed, two-sided test 
t.test(bfloLate, bfloEarly, var.equal = TRUE)

#Unequal Variance Assumed, two-sided test
t.test(SDLate, SDEarly)