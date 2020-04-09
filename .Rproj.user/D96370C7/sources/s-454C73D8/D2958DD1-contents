library(ggplot2)
library(data.table)

# Excercise 4.1 ====================================================================================================================

show(HairEyeColor)

EyeHairFreq <- apply(HairEyeColor, c('Eye', 'Hair'), sum)
EyeHairProp <- EyeHairFreq/sum(EyeHaiFreq) 

HairFreq <- apply(HairEyeColor, c('Hair'), sum)
HairProp <- HairFreq/sum(HairFreq)

EyeFreq <- apply(HairEyeColor, c('Eye'), sum)
EyeProp <- EyeFreq/sum(EyeFreq)

# Conditional probabiility of hair color given blue eyes
EyeHairProp['Blue',]/EyeProp['Blue']

# Conditional probability of hair color given brown eyes
EyeHairProp['Brown',]/EyeProp['Brown']

# Conditional probability of eye color given brown hair
EyeHairProp[,'Brown']/HairProp['Brown']

# Excercise 4.2 ====================================================================================================================

# Number of coin flips
N <- 1000

# Probability of heads (biased coin)
pHeads <- 0.8

# Random sample (heads = 1, tails = 0)
set.seed(1702)
flipSequence <- sample(c(0,1), prob = c(1-pHeads, pHeads), size = N, replace = T)

# Compute the running proportions of H(ead)
runProp <- cumsum(flipSequence)/1:N

# Draw graph
DT <- data.table(runNumber = 1:N,
                 headProportion = runProp)

flipPropPlot <- ggplot(DT, aes(log10(runNumber), headProportion)) +
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  geom_hline(yintercept = pHeads, color = 'red', linetype=2) +
  labs(x = 'Flip number', y = 'Running proportion of heads') +
  scale_x_continuous(breaks = c(0,1,2,3),
                     labels = c(1,10,100,1000)) 

# Excercise 4.3 ====================================================================================================================

# Part A
(2*4)/48

# Part B
((2*4)+(2*4))/48

# Excercise 4.4 ====================================================================================================================

# Part A - intialize
xlow  = 0
xhigh = 1
dx = 0.01

x = seq(from = xlow, to = xhigh, by = dx )

# Compute y values, i.e., probability density at each value of x:
y = 6*x*(1-x)

# Plot density function
DT <- data.table(x = x,
                 y = y)

probDensityPlot <- ggplot(DT, aes(x, y)) +
  geom_line()

# Part B
3*x^2 - 2*x^3 + C

# Excercise 4.5 ====================================================================================================================

# Part A - initialize
meanval = 0.0               
sdval = 0.2                 
xlow  = meanval - sdval 
xhigh = meanval + sdval 
dx = sdval/1000              

x = seq(from = xlow, to = xhigh, by = dx )
y = (1/(sdval*sqrt(2*pi)) ) * exp( -.5 * ((x-meanval)/sdval)^2)

sum(dx*y)

# Part B
meanval = 162
sd = 15

# Excercise 4.6 ====================================================================================================================



