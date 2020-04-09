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


