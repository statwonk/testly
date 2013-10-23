bayes_test <- function(lift, expected_conv_rate, original_conv, alternative_conv, original_participants, alternative_participants){

trials <- 100000 # This should just remain sufficiently high such that repeated runs show convergence.

# The ratio of alpha:beta should be set to about what you THINK the conversion rate should be about.
alpha <- expected_conv_rate
beta <- 100

df <- data.frame("original"=rbeta(trials, original_conv + alpha, original_participants - original_conv + beta),
                   "alternative"= rbeta(trials, alternative_conv + alpha, alternative_participants - alternative_conv + beta))

print(paste("The probability that the alternative is better than original is ", 
            100 * length(which(df$alternative>df$original))/ trials, "%", sep = "")) # the probability that original is better than alternative

print(paste("The probability that original is better than alternative is ", 
            100 * length(which(df$original>df$alternative))/trials, "%", sep = "")) # the probability that original is better than alternative

print(paste("The probability that the alternative is ", 
            lift, "%",
            " greater than the original is ", 
            100 * mean(((100 * (df$alternative - df$original)) / df$original) > lift), "%", sep = "")) 

print(paste("The probability that the original is ", 
            lift, "%",
            " greater than the alternative is ", 
            100 * mean(((100 * (df$original - df$alternative)) / df$alternative) > lift), "%", sep = ""))
}


bayes_test(lift = 50, # in percent, so e.g. - 3 means 3% or 0.03 ... just getting rid of extra decimals
           expected_conv_rate = 1, # in percent, e.g. 1 means 1%, 0.5 means 0.5%
           original_conv = 22, 
           alternative_conv = 34, 
           original_participants = 781, 
           alternative_participants = 781)


