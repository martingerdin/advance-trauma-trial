# Sample size calculation for adherence and quality of life

# Calculate the sample size required to detect a difference in ATLS adherence with 70% adherence before training and 90% adherence after training.
# The sample size calculation is based on a two-sample test of proportions with a two-sided alpha of 0.05 and a power of 0.80.

library(pwr)
adherence.sample.size <- pwr.2p.test(h = ES.h(0.50, 0.60), sig.level = 0.05, power = 0.90)

# Now calculate the sample size required to detect a difference in quality of life scores with a mean of 0.7 before training and a mean of 0.8 after training. First we need to calculate the effect size for the difference in means. We will assume a pooled standard deviation of 0.2 for the quality of life scores.
effect.size <- (0.8 - 0.7) / 0.2

qol.sample.size <- pwr.t.test(d = effect.size, sig.level = 0.05, power = 0.90)

# Let's recalculate these estimates considering that the design is a cluster-randomized trial with an intraclass correlation of 0.02

CRTSize::n4means(delta = 0.1, sigma = 0.2, m = 150, 0.02, 0.05, 0.90, 1)

4500 / 30
