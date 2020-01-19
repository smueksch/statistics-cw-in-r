### One-way ANOVA ###

# Varieties of tunip data:
turnips_a <- c(1.28, 1.18, 1.64, 1.21, 1.30, 1.43, 1.51, 1.30)
turnips_b <- c(1.61, 1.71, 1.50, 1.54, 1.41, 1.31, 1.76, 1.82)
turnips_c <- c(1.54, 1.65, 1.81, 1.76, 1.65, 1.67, 1.78, 1.86)
turnips <- c(turnips_a, turnips_b, turnips_c)
variety <- c(rep(1,8), rep(2,8), rep(3,8))

boxplot(turnips ~ variety, xlab="Variety", ylab="Turnip Yield (tonnes/hectare)")

anova(lm(turnips ~ as.factor(variety)))

### Least Significant Differences ###

# 0.02234 is within group mean square, read that off from the above ANOVA table.
# Whole formula is in notes Chapter 5, p66.
lsd <- qt(0.975, 21) * sqrt(2 * 0.02234 / 8) 
mu_a <- mean(turnips_a)
mu_b <- mean(turnips_b)
mu_c <- mean(turnips_c)

print(lsd)
print(mu_c - mu_a)
print(mu_c - mu_b)
print(mu_b - mu_a)