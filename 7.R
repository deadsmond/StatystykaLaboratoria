# Zajêcia 7 ####

# Zadanie 1 ---------------------------------------------------------------
x <- c(862, 870, 876, 866, 871, 865, 861, 873, 871, 872)
shapiro <- shapiro.test(x)
p_val <- shapiro$p.value

p_val

mean(x)

# Zadanie 2 ---------------------------------------------------------------
data_A = c(78.2, 78.5, 75.6, 78.5, 78.5, 77.4, 76.6)
data_B = c(76.1, 75.2, 75.8, 77.3, 77.3, 77.0, 74.4, 76.2, 73.5, 77.4)

boxplot(data_A, data_B)

shapiro.test(data_A)$p.value

qqnorm(data_A)
qqline(data_A)

shapiro.test(data_B)$p.value

qqnorm(data_B)
qqline(data_B)


var(data_A)

var(data_B)

# 0.3683809

mean(data_A)

mean(data_B)

t.test(data_A, data_B, var.equal = TRUE, alternative = 'greater')$p.value

# Zadanie 3 ---------------------------------------------------------------

a <- c(84, 87, 87, 90, 90, 90, 90, 93, 93, 96)
b <- c(89, 92, 98, 95, 95, 92, 95, 92, 98, 101)

boxplot(a, b)

## [1] 0.7025892

qqnorm(a)
qqline(a)

## [1] 0.691489

qqnorm(b)
qqline(b)

mean(a)

mean(b)

## [1] 0.0003786878


# Zadanie 4 ---------------------------------------------------------------

a <- c(171, 176, 179, 189, 176, 182, 173, 179, 184, 186, 189, 167, 177)
b <- c(161, 162, 163, 162, 166, 164, 168, 165, 168, 157, 161, 172)

boxplot(a, b)

## [1] 0.8595396

qqnorm(a)
qqline(a)

## [1] 0.9447828

qqnorm(b)
qqline(b)

## [1] 45.74359
## [1] 16.08333
## [1] 0.04689163

mean(a)

mean(b)

## [1] 6.928802e-07

# Zadanie 5 ---------------------------------------------------------------

w_test <- function(x, lambda_zero, alternative) {
  return('hmm')
}
