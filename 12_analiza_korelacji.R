# Zajêcia 12 ####


# zadanie 1 ---------------------------------------------------------------
# 1.

data = mtcars

# wykres rozrzutu
plot(
  data$mpg, 
  data$wt,
  xlab = "mpg",
  ylab = "wt",
  pch = 16
)

# 2. SprawdŸ za³o¿enia testu istotnoœci dla wspó³czynnika korelacji.

# za³o¿enia
shapiro.test(data$mpg)$p.value

qqnorm(data$mpg)
qqline(data$mpg, col = "red")

shapiro.test(data$wt)$p.value

qqnorm(data$wt)
qqline(data$wt, col = "red")

# 3. Wykonaj test istotnoœci dla wspó³czynnika korelacji 
# dla zmiennych mpg i wg. Oszacuj punktowo i przedzia³owo wspó³czynnik korelacji.

# testy
cor.test(data$mpg, data$wt, method = "pearson")$p.value
cor.test(data$mpg, data$wt, method = "pearson")$est
cor.test(data$mpg, data$wt, method = "pearson")$conf.int

# 4. Wykonaj polecenie punktu 3 korzystaj¹c ze wspó³czynników Kendalla i Spearmana.

# testy
cor.test(data$mpg, data$wt, method = "kendall")$p.value
cor.test(data$mpg, data$wt, method = "kendall")$est

cor.test(data$mpg, data$wt, method = "spearman")$p.value
cor.test(data$mpg, data$wt, method = "spearman")$est
