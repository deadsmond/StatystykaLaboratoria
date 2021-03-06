# Zaj�cia 12 ####


# zadanie 1 ---------------------------------------------------------------
# Zbi�r danych mtcars zawiera dane dotycz�ce pewnych cech samochod�w.
# Interesuje nas zbadanie korelacji mi�dzy zmiennymi mpg oraz wg. 

data = mtcars

# 1. Wykonaj wykres rozrzutu dla badanych cech. 

# wykres rozrzutu
plot(
  data$mpg, 
  data$wt,
  xlab = "mpg",
  ylab = "wt",
  pch = 16
)

# 2. Sprawd� za�o�enia testu istotno�ci dla wsp�czynnika korelacji.

# za�o�enia
shapiro.test(data$mpg)$p.value

qqnorm(data$mpg)
qqline(data$mpg, col = "red")

shapiro.test(data$wt)$p.value

qqnorm(data$wt)
qqline(data$wt, col = "red")

# 3. Wykonaj test istotno�ci dla wsp�czynnika korelacji 
# dla zmiennych mpg i wg. Oszacuj punktowo i przedzia�owo wsp�czynnik korelacji.

# testy
cor.test(data$mpg, data$wt, method = "pearson")$p.value
cor.test(data$mpg, data$wt, method = "pearson")$est
cor.test(data$mpg, data$wt, method = "pearson")$conf.int

# 4. Wykonaj polecenie punktu 3 korzystaj�c ze wsp�czynnik�w Kendalla i Spearmana.

# testy
cor.test(data$mpg, data$wt, method = "kendall")$p.value
cor.test(data$mpg, data$wt, method = "kendall")$est

cor.test(data$mpg, data$wt, method = "spearman")$p.value
cor.test(data$mpg, data$wt, method = "spearman")$est
