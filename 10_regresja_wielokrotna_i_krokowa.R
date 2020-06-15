# Zajêcia 10 ####


# zadanie 1 ---------------------------------------------------------------
# Zbiór danych w pliku Automobile.csv zawiera dane charakteryzuj¹ce ró¿ne typy samochodów.

data = read.csv("Automobile.csv", na = "?")

head(data)

# 1. W tym zestawie danych wystêpuj¹ braki danych. Usuñ wszystkie obserwacje, dla których
# nie mamy pe³nych informacji o wszystkich zmiennych zawartych w zbiorze danych,
# u¿ywaj¹c funkcji na.omit() . 

data <- na.omit(data)

# 2.lnteresuje nas zbudowanie modelu opisuj¹cego cenê samochodów w zale¿noœci od
# pewnych ich cech. WeŸmy pod uwagê nastêpuj¹ce zmienne: horsepower, city.mpg,
# peak.rpm, curb.weight i num.of.doors jako zmienne niezale¿ne. 

# Dopasuj model regresji liniowej do tych danych. 

temp = subset(
  data, 
  select=c(
    "horsepower", 
    "city.mpg", 
    "peak.rpm", 
    "curb.weight", 
    "num.of.doors", 
    "price"
  )
)

pairs(temp)

model <- lm(price ~ horsepower + city.mpg + peak.rpm + curb.weight + num.of.doors, data = temp)
model

# Jakie s¹ wartoœci estymatorów wspó³czynników regresji i przedzia³y ufnoœci? 
# Które zmienne s¹ stymulantami a które destymulantami?
# estymacja parametrów
coef(model)
confint(model)

# Które wspó³czynniki s¹ statystycznie istotne w skontruowanym modelu? 
# Jakie jest dopasowanie modelu?
summary(model)

# Oblicz wartoœci dopasowane przez model oraz wartoœci reszt.
fitted(model)
# reszty
residuals(model)

# 3
# Spróbuj zredukowaæ model korzystaj¹c z regresji krokowej (“backward”, “forward”, AIC, BIC).

model_0 = lm(price ~ 1, data = data)

step(model)
step(model, direction = "backward")
step(model_0, direction = "forward", scope = formula(model))
step(model_0, direction = "forward", scope = formula(model), k = log(nrow(data)))

# 4
# Dokonaj redukcji modelu metod¹ eliminacji wstecznej, 
# tak aby w kolejnych krokach z pe³nego modelu stopniowo usuwaæ najmniej istotn¹ zmienn¹ niezale¿n¹, 
# a¿ otrzymamy model ze wszystkimi istotnymi zmiennymi niezale¿nymi. 
# Jakie by³o zachowanie odpowiedniego wspó³czynnika determinacji w kolejnych modelach?

model_2 <- lm(price ~ horsepower + city.mpg + curb.weight + num.of.doors, data = data)
summary(model_2)$coefficients
summary(model_2)$adj.r.squared
  
model_2 <- lm(price ~ horsepower + curb.weight + num.of.doors, data = data)
summary(model_2)$coefficients
summary(model_2)$adj.r.squared

model_2 <- lm(price ~ horsepower + curb.weight, data = data)
summary(model_2)$coefficients
summary(model_2)$adj.r.squared


# zadanie 1.5 ---------------------------------------------------------------
# Zamiast usuwaæ obserwacje z brakuj¹cymi danymi, jak to zrobiliœmy w punkcie 1, 
# uzupe³nij je za pomoc¹ œredniej i mediany s¹siednich wartoœci dla zmiennych iloœciowych i porz¹dkowych, odpowiednio. 
# Aby to zrobiæ, u¿yj funkcji impute() dostêpnej w pakiecie Hmisc. 
# W przypadku takich danych postêpuj zgodnie z instrukcjami w punktach 2-4.



# zadanie 1.6 ---------------------------------------------------------------
# Korzystaj¹c z ostatecznych modeli uzyskanych dla obu zbiorów danych, wykonaj prognozê ceny samochodu, 
# dla którego zmienne curb.weight i horsepower s¹ równe 2823 i 154, odpowiednio. 
# Który model daje lepsz¹ prognozê, gdyby cena tego samochodu wynosi³a 1650? Jak mo¿emy to wyjaœniæ?


model <- lm(price ~ horsepower + city.mpg + peak.rpm + curb.weight + num.of.doors, data = temp)

new_data <- data.frame(
  Curb.Weight = 2823, 
  Horsepower = 154
)

predict(model, new_data, interval = "prediction")
