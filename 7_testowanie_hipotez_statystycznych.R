# Zaj�cia 7 ####

# http://enroute.pl/testy-statystyczne/
# https://www.statystyczny.pl/hipotezy-statystyczne/


# Zadanie 1 ---------------------------------------------------------------
# W pewnym regionie wykonano dziesi�� niezale�nych pomiar�w g��boko�ci morza
# i uzyskano nast�puj�ce wyniki: 862, 870, 876, 866, 871, 865, 861, 873, 871, 872. 
# Na poziomie istotno�ci a = 0,05 zweryfikuj hipotez�, 
# �e �rednia g��boko�� morza w tym regionie wynosi 870m. 

x <- c(862, 870, 876, 866, 871, 865, 861, 873, 871, 872)

shapiro.test(x)

mean(x)

t.test(x, mu = 870, alternative = "less")$p.value



# Zadanie 2 ---------------------------------------------------------------
# Producent proszku do prania A twierdzi, �e jego produkt jest znacznie lepszy ni�
# konkurencyjny proszek B. Aby zweryfikowa� to zapewnienie, CTA (Consumer Test Agency)
# przetestowa�o oba proszki do prania. W tym celu przeprowadzono pomiary stopnia wyprania 7
# kawa�k�w tkaniny z proszkiem A i uzyskano wyniki (w %):
#   18,2; 78,5; 75,6; 78,5; 78,5; 77,4; 76,6,
# i 10 kawa�k�w tkaniny z proszkiem B otrzymuj�c wyniki (w %):
#   76,1; 75,2; 75,8; 71,3; 77,8; (7,0; (4,4; 76,2; 73,5; 77,4.
# Jaki powinien  by� wniosek CTA na temat jako�ci tych proszk�w? 

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

var.test(data_A, data_B, alternative = 'less')$p.value

mean(data_A)

mean(data_B)

t.test(data_A, data_B, var.equal = TRUE, alternative = 'greater')$p.value

# Zadanie 3 ---------------------------------------------------------------
# Grupa 10 os�b zosta�a poddana badaniu maj�cym na celu zbadanie stosunku do
# szk� publicznych. Nast�pnie grupa obejrza�a film edukacyjny maj�cy na celu popraw�
# podej�cia do tego typu szk�. Wyniki s� nast�puj�ce (wy�sza warto�� oznacza lepsze podej�cie):
#  przed: 84, 87, 87, 90, 90, 90, 90, 93, 93, 96,
#  po: 89, 92, 98, 95, 95, 92, 95, 92, 98, 101.
#  Zweryfikuj, czy film znacznie poprawia stosunek do szk� publicznych. 

data_przed <- c(84, 87, 87, 90, 90, 90, 90, 93, 93, 96)
data_po <- c(89, 92, 98, 95, 95, 92, 95, 92, 98, 101)

boxplot(data_przed, data_po)

shapiro.test(data_przed)$p.value

qqnorm(data_przed)
qqline(data_przed)

shapiro.test(data_po)$p.value

qqnorm(data_po)
qqline(data_po)

mean(data_przed)

mean(data_po)

t.test(data_po, data_przed, alternative = 'greater', paired = TRUE)$p.value


# Zadanie 4 ---------------------------------------------------------------
# Zbadano wzrost 13 m�czyzn i 12 kobiet w pewnym centrum sportowym. Wyniki s� nast�puj�ce:
#    m�czy�ni: 171, 176, 179, 189, 176, 182, 173, 179, 184, 186, 189, 167, 177,
#    kobiety: 161, 162, 163, 162, 166, 164, 168, 165, 168, 157, 161, 172.
# Czy mo�emy stwierdzi�, �e �redni wzrost m�czyzn jest znacznie wi�kszy ni� wzrost kobiet? 

wzrost_m <- c(171, 176, 179, 189, 176, 182, 173, 179, 184, 186, 189, 167, 177)
wzrost_k <- c(161, 162, 163, 162, 166, 164, 168, 165, 168, 157, 161, 172)

boxplot(wzrost_m, wzrost_k)

shapiro.test(wzrost_m)$p.value

qqnorm(wzrost_m)
qqline(wzrost_m)

shapiro.test(wzrost_k)$p.value

qqnorm(wzrost_k)
qqline(wzrost_k)

var(wzrost_m)

var(wzrost_k)

var.test(wzrost_m, wzrost_k, var.equal = TRUE, alternative = 'greater')$p.value


mean(wzrost_m)

mean(wzrost_k)

t.test(wzrost_m, wzrost_k, alternative = 'greater')$p.value


# Zadanie 5 ---------------------------------------------------------------
# napisz funkcj� co�tam
w_test <- function(x, lambda_zero, alternative) {
  return('hmm')
}
