# Zajêcia 3 ####

# Zadanie 1 ---------------------------------------------------------------
multiply_while <- function(x) {
  result = x[1]
  i <- 1
  while (i <= length(x)) {
    result <- result * x[i]
    i <- i + 1
  }
  return(result)
}

multiply_repeat <- function(x) {
  result = x[1]
  i <- 1
  repeat {
    result <- result * x[i]
    i <- i + 1
    if (i > length(x)) break
  }
  return(result)
}

multiply_for <- function(x) {
  result = x[1]
  for (i in x) {
    result <- result * i
  }
  return(result)
}

x = c(1, 2, 3, 4, 5, 6)
print(multiply_while(x))
print(multiply_repeat(x))
print(multiply_for(x))


# Zadanie 2 ---------------------------------------------------------------
zad_2 <- function() {
  counter = 0
  for (i in 1:100) {
    for (j in 1:100) {
      if (choose(i, j) > 1000000) counter = counter + 1
    }
  }
  return(counter)
}

print(zad_2())

# Zadanie 3 ---------------------------------------------------------------
is_palindrome <- function(x) {
  return(all(x==rev(x)))
}

print(is_palindrome(c(1, 2, 3, 2, 1)))
print(is_palindrome(c(1, 2, 3, 2)))

# Zadanie 4 ---------------------------------------------------------------
deg_to_rad <- function(x) {
  return(x * pi / 180)
}

x <- c(deg_to_rad(0),deg_to_rad(30),deg_to_rad(45),deg_to_rad(60),deg_to_rad(90))

ramka <- data.frame(
  sin = sin(x),
  cos = cos(x),
  tg = tan(x),
  ctg = tan(x)^-1
)

print(ramka)

# Zadanie 5 ---------------------------------------------------------------
zad_5 <- function(x) {
  x = sort(x)
  return(c(head(x, 3), tail(x, 3)))
}

zad_5(c(2, 6, 1, 5, 7, 3, 4))
