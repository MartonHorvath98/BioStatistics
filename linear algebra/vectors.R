# vector operations in R
x <- c(2, 1, 4)
y <- c(1, 0, 1)

# 1.) addition of two vectors
z <- x + y
print(z)
# 2.) multiplication by a scalar
v <- 2*x
print(v)
# 3.) multiplication of vectors
w <- x*y
print(w)
  # the scalar product is (two ways):
  print(sum(x*y)) #returns a vector
  print(x %*% y) #returns an array
# 4.) length of a vector - |x|
print(sqrt(sum(x^2)))

  
