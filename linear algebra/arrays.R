# An array in R is similar to a vector but elements can have more than one
# index so the array can be multidimensional!

# 1.) initializing an array - the dimensions are defined by the 'dim" attribute
a1 <- array(dim = c(2,3)) # a 2-by-3 two-dimensional matrix, filled with NA's
print(a1)

a2 <- array(c(3,4,5,17), dim = c(2,2))
print(a2)

#to access elements, the [] can be used:
a2[1,1] # = 3; a single element - [row index, column index]
a2[1, ] # leaving out an index, we can extract a row or a column
        # in this case - the 1st row (returns a vector!!!)

# 2.) transpose a matrix
a3 <- t(a2)
print(a3)

# 3.) determinant of a matrix - det(A)
print(det(a2)) 
print(unlist(determinant(a2))) # is a generic function that returns separately 
                               # the modulus of the determinant, optionally on 
                               # the logarithm scale, and the sign of the 
                               # determinant - returns a list!!!

# 4.) simple matrix algebra
A <- matrix(c(3, 7, 1, 8), nrow = 2, ncol = 2) # a 2-by-2 matrix
  # array(c(3, 7, 1, 8), dim=c(2, 2)) - would yield the same
B <- matrix(c(0, 2, 2, -1), 2, 2)

#Addition:
print(A+B)
#Multiplication (%*%):
print(A %*% B)

# 4.) eigen value decomposition of a matrix - eigen(A) returns a list!!
E <- eigen(A)

#eigen values - a vector!!!
E$values # [eigenvalue(1), ..., eigenvalue(n)]; n = number of dimensions 

# eigen vectors - a matrix!!! 
E$vectors # an n-by-n matrix
          # eigenvector(1): E$vectors[,1] - in column 1
v1 <- E$vectors[,1]
v2 <- E$vectors[,2]

# the eigen vector - geometrically speaking - is a vector,  vectors that A only
# elongates or shrinks, its direction remains unmodified! The amount that they 
# elongate/shrink by is the eigenvalue. 
# The eigen vector of an NxN matrix satisfies the equation: Av=(lambda)v, where
# v is the eigen vector and lambda is the corresponding eigenvalue:
Av1 <- A %*% v1
lambda <- Av1 / v1 # = 9.140055 
