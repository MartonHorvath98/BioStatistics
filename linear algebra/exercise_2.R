u <- c(3,6,7)
v <- c(12,13,14)

#1.) excercise
print(u+v) #sum
print(u*v) #scalar product
print(sqrt(sum(u^2))) #length of u
print(sqrt(sum(v^2))) #length of v
print(sqrt(sum((u+v)^2))) #length of sum of u and v

#2.) excercise
B <- matrix(c(1, 0, 4, 
              2, 0, 5,
              3, 0, 6),
            nrow = 3, ncol = 3,
            byrow = T)

print(B %*% u)
print(B %*% v)
print(B %*% (u + v))
print(B %*% B %*% u)

#3.) exercise
X <- matrix(c(1, 2, 
              1, 0),
            nrow = 2, ncol = 2,
            byrow = T)
print(det(X)) # determinant of X
print(eigen(X)$values) # eigen values of X (v1 & v2)
print(det(X) == prod(eigen(X)$values)) 

#𝑋 * v1 =𝜆1 * v1
v1 = eigen(X)$vectors[,1]
Xv1 = X %*% v1
lambda1 = Xv1 / v1
print(eigen(X)$values[1] == lambda1[1])

#𝑋 * v2 =𝜆2 * v2
v2 = eigen(X)$vectors[,2]
Xv2 = X %*% v2
lambda2 = Xv2 / v2
print(eigen(X)$values[2] == lambda2[1])

draw_a_tree <- function(x, y, lines = F) {
  # x-coordinates of nodes in the tree
  # (NA values creates breaks in the line, 'lifting the pen'):
  x <- x
  # y-coordinates:
  y <- y
  # Plot a brown tree, with thick branches
  ifelse(lines,
         lines(x, y, col='green', lwd=8),
         plot(x, y, type='l', col='brown', lwd=8,
              xlim=c(-2,2), ylim=c(-2,2))) # lwd sets the line thickness
}

x1 <- c(0, 0, 0.7, 1.5, NA, 0.7, 0.8, NA, 0, -0.6, -1.2, NA, -0.6, -0.5)
y1 <- c(0, 1, 1.3, 1.4, NA, 1.3, 1.8, NA, 1.0, 1.4, 1.7, NA, 1.4, 2.0)

draw_a_tree(x1, y1)
draw_a_tree(0.5*x1, 0.5*y1, T)
