## My R Code for Riddler Classic

## Define Terms:
## Let f be the perimeter of the fabric in m and the weight of the fabric in kg (max 1)
## Let k be the weight of each individual post
## Let T be total weight (f+k) <= 1
## You want to maximize area while keeping weight under 1kg.

## When k=0.2
k <- .2

## Say there are 3 posts
(f = 1-3*k)
(A = .5 * f/3 * f/3/2*sqrt(3))

## Say there are 4 posts
(f = 1 - 4*k)
(A = (f/4)^2)

A3 <- function(k) { ## Finding the area of triangle for any value of k
  f = 1 - 3*k
  if(f > 0) {
    x = .5 * f/3 * f/3/2*sqrt(3)
    return(x)
  } else return(sqrt(-1))
}
A3(.2)
A4 <- function(k) { ## Finding the area of square for any value of k
  f = 1 - 4*k
  if(f > 0) {x = (f/4)^2; return(x)} else return(sqrt(-1))
}
A4(.2)

## Applying A3 & A4 to different values of k
ks <- seq(.001, 1, by=.001)
(A <- matrix(c(sapply(ks, A3), sapply(ks, A4)), ncol=2))
max(which(A[,2] > A[,1]))

A3(.089); A4(.089)
A3(.090); A4(.090)
## So 0.089 is the greatest value of k for which you should use 4 points instead of 3.
