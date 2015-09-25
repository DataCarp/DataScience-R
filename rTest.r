#1. Write a loop that calculates 12-factorial.

r <-1

for(i in 1:12){
	r = r * i
	print(r)
}

#2. Show how to create a numeric vector that contains the sequence from 20 to 50 by 5.

incr <- 5
numVec <- c(4:10)
incr * numVec

#3. Show how to take a trio of input numbers a, b, and c and implement the quadratic equation.

vars <- c(a<- 3, b<- 6, c<- -24)
x <-(-b+(sqrt(b^2-4*a*c))/2*a)
y <-(-b-(sqrt(b^2-4*a*c))/2*a)
