############################################################
# LinearRegression.R:
# -------------------
# tells how to built a function to do Linear Regression in R
#
############################################################
# license:
# --------
# Copyright (c) 2014 JackyCode
# Distributed under the [MIT License][MIT].
# [MIT]: http://www.opensource.org/licenses/mit-license.php
#
############################################################

## Either x or y should be a matrix
LinReg <- function (x, y) {
	if (!is.matrix(x) | !is.matrix(y)) {
		stop("Either x or y should be a matrix.")
	}
	
	coefficients <- x
	intercept <- rep(1, dim(x)[1])
	
	# transfer all the datas to matrix
	xMat <- as.matrix(cbind(intercept, coefficients))
	yMat <- y
	xTx <- t(xMat) %*% xMat
	
	# check if the matrix is siggular
	if (det(xTx) == 0.0) {
		stop("This matrix is singular, cannot do inverse.")
	}
	
	# calculate the coefficients and intercept
	beta <- solve(xTx) %*% t(xMat) %*% yMat
	
	# F-test
	n <- dim(x)[1]
	p <- dim(x)[2]
	ypred <- xMat %*% beta
	SSR <- sum((ypred-mean(yMat))**2)
	SSE <- sum((yMat-ypred)**2)
	F <- (SSR/p)/(SSE/(n-p-1))
	Fpval <- pf(44.8, 2, 47, lower.tail=FALSE)
	
	# t-test
	c <- diag(solve(xTx))
	delta_hat <- sqrt(sum((yMat-ypred)**2)/(n-p-1))
	sqrt_c_delta <- sqrt(c) * delta_hat
	T <- beta/sqrt_c_delta
	Tpval=c()
	for (i in 1:(p+1)) {
		Tpval[i] <- 1-2*(pt(abs(T[i]), n-p-1)-0.5)
	}
	
	
	# return
	return(list(coefficients=beta, Ftest=list(F=F, pvalue=Fpval), Ttest=list(T=T, pvalue=matrix(Tpval))))
}

# use 'cars' to test
x1 <- cars$speed
set.seed(100)
x2 <- rnorm(50, 0, 1)
x <- as.matrix(cbind(x1, x2))
y <- matrix(cars$dist)
LinReg(x, y)
summary(lm(y~x1+x2))