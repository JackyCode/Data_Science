############################################################
# kNN.R:
# -------------------
# kNN Algorithm in R
#
############################################################
# license:
# --------
# Copyright (c) 2014 JackyCode
# Distributed under the [MIT License][MIT].
# [MIT]: http://www.opensource.org/licenses/mit-license.php
#
############################################################

kNN <- function(x, data, lable, k=3) {
	xMat <- as.matrix(x)
	if (dim(xMat)[2] == 1) {
		xMat <- t(xMat)
	}
	m <- dim(xMat)[1]
	
	# 对x数据集中每一个点进行分类，最终将分类数据转换成矩阵存到class中
	k <- c()
	for (i in 1:m) {
		k[i] <- kNNi(xMat[i,], data, lable, k=3)
	}
	class <- as.matrix(k)
	
	return(cbind(xMat, class))
}

kNNi <- function(x, data, lable, k=3) {
	# 计算距离
	dataMat <- as.matrix(data)
	distances <- sqrt(diag((t(t(dataMat)-as.numeric(x)) %*% (t(dataMat)-as.numeric(x)))))
	
	# 取出k个最小的点
	position <- order(distances)[1:k]
	
	level <- levels(as.factor(as.character(lable[position])))
	
	# 取出取值最多的那个标签
	num <- c()
	for (i in 1:length(level)) {
		num[i] <- sum(lable[position] == level[i])
	}
	
	class <- level[which(num == max(num))]
	
	return(class)
}

## test
x <- matrix(c(1, 2, 1, 3), ncol=2)
data <- matrix(c(1, 3, 2, 3, 4, 2, 2, 1), ncol=2)
lable <- c("a", "b", "b", "a")
kNN(x, data, lable)