############################################################
# NaiveBayes.R:
# -------------------
# Naive Bayes Algorithm in R
#
############################################################
# license:
# --------
# Copyright (c) 2014 JackyCode
# Distributed under the [MIT License][MIT].
# [MIT]: http://www.opensource.org/licenses/mit-license.php
#
############################################################


# ln=FALSE: 内部运算不采用去对数的形式
# default=FALSE: 内部各个特征的初值取0；若为真，则单个特征的数目，其初值取1；类别所类别初值取p
NaBa <- function(x, data, label, ln=FALSE, default=FALSE) {
	label <- as.factor(label)
	C <- levels(label)
	
	# P(Ci)
	P_Ci <- Count_P_Ci(C, label, ln)
	
	# P(x|Ci)
	P_x_Ci <- Count_P_x_Ci(C, x, data, label, ln, default)
	
	# P(Ci|x)
	P_Ci_x <- P_Ci * P_x_Ci
	
	# find the class
	class <- C[which(P_Ci_x == max(P_Ci_x))]
	
	return(list(C=C, class=class, P_Ci_x=P_Ci_x, P_Ci=P_Ci, P_x_Ci=P_x_Ci))
}

# P(Ci)
Count_P_Ci <- function(C, label, ln=FALSE) {
	Ci <- c()
	for (i in 1:length(C)) {
		Ci[i] <- sum(label == C[i])
	}
	if (ln) {
		P_Ci <- log(Ci/sum(Ci))
	} else {
		P_Ci <- Ci/sum(Ci)
	}
	
	return(P_Ci)
}

Count_P_x_Ci <- function(C, x, data, label, ln=FALSE, default=FALSE) {
	p <- length(x)
	P_x_Ci <- c()
	for (i in 1:length(C)) {
		Ci <- data[which(label == C[i]),]
		
		# Ci类别中，x_j出现的次数，综合起来使用num_x_Ci存储
		num_x_Ci <- c()
		for (j in 1:p) {
			num_x_Ci[j] <- sum(Ci == x[j])
		}
		if (default) {
			# default = TRUE: 使用修正，初始值为1
			P_x_Ci_not_times_dT <- (num_x_Ci + 1) / (length(Ci) + p)
			P_x_Ci[i] <- ifelse(ln, sum(log(P_x_Ci_not_times_dT)), exp(sum(log(P_x_Ci_not_times_dT))))
		} else {
			# default = FALSE
			P_x_Ci_not_times_dF <- num_x_Ci / length(Ci)
			P_x_Ci[i] <- ifelse(ln, sum(log(P_x_Ci_not_times_dF)), exp(sum(log(P_x_Ci_not_times_dF))))
		}
	}
	
	return(P_x_Ci)
}

## test
x <- c(1, 2)
data <- matrix(c(1, 3, 2, 3, 4, 2, 2, 1), ncol=2)
label <- c("a", "b", "b", "a")
NaBa(x, data, label) # 存在一些问题
NaBa(x, data, label, ln=TRUE, default=TRUE) # 修正算法