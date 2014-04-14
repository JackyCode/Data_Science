############################################################
# ID3_tree.R:
# -------------------
# use ID3 to built decision tree in R
#
############################################################
# license:
# --------
# Copyright (c) 2014 JackyCode
# Distributed under the [MIT License][MIT].
# [MIT]: http://www.opensource.org/licenses/mit-license.php
#
############################################################

## main
ID3_tree <- function(data, label) {
	level <- unique(label)
	tree <- list()
	
	if (length(label) == 1) {
		# tree <- level
		print(as.character(level))
		print('finish')
	} else {
		H <- entropy(label)
		result <- choose_best_attr(H, data, label)
		# tree <- result$A
		print(as.character(result$A))
		k <- result$k
		
		attr_level <- unique(data[[k]])
		for (i in 1:length(attr_level)) {
			print(as.character(attr_level[i]))
			result <- split_data(data, lable, k, attr_level[i])
			datav <- result$datav
			labelv <- result$labelv
			
			#tree$subtree <- ID3_tree(datav, labelv)
			if (length(names(data)) == 1) {
				print(as.character(most_freq(label)))
				print('finish')
			} else {
				ID3_tree(datav, labelv)
			}
			print('finish')
		}
	}
	# return(tree)
}

## 选取最佳属性
choose_best_attr <- function(H, data, label) {
	attrs <- names(data)
	G <- c()
	for (i in 1:length(attrs)) {
		G[i] <- Gain(H, data, label, i)
	}
	
	k <- which(G == max(G))
	A <- attrs[k]
	
	return(list(A=A, k=k))
}

## 切分数据集
split_data <- function(data, lable, k, i) {
	position <- which(data[[k]] == i)

	datav <- data[position,][-k]
	labelv <- label[position]

	return(list(datav=datav, labelv=labelv))
}

## 计算熵值
entropy <- function(label) {
	level <- unique(label)
	
	num <- c()
	prob <- c()
	for (i in 1:length(level)) {
		num[i] <- sum(label == level[i])
		prob[i] <- num[i]/length(label)
	}
	
	return(-sum(prob * log(prob, 2)))
}

## 计算信息增益
Gain <- function(H, data, label, i) {
	attr_C <- unique(data[[i]])
	S <- length(label)
	
	S_v <- c()
	attr_H <- c()
	for (j in 1:length(attr_C)) {
		S_v[j] <- sum(data[[i]] == attr_C[j])
		attr_label <- label[which(data[[i]] == attr_C[j])]
		attr_H[j] <- entropy(attr_label)
	}
	
	G <- H - sum(attr_H * (S_v / S))
	
	return(G)
}

# 计算一个分类集合中，哪一个类别出现的次数最多
most_freq <- function(label) {
	C <- unique(label)
	num <- c()
	for (i in 1:length(C)) {
		num[i] <- sum(label == C[i])
	}
	return(C[which(num == max(num))])
}


age <- c('s', 's', 'y', 'y', 's', 'y', 's')
income <- c('h', 'l', 'h', 'l', 'h', 'l', 'h')
buy_iphone <- c('y', 'n', 'y', 'n', 'y', 'y', 'n')
data <- data.frame(age, income)
label <- as.factor(buy_iphone)

# H <- entropy(label)
# choose_best_attr(H, data, label)
ID3_tree(data, label)
