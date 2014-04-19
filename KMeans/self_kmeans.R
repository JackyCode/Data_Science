############################################################
# self_kmeans.R:
# -------------------
# tells how to use custom function to achieve the k-means
#
############################################################
# license:
# --------
# Copyright (c) 2014 JackyCode
# Distributed under the [MIT License][MIT].
# [MIT]: http://www.opensource.org/licenses/mit-license.php
#
############################################################

se_kmeans <- function(x, k) {
	if (!is.matrix(x)) {
		x <- as.matrix(x)
	}
	
	n <- dim(x)[1]
	## 讲样品随机分成k类，并计算其中心
	cluster <- sample(1:k, n, , replace = TRUE)
	center <- matrix(, nrow=k, ncol=dim(x)[2])
	for (i in 1:k) {
		center[i,] <- apply(x[which(cluster == i),], 2, mean)
	}
	
	## 定义change_cluster,用于每次类别变动之后的比对
	change_cluster = rep(0, n)
	
	## 循环，给每个样品分类
	while (!all(cluster == change_cluster)) {
		change_cluster = cluster
		for (i in 1:n) {
			
			## 比较距离，可以省去开平方
			dis <- diag((center - x[i,]) %*% t(center - x[i,]))
			position <- which(dis == min(dis))
			
			if (!(cluster[i] == position)) {
				# 更新类别
				ori_cluster_i <- cluster[i]
				cluster[i] <- position
				
				## 更新类别的中心
				center[ori_cluster_i,] <- apply(x[which(cluster == ori_cluster_i),], 2, mean)
				center[position,] <- apply(x[which(cluster == position),], 2, mean)
			}
		}
	}
	
	return(list(cluster=cluster, center=center))
}

x1 <- matrix(rnorm(500, 1, 0.5), 100, 5)
x2 <- matrix(rnorm(500, 2, 0.5), 100, 5)
x <- rbind(x1, x2)

clusters <- se_kmeans(x, 2)
plot(x, col=clusters$cluster, pch=as.character(clusters$cluster), cex=0.5)
points(clusters$center, col='green', pch='o', cex = 2)