############################################################
# kmeans.R:
# -------------------
# use the `builtin function` "kmeans" in R
#
############################################################
# license:
# --------
# Copyright (c) 2014 JackyCode
# Distributed under the [MIT License][MIT].
# [MIT]: http://www.opensource.org/licenses/mit-license.php
#
############################################################

x1 <- matrix(rnorm(500, 1, 0.5), 100, 5)
x2 <- matrix(rnorm(500, 2, 0.5), 100, 5)
x <- rbind(x1, x2)
clusters <- kmeans(x, 2)
clusters
plot(x, col=clusters$cluster, pch=as.character(clusters$cluster), cex=0.5)
points(clusters$centers, col='green', pch='o', cex = 2)