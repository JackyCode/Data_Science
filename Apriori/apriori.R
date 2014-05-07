###################################################################
# apriori.R:
# -------------------
# tells how to use custom function to achieve the apriori algorithm
#
###################################################################
# license:
# --------
# Copyright (c) 2014 JackyCode
# Distributed under the [MIT License][MIT].
# [MIT]: http://www.opensource.org/licenses/mit-license.php
#
############################################################

##################################################
# apriori algorithm
##################################################
# main1: create frequent itemsets
##################################################
## (dataset, minSupport) -> (frequent itemsets, support degree)
## (list: str, numeric) -> (list: str & numeric)
apriori_cfi <- function(dataSet, minS=0.5) {
    # create C1
    # C1: (list: character)
    C1 <- createC1(dataSet)
	
    # loop to find all the frequent itemsets satisfied the minS
    # store frequent itemsets to the list: 'fre_sets'
    # store support degree to the list: 'support_degree'
    fre_sets <- list()
    support_degree <- list()
	Ck <- C1
	while(length(Ck) > 0) {
	    # create L1
		# Lk_SupD: $Lk, $sup_degree
	    # Lk_SupD: (list: str $ numeric)
	    Lk_SupD <- createLk(dataSet, Ck, minS)
		Lk <- Lk_SupD$Lk
		
		if (length(Lk) > 0) {
			fre_sets[[length(fre_sets)+1]] <- Lk
			support_degree[[length(support_degree)+1]] <- Lk_SupD$sup_degree
		}
		# create Ck from Lk-1
		# Ck: (list: str)
		# if lenght(Lk)==1 or 0, then Ck == NULL
		Ck <- createCk(Lk)
	}
	
	# change the data structures: fre_sets and support_degree
	fre_sets_cds <- cds1(fre_sets)
	support_degree_cds <- cds2(support_degree)

    ## return: frequent_itemsets and the support degrees
    return(list(fre_sets=fre_sets_cds, support_degree=support_degree_cds))
}

## create C1
## (list: str) -> (list: str/character)
createC1 <- function(dataSet) {
    if (!is.list(dataSet)) {
        stop("The data set should be a list!")
    }
	
	# use union to calc all the elements in dataSet
    n <- length(dataSet)
    C1_tmp <- dataSet[[1]]
    for (i in 2:n) {
        C1_tmp <- union(C1_tmp, dataSet[[i]])
    }

	# change the data structure to object
    C1 <- list()
    for (i in 1:length(C1_tmp)) {
        C1[[i]] <- C1_tmp[i]
    }
    return(C1)
}

## create Lk
## Lk: list $Lk, $sup_degree
## (list: str, list: str, numeric) -> (list: str & numeric)
createLk <- function(dataSet, Ck, minS) {
    n <- length(dataSet)
    m <- length(Ck)

    # check if Ck[i] > minS and then restore to Ck_satisfied
    Ck_satisfied <- list()
    supportDegree <- c()
    for (i in 1:m) {
		# count the support degree of a set
		# (list: str, vector) -> (numeric)
        supportDegree_i <- countS(dataSet, Ck[[i]])
        if (supportDegree_i > minS) {
            Ck_satisfied[[length(Ck_satisfied)+1]] <- Ck[[i]]
            supportDegree <- c(supportDegree, supportDegree_i)
        }
    }

    return(list(Lk=Ck_satisfied, sup_degree=supportDegree))
}

# create Ck from Lk-1
# Ck: (list: str) -> (list: str) or (NULL)
# if lenght(Lk)==1 or 0, then Ck == NULL
createCk <- function(Lk) {
    n <- length(Lk)
    m <- length(Lk[[1]])
	
    Ck <- list()
	if (n > 1) {
	    for (i in 1:(n-1)) {
	        for (j in (i+1):n) {
	            if (m == 1) {
	                Ck[[length(Ck)+1]] <- union(Lk[[i]], Lk[[j]])
	            } else {
	                L1 <- Lk[[i]][1:(m-1)]
	                L2 <- Lk[[j]][1:(m-1)]
	                if (length(setdiff(L1, L2)) == 0) {
	                    Ck[[length(Ck)+1]] <- union(Lk[[i]], Lk[[j]])
	                }
	            }
	        }
	    }
	}
    
    return(Ck)
}

# change the data structures: fre_sets and support_degree
cds1 <- function(fre_sets) {
	n <- length(fre_sets)
	
	fre_sets_cds <- list()
	a <- 1
	for (i in 1:n) {
		for (j in 1:length(fre_sets[[i]])) {
			fre_sets_cds[[a]] <- fre_sets[[i]][[j]]
			a = a + 1
		}
	}
	
	return(fre_sets_cds)
}

cds2 <- function(support_degree) {
	n <- length(support_degree)
	
	support_degree_cds <- support_degree[[1]]
	
	if (n > 1) {
		for (i in 2:n) {
			support_degree_cds <- c(support_degree_cds, support_degree[[i]])
		}
	}
	
	return(support_degree_cds)
}

# count the support degree of a set
# (list: str, vector) -> (numeric)
countS <- function(dataSet, set) {
    n <- length(dataSet)
    m <- length(set)

    count <- 0
    for (i in 1:n) {
        if (length(intersect(dataSet[[i]], set)) == m) {
            count = count + 1
        }
    }

    return(count/n)
}

##################################################
# main2: look for the association rules
##################################################
## (frequent itemsets, support degree, minConfidence) 
##       -> (association rules, confidence degree)
## (list: str, vector: numeric, numeric) -> (list: str & numeric)
apriori_ar <- function(frequentSet, supportDegree, minC=0.6) {
	n <- length(frequentSet)
	
	# look for the frequentSet which have more than two elements
	for (i in 1:n) {
		if (length(frequentSet[[i]]) > 1) {
			first <- i
			break
		}
	}
	
	# look for association rules from frequent itemsets
	ass_rules <- list()
	for (i in first:n) {
		ass_rules[[length(ass_rules)+1]] <- createRules(frequentSet[[i]], frequentSet, supportDegree, minC)
	}
	
	return(list(association_rules=ass_rules))
	
}

# look for association rules from frequent itemsets
# (vector: character, list: str, vector: numeric, numeric)
#        -> (list: str & str & vector)
# return: association rules former & association rules latter & confidence degree
createRules <- function(fre_item, frequentSet, supportDegree, minC) {
	n <- length(fre_item)
	
	# create all the rules
	rules_latter <- list()
	rules_former <- list()
	confdegree <- c()
	for (i in 1:(n-1)) {
		rules_latter_tmp <- combn(fre_item, i)
		rules_former_tmp <- matrix(apply(rules_latter_tmp, 2, 
			function(item) setdiff(fre_item, item)), ncol=dim(rules_latter_tmp)[2])
		
		for (j in 1:dim(rules_former_tmp)[2]) {
			# calc the confidence degree one by one
			ConfDe <- calc_ConfDe(rules_former_tmp[,j], fre_item, frequentSet, supportDegree)
			# check if it is satisfied
			if (ConfDe >= minC) {
				rules_former[[length(rules_former)+1]] <- rules_former_tmp[,j]
				rules_latter[[length(rules_latter)+1]] <- rules_latter_tmp[,j]
				confdegree <- c(confdegree, ConfDe)
			}
		}
	}
	
	return(list(rules_former=rules_former, rules_latter=rules_latter, confdegree=confdegree))	
}

# calc the confidence degree one by one
# (vector: char, vector: character, list: str, vector: numeric)
#     -> (numeric)
calc_ConfDe <- function(rules_former, fre_item, frequentSet, supportDegree) {
	former_support <- supportDegree[look_sup(rules_former, frequentSet)]
	union_support <- supportDegree[look_sup(fre_item, frequentSet)]
	
	return(union_support/former_support)
}

# find the position where the set is exactly equal to the frequentSet
# (vector: character, list: str) -> (numeric)
look_sup <- function(set, frequentSet) {
	for (i in 1:length(frequentSet)) {
		if (setequal(set, frequentSet[[i]])) {
			position <- i
			break
		}
	}
	
	return(position)
}

## test
dataSet <- list(item1=c("A", "B"), item2=c("A", "C"),
    item3=c("A", "C"), item4=c("A", "B", "C"), item5=c("B", "C"))
	
result <- apriori_cfi(dataSet, minS=0.5)
result
apriori_ar(result$fre_sets, result$support_degree, 0.6)