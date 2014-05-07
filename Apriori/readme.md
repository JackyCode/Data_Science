### Apriori算法
***

`apriori.R`: 在R中自定义函数实现Apriori算法

如何使用R中自带的函数处理Apriori算法，见我的博客[数据科学之机器学习14: 关联分析之apriori算法](http://jackycode.github.io/blog/2014/05/07/apriori/)

***

有关Apriori算法，可以参见我的博客：[数据科学之机器学习14: 关联分析之apriori算法](http://jackycode.github.io/blog/2014/05/07/apriori/)

***

### 自定义函数用法

##### 1. 寻找频繁项集

``` r
result <- apriori_cfi(dataSet, minS=0.5)
```

`dataSet`为数据集；`minS`为设定的最小支持度，默认为0.5。

##### 2. 寻找关联规则

``` r
apriori_ar(result$fre_sets, result$support_degree, minC)
```

`result$fre_sets`和`result$support_degree`为函数`apriori_cfi`的输出，即是频繁项集和对应的支持度；`minC`是设定的最小可信度，默认为0.6。

例子见我的博客，地址同上。