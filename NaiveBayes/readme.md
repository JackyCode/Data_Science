### 朴素贝叶斯分类
***

在R中自定义一个函数，实现朴素贝叶斯分类。

***

有关Naive Bayes，可以参见我的博客：[数据科学之机器学习6: 分类之朴素贝叶斯](http://jackycode.github.io/blog/2014/04/08/naive-bayes/)

***

### 函数用法
`NaBa(x, data, lable, ln=FALSE, default=FALSE)`

其中，`x`是需要分类的输入样本；`data`是测试样本；`lable`是测试样本对应的类别标签；`ln`是指是否需要进行取对数修正；`default`指是否让基数初值从1开始。

样例见`NaiveBayes.R`中的测试。