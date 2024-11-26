rm(list=ls())

# Q1 对于 p=0,0.1,...,0.9,1 绘制 B(20,p) 的密度图像，
## 考察随着成功概率由小到大的变化，密度图像的变化特征
x <- 0:20             # B(20,p) 所有可能取值：0,1,...,20
par(mfrow = c(4, 3))  # 定义多张图片的排列方式：4行3列（因为p=0,0.1,...,1一共有11种取值

# 使用for循环绘制11张图像
for (p in seq(0,1, by=0.1)){
  y <- dbinom(x,20,p) # 计算B(20,p) 所有的密度
  plot(x, y,
       type="h",  # 选择绘图的类型
       lwd=2,     # 设置线的粗细: lwd: line's width
       xlab = "ξ",        # 设置x轴的标签: xlab: x's label
       ylab = "density",  # 设置y轴的标签
       xlim = c(0,20),    # 设置x轴的范围: xlim: x's limitations
       ylim = c(0,1),     # 设置y轴的范围
       main=paste("ξ~B(20,",p,")",sep=""))  # 设置图像的题目
}


# Q2 已知 X~B(4,1/6)，模拟X的m=10次的观测值，用f_i 表示m次观测中 {X=i} 的频率，
## 绘制密度矩阵的密度图像
rm(list=ls())

all_df = data.frame("x" = 0:4)
par(mfrow=c(2,2))
for (i in 1:4){
  m <- 10^i
  x <- rbinom(m, 4, 1/6)
  
  tmp_freq = c()
  k <- 0:4
  for (j in k){
    tmp_freq <- c(tmp_freq, sum(x == j)/m)
  }
  
  plot(k, tmp_freq,
       type="h",
       lwd=2,
       xlab="x",
       ylab="frequence",
       xlim=c(0,4),
       ylim=c(0,0.7),
       main=paste("m=",m,sep="")
  )
  points(k, dbinom(k,4,1/6), col="red")
}


# Q3 袋中有10个红球和10个黑球。从袋中取后放回的方法依次任取9个球，用X表示取出红球的个数；
## 从袋中用取后不放回的方法依次任取9个球，用 Y 表示取出红球的个数，将X和Y的分布函数曲线用不同
## 颜色绘制在同一图中（绘制坐标位于区间[0,9.5]内），解释 X和Y的分布函数为什么不同。

rm(list=ls())
par(mfrow=c(1,1))
k <- 0:9
p_X <- pbinom(k, 9, 0.5)
p_Y <- phyper(k, 10, 10, 9)


plot(c(0,1),c(p_X[1],p_X[1]),
     type="l",
     lwd=.5,
     col="blue",
     xlim=c(0,9.5),
     ylim=c(0,1),
     xlab="x",
     ylab="p")
lines(c(0,1), c(p_Y[1], p_Y[1]),
     type="l",
     lwd=.5,
     col="red",
      )
points(1, p_X[1],
       col="blue",
       lwd=.5)

points(1, p_Y[1],
       col="red",
       lwd=.5)

for (i in 2:10){
  lines(c(i-1,i), c(p_X[i], p_X[i]),
        type = "l",
        lwd = .5,
        col = "blue")
  lines(c(i-1,i), c(p_Y[i], p_Y[i]),
        type = "l",
        lwd = .5,
        col = "red")
  points(i, p_X[i],
         col="blue",
         lwd=.5)
  
  points(i, p_Y[i],
         col="red",
         lwd=.5)
}

legend(6, 0.1, legend=c("X~B(9,0.5)", "Y~H(20,10,9)"),
       col=c("blue", "red"), lty=c(1,1), cex=0.6)

## 对于 X（有放回抽样），红球每次被取出的概率固定，因此分布函数较为平滑，概率集中在 X = 4 附近。
## 对于 Y（无放回抽样），取出的红球数量影响后续抽样的概率，因此分布更加集中，分布曲线变化较为陡峭。