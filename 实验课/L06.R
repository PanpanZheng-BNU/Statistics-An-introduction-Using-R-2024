m <- 10000
n <- 1:m
barX <- n
myCol <- c("blue", "green", "yellow", "grey", "pink", "black") # 设置线的颜色
for (i in 1:6){
  x <- runif(m) # 模拟m个U(0,1)的随机数
  for (j in 1:m){
    barX[j] <- mean(x[1:j]) # 计算前j次模拟的随机数的均值
  }
  if (i == 1) {
    plot(n, barX, col=myCol[i], type="l", lwd=1.5, ylim=c(0,1), ylab="mean")
  } else{
    lines(n, barX, col=myCol[i], lwd=1.5)
  }
  if (i == 6){
    abline(h=0.5, col="red", lwd=2)
  }
}
