# Q1 从 1:10 中用取后放回的方法依次任意抽取8个数， 计算事件A =｛1,2,3,4,5｝的频率。
A <- 1:5
samp <- sample(1:10,8,T)
freq_A = mean(samp %in% A)
print(freq_A)

# Q2 从 1:10 中用有放回的方法依次任意抽取8个数， 计算事件A =｛1,2,3,4,5｝的频率。
A <- 1:5
samp <- sample(1:10,8,F)
freq_A = mean(samp %in% A)
print(freq_A)

# Q3 分别将 Q1 和 Q2 重复100次，分别得到两个含有100个频率值的向量 x = {xi} 和 u = {ui}
x = c()
u = c()
A <- 1:5

for (i in 1:100){
  samp1 <- sample(1:10,8,T)
  samp2 <- sample(1:10,8,F)
  
  x <- c(x, mean(samp1 %in% A))
  u <- c(u, mean(samp2 %in% A))
}

y1 = rep(1,100)
y2 = rep(2,100)
plot(x, y1, 
     type = "p",
     col="red",       # 设置颜色 (col: color)
     ylim=c(0.9,2.1), # 设置y轴范围 (ylim: y's limitation)
     xlim=c(0,1))     # 设置x轴范围 (xlim: x's limitation)
points(u, y2, 
       col="blue")

