library(ggplot2)

# 定义计算函数1 formula nuclide 1
calculate_Rij <- function(Nj, Pi, Pj, lambda_i, lambda_j) {
  Rij <- (Pi/Nj) * (1/((Pj/Nj) + lambda_i - lambda_j))
  return(Rij)
}

# 定义计算函数2 formula for nuclide 2
calculate_Rij2 <- function(Nj, Pi, Pj, lambda_i, lambda_j) {
  Rij2 <- (Pi/(Nj*lambda_i)) * (1 - ((1 - Nj*lambda_j/Pj)^(lambda_i/lambda_j)))
  return(Rij2)
}

# 生成Nj的序列
Nj_values <- seq(1, 10000000, length.out = 10000)

# 设置参数 (以Be Al 为例)
# example of Be Al
#j=Be i=Al
Pi <- 31.1
Pj <- 4.6
lambda_i <- 0.967 * (10^(-6))
lambda_j <- 0.510 * (10^(-6))

# 计算Rij和Rij2的值
#calculating Rij,Rij2
Rij_values <- calculate_Rij(Nj_values, Pi, Pj, lambda_i, lambda_j)
Rij2_values <- calculate_Rij2(Nj_values, Pi, Pj, lambda_i, lambda_j)

# 创建数据框架 dataframe
data <- data.frame(Nj = log10(Nj_values), Rij = Rij_values, Rij2 = Rij2_values)
# annotations for your points
annotate_points <- data.frame(
 x = log10(c(248811.9211, 187374.8254)),
 y = c(4.95713714, 6.049948504)
)
Ale = c(7.96E+04,6.90E+04)
Bee = c(5.57E+03,5.27E+03)
# 准备绘制误差条的数据
error_data <- data.frame(
  # 纵向误差条的上下界，这里不用log10，因为y轴已经是线性的
  Nj = log10(Nj_values),
  x = annotate_points$x,
  y = annotate_points$y,
  ymin = c(4.54E+00,5.53E+00),
  ymax = c(5.40E+00,6.60E+00),
  # 横向误差条的左右界
  xmin = log10(c(248811.9211 - 7.96E+04, 187374.8254 - 6.90E+04)),
  xmax = log10(c(248811.9211 + 7.96E+04, 187374.8254 + 6.90E+04))
)

# 绘制图像
p <- ggplot(data, aes(x = Nj)) +
  geom_line(aes(y = Rij), color = "black") +
  geom_line(aes(y = Rij2), color = "black") +
  geom_ribbon(aes(ymin = Rij, ymax = Rij2), fill = "grey", alpha = 0.5) +
  # 添加点
  geom_point(aes(x = x, y = y), data = annotate_points, color = "black", shape = 15,cex=2) +
  # 添加纵向误差条
 geom_errorbar(aes(x = x, ymin = ymin, ymax = ymax), data = error_data, width = 0.05, color = "black",linetype="dashed") +
  # 添加横向误差条
 geom_errorbarh(aes(y = y, xmin = xmin, xmax = xmax), data = error_data, height = 0.1, color = "black",linetype="dashed") +
  scale_x_continuous(limits = c(4, 7.1)) +
  scale_y_continuous(name = "26-Al/10-Be") +
  ggtitle("Steady State Erosion Island Diagram") +
  theme_bw()

  

print(p)
#ggsave("E:/work/南极做图/20240319/erosion island.tiff", width = 8, height = 6, dpi = 300)