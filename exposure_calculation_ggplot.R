library(ggplot2)
library(ggplot2)
library(openxlsx)

# Load data 
# remember to change your own data
data <- read.xlsx("D:/R/Rworkspace/20240311/N data.xlsx")

# Function to calculate T
calculate_T <- function(N, P, rou, lambda, ero, Lambda) {
  A <- lambda + (rou * ero) / Lambda
  T <- (-log(1-(N*A)/P))/A
  return(T)
}

# Set parameters
# current example parameters for antarctica
# parameters from Lal,1991 Stone,2000
P <- 5.1
rou <- 2.7
lambda <- 0.510*(10^(-6)) 
Lambda <- 140

# Calculate T for different erosion rates
N_values <- seq(1, 1000000, length.out = 10000)
ero_values <- c(0, 0.001, 0.002, 0.0015)
T_0.001ero <- log10(calculate_T(N_values, P, rou, lambda, 0.001, Lambda))
T_0.002ero <- log10(calculate_T(N_values, P, rou, lambda, 0.002, Lambda))
# Assuming 'data' is your data frame with 'N', 'errors', and 'ID' columns,
# and you've already calculated 'T_zeroero', 'T_0.001ero', 'T_0.002ero', and 'T_treuero' as before.

# Create a new data frame for plotting
plot_data <- data.frame(
  T = c(log10(T_zeroero), log10(T_0.001ero), log10(T_0.002ero), log10(T_treuero)),
  N = rep(log10(N_values), times = 4),
  Erosion = factor(rep(c("zero", "0.001", "0.002", "true"), each = length(N_values)))
)

plot_data2 <- data.frame(
  T = c(T_0.001ero, T_0.002ero),
  N = rep(log10(N_values), times = 2),
  Erosion = factor(rep(c("0.001", "0.002"), each = length(N_values))),
  xmax = T_0.002ero,
  xmin = T_0.001ero
)

# Create a base ggplot object
p <- ggplot() +
  geom_line(data = subset(plot_data2, Erosion == "0.001"), aes(x = T, y = N, group = Erosion, color = Erosion), linetype = "solid") +
  geom_line(data = subset(plot_data2, Erosion == "0.002"), aes(x = T, y = N, group = Erosion, color = Erosion), linetype = "solid") +
  geom_ribbon(data = plot_data2, aes(y = N, xmin = xmin, xmax = 6, fill = Erosion), alpha = 0.2)  +

  geom_line(data = subset(plot_data, Erosion != "true"), aes(x = T, y = N, group = Erosion, color = Erosion), linetype = "solid",size=1) +
  geom_line(data = subset(plot_data, Erosion == "true"), aes(x = T, y = N, group = Erosion, color = Erosion), linetype = "dashed") +
  #geom_ribbon(data = subset(plot_data, Erosion %in% c("0.001", "0.002")), aes(x = T, ymin = 0, ymax = N, fill = "grey"), alpha = 0.5)  +
  geom_point(data = data, aes(x = log10(calculate_T(N, P, rou, lambda, ero, Lambda)), y = log10(N)), color = "black", size = 2,shape = 15) +
  theme_classic() +
  labs(x = "exposure age lg(T) ka", y = "10-Be concentration", color = "Erosion Rate") +
  scale_color_manual(values = c("zero" = "black", "0.001" = "black", "0.002" = "black", "true" = "black")) +
  #ggtitle("Diagram of different erosion rates") +
  theme(legend.position = "none",
  panel.border = element_rect(color = "black", fill = NA, size = 1),  # 设置图的框的颜色、填充和大小
  panel.background = element_rect(fill = "white"),  # 设置图的背景颜色
  plot.title = element_text(hjust = 0.5))+# Hide the legend if not needed
  geom_vline(xintercept = log10(c(90000, 80000, 70000,60000,50000,40000,30000,20000,10000)), color="grey", linetype="dashed")
  #geom_hline(yintercept = log10(c(900000, 800000, 700000,600000,500000,400000,300000,200000,100000)), color="grey", linetype="dashed")

# Add text labels for specific samples
label_data <- subset(data, ID %in% c("1122-3", "1210-1", "1121-1", "1130-1", "1122-5"))
p <- p + geom_text(data = label_data, aes(x = log10(calculate_T(N, P, rou, lambda, ero, Lambda)), y = log10(N), label = ID), hjust = 1.5, vjust = -2, color = "black")
#p <- p + geom_text(data = label_data, aes(x = log10(calculate_T(N, P, rou, lambda, ero, Lambda)), y = log10(N), label = ID), hjust = 3, vjust = -3, color = "black")

# Add error bars
# Assuming 'data$errors' contains the error values for the 'N' measurements
p <- p + geom_errorbar(data = data, aes(x = log10(calculate_T(N, P, rou, lambda, ero, Lambda)), ymin = log10(N - errors), ymax = log10(N + errors)), width = 0.01, color = "black")+
  coord_cartesian(xlim = c(4.1, 5.1), ylim = c(4.75, 5.75))
  #theme(#axis.title=element_blank())
p <- p + 
  scale_y_continuous(
    # 添加刻度ticks
    breaks = log10(c(900000, 800000, 700000, 600000, 500000, 400000, 300000, 200000, 100000)))+
  scale_x_continuous(
    # 添加刻度 ticks
    breaks = log10(c(90000, 80000, 70000, 60000, 50000, 40000, 30000, 20000, 10000)))
p<- p+theme(axis.title=element_blank())
# Now print the plot
print(p)
#you can use ggsave("exposure_calculation.tiff", width = 8, height = 6, dpi = 300) to save