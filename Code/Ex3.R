#Thêm các package cần thiết
library(mvtnorm)
library(ggplot2)

# Tìm mật độ xác suất tiên nghiệm tại điểm x
# Tham số đầu vào:
#  - x: điểm cần tính xác suất tiên nghiệm

pi_Norm <- function(x) {
  mean <- c(4, 5)
  cov <- matrix(c(1.1, 0.9, 0.9, 1.3), nrow = 2)
  return(dmvnorm(x = x, mean = mean, sigma = cov))
}

# Tìm mật độ xác suất bất kỳ tại điểm x' khi biết vị trí x: p(x*|x)
# Tham số đầu vào:
#  - x_next: điểm dự định
#  - x_current: điểm hiện tại

p_Norm <- function(x_next, x_current) {
  cov <- matrix(c(3.0,0.0,0.0,3.0), nrow = 2)
  return(dmvnorm(x = x_next, mean = x_current, sigma = cov))
}

# Lấy 1 điểm từ phân bố bất kỳ p(x)
# Tham số đầu vào:
#  - x_current: điểm hiện tại

sample_from_p <- function(x_current) {
  cov <- matrix(c(3.0,0.0,0.0,3.0), nrow = 2)
  x_next <- rmvnorm(n = 1, mean = x_current, sigma = cov)
  return(x_next)
}

# Thuật toán Metropolis-Hastings
# Tham số đầu vào:
#  - pi: hàm phân bố xác suất tiên nghiệm
#  - p: hàm phân bố xác suất có dạng phân phối Chuẩn 
#  - f: hàm lấy X bất kỳ từ phân bố xác suất bất kỳ p
#  - N: số lượng mẫu cần lấy
#  - init: điểm xuất phát cho thuật toán

MH <- function(pi, p, f, N, init){
  # Khởi tạo mẫu trả về
  samples <- data.frame(matrix(init, nrow = 1, byrow = TRUE))
  for(i in 1:(N-1)){
    # Lấy điểm hiện tại
    x_current <- as.numeric((tail(samples, n = 1)))
    names(x_current) <- colnames(samples)
    
    # Lấy điểm kế tiếp từ phân phối xác suất bất kỳ p
    x_next <- f(x_current)
    
    # Tính hệ số r
    alpha = (pi(x_next)*p(x_current,x_next)) / (pi(x_current)*p(x_next,x_current))
    r <- min(1, alpha)
    
    # Khởi tạo giá trị q từ phân phối đều liên tục U(0,1)
    q <- runif(1, 0.0, 1.0)
    
    # Xác định điểm kê tiếp
    if (q < r){
      names(x_next) <- colnames(samples)
      samples <- rbind(samples, x_next)
    }
    else {
      names(x_current) <- colnames(samples)
      samples <- rbind(samples, x_current)
    }
  }
  return(samples)
}

# Điểm khởi tạo
init <- c(3.1, 4.2)
# Số lượng điểm
n <- 10000
samples <- MH(pi_Norm, p_Norm, sample_from_p, n, init)
# Vẽ
#ggplot(data = samples,aes(x = X1, y = X2)) + geom_point() + xlab(expression(beta)) + ylab(expression(gamma))
ggplot(data = samples,aes(x = X1, y = X2)) + stat_density2d(aes(fill = ..level..), geom = "polygon", h = 0.26) + scale_fill_gradient(low = "grey85", high = "grey35", guide = FALSE) + xlab(expression(beta)) + ylab(expression(gamma))
