library(R.matlab)

data <- readMat("Lab3.mat")
sig <- ((data$data * as.numeric(data$hdr[10])) / 3200) * 10E5
time <- (1:length(sig))/as.numeric(data$hdr[9])

data$hdr


### Q1`
plot(time, sig - mean(sig), type = "l", xlab = "Time (s)")

### Q2
dt <- 1 / as.numeric(data$hdr[9])
t2 <- time[2:length(time)] - (dt / 2)
acc <- rep(NA,length(sig)-1)
for (i in 2:length(sig)){
	acc[i-1] <- (sig[i] - sig[i-1]) / dt
}
plot(t2, acc, type = "l")

