##############				Jake Graham                             ############
##############                           DSP Lab 4                              ############

######  Source located at https://github.com/JakeGraham/Classes/tree/master/DSP/Lab4 #######
###   To reproduce, in terminal "Rscript GRAHAM_LAB_4.R; gnome-open GRAHAM_LAB_4.pdf    ####

############################################################################################

library(R.matlab)
library(signal)
 
### Name etc
plot(NULL,col = "white", xaxt = "n", yaxt = "n", ylab = NA, xlab = NA, bty = "n",xlim = c(0,10), ylim = c(0,10))
text(5, 6, "Jake Graham", cex = 2.5)
text(5, 5, "DSP Lab 4", cex = 2.5)

data <- readMat("Lab4.mat")
str(data)
Cd <- data$C[,1] # CO2 (dirty)
time <- data$ts[,1]


### Q1
plot(time, Cd, type = "l", main = "Q1", xlab = "Year", ylab = "CO2 (ppm)")
system("mv Rplots.pdf GRAHAM_LAB_4.pdf")
C <- runmed(Cd, 3)
lines(time, C, col = "red")
legend("topleft", c("Raw", "Filtered"), col = c("black", "red"), lwd = 2, bty = "n")


### Q2
imps <- seq(0, pi, length = 31)
imp <- sin(imps) / sum(sin(imps))
op2 <- conv(C, imp)
plot(time[16:(length(time) - 15)], op2[31:(length(op2) - 30)], type = "l", main = "Q2", xlab = "Year", ylab = "CO2 (ppm)")


### Q3
i3 <- c(imp[1:15], imp[17:31])
i3 <- c(-i3[1:15], sum(i3), -i3[16:30])
op3 <- conv(C, i3)
length(op3)
length(time)
plot(time[16:(length(time) - 15)], op3[31:(length(op3) - 30)], type = "l",  main = "Q3", xlab = "Year", ylab = "CO2 (ppm)")

