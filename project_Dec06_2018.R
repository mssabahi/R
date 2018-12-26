rm(list=ls()) 
#library(yuima)
library(ISwR)
library(xts)
library(fredr)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics)
library(factoextra)
library(FactoMineR)
library(bbmle)
library(fitdistrplus)
library(yuimaGUI)
library(sde)
library(fitdistrplus)
library(devtools)




#getwd()
fredr_set_key("fff0cc91bd59def9371938c21a88169e")

cad_ls <- c('CAD_01','CAD_02','CAD_03','CAD_04','CAD_05','CAD_06','CAD_07','CAD_08',
            'CAD_09','CAD_10','CAD_11','CAD_12')

us_ls <- c('US_01','US_02','US_03','US_04','US_05','US_06','US_07','US_08',
            'US_09','US_10','US_11','US_12')

libor_USD_ser_id <- c('USD1MTD156N','USD2MTD156N','USD3MTD156N','USD4MTD156N','USD5MTD156N','USD6MTD156N','USD7MTD156N','USD8MTD156N','USD9MTD156N','USD10MD156N','USD11MD156N','USD12MD156N')
libor_CAD_ser_id <- c('CAD1MTD156N','CAD2MTD156N','CAD3MTD156N','CAD4MTD156N','CAD5MTD156N','CAD6MTD156N','CAD7MTD156N','CAD8MTD156N','CAD9MTD156N','CAD10MD156N','CAD11MD156N','CAD12MD156N')

tenor_USD_data <- list()
tenor_CAD_data <- list()


for (i in 1:12) 
{
  tenor_USD <- fredr(series_id = libor_USD_ser_id[i], observation_start = as.Date("2005-01-01"), observation_end =as.Date("2018-01-01"),
                     frequency = "d")
  tenor_CAD <- fredr(series_id = libor_CAD_ser_id[i], observation_start = as.Date("2005-01-01"), observation_end =as.Date("2018-01-01"),
                     frequency = "d")
  tenor_USD_data[[i]] <- tenor_USD # add it to your list
  tenor_CAD_data[[i]] <- tenor_CAD
}


cad_ten_raw_ls = list() # raw interest rate
cad_ten_raw_df <- c()

for (i in 1:length(tenor_CAD_data))
{
  #cad_ten_raw_ls[[i]]  <- assign(paste('xts_cad_',i,sep=''),xts(x=tenor_CAD_data[[i]]$value,
                                       # order.by=as.POSIXct(tenor_CAD_data[[i]]$date))) 
  cad_ten_raw_ls[[i]]  <- xts(x=tenor_CAD_data[[i]]$value,
                                        order.by=as.POSIXct(tenor_CAD_data[[i]]$date))
  
  colnames(cad_ten_raw_ls[[i]]) <- c('value')
  cad_ten_raw_df <- cbind(cad_ten_raw_df, cad_ten_raw_ls[[i]]) 
}
colnames(cad_ten_raw_df) <- cad_ls


############################ Saving the pictures

for (i in c(1,4,7,10))
{

file_name = paste("./Google Drive/Data Analysis/lantern/R/CAD_IR_", i, ".png", sep="")
png(file=file_name,width=600, height=475 )
print(plot(cad_ten_raw_df[,i:(i+2)], multi.panel = T, type="l"))
dev.off()
}


############################# 

# removing nan values and calculating 1 day and 30 day increment

for (i in 1:length(tenor_USD_data))
{
  tenor_USD_data[[i]] <- na.omit(tenor_USD_data[[i]])
  tenor_USD_data[[i]]$one_day_inc <- 0
  tenor_USD_data[[i]]$thirty_day_inc <- 0
  tenor_USD_data[[i]]$one_day_inc[1:(nrow(tenor_USD_data[[i]])-1)] <- (diff(tenor_USD_data[[i]]$value))
  tenor_USD_data[[i]]$thirty_day_inc[1:(nrow(tenor_USD_data[[i]])-30)] <- diff(tenor_USD_data[[i]]$value,30)
  
  tenor_CAD_data[[i]] <- na.omit(tenor_CAD_data[[i]])
  tenor_CAD_data[[i]]$one_day_inc <- 0
  tenor_CAD_data[[i]]$thirty_day_inc <- 0
  tenor_CAD_data[[i]]$one_day_inc[1:(nrow(tenor_CAD_data[[i]])-1)] <- (diff(tenor_CAD_data[[i]]$value))
  tenor_CAD_data[[i]]$thirty_day_inc[1:(nrow(tenor_CAD_data[[i]])-30)] <- diff(tenor_CAD_data[[i]]$value,30)

}

#####################################################
cad_ten_1D_ls = list() # 1Day increment interest rate
cad_ten_30D_ls = list()
cad_ten_1D_df <- c()
cad_ten_30D_df <- c()


for (i in 1:length(tenor_CAD_data))
{
  cad_ten_1D_ls[[i]]  <- xts(x=tenor_CAD_data[[i]]$one_day_inc,
                              order.by=as.POSIXct(tenor_CAD_data[[i]]$date))
  cad_ten_30D_ls[[i]]  <- xts(x=tenor_CAD_data[[i]]$thirty_day_inc,
                             order.by=as.POSIXct(tenor_CAD_data[[i]]$date))
  
  colnames(cad_ten_1D_ls[[i]]) <- c('value')
  colnames(cad_ten_30D_ls[[i]]) <- c('value')
  
  cad_ten_1D_df <- cbind(cad_ten_1D_df, cad_ten_1D_ls[[i]]) 
  cad_ten_30D_df <- cbind(cad_ten_30D_df, cad_ten_30D_ls[[i]]) 
  
}
colnames(cad_ten_1D_df) <- cad_ls
colnames(cad_ten_30D_df) <- cad_ls

for (i in c(1,4,7,10))
{
  file_name_1D = paste("./Google Drive/Data Analysis/lantern/R/CAD_1D_IR_", i, ".png", sep="")
  png(file=file_name_1D,width=600, height=475 )
  print(plot(cad_ten_1D_df[,i:(i+2)], multi.panel = T, type="l"))
  dev.off()
}

for (i in c(1,4,7,10))
{
  file_name_30D = paste("./Google Drive/Data Analysis/lantern/R/CAD_30D_IR_", i, ".png", sep="")
  png(file=file_name_30D,width=600, height=475 )
  print(plot(cad_ten_30D_df[,i:(i+2)], multi.panel = T, type="l"))
  dev.off()
}

### plot the histogram for 30 day incement
file_name_30D_hist = paste("./Google Drive/Data Analysis/lantern/R/CAD_30D_IR_Hist.png", sep="")

png(file_name_30D_hist,width=600, height=475, res = 100)

par(mfrow = c(3, 4))  # Set up a 3 x 4 plotting space

# Create the loop.vector (all the columns)
loop.vector <- 1:12

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- tenor_CAD_data[[i]]$thirty_day_inc
  hist(x, main = paste("CAD", i),xlab = "30 Day Increment")
       #xlim = c(0, 100))
}
dev.off()
###################################################
### plot the histogram for 1 day incement

file_name_1D_hist = paste("./Google Drive/Data Analysis/lantern/R/CAD_1D_IR_Hist.png", sep="")

png(file_name_1D_hist,width=600, height=475, res = 100)

par(mfrow = c(3, 4))  # Set up a 3 x 4 plotting space

loop.vector <- 1:12

for (i in loop.vector) { # Loop over loop.vector
  
  x <- tenor_CAD_data[[i]]$one_day_inc
  hist(x, main = paste("CAD", i),xlab = "1 Day Increment")
  #xlim = c(0, 100))
}
dev.off()

############################# Average and standard deviation
cad_ten_cl_ls = list() # raw interest rate clean data
cad_ten_cl_df <- c()

for (i in 1:length(tenor_CAD_data))
{
  #cad_ten_raw_ls[[i]]  <- assign(paste('xts_cad_',i,sep=''),xts(x=tenor_CAD_data[[i]]$value,
  # order.by=as.POSIXct(tenor_CAD_data[[i]]$date))) 
  cad_ten_cl_ls[[i]]  <- xts(x=tenor_CAD_data[[i]]$value,
                              order.by=as.POSIXct(tenor_CAD_data[[i]]$date))
  
  colnames(cad_ten_cl_ls[[i]]) <- c('value')
  cad_ten_cl_df <- cbind(cad_ten_cl_df, cad_ten_cl_ls[[i]]) 
}
colnames(cad_ten_cl_df) <- cad_ls

mul_ls = c(1,-1,2,-2)
cad_ten_sd <- apply(cad_ten_cl_df, 2,sd)
cad_ten_mean <- apply(cad_ten_cl_df, 2,mean)
cad_ten_stat = cad_ten_mean
for (i in mul_ls)
{
  cad_ten_stat = rbind(cad_ten_stat, (cad_ten_mean+i*cad_ten_sd)) 
}
rownames(cad_ten_stat) = c('mean','mean+sd','mean-sd','mean+2sd','mean-2sd')

file_name_ten_stat = paste("./Google Drive/Data Analysis/lantern/R/CAD_tenor_stat.png", sep="")
png(file_name_ten_stat,width=600, height=475,res=100)
matplot(1:12, t(cad_ten_stat), type='l', xlab='Tenors', ylab='IR')
#legend("bottomleft",legend=c('mean','mean+sd','mean-sd','mean+2sd','mean-2sd'))
dev.off()
#############################################################
## Corrolation matrix
# 1D increment

M <- cor(cad_ten_1D_df)
file_name_cad_cormat = paste("./Google Drive/Data Analysis/lantern/R/CAD_ten.cormat.png", sep="")
png(file_name_cad_cormat,width=600, height=475, res = 100)
corrplot(M, method = "number")
dev.off()


file_name_cad_cormat2 = paste("./Google Drive/Data Analysis/lantern/R/CAD_ten.cormat2.png", sep="")
png(file_name_cad_cormat2,width=600, height=475, res = 100)
corrplot(M, method = "pie")
dev.off()


# 30Day increment
M30D <- cor(cad_ten_30D_df)
file_name_cad_cormat = paste("./Google Drive/Data Analysis/lantern/R/CAD_ten_30D.cormat.png", sep="")
png(file_name_cad_cormat,width=600, height=475, res = 100)
corrplot(M30D, method = "number")
dev.off()


file_name_cad_cormat2 = paste("./Google Drive/Data Analysis/lantern/R/CAD_ten_30D.cormat2.png", sep="")
png(file_name_cad_cormat2,width=600, height=475, res = 100)
corrplot(M30D, method = "pie")
dev.off()


################################## PCA of Tenors
cad_ten_pca <- PCA(cad_ten_cl_df, scale.unit = TRUE, ncp = 12, graph = TRUE)
eig.val <- get_eigenvalue(cad_ten_pca)
file_name_pca_ten = paste("./Google Drive/Data Analysis/lantern/R/PCA_ten.png", sep="")
png(file_name_pca_ten,width=600, height=475, res = 100)
fviz_eig(cad_ten_pca, xlab='Dimensions',ylab='Explained Variances %', ylim = c(0, 100), title='IR')
dev.off()

### PCA of increments
# 1D
cad_ten_1D_pca <- PCA(cad_ten_1D_df, scale.unit = TRUE, ncp = 12, graph = TRUE)
eig.val <- get_eigenvalue(cad_ten_1D_pca)
file_name_pca_ten = paste("./Google Drive/Data Analysis/lantern/R/PCA_ten_1D.png", sep="")
png(file_name_pca_ten,width=600, height=475, res = 100)
fviz_eig(cad_ten_1D_pca, xlab='Dimensions',ylab='Explained Variances %', ylim = c(0, 100),title='IR-1D Increment')
dev.off()

# 30D
cad_ten_30D_pca <- PCA(cad_ten_30D_df, scale.unit = TRUE, ncp = 12, graph = TRUE)
eig.val <- get_eigenvalue(cad_ten_30D_pca)
file_name_pca_ten = paste("./Google Drive/Data Analysis/lantern/R/PCA_ten_30D.png", sep="")
png(file_name_pca_ten,width=600, height=475, res = 100)
fviz_eig(cad_ten_30D_pca, xlab='Dimensions',ylab='Explained Variances %', ylim = c(0, 100),title='IR-30D Increment')
dev.off()


###################Scaling Factor

cad_ten_1D_sd <- apply(cad_ten_1D_df, 2,sd)
cad_ten_30D_sd <- apply(cad_ten_30D_df, 2,sd)
cad_ten_sds <- rbind(cad_ten_30D_sd,cad_ten_1D_sd,cad_ten_30D_sd/cad_ten_1D_sd )

sqrt(30)*cad_ten_1D_sd/cad_ten_30D_sd
rownames(cad_ten_sds)=c('cad_ten_30D_sd','cad_ten_1D_sd','cad_ten_30D_sd/cad_ten_1D_sd')

cad_ten_cl_30df <- cad_ten_cl_df[seq(1,(nrow(cad_ten_cl_df)),29),] # nrow is 1/29 times the origina matrix

cad_ten_cl_df_sd <- apply(cad_ten_cl_df, 2,sd)
cad_ten_cl_30df_sd <- apply(cad_ten_cl_30df, 2,sd)

sqrt(30)*cad_ten_cl_df_sd/cad_ten_cl_30df_sd
cad_ten_cl_df_sd/cad_ten_cl_30df_sd




###################################  several distribution fits
# 30D incerement
file_name_30D_normal_hist = paste("./Google Drive/Data Analysis/lantern/R/CAD_30D_IR_Normal_Hist.png", sep="")

png(file_name_30D_normal_hist,width=600, height=475, res = 100)

par(mfrow = c(3, 4))  # Set up a 3 x 4 plotting space

loop.vector <- 1:12

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- cad_ten_30D_df[,i]
  hist(x, main = paste("CAD", i),freq = F,xlab = "30 Day Increment")
  mu = mean(x)
  sigma = sd(x)
  yrange = seq(min(x), max(x), 0.01)
  points(yrange,dnorm(yrange, mu, sigma), type="l", col="red", lwd=2)
  #xlim = c(0, 100))
}
dev.off()

# 1D incerement

file_name_1D_normal_hist = paste("./Google Drive/Data Analysis/lantern/R/CAD_1D_IR_Normal_Hist.png", sep="")

png(file_name_1D_normal_hist,width=600, height=475, res = 100)

par(mfrow = c(3, 4))  # Set up a 3 x 4 plotting space

loop.vector <- 1:12

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- cad_ten_1D_df[,i]
  hist(x, main = paste("CAD", i),freq = F,xlab = "1 Day Increment")
  mu = mean(x)
  sigma = sd(x)
  yrange = seq(min(x), max(x), 0.01)
  points(yrange,dnorm(yrange, mu, sigma), type="l", col="red", lwd=2)
  #xlim = c(0, 100))
}
dev.off()





# Cullen and Frey (Kurtosis vs square of skewness) graph
# 1D increment
for (i in 1:length(tenor_CAD_data))
{
  file_name_cad_cullen = paste("./Google Drive/Data Analysis/lantern/R/CAD_ten_1D.cullen_", i, ".png", sep="")
  png(file_name_cad_cullen,width=600, height=475, res = 100)
  
  descdist(as.vector(cad_ten_1D_df[,i]), boot = 1000)
  legend(legend = paste("CAD_1D_",i), "bottomright", lty=c(1), lwd=c(2), col=c("red"))
  usr1 = par('usr')
  dev.off()
  
}

# 30D increment
for (i in 1:length(tenor_CAD_data))
{
  file_name_cad_cullen = paste("./Google Drive/Data Analysis/lantern/R/CAD_ten_30D.cullen_", i, ".png", sep="")
  png(file_name_cad_cullen,width=600, height=475, res = 100)
  
  descdist(as.vector(cad_ten_30D_df[,i]), boot = 1000)
  legend(legend = paste("CAD_30D_",i), "bottomright", lty=c(1), lwd=c(2), col=c("red"))
  usr1 = par('usr')
  dev.off()
  
}

#################Vasicek Model

vs_model = function(cad) {
  n = length(cad)
  delta = 1  # delta 
  Sx = sum(cad[1:n - 1])
  Sy = sum(cad[2:n])
  Sxx = sum((cad[1:n - 1])^2)
  Syy = sum((cad[2:n])^2)
  Sxy = sum((cad[1:n - 1]) * (cad[2:n]))
  mu = (Sy * Sxx - Sx * Sxy)/((n - 1) * (Sxx - Sxy) - (Sx^2 - Sx * Sy))
  theta = -log((Sxy - mu * Sx - mu * Sy + (n - 1) * mu^2)/(Sxx - 2 * mu * 
                                                             Sx + (n - 1) * mu^2))/delta
  a = exp(-theta * delta)
  sigmah2 = (Syy - 2 * a * Sxy + a^2 * Sxx - 2 * mu * (1 - a) * (Sy - a * 
                                                                   Sx) + (n - 1) * mu^2 * (1 - a)^2)/(n - 1)
  sigma = sqrt((sigmah2) * 2 * theta/(1 - a^2))
  theta = list(theta = theta, mu = mu, sigma = sigma, sigmah2 = sigmah2)
  return(theta)
}

cad = as.vector(cad_ten_cl_df[1:100,1])
usd = as.
vs_model(cad)

#par(mfrow = c(2, 2))  # Set up a 3 x 4 plotting space

file_name_cad_vas = paste("./Google Drive/Data Analysis/lantern/R/CAD_ten_vas.png")
png(file_name_cad_vas,width=600, height=475, res = 100)

plot(cad, type = "l", lwd = 2.5,xlim=c(1,100), ylim = c(2.56, 2.63), xlab = "Date", ylab="IR", 
     col = 1)
dev.off()

grid = setSampling(Terminal = 1, n = 1000)
m1 = setModel(drift = "theta*(mu-x)", diffusion = "sigma", state.var = "x", 
              time.var = "t", solve.var = "x", xinit = vs_model(cad)[[2]])
Xcad = simulate(m1, true.param = list(mu = vs_model(cad)[[2]], sigma = vs_model(cad)[[3]], 
                                     theta = vs_model(cad)[[1]]), sampling = grid)
plot(Xcad)



simnum = 2000
#dist = c(0.31, 0.52, 0.6, 0.7, 0.95)
newsim = function(i) {
  simulate(m1, true.param = list(mu = vs_model(cad)[[2]], sigma = vs_model(cad)[[3]], 
                                 theta = vs_model(cad)[[1]]))@data@original.data
}
# newsim(1) simulation 1000 times, each time there are 100 time periods
file_name_cad_vas = paste("./Google Drive/Data Analysis/lantern/R/CAD_ten_vas_sim.png")
png(file_name_cad_vas_sim,width=600, height=475, res = 100)
sim = sapply(1:simnum, function(x) newsim(x))
# transfor to time seires format.
m2 = t(sim)
mcad <- apply(m2, 2, mean)

# plot the mean of the 1000 time simulation for the 100 time periods

plot(mcad, type = "l",ylab='Simulated IR', xlab ='Days' )

dev.off()




