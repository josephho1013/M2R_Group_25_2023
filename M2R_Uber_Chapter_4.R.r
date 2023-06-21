rMin=0;rMax=1
dMin=0;dMax=2*pi;
rDelta=rMax-rMin;dDelta=dMax-dMin

#Point process parameters
lambda=500; #intensity (ie mean density) of the Poisson process

#Simulate Poisson point process
custnumbPoints=rpois(1,lambda);#Poisson number of points
custrr=rDelta*runif(custnumbPoints)+rMin;#x coordinates of Poisson points
orderedcustrr = sort(custrr, decreasing = FALSE)
custdd=dDelta*runif(custnumbPoints)+dMin;#y coordinates of Poisson points
custxx=orderedcustrr*cos(custdd)
custyy=orderedcustrr*sin(custdd)

rMin=0;rMax=1
dMin=0;dMax=2*pi;
rDelta=rMax-rMin;dDelta=dMax-dMin

#Point process parameters
lambda=700; #intensity (ie mean density) of the Poisson process

#Simulate Poisson point process
carnumbPoints=rpois(1,lambda);#Poisson number of points
carrr=rDelta*runif(carnumbPoints)+rMin;#x coordinates of Poisson points
orderedcarrr = sort(carrr, decreasing = FALSE)
cardd=dDelta*runif(carnumbPoints)+dMin;#y coordinates of Poisson points
carxx=orderedcarrr*cos(cardd)
caryy=orderedcarrr*sin(cardd)

#Plotting
plot(custxx,custyy,'p',xlab='x',ylab='y',col='blue')
points(carxx,caryy, col='red',pch="*")
legend("topleft", inset=.05, 
       c("cars","customers"), fill=c('red', 'blue'))

#algorithm1
xx2 <- carxx
yy2 <- caryy

mini <- min(c(length(custxx), length(carxx)))
match <- c()
time <- c()
totaldist <- 0
for (i in 1:mini){
  xdist <- (custxx[i] - xx2) ^ 2
  ydist <- (custyy[i] - yy2) ^ 2
  distance <- sqrt(xdist + ydist)
  totaldist <- totaldist + min(distance)
  index <- which.min(distance)
  time <- append(time, min(distance))
  xx2[index] = 20
  yy2[index] = 20
  match <- append(match, index)
}
print(match)
print(time)
plot(time, ylab='waiting time', ylim = range(c(0, 0.5)))
avedist <- sum(totaldist)/mini
print(avedist)
sortedtime <- sort(time)
library(pracma)
timelimspace <- linspace(0, 0.2, n = 1000)
prop <- c()
for (i in 1:1000){
  prop[i] = sum(time < timelimspace[i])/mini
}
plot(timelimspace, prop, type = "l", lty = 1)
lines(timelimspace, prop, type = "l", lty = 1)

cust <- c()
for (j in 1:mini){
  cust[2*j-1] = custxx[j]
  cust[2*j] = custyy[j]
}

# First set of points
set1 <- matrix(cust, ncol = 2, byrow = TRUE)

car <- c()
for (j in 1:mini){
  car[2*j-1] = carxx[match[j]]
  car[2*j] = caryy[match[j]]
}

# Second set of points
set2 <- matrix(car, ncol = 2, byrow = TRUE)

# Create a new plot
plot(set1, type = "n", xlim = range(c(set1[, 1], set2[, 1])), ylim = range(c(set1[, 2], set2[, 2])),
     xlab = "X-axis", ylab = "Y-axis")

# Plot the points from set1 as blue dots
points(set1, col = "blue", pch = 16)

# Plot the points from set2 as red dots
points(set2, col = "red", pch = 16)
points(carxx,caryy, col='red',pch="*")

# Connect the points from both sets with lines
for (i in 1:nrow(set1)) {
  lines(c(set1[i, 1], set2[i, 1]), c(set1[i, 2], set2[i, 2]), col = "black")
}


#algorithm2
xx4 <- carxx
yy4 <- caryy
xx5 <- rev(custxx)
yy5 <- rev(custyy)

mini <- min(c(length(custxx), length(carxx)))
match3 <- c()
time3 <- c()
totaldist3 <- 0
for (i in 1:mini){
  xdist <- (xx5[i] - xx4) ^ 2
  ydist <- (yy5[i] - yy4) ^ 2
  distance <- sqrt(xdist + ydist)
  totaldist3 <- totaldist3 + min(distance)
  index <- which.min(distance)
  time3 <- append(time3, min(distance))
  xx4[index] = 20
  yy4[index] = 20
  match3 <- append(match3, index)
}
print(match3)
print(time3)
plot(time3, ylab='waiting time', ylim = range(c(0, 0.5)))
avedist3 <- sum(totaldist3)/mini
print(avedist3)
sortedtime <- sort(time3)
timelimspace <- linspace(0, 0.2, n = 1000)
prop <- c()
for (i in 1:1000){
  prop[i] = sum(time < timelimspace[i])/mini
}
prop3 <- c()
for (i in 1:1000){
  prop3[i] = sum(time3 < timelimspace[i])/mini
}
plot(timelimspace, prop, xlab="waiting time", ylab="cdf", type = "l", lty = 1)
lines(timelimspace, prop, col = 'red', type = "l", lty = 1)
lines(timelimspace, prop3, col = 'blue', type = "l", lty = 1)
legend("topleft", inset=.05, 
       c("algorithm 1","algorithm 2"), fill=c('red', 'blue'))


car <- c()
for (j in 1:mini){
  car[2*(mini+1-j)-1] = carxx[match3[j]]
  car[2*(mini+1-j)] = caryy[match3[j]]
}
set3 <- matrix(car, ncol = 2, byrow = TRUE)
plot(set1, type = "n", xlim = range(c(set1[, 1], set3[, 1])), ylim = range(c(set1[, 2], set3[, 2])),
     xlab = "X-axis", ylab = "Y-axis")
points(set1, col = "blue", pch = 16)
points(set3, col = "red", pch = 16)
points(carxx,caryy, col='red',pch="*")
for (i in 1:nrow(set1)) {
  lines(c(set1[i, 1], set3[i, 1]), c(set1[i, 2], set3[i, 2]), col = "black")
}

#binary quantile regression plot
zeros <- c(1:mini)*0
ones <- c(1:mini)^0
plot(zeros, time, xlim = range(c(0, 1)))
points(ones, time3)


Di <- append(zeros, ones)
times <- append(time, time3)
data1 <- data.frame(Di, times)
library(quantreg)
library(ggplot2)
ggplot(data1, aes(Di, times))+
  geom_point()+
  xlim(-0.2, 1.2)+
  geom_smooth(method = lm, se = F, color = "red")+
  geom_quantile(color = "blue", quantiles = 0.5)+
  geom_quantile(color = "black", alpha = 0.3, 
                quantiles = seq(.05, .95, by = 0.05))


#simulate 500 times
totalporp1 <- 0
totalporp2 <- 0
totaltime1 <- 0
totaltime2 <- 0
maxtime1 <- 0
maxtime2 <- 0
simnum <- 500
for (y in 1:simnum){
  rMin=0;rMax=1
  dMin=0;dMax=2*pi;
  rDelta=rMax-rMin;dDelta=dMax-dMin
  
  
  lambda=500;
  
  custnumbPoints=rpois(1,lambda);
  custrr=rDelta*runif(custnumbPoints)+rMin;
  orderedcustrr = sort(custrr, decreasing = FALSE)
  custdd=dDelta*runif(custnumbPoints)+dMin;
  custxx=orderedcustrr*cos(custdd)
  custyy=orderedcustrr*sin(custdd)
  
  rMin=0;rMax=1
  dMin=0;dMax=2*pi;
  rDelta=rMax-rMin;dDelta=dMax-dMin
  
  
  lambda=700;
  
  
  carnumbPoints=rpois(1,lambda);
  carrr=rDelta*runif(carnumbPoints)+rMin;
  orderedcarrr = sort(carrr, decreasing = FALSE)
  cardd=dDelta*runif(carnumbPoints)+dMin;
  carxx=orderedcarrr*cos(cardd)
  caryy=orderedcarrr*sin(cardd)
  
  xx2 <- carxx
  yy2 <- caryy
  mini <- min(c(length(custxx), length(carxx)))
  match <- c()
  time <- c()
  totaldist <- 0
  for (i in 1:mini){
    xdist <- (custxx[i] - xx2) ^ 2
    ydist <- (custyy[i] - yy2) ^ 2
    distance <- sqrt(xdist + ydist)
    totaldist <- totaldist + min(distance)
    index <- which.min(distance)
    time <- append(time, min(distance))
    xx2[index] = 20
    yy2[index] = 20
    match <- append(match, index)
  }
  prop <- c()
  for (i in 1:1000){
    prop[i] = sum(time < timelimspace[i])/mini
  }
  totalporp1 <- totalporp1 + prop
  totaltime1 <- totaltime1 + totaldist / mini
  maxtime1 <- maxtime1 + max(time)
  
  xx4 <- carxx
  yy4 <- caryy
  xx5 <- rev(custxx)
  yy5 <- rev(custyy)
  
  mini <- min(c(length(custxx), length(carxx)))
  match3 <- c()
  time3 <- c()
  totaldist3 <- 0
  for (i in 1:mini){
    xdist <- (xx5[i] - xx4) ^ 2
    ydist <- (yy5[i] - yy4) ^ 2
    distance <- sqrt(xdist + ydist)
    totaldist3 <- totaldist3 + min(distance)
    index <- which.min(distance)
    time3 <- append(time3, min(distance))
    xx4[index] = 20
    yy4[index] = 20
    match3 <- append(match3, index)
  }
  prop3 <- c()
  for (i in 1:1000){
    prop3[i] = sum(time3 < timelimspace[i])/mini
  }
  totalporp2 <- totalporp2 + prop3
  totaltime2 <- totaltime2 + totaldist3 / mini
  maxtime2 <- maxtime2 + max(time3)
}
aveprop1 <- totalporp1 / simnum
aveprop2 <- totalporp2 / simnum
plot(timelimspace, aveprop1, xlab="average waiting time", ylab="cdf", type = "l", lty = 1)
lines(timelimspace, aveprop1, col = 'red', type = "l", lty = 1)
lines(timelimspace, aveprop2, col = 'blue', type = "l", lty = 1)
legend("topleft", inset=.05, 
       c("algorithm 1","algorithm 2"), fill=c('red', 'blue'))
avetime1 <- totaltime1 / simnum
avetime2 <- totaltime2 / simnum
avemaxtime1 <- maxtime1 / simnum
avemaxtime2 <- maxtime2 / simnum

