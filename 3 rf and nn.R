rm(list = ls())

library(data.table)
library(openair)

#first load the headers
h = fread('/Users/semenlitvin/Documents/GCRG/lab demo/out/ivo-Results_data.txt',nrows = 0,header = T)
#now load the dataset
dat = fread('/Users/semenlitvin/Documents/GCRG/lab demo/out/ivo-Results_data.txt',skip = 2,header = F,na.strings = c('-9999','-10000','-9.9990e+03'))

#assign headers to the data
names(dat) = names(h)

#remake a proper timestamp
#break apart the time column
hour = floor(x = dat$Hour)
dif = dat$Hour - hour
min = dif*60
time = paste(hour,min,sep = ':')
date = paste(dat$Year,dat$DoY,sep = '-')
ts   = as.POSIXct(paste(date,time,sep = ' '),format = '%Y-%j %H:%M',tz='GMT')
dat$ts = ts

#random forest gapfilling
library(missForest)
#refine what to use for the RF
rfdat = data.frame(dat$NEE,dat$Rg_f,dat$Tair_f,dat$Tsoil_f,dat$VPD_f)

summary(rfdat)

mat = as.matrix(x = rfdat)

fil = missForest(xmis = mat,maxiter = 5,ntree = 10,variablewise = T,verbose = T)

#this is the new dataframe created
gf = data.frame(fil$ximp)

#plots
nee = ifelse(is.na(dat$NEE),gf$dat.NEE,NA) #remove data where original was present
plot(dat$NEE_f,nee);abline(0,1,col='red') #compare this gapfilling to the results of the other

plot(gf$dat.NEE,col='red');points(dat$NEE) #look at the filling of results

#mean square error in the random forest
fil$OOBerror

#do a neural network
library(neuralnet)
library(reshape)

#do on the whole dataset ###########################################
nndat = data.frame(dat$ts,dat$NEE,dat$Rg_f,dat$Tair_f,dat$Tsoil_f,dat$VPD_f)
names(nndat) = c('ts','nee','rg','airt','soilt','vpd')

#rescale all varialbes between 0 and 1
res = rescaler(nndat,type = 'range',na.rm = T)

#remove NAs for model training
inp = res[complete.cases(res),]

#train the neural network, lowering the threshold can improve the predictions but also increases the time the network takes to form. Increasing the reps can also help. the Step max may need to increase as you lower the threshold as well,it will need to take more steps and attempts to get somthing with a lower error
nn = neuralnet(data = inp,formula = nee ~ rg + vpd + airt,
               hidden = 6,threshold = 0.5,stepmax = 10000,rep = 3,act.fct = 'logistic',linear.output = F,err.fct="sse")

nnresult = compute(nn,res[,2:6])$net.result

plot(nnresult)

#check out how the neural network processed data
plot(nn)

# de-scale the data back to normal units
mindat=min(nndat$nee, na.rm=T)
maxdat=max(nndat$nee, na.rm=T)
nnresult = nnresult * (maxdat-mindat) + mindat

plot(nnresult,col='red',ylim = c(-5,5));points(nndat$nee)

#subset for summer time, see how it works when there's a consistent pattern. Neural networks work better when predictors are steadier.
gs = subset(nndat,nndat$ts >= as.POSIXct('2014-6-15 00:00') & nndat$ts <= as.POSIXct('2014-8-15 00:00'))

res = rescaler(gs, type="range",na.rm=T) 

#remove NAs for model training
inp = res[complete.cases(res),]

nn = neuralnet(data = inp,formula = nee ~ rg + vpd + airt,
               hidden = 6,threshold = 0.5,stepmax = 10000,rep = 3,act.fct = 'logistic',linear.output = F,err.fct="sse")

nndat = compute(nn,res[,2:6])$net.result

plot(nndat)

# de-scale the data back to normal units
mindat=min(gs$nee, na.rm=T)
maxdat=max(gs$nee, na.rm=T)
nndat = nndat * (maxdat-mindat) + mindat

plot(nndat)

#error assessment
hist(gs$nee-nndat,50) # histogram of the difference between real and modeled

#regression of real vs modeled
plot(gs$nee,nndat);abline(a = 0,b = 1,col='red',lty=2,lwd=3)
mod = lm(nndat ~ gs$nee)
summary(mod)

gs$nndat = nndat

#time series of result, red is modeled data
plot(gs$ts,gs$nee,type = 'l',xlim = as.POSIXct(c('2014-7-1','2014-7-10')));lines(gs$ts,gs$nndat,col='red');abline(h=0)

