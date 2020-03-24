rm(list = ls())

library(data.table)
library(openair)

#load in the data, here the data has already been cleaned and organized into one file
dat = fread('/Users/semenlitvin/Documents/GCRG/lab demo/ivo.csv')

#add a timestamp in posixct format
#I always use tz = GMT because it doesn't adjust for daylight savings time
# posixct looks for format YYYY-mm-dd HH:MM so if it's in the format you don't have to tell it the format
dat$date = as.POSIXct(dat$ts,tz='GMT')

#reddy is expecting the following format followed by their units (2 column header)
#Year DoY Hour  NEE H LE  Ustar Rg  Tair  Tsoil RH

#look at the data we need/want
par(mfrow = c(3,1)) #creates a two by two plot area
par(mar = c(4,4,1,1)) #sets the margins from bottom, left, top, right

plot(dat$date,dat$co2_flux)
plot(dat$date,dat$LE)
plot(dat$date,dat$H)

plot(dat$date,dat$U_)
plot(dat$date,dat$par.in)
plot(dat$date,dat$tair)

plot(dat$date,dat$rh)
plot(dat$date,dat$st1.15)

# subset the dataset to the whole years we are interested in
sub = selectByDate(mydata = dat,start = '2014-1-1',end = '2015-12-31')
sub = sub[!duplicated(sub$ts),] #remove duplicates (if any) by timestamps

#change the date column into what REddy, the MDS partitioning and gap filling code, will be expecting
Year = format(sub$date,'%Y') # year
DoY  = format(sub$date,'%j') #julian day
h    = as.numeric(format(sub$date,'%H')) #full hours
h.5  = as.numeric(ifelse(format(sub$date,'%M') == '00',0,0.5)) #half hour decimals
Hour = h+h.5 #Hour in the expected format

#combine the variables into a new consolodiated dataframe, PAR is being devided by 2.1 to convert to Wm-2
reddy = data.frame(Year,DoY,Hour,sub$co2_flux,sub$H,sub$LE,sub$U_,sub$par.in/2.1,sub$tair,sub$st1.15,sub$rh)
#set the names to what REddy expects
names(reddy) = c('Year','DoY','Hour','NEE','H','LE','Ustar','Rg','Tair','Tsoil','RH')

#check some commonly violated assumptions
summary(reddy$Rg) #no negatives in incoming radiation
reddy$Rg = ifelse(reddy$Rg < 0,0,reddy$Rg) #set negatives to 0
summary(reddy$RH) # nothing above 100 is allowed for rh
reddy$RH = ifelse(reddy$RH > 100,100,reddy$RH) #set above 100 to 100

#add the second header row and save as a .txt
h2 = c('--','--','--','umolm-2s-1','Wm-2','Wm-2','ms-1','Wm-2','DegC','DegC','%')
names(h2) = names(reddy)
h2 = as.character(h2)
reddy2 = rbind(h2,reddy)
reddy2$Year = as.character(reddy2$Year)
reddy2$DoY  = as.character(reddy2$DoY)
reddy2[1,1] = '--'
reddy2[1,2] = '--'

#save the new dataframe as a .txt files that is tab separated
write.table(x = reddy2,file = '/Users/semenlitvin/Documents/GCRG/lab demo/reddy_ivo.txt',row.names = F,sep = '\t',quote = F)

