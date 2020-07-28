#SV Scarpino
#July 2020
#COVID in HK

###########
#libraries#
###########
library(lme4)
library(ggplot2)

######
#Data#
######
dat <- read.csv("http://www.chp.gov.hk/files/misc/enhanced_sur_covid_19_eng.csv")
dat$Date.of.onset <- as.POSIXct(strptime(dat$Date.of.onset, format = "%d/%m/%Y"))
dat$New <- rep(1, nrow(dat))

#############
#Growth rate#
#############
dates <- seq(from = min(dat$Date.of.onset, na.rm = TRUE), to = max(dat$Date.of.onset, na.rm = TRUE), by = 60*60*24)
lag <- 4

#this was legacy code from a mixed effects model
doubling_prov <- list()
doubling_fixed <- list()
for(i in (lag+1):(length(dates))){
  use.i <- which(dat$Date.of.onset >= dates[i-lag] & dat$Date.of.onset <= dates[i])
  
  data.i <- dat[use.i, ]
  
  data.i$DATE <- as.numeric(data.i$Date.of.onset - dates[i-3], unit = "days")
  by_date.i <- by(data = data.i$New,  INDICES = data.i$DATE, FUN = sum, na.rm = TRUE)
  new.i <- as.numeric(by_date.i)
  dates.i <- as.numeric(names(by_date.i))
  
  mod3.i <- try(lm(log(new.i + 1) ~ dates.i), silent = TRUE)
  
  if(is(mod3.i)[1] == "try-error"){
    fixed.i <- NA
    doubling.i <- NA
    mob.i <- NA
    names.i <- NA
  }else{
    fixed.i <-   NA
    doubling.i <- mod3.i$coefficients[2]
    names.i <- NA
  }
  
  doubling_prov[[i-3]] <- doubling.i
  names(doubling_prov[[i-3]]) <- names.i
  doubling_fixed[[i-3]] <- fixed.i
}

rates <- unlist(lapply(doubling_prov, function(x) x))
prov <- unlist(lapply(doubling_prov, function(x) names(x)))
times <- rep(dates[4:(length(dates))], times = unlist(lapply(doubling_prov, function(x) length(x))))
dat.plot <- data.frame(rates, times, prov)

#######
#Plots#
#######
quartz(width = 8, height = 6)
ggplot(dat.plot, (aes(x = times, y = rates))) + geom_smooth(color = "#b2182b") + geom_line(size = 1.2) + xlab("Onset date (2020)") + ylab("COVID19 growth rate") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01))+ggtitle(label = "Hong Kong (COVID-19)")+geom_hline(yintercept = 0, linetype = "dashed", color = "#4393c3")

ggplot(data = dat, aes(x = Date.of.onset)) + geom_bar() + xlab("Onset date (2020)") + ylab("Daily new COVID-19 cases") + theme(legend.position = "right", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01))+ggtitle(label = "Hong Kong (COVID-19)")