###### American plaice stock assessment data GAM work DISTRIBUTION analysis
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr) 
here()
####Making transparent colors for plots below: ####
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
#t_col("#FFADAD",50,"plot_greent")
#### load excel files for growth analysis#####
Fall_Growth_data<-read_excel("C:/Users/jbehan/Box/Kerr Lab/Fisheries Science Lab/NCLIM/American plaice/Behan_plaice_2/data/GAM_growth_data.xlsx",sheet="Fall_WAA_anomalies", col_names = TRUE)
Spring_Growth_data<-read_excel("C:/Users/jbehan/Box/Kerr Lab/Fisheries Science Lab/NCLIM/American plaice/Behan_plaice_2/data/GAM_growth_data.xlsx",sheet="Spring_WAA_anomalies", col_names = TRUE)

####Bottom temperature data ####
load(here("data/fvcom_bottom_temp/yearly_avg/mf_yearly_clip/ALLmf_yearly_bottomtemp_clip.RData")) #Yearly bt averages (March (year-1) - February (year) = year prior to start of spring survey)
load(here("data/fvcom_bottom_temp/yearly_avg/os_yearly_clip/ALLos_yearly_bottomtemp_clip.RData")) #Yearly bt averages (October (year-1) - September (year) = year prior to start of Fall survey)

Fall_Growth_data<-merge(Fall_Growth_data,ALLos_yearly_bottomtemp_clip,by="Year")
names(Fall_Growth_data)[20]<-"Annual_temp"
Spring_Growth_data <- merge(Spring_Growth_data,ALLmf_yearly_bottomtemp_clip,by="Year")
names(Spring_Growth_data)[20]<-"Annual_temp"
### clipped regional temperature data#########
###choice to test 6-month avg prior to start of each seasonal survey comes from Fredston-Hermann et al. 2019##
load(here("data/fvcom_bottom_temp/month6avg/m6_clip/m6_SP_bottomtemp_clip.RData")) #6-month bottom temp averages (September-February)
load(here("data/fvcom_bottom_temp/month6avg/m6_clip/m6_FL_bottomtemp_clip.RData")) #6-month bottom temp averages (April-September)
Fall_Growth_data<-merge(Fall_Growth_data,m6_FL_bottomtemp_clip,by="Year")
names(Fall_Growth_data)[21]<-"month6_bt_avg"
Spring_Growth_data <- merge(Spring_Growth_data,m6_SP_bottomtemp_clip,by="Year")
names(Spring_Growth_data)[21]<-"month6_bt_avg"

###Other data
Plaice_NEFSC_Biomass<-read.csv(here("data/recruitment_dfs/Plaice_NEFSC_Biomass.csv"), header=TRUE, stringsAsFactors=FALSE) #biomass is in kg/tow units
depth_lat<-read.csv("C:/Users/jbehan/Box/Kerr Lab/Fisheries Science Lab/NCLIM/American plaice/Data/DisMAP_plaice_depth_lat.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(depth_lat)[1] <- "Year"
##Add new 6month averages & plaice biomass & depth/lat to AGE1 dataframes####
Fall_Growth_data <- merge(Fall_Growth_data , Plaice_NEFSC_Biomass[c(1,2)],by="Year")
colnames(Fall_Growth_data)[22] <- "Plaice_biomass"
Spring_Growth_data <- merge(Spring_Growth_data , Plaice_NEFSC_Biomass[c(1,4)],by="Year")
colnames(Spring_Growth_data)[22] <- "Plaice_biomass"

###### Anomaly Base Period########
### using 1981-2010 as baseline anomaly period as NOAA does####

Month6_SP_bp<-colMeans(m6_SP_bottomtemp_clip[4:33,2])
Month6_FL_bp<-colMeans(m6_FL_bottomtemp_clip[4:33,2])
Annual_mf_bp<-colMeans(ALLmf_yearly_bottomtemp_clip[4:33,2])
Annual_os_bp<-colMeans(ALLos_yearly_bottomtemp_clip[4:33,2])
#####
##### Calculate temperature anomaly columns#####
#Fall
Fall_Growth_data$month6_bt_anomaly<- Fall_Growth_data$month6_bt_avg - Month6_FL_bp
Fall_Growth_data$Annual_os_bt_anomaly<- Fall_Growth_data$Annual_temp - Annual_os_bp
Fall_Growth_data <- Fall_Growth_data[ -c(20,21) ]
#Spring
Spring_Growth_data$month6_bt_anomaly<- Spring_Growth_data$month6_bt_avg - Month6_SP_bp
Spring_Growth_data$Annual_mf_bt_anomaly<- Spring_Growth_data$Annual_temp - Annual_mf_bp
Spring_Growth_data<- Spring_Growth_data[ -c(20,21) ]
#######
#PLOT Weight at age changes together on same plot Ages 1-6 ####
plot(age1 ~ Year, data=Fall_Growth_data, main="FALL Weight at Age Anomalies Overtime Ages 1-5", xlab="Year",ylab="Weight at Age Anomalies (kg)", type="l",pch=16,cex=1.0,col="red3",ylim= c(-0.13,0.17),lwd=3.0)
lines(age2 ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="magenta",lwd=3.0)
lines(age3 ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="darkorange1",lwd=3.0)
lines(age4 ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="yellow",lwd=3.0)
lines(age5 ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="green3",lwd=3.0)
grid(NULL,NULL)
legend(1980, 0.17, legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5"),
       col=c("red3", "magenta","darkorange1","yellow","green3"), lty=1, cex=1.0,lwd=3.0)
#PLOT Weight at age changes together on same plot Ages 6-11+ ####
plot(age6 ~ Year, data=Fall_Growth_data, main="FALL Weight at Age Anomalies Overtime Ages 6-11+", xlab="Year",ylab="Weight at Age Anomalies (kg)", type="l",pch=16,cex=1.0,col="green",ylim= c(-0.75,1.15),lwd=3.0)
lines(age7 ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="cyan",lwd=3.0)
lines(age8 ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="blue3",lwd=3.0)
lines(age9 ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="mediumorchid3",lwd=3.0)
lines(age10 ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="gray50",lwd=3.0)
lines(age11plus ~ Year, data=Fall_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="black",lwd=3.0)
grid(NULL,NULL)
legend(1980, 1.0, legend=c("Age 6","Age 7", "Age 8","Age 9", "Age 10","Age 11+" ),
       col=c("green", "cyan","blue3","mediumorchid3","gray50","black"), lty=1, cex=1.0,lwd=3.0)
#SPRING PLOT Weight at age changes together on same plot Ages 1-6 ####
plot(age1 ~ Year, data=Spring_Growth_data, main="SPRING Weight at Age Anomalies Overtime Ages 1-5", xlab="Year",ylab="Weight at Age Anomalies (kg)", type="l",pch=16,cex=1.0,col="red3",ylim= c(-0.13,0.15),lwd=3.0)
lines(age2 ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="magenta",lwd=3.0)
lines(age3 ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="darkorange1",lwd=3.0)
lines(age4 ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="yellow",lwd=3.0)
lines(age5 ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="green3",lwd=3.0)
grid(NULL,NULL)
legend(1980, 0.15, legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5"),
       col=c("red3", "magenta","darkorange1","yellow","green3"), lty=1, cex=1.0,lwd=3.0)
#SPRING PLOT Weight at age changes together on same plot Ages 6-11+ ####
plot(age6 ~ Year, data=Spring_Growth_data, main="SPRING Weight at Age Anomalies Overtime Ages 6-11+", xlab="Year",ylab="Weight at Age Anomalies (kg)", type="l",pch=16,cex=1.0,col="green",ylim= c(-0.80,1.30),lwd=3.0)
lines(age7 ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="cyan",lwd=3.0)
lines(age8 ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="blue3",lwd=3.0)
lines(age9 ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="mediumorchid3",lwd=3.0)
lines(age10 ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="gray50",lwd=3.0)
lines(age11plus ~ Year, data=Spring_Growth_data, xlab="Year", type="l",pch=16,cex=1.0,col="black",lwd=3.0)
grid(NULL,NULL)
legend(1980, 1.20, legend=c("Age 6","Age 7", "Age 8","Age 9", "Age 10","Age 11+" ),
       col=c("green", "cyan","blue3","mediumorchid3","gray50","black"), lty=1, cex=1.0,lwd=3.0)
#SEASONAL PLOT Mean condition ####
plot(mean_cond ~ Year, data=Fall_Growth_data, main="Plaice Mean Condition", xlab="Year",ylab="Ratio of Observed Weight:Predicted Weight, Based on Length ", type="l",pch=16,cex=1.0,col="blue",ylim= c(0.90,1.03),xlim= c(1990,2020),lwd=3.0)
my_mod<-lm(Fall_Growth_data$mean_cond~Fall_Growth_data$Year)
abline(lm(Fall_Growth_data$mean_cond~Fall_Growth_data$Year),col="blue",lwd=2,lty=2)
grid(NULL,NULL)
legend(2000, 1.02, legend=c(paste('Regression   y =', round(coef(my_mod)[[2]],digits=4), 'x +',round(coef(my_mod)[[1]],digits=3))))

####### Check Outliars######
summary(Fall_Growth_data)
summary(Spring_Growth_data)

#SPRING AGE 1
par(mar=c(2,2,0,0), mfrow=c(2,5))
dotchart(Spring_Growth_data$GSI6)
dotchart(Spring_Growth_data$AMO6)
dotchart(Spring_Growth_data$NAO6)
dotchart(Spring_Growth_data$GSI12)
dotchart(Spring_Growth_data$AMO12)
dotchart(Spring_Growth_data$NAO12)
dotchart(Spring_Growth_data$month6_bt_anomaly)
dotchart(Spring_Growth_data$Annual_mf_bt_anomaly)
dotchart(Spring_Growth_data$mean_cond)
dotchart(Spring_Growth_data$Plaice_biomass)
#Fall age 1
par(mar=c(2,2,0,0), mfrow=c(2,5))
dotchart(Fall_Growth_data$GSI6)
dotchart(Fall_Growth_data$AMO6)
dotchart(Fall_Growth_data$NAO6)
dotchart(Fall_Growth_data$GSI12)
dotchart(Fall_Growth_data$AMO12)
dotchart(Fall_Growth_data$NAO12)
dotchart(Fall_Growth_data$month6_bt_anomaly)
dotchart(Fall_Growth_data$Annual_os_bt_anomaly)
dotchart(Fall_Growth_data$mean_cond)
dotchart(Fall_Growth_data$Plaice_biomass)
##########
######Boxplots######
view_boxplot_fun<- function (data){
  layout(matrix(8:1, ncol=8, byrow=FALSE))
  boxplot(data[14],varwidth = TRUE, xlab = "GSI6", ylab = "Latitude anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[16],varwidth = TRUE, xlab = "AMO6", ylab = "Change in SST (Deg C) ", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[15],varwidth = TRUE, xlab = "NAO6", ylab = "SS level pressure difference Anomalies (hPa) ", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[17],varwidth = TRUE, xlab = "GSI12", ylab = "Latitude anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[19],varwidth = TRUE, xlab = "AMO12", ylab = "Change in SST (Deg C) ", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[18],varwidth = TRUE, xlab = "NAO12", ylab = "SS level pressure difference Anomalies (hPa) ", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[21],varwidth = TRUE, xlab = "6 month avg bt", ylab = "Bt Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[22],varwidth = TRUE, xlab = "Annual bt", ylab = "Bt Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
}
view_boxplot_fun(Fall_Growth_data)
view_boxplot_fun(Spring_Growth_data)
##### check distribtuion ####
#SPRING 
par(mar=c(2,2,2,0), mfrow=c(2,4))
hist(Spring_Growth_data$GSI6)
hist(Spring_Growth_data$AMO6)
hist(Spring_Growth_data$NAO6)
hist(Spring_Growth_data$GSI12)
hist(Spring_Growth_data$AMO12)
hist(Spring_Growth_data$NAO12)
hist(Spring_Growth_data$month6_bt_anomaly)
hist(Spring_Growth_data$Annual_mf_bt_anomaly)

hist(Spring_Growth_data$mean_cond, main="Mean Condition")

par(mar=c(2,2,2,0), mfrow=c(2,6))
hist(Spring_Growth_data$age1, main="Age 1 WAA Anomaly")
hist(Spring_Growth_data$age2, main="Age 2 WAA Anomaly")
hist(Spring_Growth_data$age3, main="Age 3 WAA Anomaly")
hist(Spring_Growth_data$age4, main="Age 4 WAA Anomaly")
hist(Spring_Growth_data$age5, main="Age 5 WAA Anomaly")
hist(Spring_Growth_data$age6, main="Age 6 WAA Anomaly")
hist(Spring_Growth_data$age7, main="Age 7 WAA Anomaly")
hist(Spring_Growth_data$age8, main="Age 8 WAA Anomaly")
hist(Spring_Growth_data$age9, main="Age 9 WAA Anomaly")
hist(Spring_Growth_data$age10, main="Age 10 WAA Anomaly")
hist(Spring_Growth_data$age11plus, main="Age 11+ WAA Anomaly")

#Fall 
par(mar=c(2,2,2,0), mfrow=c(2,4))
hist(Fall_Growth_data$GSI6)
hist(Fall_Growth_data$AMO6)
hist(Fall_Growth_data$NAO6)
hist(Fall_Growth_data$GSI12)
hist(Fall_Growth_data$AMO12)
hist(Fall_Growth_data$NAO12)
hist(Fall_Growth_data$month6_bt_anomaly)
hist(Fall_Growth_data$Annual_mf_bt_anomaly)

par(mar=c(2,2,2,0), mfrow=c(1,1))
hist(Spring_Growth_data$mean_cond, main="Mean Condition")

par(mar=c(2,2,2,0), mfrow=c(2,6))
hist(Fall_Growth_data$age1, main="Age 1 WAA Anomaly")
hist(Fall_Growth_data$age2, main="Age 2 WAA Anomaly")
hist(Fall_Growth_data$age3, main="Age 3 WAA Anomaly")
hist(Fall_Growth_data$age4, main="Age 4 WAA Anomaly")
hist(Fall_Growth_data$age5, main="Age 5 WAA Anomaly")
hist(Fall_Growth_data$age6, main="Age 6 WAA Anomaly")
hist(Fall_Growth_data$age7, main="Age 7 WAA Anomaly")
hist(Fall_Growth_data$age8, main="Age 8 WAA Anomaly")
hist(Fall_Growth_data$age9, main="Age 9 WAA Anomaly")
hist(Fall_Growth_data$age10, main="Age 10 WAA Anomaly")
hist(Fall_Growth_data$age11plus, main="Age 11+ WAA Anomaly")
#####shapiro test for normality#####
#if p >0.05, we can assume normality
#SPRING
shapiro.test(Spring_Growth_data$GSI6)
shapiro.test(Spring_Growth_data$AMO6)
shapiro.test(Spring_Growth_data$NAO6)
shapiro.test(Spring_Growth_data$GSI12)
shapiro.test(Spring_Growth_data$AMO12)
shapiro.test(Spring_Growth_data$NAO12)
shapiro.test(Spring_Growth_data$month6_bt_anomaly)
shapiro.test(Spring_Growth_data$Annual_mf_bt_anomaly)
shapiro.test(Spring_Growth_data$mean_cond)
shapiro.test(Spring_Growth_data$age1) #not normal
shapiro.test(Spring_Growth_data$age2)
shapiro.test(Spring_Growth_data$age3) #not normal
shapiro.test(Spring_Growth_data$age4)
shapiro.test(Spring_Growth_data$age5)
shapiro.test(Spring_Growth_data$age6) #not normal
shapiro.test(Spring_Growth_data$age7) #not normal
shapiro.test(Spring_Growth_data$age8) #not normal
shapiro.test(Spring_Growth_data$age9) #not normal
shapiro.test(Spring_Growth_data$age10) #not normal
shapiro.test(Spring_Growth_data$age11plus) #not normal
#Fall
shapiro.test(Fall_Growth_data$GSI6)
shapiro.test(Fall_Growth_data$AMO6)
shapiro.test(Fall_Growth_data$NAO6)
shapiro.test(Fall_Growth_data$GSI12)
shapiro.test(Fall_Growth_data$AMO12)
shapiro.test(Fall_Growth_data$NAO12)
shapiro.test(Fall_Growth_data$month6_bt_anomaly) #not normal
shapiro.test(Fall_Growth_data$Annual_os_bt_anomaly) #not normal
shapiro.test(Fall_Growth_data$mean_cond)
shapiro.test(Fall_Growth_data$age1) #not normal
shapiro.test(Fall_Growth_data$age2)
shapiro.test(Fall_Growth_data$age3)
shapiro.test(Fall_Growth_data$age4) #not normal
shapiro.test(Fall_Growth_data$age5)
shapiro.test(Fall_Growth_data$age6) #not normal
shapiro.test(Fall_Growth_data$age7) #not normal
shapiro.test(Fall_Growth_data$age8) #not normal
shapiro.test(Fall_Growth_data$age9) #not normal
shapiro.test(Fall_Growth_data$age10) #not normal
shapiro.test(Fall_Growth_data$age11plus) #not normal
####### Check Correlation matrix######
############Correlation Coefficient Test#############
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1 + rnorm(nrow(dataz)) ,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}
Mypairs <- function(Z) {
  MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
        cex.labels =  2,
        lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
          panel.cor(x, y, digits, prefix, cex.cor)},
        upper.panel =  function(x, y) points(x, y,
                                             pch = 16, cex = 0.8,
                                             col = gray(0.1)))
  #print(P)
}

Mypairs(Fall_Growth_data[c(14:22)])
Mypairs(Spring_Growth_data[c(14:22)])

###Plot GAM Response Curves------ FUNCTION ####
GAM_CURVE_FUN<- function(gam_name,data_column,data_Year,x_lab,y_lab,select1,title,position){
  par(mar=c(5,4.5,3,1))
  plot.gam(gam_name,xlab=paste(x_lab),ylab=paste(y_lab), select=select1,cex.lab=1.5,cex.axis=1.4,rug=FALSE,shade = TRUE,col = "black",shade.col=t_col("lightgray",50,"plot_gray"),lwd = 2.5)
  rug(subset(data_column, data_Year <=1989), ticksize=0.03, side=1, lwd=2.5,col="blue")
  rug(subset(data_column, data_Year <=1999 & data_Year >=1990), ticksize=0.05, side=1, lwd=2.5,col="green")
  rug(subset(data_column, data_Year <=2009 & data_Year >=2000), ticksize=0.05, side=1, lwd=2.5,col="orange")
  rug(subset(data_column, data_Year <=2019 & data_Year >=2010), ticksize=0.03, side=1, lwd=2.5,col="red")
  legend(paste(position),inset=c(0.05,0.05), legend =c('1980-1989', '1990-1999','2000-2009', '2010-2019'), pch=16, pt.cex=1.5, cex=1.2, bty='n',
         col = c("blue", "green","orange", "red"),title="Decade")
  abline(h=0, lty=2, col="tomato3", lwd=2.0)
  title(main=paste(title),cex.main=2.0)
}
###### EXPLORATORY GAMs for growth#######
##### MEAN CONDITION vs. Environment ###########
#### FALL MEAN CONDITION ####
condition_FALL<-gam((mean_cond) ~ s(GSI6, k=4)+s(AMO6, k=4)+s(NAO6, k=4)+s(GSI12, k=4)+s(AMO12, k=4)+s(NAO12, k=4)+s(month6_bt_anomaly, k=4)+s(Annual_os_bt_anomaly, k=4)+s(Plaice_biomass, k=4), family=tw(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(condition_FALL) # Find significant variables based on p-value
condition_FALL$aic

#full model, duplicates removed:
condition_FALL<-gam((mean_cond) ~ s(AMO6, k=5)+s(GSI12, k=5)+s(NAO12, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=tw(),method = "REML",data=Fall_Growth_data)
summary(condition_FALL)
condition_FALL$aic
#reduced model:
condition_FALL<-gam((mean_cond) ~ s(AMO6, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=tw(),method = "REML",data=Fall_Growth_data)
summary(condition_FALL)
condition_FALL$aic

#####
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(condition_FALL,pch=20, cex=1.2,cex.lab=1.5)
##### GAM Curves###
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(condition_FALL,Fall_Growth_data$AMO6,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on Mean Condition",select1=1,title="Fall Mean Condition",position="top")
GAM_CURVE_FUN(condition_FALL,Fall_Growth_data$Annual_os_bt_anomaly,Fall_Growth_data$Year,x_lab="Annual Bottom Temperature Anomalies (Degrees C)",y_lab="PE on Mean Condition",select1=2,title=NULL,position="topleft")
GAM_CURVE_FUN(condition_FALL,Fall_Growth_data$Plaice_biomass,Fall_Growth_data$Year,x_lab="AM plaice SSB (kg/tow)",y_lab="PE on Mean Condition",select1=3,title=NULL,position="topleft")
######
###### SPRING MEAN CONDITION######
#no spring GAM for mean condition because data is based off fall survey
##### MEAN WEIGHT AT AGE ANOMALY vs. Environment ###########
#####Age 1######
###FALL
age1_FALL<-gam((age1) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age1_FALL) # Find significant variables based on p-value
age1_FALL$aic

#full model, duplicates removed:
age1_FALL<-gam((age1) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO12, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age1_FALL)
age1_FALL$aic
#reduced model:
age1_FALL<-gam((age1) ~ s(Annual_os_bt_anomaly), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age1_FALL)
age1_FALL$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age1_FALL,pch=20, cex=1.2,cex.lab=1.4)
###SPRING
age1_SPRING<-gam((age1) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age1_SPRING) # Find significant variables based on p-value
age1_SPRING$aic

#full model, duplicates removed:
age1_SPRING<-gam((age1) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO12, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age1_SPRING)
age1_SPRING$aic
#reduced model: (NOTHING SIGNIFICANT)
age1_SPRING<-gam((age1) ~ s(Annual_mf_bt_anomaly), family=guassian(),method = "REML",data=Spring_Growth_data)
summary(age1_SPRING)
age1_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age1_SPRING,pch=20, cex=1.2,cex.lab=1.4)
##### GAM Curves###
layout(matrix(1:3, ncol=1, byrow=FALSE))
plot.new()
plot.new()
GAM_CURVE_FUN(age1_FALL,Fall_Growth_data$Annual_os_bt_anomaly,Fall_Growth_data$Year,x_lab="Annual Bottom Temperature Anomalies (Degrees C)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 1 Weight at Age Anomaly",position="topleft")
#####Age 2######
###FALL
age2_FALL<-gam((age2) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age2_FALL) # Find significant variables based on p-value
age2_FALL$aic

#full model, duplicates removed:
age2_FALL<-gam((age2) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age2_FALL)
age2_FALL$aic
#reduced model:
age2_FALL<-gam((age2) ~ s(GSI12, k=5)+s(AMO12, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age2_FALL)
age2_FALL$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age2_FALL,pch=20, cex=1.2,cex.lab=1.4)
###SPRING
age2_SPRING<-gam((age2) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age2_SPRING) # Find significant variables based on p-value
age2_SPRING$aic

#full model, duplicates removed:
age2_SPRING<-gam((age2) ~ s(GSI6, k=5)+s(AMO6, k=5)+s(NAO12, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age2_SPRING)
age2_SPRING$aic
#reduced model: (NOTHING SIGNIFICANT)
age2_SPRING<-gam((age2) ~ s(GSI6, k=5)+s(AMO6, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age2_SPRING)
age2_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age2_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves ages 1 & 2  FALL #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age1_FALL,Fall_Growth_data$Annual_os_bt_anomaly,Fall_Growth_data$Year,x_lab="Annual Bottom Temperature Anomalies (Degrees C)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 1 Weight at Age Anomaly",position="topleft")
GAM_CURVE_FUN(age2_FALL,Fall_Growth_data$GSI12,Fall_Growth_data$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 2 Weight at Age Anomaly",position="topleft")
GAM_CURVE_FUN(age2_FALL,Fall_Growth_data$AMO12,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="topleft")
########## GAM CURVES age 2 SPRING #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age2_SPRING,Spring_Growth_data$GSI6,Spring_Growth_data$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 2 Weight at Age Anomaly",position="topleft")
GAM_CURVE_FUN(age2_SPRING,Spring_Growth_data$AMO6,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="topleft")
plot.new()
#####Age 3######
###FALL
age3_FALL<-gam((age3) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age3_FALL) # Find significant variables based on p-value
age3_FALL$aic

#full model, duplicates removed:
age3_FALL<-gam((age3) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age3_FALL)
age3_FALL$aic
#reduced model:
age3_FALL<-gam((age3) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age3_FALL)
age3_FALL$aic

age3_FALL<-gam((age3) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age3_FALL)
age3_FALL$aic


par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age3_FALL,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 3  FALL #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age3_FALL,Fall_Growth_data$GSI12,Fall_Growth_data$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 3 Weight at Age Anomaly",position="topleft")
GAM_CURVE_FUN(age3_FALL,Fall_Growth_data$AMO12,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="topleft")
GAM_CURVE_FUN(age3_FALL,Fall_Growth_data$Plaice_biomass,Fall_Growth_data$Year,x_lab="AM plaice SSB (kg/tow)",y_lab="PE on WAA Anomaly",select1=3,title=NULL,position="topleft")
###SPRING
age3_SPRING<-gam((age3) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age3_SPRING) # Find significant variables based on p-value
age3_SPRING$aic

#full model, duplicates removed:
age3_SPRING<-gam((age3) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age3_SPRING)
age3_SPRING$aic
#reduced model:
age3_SPRING<-gam((age3) ~ s(GSI12, k=5)+s(AMO12, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age3_SPRING)
age3_SPRING$aic


par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age3_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 3  SPRING #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age3_SPRING,Spring_Growth_data$GSI12,Spring_Growth_data$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 3 Weight at Age Anomaly",position="topleft")
GAM_CURVE_FUN(age3_SPRING,Spring_Growth_data$AMO12,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="top")
plot.new()
#####Age 4######
###FALL
age4_FALL<-gam((age4) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age4_FALL) # Find significant variables based on p-value
age4_FALL$aic

#full model, duplicates removed:
age4_FALL<-gam((age4) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO12, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age4_FALL)
age4_FALL$aic
#reduced model:
age4_FALL<-gam((age4) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age4_FALL)
age4_FALL$aic


par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age4_FALL,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 4  FALL #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age4_FALL,Fall_Growth_data$GSI12,Fall_Growth_data$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 4 Weight at Age Anomaly",position="topleft")
GAM_CURVE_FUN(age4_FALL,Fall_Growth_data$AMO12,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="topleft")
GAM_CURVE_FUN(age4_FALL,Fall_Growth_data$Plaice_biomass,Fall_Growth_data$Year,x_lab="AM plaice SSB (kg/tow)",y_lab="PE on WAA Anomaly",select1=3,title=NULL,position="topleft")
###SPRING
age4_SPRING<-gam((age4) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age4_SPRING) # Find significant variables based on p-value
age4_SPRING$aic

#full model, duplicates removed:
age4_SPRING<-gam((age4) ~ s(GSI6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age4_SPRING)
age4_SPRING$aic
#reduced model:
age4_SPRING<-gam((age4) ~ s(GSI6, k=5)+s(AMO12, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age4_SPRING)
age4_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age4_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 4  SPRING #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age4_SPRING,Spring_Growth_data$GSI6,Spring_Growth_data$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 4 Weight at Age Anomaly",position="topleft")
GAM_CURVE_FUN(age4_SPRING,Spring_Growth_data$AMO12,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="top")
plot.new()
#####Age 5######
###FALL
age5_FALL<-gam((age5) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age5_FALL) # Find significant variables based on p-value
age5_FALL$aic

#full model, duplicates removed:
age5_FALL<-gam((age5) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO12, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age5_FALL)
age5_FALL$aic
#reduced model:
age5_FALL<-gam((age5) ~ s(AMO12, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age5_FALL)
age5_FALL$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age5_FALL,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 5  FALL #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
plot.new
GAM_CURVE_FUN(age5_FALL,Fall_Growth_data$AMO12,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 5 Weight at Age Anomaly" ,position="bottomleft")
GAM_CURVE_FUN(age5_FALL,Fall_Growth_data$Plaice_biomass,Fall_Growth_data$Year,x_lab="AM plaice SSB (kg/tow)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="topleft")
###SPRING
age5_SPRING<-gam((age5) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age5_SPRING) # Find significant variables based on p-value
age5_SPRING$aic

#full model, duplicates removed:
age5_SPRING<-gam((age5) ~ s(GSI12, k=5)+s(AMO6, k=5)+s(NAO6, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age5_SPRING)
age5_SPRING$aic
#reduced model:
age5_SPRING<-gam((age5) ~ s(GSI12, k=5)+s(AMO6, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age5_SPRING)
age5_SPRING$aic

age5_SPRING<-gam((age5) ~ s(GSI12, k=5)+s(AMO6, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)#to include gsi or 6month bt?
summary(age5_SPRING)
age5_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age5_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 5  SPRING #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age5_SPRING,Spring_Growth_data$GSI12,Spring_Growth_data$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 5 Weight at Age Anomaly",position="topleft")
GAM_CURVE_FUN(age5_SPRING,Spring_Growth_data$AMO6,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="top")
GAM_CURVE_FUN(age5_SPRING,Spring_Growth_data$Plaice_biomass,Spring_Growth_data$Year,x_lab="AM plaice SSB (kg/tow)",y_lab="PE on WAA Anomaly",select1=3,title=NULL,position="bottom")

#####Age 6######
###FALL
age6_FALL<-gam((age6) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age6_FALL) # Find significant variables based on p-value
age6_FALL$aic

#full model, duplicates removed:
age6_FALL<-gam((age6) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age6_FALL)
age6_FALL$aic
#reduced model:
age6_FALL<-gam((age6) ~ s(AMO12, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age6_FALL)
age6_FALL$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age6_FALL,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 6  FALL #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age6_FALL,Fall_Growth_data$AMO12,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 6 Mean Weight at Age Anomaly",position="bottomleft")
###SPRING
age6_SPRING<-gam((age6) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age6_SPRING) # Find significant variables based on p-value
age6_SPRING$aic

#full model, duplicates removed:
age6_SPRING<-gam((age6) ~ s(GSI12, k=5)+s(AMO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age6_SPRING)
age6_SPRING$aic
#reduced model:
age6_SPRING<-gam((age6) ~ s(GSI12, k=5)+s(AMO6, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age6_SPRING)
age6_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age6_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 6  SPRING #####
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age6_SPRING,Spring_Growth_data$GSI12,Spring_Growth_data$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 6 Weight at Age Anomaly",position="bottomright")
GAM_CURVE_FUN(age6_SPRING,Spring_Growth_data$AMO6,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="bottomleft")
GAM_CURVE_FUN(age6_SPRING,Spring_Growth_data$Plaice_biomass,Spring_Growth_data$Year,x_lab="AM plaice SSB (kg/tow)",y_lab="PE on WAA Anomaly",select1=3,title=NULL,position="bottom")
#####Age 7######
###FALL age 7######
age7_FALL<-gam((age7) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age7_FALL) # Find significant variables based on p-value
age7_FALL$aic

#full model, duplicates removed:
age7_FALL<-gam((age7) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age7_FALL)
age7_FALL$aic
#reduced model:
age7_FALL<-gam((age7) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age7_FALL)
age7_FALL$aic

age7_FALL<-gam((age7) ~ s(AMO12, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age7_FALL)
age7_FALL$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age7_FALL,pch=20, cex=1.2,cex.lab=1.4)
###SPRING######
age7_SPRING<-gam((age7) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age7_SPRING) # Find significant variables based on p-value
age7_SPRING$aic

#full model, duplicates removed:
age7_SPRING<-gam((age7) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age7_SPRING)
age7_SPRING$aic
#reduced model:
age7_SPRING<-gam((age7) ~ s(AMO12, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age7_SPRING)
age7_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age7_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 7  SPRING/Fall
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age7_FALL,Fall_Growth_data$AMO12,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 7 Weight at Age Anomaly",position="topright")
GAM_CURVE_FUN(age7_SPRING,Spring_Growth_data$AMO12,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 7 Weight at Age Anomaly",position="topright")
plot.new()
#####Age 8######
###FALL age 8######
age8_FALL<-gam((age8) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age8_FALL) # Find significant variables based on p-value
age8_FALL$aic

#full model, duplicates removed:
age8_FALL<-gam((age8) ~ s(GSI6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age8_FALL)
age8_FALL$aic
#reduced model: (Nothing significant)
###SPRING######
age8_SPRING<-gam((age8) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age8_SPRING) # Find significant variables based on p-value
age8_SPRING$aic

#full model, duplicates removed:
age8_SPRING<-gam((age8) ~ s(GSI12, k=5)+s(AMO6, k=5)+s(NAO12, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age8_SPRING)
age8_SPRING$aic
#reduced model:
age8_SPRING<-gam((age8) ~ s(AMO6, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age8_SPRING)
age8_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age8_SPRING,pch=20, cex=1.2,cex.lab=1.4)
#####Age 9######
###FALL age 9######
age9_FALL<-gam((age9) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age9_FALL) # Find significant variables based on p-value
age9_FALL$aic

#full model, duplicates removed:
age9_FALL<-gam((age9) ~ s(GSI6, k=5)+s(AMO6, k=5)+s(NAO6, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age9_FALL)
age9_FALL$aic
#reduced model:
age9_FALL<-gam((age9) ~ s(AMO6, k=5)+s(month6_bt_anomaly, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age9_FALL)
age9_FALL$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age9_FALL,pch=20, cex=1.2,cex.lab=1.4)
###SPRING######
age9_SPRING<-gam((age9) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age9_SPRING) # Find significant variables based on p-value
age9_SPRING$aic

#full model, duplicates removed:
age9_SPRING<-gam((age9) ~ s(GSI12, k=5)+s(AMO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age9_SPRING)
age9_SPRING$aic
#reduced model:
age9_SPRING<-gam((age9) ~ s(AMO6, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age9_SPRING)
age9_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age9_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 7  SPRING/Fall
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age9_FALL,Fall_Growth_data$AMO6,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 9 Weight at Age Anomaly",position="topright")
GAM_CURVE_FUN(age9_FALL,Fall_Growth_data$month6_bt_anomaly,Fall_Growth_data$Year,x_lab="6-Month Mean Bottom Temperature Anomaly (°C)",y_lab="PE on WAA Anomaly",select1=2,title=NULL,position="topright")
GAM_CURVE_FUN(age9_SPRING,Spring_Growth_data$AMO6,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 9 Weight at Age Anomaly",position="topright")


#####Age 10######
###FALL age 10######
age10_FALL<-gam((age10) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age10_FALL) # Find significant variables based on p-value
age10_FALL$aic

#full model, duplicates removed:
age10_FALL<-gam((age10) ~ s(GSI6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age10_FALL)
age10_FALL$aic
#reduced model:
age10_FALL<-gam((age10) ~ s(GSI6, k=5)+s(AMO12, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age10_FALL)
age10_FALL$aic

age10_FALL<-gam((age10) ~ s(AMO12, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age10_FALL)
age10_FALL$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age10_FALL,pch=20, cex=1.2,cex.lab=1.4)
###SPRING######
age10_SPRING<-gam((age10) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age10_SPRING) # Find significant variables based on p-value
age10_SPRING$aic

#full model, duplicates removed:
age10_SPRING<-gam((age10) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO12, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age10_SPRING)
age10_SPRING$aic
#reduced model:
age10_SPRING<-gam((age10) ~ s(AMO12, k=5)+s(Annual_mf_bt_anomaly, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age10_SPRING)
age10_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age10_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 10 SPRING/Fall
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age10_FALL,Fall_Growth_data$AMO12,Fall_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Fall Age 10 Weight at Age Anomaly",position="topright")
GAM_CURVE_FUN(age10_SPRING,Spring_Growth_data$AMO12,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 10 Weight at Age Anomaly",position="topright")
GAM_CURVE_FUN(age10_SPRING,Spring_Growth_data$Annual_mf_bt_anomaly,Spring_Growth_data$Year,x_lab="Annual Mean Bottom Temperature Anomaly (Δ °C)",y_lab="PE on WAA Anomaly",select1=2,title="Spring Age 10 Weight at Age Anomaly",position="topright")
#####Age 11+######
###FALL age 11+######
age11plus_FALL<-gam((age11plus) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data) # Build GAM with all possible variables
summary(age11plus_FALL) # Find significant variables based on p-value
age11plus_FALL$aic

#full model, duplicates removed:
age11plus_FALL<-gam((age11plus) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(month6_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age11plus_FALL)
age11plus_FALL$aic
#reduced model:
age11plus_FALL<-gam((age11plus) ~ s(GSI12, k=5)+s(AMO12, k=5)+s(month6_bt_anomaly, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age11plus_FALL)
age11plus_FALL$aic

age11plus_FALL<-gam((age11plus) ~ s(AMO12, k=5), family=gaussian(),method = "REML",data=Fall_Growth_data)
summary(age11plus_FALL)
age11plus_FALL$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age11plus_FALL,pch=20, cex=1.2,cex.lab=1.4)
###SPRING######
age11plus_SPRING<-gam((age11plus) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data) # Build GAM with all possible variables
summary(age11plus_SPRING) # Find significant variables based on p-value
age11plus_SPRING$aic

#full model, duplicates removed:
age11plus_SPRING<-gam((age11plus) ~ s(GSI12, k=5)+s(AMO6, k=5)+s(NAO12, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(Plaice_biomass, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age11plus_SPRING)
age11plus_SPRING$aic
#reduced model:
age11plus_SPRING<-gam((age11plus) ~ s(AMO12, k=5)+s(Annual_mf_bt_anomaly, k=5), family=gaussian(),method = "REML",data=Spring_Growth_data)
summary(age11plus_SPRING)
age11plus_SPRING$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(age11plus_SPRING,pch=20, cex=1.2,cex.lab=1.4)
######## GAM Curves age 11+ SPRING
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(age11plus_SPRING,Spring_Growth_data$AMO12,Spring_Growth_data$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on WAA Anomaly",select1=1,title="Spring Age 11+ Weight at Age Anomaly",position="bottomleft")
GAM_CURVE_FUN(age11plus_SPRING,Spring_Growth_data$Annual_mf_bt_anomaly,Spring_Growth_data$Year,x_lab="Annual Mean Bottom Temperature Anomaly (Δ °C)",y_lab="PE on WAA Anomaly",select1=2,title="Spring Age 11+ Weight at Age Anomaly",position="bottomleft")
plot.new()

##### PLOT FALL AGES 1-4 ######
layout(matrix(1:4, ncol=1, byrow=FALSE))
par("mar"=c(4, 5, 1, 1))
plot.gam(age1_FALL, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Annual Bottom Temperature Anomalies (°C)")), cex.lab=1.6,cex.axis=1.3,col = "darkgreen",shade = TRUE,shade.col=t_col("green",70,"plot_rdt"),lwd = 4, lty=2,rug=FALSE)
rug(Fall_Growth_data$Annual_os_bt_anomaly, ticksize = 0.1, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topleft", inset=0.04, # position
       legend = c("Age 1"), col = c("darkgreen"),
       cex = 1.2,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
# second plot 
plot(age2_FALL, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Gulf Stream Index (Deg Lat)")), cex.lab=1.6,cex.axis=1.3,col="#000099",shade = TRUE,shade.col=t_col("#000099",40,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.75,1.75),ylim = c(-0.025,0.05),rug=FALSE) #plot age 2
rug(Fall_Growth_data$GSI12, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 3
plot(age3_FALL, select =1, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(-0.75,1.75),ylim = c(-0.025,0.05),rug=FALSE)
par(new = TRUE) #plot age 4
plot(age4_FALL, select =1, scale =0,ylab = "", xlab = "",col="#00cccc",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",75,"plot_cyan"),lwd = 4,lty=3,xlim = c(-0.75,1.75),ylim = c(-0.025,0.05),rug=FALSE)
legend("topleft", inset=0.04, # position
       legend = c("Age 2","Age 3","Age 4"),col = c("#000099","#FF0000","#00cccc"),
       cex = 1.2,lwd = c(4,4,4),lty = c(2,2,3),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
# third plot 
plot(age2_FALL, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Atlantic Multidecadal Oscillation (SST °C)")), cex.lab=1.6,cex.axis=1.3,col="#000099",shade = TRUE,shade.col=t_col("#000099",40,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.3,0.3),ylim = c(-0.05,0.05),rug=FALSE) #plot age 2
rug(Fall_Growth_data$AMO12, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 3
plot(age3_FALL, select =2, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(-0.3,0.3),ylim = c(-0.05,0.05),rug=FALSE)
par(new = TRUE) #plot age 4
plot(age4_FALL, select =2, scale =0,ylab = "", xlab = "",col="#00cccc",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",75,"plot_cyan"),lwd = 4,lty=3,xlim = c(-0.3,0.3),ylim = c(-0.05,0.05),rug=FALSE)
legend("topright", inset=0.04, # position
       legend = c("Age 2","Age 3","Age 4"),col = c("#000099","#FF0000","#00cccc"),
       cex = 1.2,lwd = c(4,4,4),lty = c(2,2,3),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
# Forth plot 
plot(age3_FALL, select =3, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("AM plaice SSB (kg/tow)")), cex.lab=1.6,cex.axis=1.3,col="#FF0000",shade = TRUE,shade.col=t_col("#FFADAD",50,"plot_red"),lwd = 4,lty=2,xlim = c(1,6),ylim = c(-0.05,0.075),rug=FALSE) #plot age 3
rug(Fall_Growth_data$Plaice_biomass, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 4
plot(age4_FALL, select =3, scale =0,ylab = "", xlab = "",col="#00cccc",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",70,"plot_cyan"),lwd = 4,lty=3,xlim = c(1,6),ylim = c(-0.05,0.075),rug=FALSE)
legend("topleft", inset=0.04, # position
       legend = c("Age 3","Age 4"),col = c("#FF0000","#00cccc"),
       cex = 1.2,lwd = c(4,4),lty = c(2,3),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border

##### PLOT FALL AGES 5-11+ ######
layout(matrix(1:3, ncol=1, byrow=FALSE))
par("mar"=c(4, 5, 1, 1))
#First Plot
plot.gam(age9_FALL, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("6 Month Mean Bottom Temperature Anomalies (°C)")), cex.lab=1.6,cex.axis=1.3,col = "darkgreen",shade = TRUE,shade.col=t_col("green",70,"plot_rdt"),lwd = 4, lty=2,rug=FALSE)
rug(Fall_Growth_data$month6_bt_anomaly, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topright", inset=0.04, # position
       legend = c("Age 9"), col = c("darkgreen"),
       cex = 1.2,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
# second plot 
plot(age5_FALL, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("AM plaice SSB (kg/tow)")), cex.lab=1.6,cex.axis=1.3,col="darkgray",shade = TRUE,shade.col=t_col("gray",40,"plot_gray"),lwd = 4,lty=2,xlim = c(1,6),ylim = c(-0.1,0.1),rug=FALSE) #plot age5
rug(Fall_Growth_data$Plaice_biomass, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topleft", inset=0.04, # position
       legend = c("Age 5"),col = c("darkgray"),
       cex = 1.2,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
# third plot 
plot(age5_FALL, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Atlantic Multidecadal Oscillation (SST °C)")), cex.lab=1.5,cex.axis=1.3,col="darkgray",shade = TRUE,shade.col=t_col("gray",10,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE) #plot age 5
rug(Fall_Growth_data$AMO12, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 6
plot(age6_FALL, select =1, scale =0,ylab = "", xlab = "",col="#FF0000",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#FFADAD",50,"plot_red"),lwd = 4,lty=2,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
par(new = TRUE) #plot age 7
plot(age7_FALL, select =1, scale =0,ylab = "", xlab = "",col="#00cccc",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("cyan",90,"plot_cyan"),lwd = 4,lty=2,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
par(new = TRUE) #plot age 9
plot(age9_FALL, select =1, scale =0,ylab = "", xlab = "",col="darkgreen",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#00cc00",85,"plot_green"),lwd = 4,lty=3,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
rug(Fall_Growth_data$AMO6, ticksize = 0.05, side = 1, lwd = 3, col = "green")
par(new = TRUE) #plot age 10
plot(age10_FALL, select =1, scale =0,ylab = "", xlab = "",col="#000099",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#000099",40,"plot_blue"),lwd = 4,lty=3,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
par(new = TRUE) #plot age 11+
plot(age11plus_FALL, select =1, scale =0,ylab = "", xlab = "",col="orange",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#ff751a",60,"plot_orange"),lwd = 4,lty=3,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
legend("topright", inset=0.04, # position
       legend = c("Age 5","Age 6","Age 7","Age 9","Age 10","Age 11+"),col = c("darkgray","#FF0000","#00cccc","darkgreen","#000099","orange"),
       cex = 1.2,lwd = c(4,4,4,4,4,4),lty = c(2,2,2,3,3,3),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border


##### PLOT SPRING AGES 2-4 ######
layout(matrix(1:2, ncol=1, byrow=FALSE))
par("mar"=c(4, 5, 1.5, 1))
plot(age2_SPRING, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Atlantic Multidecadal Oscillation (SST °C)")), cex.lab=1.3,cex.axis=1.0,col="#000099",shade = TRUE,shade.col=t_col("#000099",40,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.3,0.3),ylim = c(-0.05,0.05),rug=FALSE,
     main= "Ages 2-4 Spring GAM Response Curves") #plot age 2
rug(Spring_Growth_data$AMO12, ticksize = 0.1, side = 1, lwd = 2.3, col = "black")
rug(Spring_Growth_data$AMO6, ticksize = 0.07, side = 1, lwd = 2.5, col = "#000099")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 3
plot(age3_SPRING, select =2, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(-0.3,0.3),ylim = c(-0.05,0.05),rug=FALSE)
par(new = TRUE) #plot age 4
plot(age4_SPRING, select =2, scale =0,ylab = "", xlab = "",col="#00cccc",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",75,"plot_cyan"),lwd = 4,lty=3,xlim = c(-0.3,0.3),ylim = c(-0.05,0.05),rug=FALSE)
legend("topright", inset=0.04, # position
       legend = c("Age 2","Age 3","Age 4"),col = c("#000099","#FF0000","#00cccc"),
       cex = 1.0,lwd = c(4,4,4),lty = c(2,2,3),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
# second plot 
plot(age2_SPRING, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Gulf Stream Index (Deg Lat)")), cex.lab=1.3,cex.axis=1.0,col="#000099",shade = TRUE,shade.col=t_col("#000099",40,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.75,1.75),ylim = c(-0.03,0.05),rug=FALSE) #plot age 2
rug(Spring_Growth_data$GSI6, ticksize = 0.1, side = 1, lwd = 2.3, col = "black")
rug(Spring_Growth_data$GSI12, ticksize = 0.07, side = 1, lwd = 2.5, col = "red")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 3
plot(age3_SPRING, select =1, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(-0.75,1.75),ylim = c(-0.03,0.05),rug=FALSE)
par(new = TRUE) #plot age 4
plot(age4_SPRING, select =1, scale =0,ylab = "", xlab = "",col="#00cccc",axes = FALSE,shade = TRUE,
     shade.col=t_col("cyan",75,"plot_cyan"),lwd = 4,lty=3,xlim = c(-0.75,1.75),ylim = c(-0.03,0.05),rug=FALSE)
legend("topleft", inset=0.04, # position
       legend = c("Age 2","Age 3","Age 4"),col = c("#000099","#FF0000","#00cccc"),
       cex = 1.0,lwd = c(4,4,4),lty = c(2,2,3),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border


##### PLOT SPRING AGES 5-11+ ######
layout(matrix(1:4, ncol=1, byrow=FALSE))
par("mar"=c(4, 5, 1.5, 1))
#First Plot
plot(age5_SPRING, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Atlantic Multidecadal Oscillation (SST °C)")), cex.lab=1.3,cex.axis=1.3,col="#3b3b3b",shade = TRUE,shade.col=t_col("gray",10,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE,
     main="Spring Ages 5-11+ GAM Response Curves") #plot age 5
rug(Spring_Growth_data$AMO12, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 6
plot(age6_SPRING, select =2, scale =0,ylab = "", xlab = "",col="#FF0000",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#FFADAD",50,"plot_red"),lwd = 4,lty=2,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
par(new = TRUE) #plot age 7
plot(age7_SPRING, select =1, scale =0,ylab = "", xlab = "",col="#00cccc",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("cyan",90,"plot_cyan"),lwd = 4,lty=2,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
par(new = TRUE) #plot age 8
plot(age8_SPRING, select =1, scale =0,ylab = "", xlab = "",col="#ff33ff",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#ff33ff",90,"plot_purple"),lwd = 4,lty=2,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
par(new = TRUE) #plot age 9
plot(age9_SPRING, select =1, scale =0,ylab = "", xlab = "",col="darkgreen",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#00cc00",85,"plot_green"),lwd = 4,lty=3,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
rug(Spring_Growth_data$AMO6, ticksize = 0.05, side = 1, lwd = 3, col = "green")
par(new = TRUE) #plot age 10
plot(age10_SPRING, select =1, scale =0,ylab = "", xlab = "",col="#000099",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#000099",40,"plot_blue"),lwd = 4,lty=3,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
par(new = TRUE) #plot age 11+
plot(age11plus_SPRING, select =1, scale =0,ylab = "", xlab = "",col="orange",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#ff751a",60,"plot_orange"),lwd = 4,lty=3,xlim = c(-0.3,0.4),ylim = c(-0.3,0.4),rug=FALSE)
legend("topright", inset=0.04, # position
       legend = c("Age 5","Age 6","Age 7","Age 8","Age 9","Age 10","Age 11+"),col = c("#3b3b3b","#FF0000","#00cccc","#ff33ff","darkgreen","#000099","orange"),
       cex = 0.9,lwd = c(4,4,4,4,4,4,4),lty = c(2,2,2,2,3,3,3),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95",ncol=2) # border

# second plot 
plot(age5_SPRING, select =3, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("AM plaice SSB (kg/tow)")), cex.lab=1.3,cex.axis=1.3,col="#3b3b3b",shade = TRUE,shade.col=t_col("gray",30,"plot_gray"),lwd = 4,lty=2,xlim = c(0.5,6),ylim = c(-0.1,0.1),rug=FALSE) #plot age5
rug(Spring_Growth_data$Plaice_biomass, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0.0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 6
plot(age6_SPRING, select =3, scale =0,ylab = "", xlab = "",col="#FF0000",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#FFADAD",60,"plot_red"),lwd = 4,lty=2,xlim = c(0.5,6),ylim = c(-0.1,0.1),rug=FALSE)
legend("topleft", inset=0.04, # position
       legend = c("Age 5","Age 6"),col = c("#3b3b3b","#FF0000"),
       cex = 1.1,lwd = c(4,4),lty = c(2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border

# third plot 
plot(age5_SPRING, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Gulf Stream Index (Deg Lat)")), cex.lab=1.3,cex.axis=1.3,col="#3b3b3b",shade = TRUE,shade.col=t_col("gray",40,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.75,1.75),ylim = c(-0.03,0.1),rug=FALSE) #plot age 5
rug(Spring_Growth_data$GSI12, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 6
plot(age6_SPRING, select =1, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(-0.75,1.75),ylim = c(-0.03,0.1),rug=FALSE)
legend("topleft", inset=0.04, # position
       legend = c("Age 5","Age 6"),col = c("#3b3b3b","#FF0000"),
       cex = 1.2,lwd = c(4,4,4),lty = c(2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#fourth plot
plot.gam(age10_SPRING, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Annual Mean Bottom Temperature Anomalies (°C)")), cex.lab=1.3,cex.axis=1.3,col = "#000099",shade = TRUE,shade.col=t_col("#000099",40,"plot_rdt"),lwd = 4, lty=2,xlim = c(-0.4,0.5),ylim = c(-0.15,0.2),rug=FALSE)
rug(Spring_Growth_data$Annual_mf_bt_anomaly, ticksize = 0.07, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 11+
plot(age11plus_SPRING, select =2, scale =0,ylab = "", xlab = "",col="orange",cex.axis=1.3,shade = TRUE,
     shade.col=t_col("#ff751a",60,"plot_orange"),lwd = 4,lty=3,xlim = c(-0.4,0.5),ylim = c(-0.15,0.2),rug=FALSE)
legend("topright", inset=0.04, # position
       legend = c("Age 10","Age 11+"), col = c("#000099","orange"),
       cex = 1.2,lwd = c(4,4),lty = c(2,3),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border

