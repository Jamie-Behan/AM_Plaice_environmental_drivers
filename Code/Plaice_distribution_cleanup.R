###### American plaice stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr) 
here()
#### load .csv files that were created in "make_dfs_for_plaice_recruitment.R" file#####
Fall_distribution.df<-read_excel(here("data/distribution_dfs/Fall_distribution.xlsx"))
Fall_distribution.df$depth_Fall<-abs(Fall_distribution.df$depth_Fall)
Spring_distribution.df<-read_excel(here("data/distribution_dfs/Spring_distribution.xlsx"))
Spring_distribution.df$depth_Spring<-abs(Spring_distribution.df$depth_Spring)

### clipped regional temperature data#########
load(here("data/fvcom_bottom_temp/month6avg/m6_clip/m6_SP_bottomtemp_clip.RData")) #6-month bottom temp averages (September-February)
load(here("data/fvcom_bottom_temp/month6avg/m6_clip/m6_FL_bottomtemp_clip.RData")) #6-month bottom temp averages (April-September)
load(here("data/fvcom_bottom_temp/yearly_avg/mf_yearly_clip/ALLmf_yearly_bottomtemp_clip.RData")) #Yearly bt averages (March (year-1) - February (year) = year prior to start of spring survey)
load(here("data/fvcom_bottom_temp/yearly_avg/os_yearly_clip/ALLos_yearly_bottomtemp_clip.RData"))
###### Anomaly Base Period########
### using 1981-2010 as baseline anomaly period as NOAA does####

Month6_FL_bp<-colMeans(m6_FL_bottomtemp_clip[3:32,2])
Month6_SP_bp<-colMeans(m6_SP_bottomtemp_clip[3:32,2])
Annual_mf_bp<-colMeans(ALLmf_yearly_bottomtemp_clip[4:33,2])
Annual_os_bp<-colMeans(ALLos_yearly_bottomtemp_clip[4:33,2])
#####
##### Calculate temperature anomaly columns#####

#Fall
temp<-m6_FL_bottomtemp_clip[2:41,2]
Fall_distribution.df$month6_bt_anomaly<- temp$Avg_bottom_temp  - Month6_FL_bp
temp<-ALLos_yearly_bottomtemp_clip[2:41,2]
Fall_distribution.df$Annual_os_bt_anomaly<- temp$Avg_bottom_temp  - Annual_os_bp

#Spring
temp<-m6_SP_bottomtemp_clip[2:41,2]
Spring_distribution.df$month6_bt_anomaly<- temp$Avg_bottom_temp - Month6_SP_bp
temp<-ALLmf_yearly_bottomtemp_clip[2:41,2]
Spring_distribution.df$Annual_mf_bt_anomaly<- temp$Avg_bottom_temp - Annual_mf_bp


rm(m6_FL_bottomtemp_clip,m6_SP_bottomtemp_clip,ALLos_yearly_bottomtemp_clip,ALLmf_yearly_bottomtemp_clip,temp)

####### Check Outliars######
summary(Fall_distribution.df)
summary(Spring_distribution.df)

#SPRING AGE 1
par(mar=c(2,2,0,0), mfrow=c(2,5))
dotchart(Spring_distribution.df$Spring_numtow_Age1)
dotchart(Spring_distribution.df$GSI6)
dotchart(Spring_distribution.df$GSI12)
dotchart(Spring_distribution.df$NAO6)
dotchart(Spring_distribution.df$NAO12)
dotchart(Spring_distribution.df$AMO6)
dotchart(Spring_distribution.df$AMO12)
dotchart(Spring_distribution.df$month4_sst_anomaly)
dotchart(Spring_distribution.df$month6_bt_anomaly)
dotchart(Spring_distribution.df$Annual_mf_bt_anomaly)

#Fall age 1
par(mar=c(2,2,0,0), mfrow=c(2,5))
dotchart(Fall_distribution.df$Fall_numtow_Age1)
dotchart(Fall_distribution.df$GSI6)
dotchart(Fall_distribution.df$GSI12)
dotchart(Fall_distribution.df$NAO6)
dotchart(Fall_distribution.df$NAO12)
dotchart(Fall_distribution.df$AMO6)
dotchart(Fall_distribution.df$AMO12)
dotchart(Fall_distribution.df$month4_sst_anomaly)
dotchart(Fall_distribution.df$month6_bt_anomaly)
dotchart(Fall_distribution.df$Annual_os_bt_anomaly)

##########
#PLOT Seasonal Depth changes together on same plot
layout(matrix(1:2, ncol=1, byrow=TRUE))
par("mar"=c(4, 5, 1.5, 1.5))
plot(depth_Fall ~ Year, data=Fall_distribution.df, main="Mean Depth of Occurance Overtime", xlab="Year",ylab="Mean Depth", type="b",pch=16,cex=1.2,col="darkblue",ylim= c(170,130), cex.lab=1.4,cex.axis=1.1)
abline(lm(Fall_distribution.df$depth_Fall~Fall_distribution.df$Year),col="blue",lwd=3)
lines(depth_Spring ~ Year, data=Spring_distribution.df, xlab="Year", type="b",pch=16,cex=1.2,col="darkred")
abline(lm(Spring_distribution.df$depth_Spring~Spring_distribution.df$Year),col="red",lwd=3)
legend(1985, 160, legend=c("Spring", "Fall"),
       col=c("red", "blue"), lty=1,lwd=3, cex=1.0)
#PLOT Seasonal Latitude changes together on same plot
plot(Lat_Fall ~ Year, data=Fall_distribution.df, main="Mean Latitude of Occurance Overtime", xlab="Year",ylab="Mean Latitude", type="b",pch=16,cex=1.2,col="darkblue", cex.lab=1.4,cex.axis=1.1)
abline(lm(Fall_distribution.df$Lat_Fall~Fall_distribution.df$Year),col="blue",lwd=3)
lines(Lat_Spring ~ Year, data=Spring_distribution.df, xlab="Year", type="b",pch=16,cex=1.2,col="darkred")
abline(lm(Spring_distribution.df$Lat_Spring~Spring_distribution.df$Year),col="red",lwd=3)
legend(1985, 42.7, legend=c("Spring", "Fall"),
       col=c("red", "blue"), lty=1,lwd=3, cex=1.0)
######Boxplots######
view_boxplot_fun<- function (data){
  layout(matrix(1:12, ncol=6, byrow=TRUE))
  boxplot(data[8],varwidth = TRUE, xlab = "Avg Depth", ylab = "Meters", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[9],varwidth = TRUE, xlab = "Avg Latitude", ylab = "Degrees Lat", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[2],varwidth = TRUE, xlab = "GSI6", ylab = "Latitude anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[3],varwidth = TRUE, xlab = "GSI12", ylab = "Latitude anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[4],varwidth = TRUE, xlab = "AMO6", ylab = "SST Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[7],varwidth = TRUE, xlab = "AMO12", ylab = "SST Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[3],varwidth = TRUE, xlab = "NAO6", ylab = "SS level pressure difference Anomalies (hPa) ", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[6],varwidth = TRUE, xlab = "NAO12", ylab = "SS level pressure difference Anomalies (hPa) ", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[11],varwidth = TRUE, xlab = "6 month avg bt", ylab = "Bt Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[12],varwidth = TRUE, xlab = "Annual os bt", ylab = "Bt Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[10],varwidth = TRUE, xlab = "SSB", ylab = "kg/tow", data = data,cex.lab=1.5, cex.axis=1.5)
}
view_boxplot_fun(Fall_distribution.df)
view_boxplot_fun(Spring_distribution.df)

##### check distribtuion ####
#SPRING AGE 1
par(mar=c(2,2,2,0), mfrow=c(2,6))
hist(Spring_distribution.df$depth_Spring)
hist(Spring_distribution.df$depth_Spring)
hist(Spring_distribution.df$GSI6)
hist(Spring_distribution.df$GSI12)
hist(Spring_distribution.df$NAO6)
hist(Spring_distribution.df$NAO12)
hist(Spring_distribution.df$AMO6)
hist(Spring_distribution.df$AMO12)
hist(Spring_distribution.df$SSB)
hist(Spring_distribution.df$month6_bt_anomaly)
hist(Spring_distribution.df$Annual_mf_bt_anomaly)
#Fall age 1
par(mar=c(2,2,2,0), mfrow=c(2,6))
hist(Fall_distribution.df$depth_Fall)
hist(Fall_distribution.df$Lat_Fall)
hist(Fall_distribution.df$GSI6)
hist(Fall_distribution.df$GSI12)
hist(Fall_distribution.df$NAO6)
hist(Fall_distribution.df$NAO12)
hist(Fall_distribution.df$AMO6)
hist(Fall_distribution.df$AMO12)
hist(Fall_distribution.df$SSB)
hist(Fall_distribution.df$month6_bt_anomaly)
hist(Fall_distribution.df$Annual_os_bt_anomaly)
#####shapiro test for normality#####
#if p >0.05, we can assume normality
#SPRING AGE 1
shapiro.test(Spring_distribution.df$depth_Spring)
shapiro.test(Spring_distribution.df$depth_Spring)
shapiro.test(Spring_distribution.df$GSI6)
shapiro.test(Spring_distribution.df$GSI12)
shapiro.test(Spring_distribution.df$NAO6)
shapiro.test(Spring_distribution.df$NAO12)
shapiro.test(Spring_distribution.df$AMO6)
shapiro.test(Spring_distribution.df$AMO12)
shapiro.test(Spring_distribution.df$SSB)
shapiro.test(Spring_distribution.df$month6_bt_anomaly)
shapiro.test(Spring_distribution.df$Annual_mf_bt_anomaly)
#Fall age 1
shapiro.test(Fall_distribution.df$depth_Spring)
shapiro.test(Fall_distribution.df$depth_Spring)
shapiro.test(Fall_distribution.df$GSI6)
shapiro.test(Fall_distribution.df$GSI12)
shapiro.test(Fall_distribution.df$NAO6)
shapiro.test(Fall_distribution.df$NAO12)
shapiro.test(Fall_distribution.df$AMO6)
shapiro.test(Fall_distribution.df$AMO12)
shapiro.test(Fall_distribution.df$SSB)
shapiro.test(Fall_distribution.df$month6_bt_anomaly)
shapiro.test(Fall_distribution.df$Annual_os_bt_anomaly)

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

Mypairs(Fall_distribution.df[c(2:7,10:12)]) #age1
Mypairs(Spring_distribution.df[c(2:7,10:12)]) #age1


#PLOT Seasonal Depth changes together on same plot
plot(depth_Fall ~ Year, data=Fall_distribution.df, main="Mean Depth of Occurance Overtime", xlab="Year",ylab="Mean Depth", type="b",pch=16,cex=1.0,col="darkblue",ylim= c(-170,-130))
abline(lm(Fall_distribution.df$depth_Fall~Fall_distribution.df$Year),col="blue",lwd=2)
lines(depth_Spring ~ Year, data=Spring_distribution.df, xlab="Year", type="b",pch=16,cex=1.0,col="darkred")
abline(lm(Spring_distribution.df$depth_Spring~Spring_distribution.df$Year),col="red",lwd=2)
legend(1985, -160, legend=c("Spring", "Fall"),
       col=c("red", "blue"), lty=1, cex=1.0)
#PLOT Seasonal Latitude changes together on same plot
plot(Lat_Fall ~ Year, data=Fall_distribution.df, main="Mean Latitude of Occurance Overtime", xlab="Year",ylab="Mean Latitude", type="b",pch=16,cex=1.0,col="darkblue")
abline(lm(Fall_distribution.df$Lat_Fall~Fall_distribution.df$Year),col="blue",lwd=2)
lines(Lat_Spring ~ Year, data=Spring_distribution.df, xlab="Year", type="b",pch=16,cex=1.0,col="darkred")
abline(lm(Spring_distribution.df$Lat_Spring~Spring_distribution.df$Year),col="red",lwd=2)
legend(1985, 42.7, legend=c("Spring", "Fall"),
       col=c("red", "blue"), lty=1, cex=1.0)


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
###### EXPLORATORY GAMs (March/October (year-1) - February/September (year))#######
##### DEPTH (Fall) vs. potential environmental influences###########
FL_Depth<-gam((depth_Fall) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Fall_distribution.df) # Build GAM with all possible variables
summary(FL_Depth) # Find significant variables based on p-value
FL_Depth$aic

#full model, duplicates removed:
FL_Depth<-gam((depth_Fall) ~ s(GSI12, k=5)+s(NAO12, k=5)+s(AMO12, k=5)+s(month6_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Fall_distribution.df) # Build GAM with all possible variables
summary(FL_Depth) # Find significant variables based on p-value
FL_Depth$aic

#reduced model:
FL_Depth<-gam((depth_Fall) ~ s(NAO12, k=5)+s(month6_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Fall_distribution.df) # Build GAM with all possible variables
summary(FL_Depth) # Find significant variables based on p-value
FL_Depth$aic


###Plot GAM
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(FL_Depth,Fall_distribution.df$NAO12,Fall_distribution.df$Year,x_lab="North Atlantic Oscillation (hPa)",y_lab="PE on Mean Depth",select1=1,title="Environmental Effects on Fall Mean Depth",position="bottomleft")
GAM_CURVE_FUN(FL_Depth,Fall_distribution.df$month6_bt_anomaly,Fall_distribution.df$Year,x_lab="6 Month Mean Bottom Temperature Anomlay",y_lab="PE on Mean Depth",select1=2,title=NULL,position="topleft")
GAM_CURVE_FUN(FL_Depth,Fall_distribution.df$SSB,Fall_distribution.df$Year,x_lab="SSB (kg/tow)",y_lab="PE on Mean Depth",select1=3,title=NULL,position="topleft")

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_Depth,pch=20, cex=1.2,cex.lab=1.5)


##### DEPTH (Spring tow) vs. potential environmental influences##########
SP_Depth<-gam((depth_Spring) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Spring_distribution.df) # Build GAM with all possible variables
summary(SP_Depth) # Find significant variables based on p-value
SP_Depth$aic
##full model with duplicates removed:
SP_Depth<-gam((depth_Spring) ~ s(GSI12, k=5)+s(NAO6, k=5)+s(AMO6, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Spring_distribution.df) # Build GAM with all possible variables
summary(SP_Depth) # Find significant variables based on p-value
SP_Depth$aic
#reduced model:
SP_Depth<-gam((depth_Spring) ~ s(NAO6, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Spring_distribution.df) # Build GAM with all possible variables
summary(SP_Depth) # Find significant variables based on p-value
SP_Depth$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_Depth,pch=20, cex=1.2,cex.lab=1.5)

##### LATITUDE (Fall) vs. potential environmental influences###########
FL_Lat<-gam((Lat_Fall) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_os_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Fall_distribution.df) # Build GAM with all possible variables
summary(FL_Lat) # Find significant variables based on p-value
FL_Lat$aic

#full model, duplicates removed:
FL_Lat<-gam((Lat_Fall) ~ s(GSI6, k=5)+s(NAO6, k=5)+s(AMO6, k=5)+s(Annual_os_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Fall_distribution.df) # Build GAM with all possible variables
summary(FL_Lat) # Find significant variables based on p-value
FL_Lat$aic

#reduced model:
FL_Lat<-gam((Lat_Fall) ~ s(AMO6, k=5)+s(Annual_os_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Fall_distribution.df) # Build GAM with all possible variables
summary(FL_Lat) # Find significant variables based on p-value
FL_Lat$aic


###Plot GAM
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(FL_Lat,Fall_distribution.df$AMO6,Fall_distribution.df$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on Mean Latitude",select1=1,title="Environmental Effects on Fall Mean Latitude",position="topleft")
GAM_CURVE_FUN(FL_Lat,Fall_distribution.df$Annual_os_bt_anomaly,Fall_distribution.df$Year,x_lab="Annual Mean Bottom Temperature Anomlay",y_lab="PE on Mean Latitude",select1=2,title=NULL,position="topleft")
GAM_CURVE_FUN(FL_Lat,Fall_distribution.df$SSB,Fall_distribution.df$Year,x_lab="SSB (kg/tow)",y_lab="PE on Mean Latitude",select1=3,title=NULL,position="topleft")

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_Lat,pch=20, cex=1.2,cex.lab=1.5)


##### Latitude (Spring tow) vs. potential environmental influences##########
SP_numtow<-gam((Lat_Spring) ~ s(GSI6, k=5)+s(GSI12, k=5)+s(NAO6, k=5)+s(NAO12, k=5)+s(AMO6, k=5)+s(AMO12, k=5)+s(month6_bt_anomaly, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Spring_distribution.df) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic
##full model with duplicates removed:
SP_numtow<-gam((Lat_Spring) ~ s(GSI12, k=5)+s(NAO6, k=5)+s(AMO12, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Spring_distribution.df) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic
#reduced model:
SP_numtow<-gam((Lat_Spring) ~ s(NAO6, k=5)+s(Annual_mf_bt_anomaly, k=5)+s(SSB, k=5), family=tw(),method = "REML",data=Spring_distribution.df) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_numtow,pch=20, cex=1.2,cex.lab=1.5)

###Plot GAM
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(SP_numtow,Spring_distribution.df$NAO6,Spring_distribution.df$Year,x_lab="North Atlantic Oscillation (hPa)",y_lab="PE on Mean Latitude",select1=1,title="Environmental Effects on Spring Mean Latitude",position="topleft")
GAM_CURVE_FUN(SP_numtow,Spring_distribution.df$Annual_mf_bt_anomaly,Spring_distribution.df$Year,x_lab="Annual Mean Bottom Temperature Anomlay",y_lab="PE on Mean Latitude",select1=2,title=NULL,position="topleft")
GAM_CURVE_FUN(SP_numtow,Spring_distribution.df$SSB,Spring_distribution.df$Year,x_lab="SSB (kg/tow)",y_lab="PE on Mean Latitude",select1=3,title=NULL,position="topleft")
##############
##### Make figures for RMarkdown#####
par(mar=c(4.5,4.5,1.5,1.5))
layout(matrix(1:6, ncol=2, byrow=TRUE))
qq.gam(FL_Depth, rep = 0, level = 0.9, type = "deviance", rl.col = 2, 
       rep.col = "gray80",pch=20, cex=1.2,cex.lab=1.2,main = "Fall Depth QQ")
hist(residuals(FL_Depth, type ="deviance"), xlab = "Residuals", main = "Fall Depth Histogram of residuals",cex.lab=1.2)
qq.gam(FL_Lat, rep = 0, level = 0.9, type = "deviance", rl.col = 2, 
       rep.col = "gray80",pch=20, cex=1.2,cex.lab=1.2,main = "Fall Latitude QQ")
hist(residuals(FL_Lat, type ="deviance"), xlab = "Residuals", main = "Fall Latitude Histogram of residuals",cex.lab=1.2)
qq.gam(SP_numtow, rep = 0, level = 0.9, type ="deviance", rl.col = 2, 
       rep.col = "gray80",pch=20, cex=1.2,cex.lab=1.2,main = "Spring Latitude QQ")
hist(residuals(SP_numtow, type ="deviance"), xlab = "Residuals", main = "Spring Latitude Histogram of residuals",cex.lab=1.2)

##### Combine NAO and bottom temp curves for Fall Depth ####
layout(matrix(1:3, ncol=1, byrow=TRUE))
par("mar"=c(4, 5, 1, 1))
plot.gam(FL_Depth, select =1, scale =0,ylab = expression(bold("PE on Mean Depth")), xlab = expression(bold("Mean Anomalies")), cex.lab=1.6,cex.axis=1.3,col = "#075fb8",shade = TRUE,shade.col=t_col("#075fb8",70,"plot_blt"),lwd = 4, lty=2,rug=FALSE,xlim = c(-1.025,1.025),ylim = c(-0.1,0.1))
rug(Fall_distribution.df$NAO12, ticksize = 0.07, side = 1, lwd = 2.7, col = "blue")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot spring
plot(FL_Depth, select =2, scale =0,ylab = "", xlab = "",col="#0603cD",axes = FALSE,shade = TRUE,
     shade.col=t_col("#0603cD",45,"plot_blue"),lwd = 4,lty=2,xlim = c(-1.025,1.025),ylim = c(-0.1,0.1),rug=FALSE)
rug(Spring_distribution.df$month6_bt_anomaly, ticksize = 0.05, side = 1, lwd = 2.9, col = "#0603cD")
legend("topleft", inset=0.04, # position
       legend = c("Fall Annual NAO Anomaly (hPa)","Fall 6-Month Bt Anomaly (°C)"), col = c("#075fb8","#0603cD"),
       cex = 1.3,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#### Second plot
plot.gam(FL_Lat, select =2, scale =0,ylab = expression(bold("PE on Mean Latitude")), xlab = expression(bold("Annual Mean Bottom Temperature Anomalies (°C)")), cex.lab=1.6,cex.axis=1.6,col = "#075fb8",shade = TRUE,shade.col=t_col("#075fb8",70,"plot_blt"),lwd = 4, lty=2,rug=FALSE,xlim = c(-0.55,1.4),ylim = c(-0.0025,0.009))
rug(Fall_distribution.df$Annual_os_bt_anomaly, ticksize = 0.07, side = 1, lwd = 2.7, col = "blue")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot spring
plot(SP_numtow, select =2, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(-0.55,1.4),ylim = c(-0.0025,0.009),rug=FALSE)
rug(Spring_distribution.df$Annual_mf_bt_anomaly, ticksize = 0.05, side = 1, lwd = 2.9, col = "#FF0000")
legend("topleft", inset=0.04, # position
       legend = c("Fall","Spring"), col = c("#075fb8","#FF0000"),
       cex = 1.3,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#### Third plot

plot.gam(FL_Lat, select =3, scale =0,ylab = expression(bold("PE on Mean Latitude")), xlab = expression(bold("Am Plaice Mean SSB (kg/tow)")), cex.lab=1.6,cex.axis=1.3,col = "#075fb8",shade = TRUE,shade.col=t_col("#075fb8",70,"plot_blt"),lwd = 4, lty=2,rug=FALSE,xlim = c(0.7,6),ylim = c(-0.0025,0.009))
rug(Fall_distribution.df$SSB, ticksize = 0.07, side = 1, lwd = 2.7, col = "blue")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot spring
plot(SP_numtow, select =3, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(0.7,6),ylim = c(-0.0025,0.009),rug=FALSE)
rug(Spring_distribution.df$SSB, ticksize = 0.05, side = 1, lwd = 2.9, col = "#FF0000")
legend("topleft", inset=0.04, # position
       legend = c("Fall","Spring"), col = c("#075fb8","#FF0000"),
       cex = 1.3,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
 
