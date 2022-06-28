###### American plaice stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr) 
here()
#### load .csv files that were created in "make_dfs_for_plaice_recruitment.R" file#####
AGE1ft.df<-read.csv(here("data/recruitment_dfs/Fall_recruitment_1.csv"), header=TRUE, stringsAsFactors=FALSE)
AGE1st.df<-read.csv(here("data/recruitment_dfs/Spring_recruitment_1.csv"), header=TRUE, stringsAsFactors=FALSE)


### clipped regional temperature data#########
load(here("data/fvcom_SST/month4avg/m4_clip/m4_sst_clip.RData")) #4 month SST averages (March-June)
load(here("data/fvcom_bottom_temp/month6avg/m6_clip/m6_bottomtemp_clip.RData")) #6-month bottom temp averages (Match-August)
load(here("data/fvcom_bottom_temp/yearly_avg/mf_yearly_clip/ALLmf_yearly_bottomtemp_clip.RData")) #Yearly bt averages (March (year-1) - February (year) = year prior to start of spring survey)
load(here("data/fvcom_bottom_temp/yearly_avg/os_yearly_clip/ALLos_yearly_bottomtemp_clip.RData"))
###### Anomaly Base Period########
### using 1981-2010 as baseline anomaly period as NOAA does####

Month6_bp<-colMeans(m6_bottomtemp_clip[4:33,2])
Annual_mf_bp<-colMeans(ALLmf_yearly_bottomtemp_clip[4:33,2])
Annual_os_bp<-colMeans(ALLos_yearly_bottomtemp_clip[4:33,2])
Month4_SST_bp<-colMeans(m4_sst_clip[4:33,2])

rm(m6_bottomtemp_clip,m4_sst_clip,ALLos_yearly_bottomtemp_clip,ALLmf_yearly_bottomtemp_clip)
#####
##### Calculate temperature anomaly columns#####
#Fall
AGE1ft.df$month6_bt_anomaly<- AGE1ft.df$month6_avg_bottom_temp - Month6_bp
AGE1ft.df$Annual_os_bt_anomaly<- AGE1ft.df$yearly_bt - Annual_os_bp
AGE1ft.df$month4_sst_anomaly<- AGE1ft.df$month4_avg_sst - Month4_SST_bp
#Spring
AGE1st.df$month6_bt_anomaly<- AGE1st.df$month6_avg_bottom_temp - Month6_bp
AGE1st.df$Annual_mf_bt_anomaly<- AGE1st.df$yearly_bt - Annual_mf_bp
AGE1st.df$month4_sst_anomaly<- AGE1st.df$month4_avg_sst - Month4_SST_bp


##### Remove absolute temperature columns ####
AGE1ft.df <- subset (AGE1ft.df, select = -c(month4_avg_sst,yearly_bt,month6_avg_bottom_temp))
colnames(AGE1ft.df)[1] <- "Year"
AGE1st.df <- subset (AGE1st.df, select = -c(month4_avg_sst,yearly_bt,month6_avg_bottom_temp))
colnames(AGE1st.df)[1] <- "Year"
#######
####### Check Outliars######
summary(AGE1ft.df)
summary(AGE1st.df)

#SPRING AGE 1
par(mar=c(2,2,0,0), mfrow=c(2,5))
dotchart(AGE1st.df$Spring_numtow_Age1)
dotchart(AGE1st.df$GSI6)
dotchart(AGE1st.df$GSI12)
dotchart(AGE1st.df$NAO6)
dotchart(AGE1st.df$NAO12)
dotchart(AGE1st.df$AMO6)
dotchart(AGE1st.df$AMO12)
dotchart(AGE1st.df$month4_sst_anomaly)
dotchart(AGE1st.df$month6_bt_anomaly)
dotchart(AGE1st.df$Annual_mf_bt_anomaly)

#Fall age 1
par(mar=c(2,2,0,0), mfrow=c(2,5))
dotchart(AGE1ft.df$Fall_numtow_Age1)
dotchart(AGE1ft.df$GSI6)
dotchart(AGE1ft.df$GSI12)
dotchart(AGE1ft.df$NAO6)
dotchart(AGE1ft.df$NAO12)
dotchart(AGE1ft.df$AMO6)
dotchart(AGE1ft.df$AMO12)
dotchart(AGE1ft.df$month4_sst_anomaly)
dotchart(AGE1ft.df$month6_bt_anomaly)
dotchart(AGE1ft.df$Annual_os_bt_anomaly)

##########
######Boxplots######
view_boxplot_fun<- function (data){
  layout(matrix(1:12, ncol=6, byrow=TRUE))
  boxplot(data[2],varwidth = TRUE, xlab = "number/tow", ylab = "Numbers per Tow", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[3],varwidth = TRUE, xlab = "GSI6", ylab = "Latitude anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[6],varwidth = TRUE, xlab = "GSI12", ylab = "Latitude anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[5],varwidth = TRUE, xlab = "AMO6", ylab = "SST Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[8],varwidth = TRUE, xlab = "AMO12", ylab = "SST Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[4],varwidth = TRUE, xlab = "NAO6", ylab = "SS level pressure difference Anomalies (hPa) ", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[7],varwidth = TRUE, xlab = "NAO12", ylab = "SS level pressure difference Anomalies (hPa) ", data = data,cex.lab=1.5, cex.axis=1.5)
  
  boxplot(data[10],varwidth = TRUE, xlab = "NAO 2 year lag", ylab = "SS level pressure difference Anomalies (hPa)", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[11],varwidth = TRUE, xlab = "6 month avg bt", ylab = "Bt Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[12],varwidth = TRUE, xlab = "Annual os bt", ylab = "Bt Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[13],varwidth = TRUE, xlab = "4 month sst", ylab = "SST Anomalies", data = data,cex.lab=1.5, cex.axis=1.5)
}
view_boxplot_fun(AGE1ft.df)
view_boxplot_fun(AGE1st.df)

##### check distribtuion ####
#SPRING AGE 1
par(mar=c(2,2,2,0), mfrow=c(2,6))
hist(AGE1st.df$Spring_numtow_Age1)
hist(AGE1st.df$GSI6)
hist(AGE1st.df$GSI12)
hist(AGE1st.df$NAO6)
hist(AGE1st.df$NAO12)
hist(AGE1st.df$NAO12_lag2)
hist(AGE1st.df$AMO6)
hist(AGE1st.df$AMO12)
hist(AGE1st.df$month4_sst_anomaly)
hist(AGE1st.df$month6_bt_anomaly)
hist(AGE1st.df$Annual_mf_bt_anomaly)
#Fall age 1
par(mar=c(2,2,2,0), mfrow=c(2,6))
hist(AGE1ft.df$Fall_numtow_Age1)
hist(AGE1ft.df$GSI6)
hist(AGE1ft.df$GSI12)
hist(AGE1ft.df$NAO6)
hist(AGE1ft.df$NAO12)
hist(AGE1ft.df$NAO12_lag2)
hist(AGE1ft.df$AMO6)
hist(AGE1ft.df$AMO12)
hist(AGE1ft.df$month4_sst_anomaly)
hist(AGE1ft.df$month6_bt_anomaly)
hist(AGE1ft.df$Annual_os_bt_anomaly)
#####shapiro test for normality#####
#if p >0.05, we can assume normality
#SPRING AGE 1
shapiro.test(AGE1st.df$Spring_numtow_Age1)
shapiro.test(AGE1st.df$GSI6)
shapiro.test(AGE1st.df$GSI12)
shapiro.test(AGE1st.df$NAO6)
shapiro.test(AGE1st.df$NAO12)
shapiro.test(AGE1st.df$NAO12_lag2)
shapiro.test(AGE1st.df$AMO6)
shapiro.test(AGE1st.df$AMO12)
shapiro.test(AGE1st.df$month4_sst_anomaly)
shapiro.test(AGE1st.df$month6_bt_anomaly)
shapiro.test(AGE1st.df$Annual_mf_bt_anomaly)
#Fall age 1
shapiro.test(AGE1ft.df$Fall_numtow_Age1)#not normal
shapiro.test(AGE1ft.df$GSI6)
shapiro.test(AGE1ft.df$GSI12)
shapiro.test(AGE1ft.df$NAO6)
shapiro.test(AGE1ft.df$NAO12)
shapiro.test(AGE1ft.df$NAO12_lag2)
shapiro.test(AGE1ft.df$AMO6)
shapiro.test(AGE1ft.df$AMO12)
shapiro.test(AGE1ft.df$month4_sst_anomaly)
shapiro.test(AGE1ft.df$month6_bt_anomaly)
shapiro.test(AGE1ft.df$Annual_os_bt_anomaly)
#spring age 2
shapiro.test(AGE2st.df$Spring_numtow_Age2)#not normal
shapiro.test(AGE2st.df$lagGSI2)
shapiro.test(AGE2st.df$AMO_yearly)
shapiro.test(AGE2st.df$month4_avg_sst)
shapiro.test(AGE2st.df$month6_avg_bottom_temp)#not normal
shapiro.test(AGE2st.df$NAO)
shapiro.test(AGE2st.df$NAO2)
shapiro.test(AGE2st.df$yearly_bt)#not normal
shapiro.test(AGE2st.df$month6_bt_anomaly)#not normal
shapiro.test(AGE2st.df$Annual_mf_bt_anomaly)#not normal
shapiro.test(AGE2st.df$month4_sst_anomaly)
#fall age 2
shapiro.test(AGE2ft.df$Fall_numtow_Age2)#not normal
shapiro.test(AGE2ft.df$lagGSI2)
shapiro.test(AGE2ft.df$AMO_yearly)#not normal
shapiro.test(AGE2ft.df$month4_avg_sst)
shapiro.test(AGE2ft.df$month6_avg_bottom_temp)#not normal
shapiro.test(AGE2ft.df$NAO)
shapiro.test(AGE2ft.df$NAO2)
shapiro.test(AGE2ft.df$yearly_bt)#not normal
shapiro.test(AGE2ft.df$month6_bt_anomaly)#not normal
shapiro.test(AGE2ft.df$Annual_os_bt_anomaly)#not normal
shapiro.test(AGE2ft.df$month4_sst_anomaly)

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

Mypairs(AGE1ft.df[c(3:8,10:13)]) #age1
Mypairs(AGE1st.df[c(3:8,10:13)]) #age1


###### CALCULATE RECRUITS/SPAWNER#######
### Dependent variable is recruitment survival success as denoted as recruit abundance in year t per spawner biomass in year t-1 (Rt/SSBt-1) as executed in Perretti et al (2017).
#load SSB data
Plaice_NEFSC_Biomass<-read.csv(here("data/recruitment_dfs/Plaice_NEFSC_Biomass.csv"), header=TRUE, stringsAsFactors=FALSE) #biomass is in kg/tow units
# add R/SSB column to dataframes

AGE1ft.df <- mutate(AGE1ft.df, RSSB = Fall_numtow_Age1 / lag(Plaice_NEFSC_Biomass[c(1:39),2]))
AGE1st.df <- mutate(AGE1st.df, RSSB = Spring_numtow_Age1 / lag(Plaice_NEFSC_Biomass[c(1:40),4]))

#### PLOT R/SSB COMPARING EACH SEASON #####
#FALL

plot(RSSB ~ Year, data=AGE1ft.df, main="Seasonal Plaice R/SSB Over Time", xlab="Year",ylab="R(year)/SSB(year-1)", type="b",pch=16,cex=1.2,lwd=2.5,col="darkblue", cex.lab=1.4,cex.axis=1.1)
abline(lm(AGE1ft.df$RSSB~AGE1ft.df$Year),col="blue",lwd=3)
lines(RSSB ~ Year, data=AGE1st.df, xlab="Year", type="b",pch=16,cex=1.2,col="darkred",lwd=2.5)
abline(lm(AGE1st.df$RSSB~AGE1st.df$Year),col="red",lwd=3)
legend(1980, 3.5, legend=c("Spring", "Fall"),
       col=c("red", "blue"), lty=1, lwd=3, cex=1.2)


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
##### Age 1 recruits (Fall tow) vs. potential environmental influences###########
FL_numtow<-gam((RSSB) ~ s(GSI6, k=4)+s(GSI12, k=4)+s(NAO6, k=4)+s(NAO12, k=4)+s(AMO6, k=4)+s(AMO12, k=4)+s(NAO12_lag2, k=4)+s(month6_bt_anomaly, k=4)+s(Annual_os_bt_anomaly, k=4)+s(month4_sst_anomaly, k=4), family=tw(),method = "REML",data=AGE1ft.df) # Build GAM with all possible variables
summary(FL_numtow) # Find significant variables based on p-value
FL_numtow$aic

#full model, duplicates removed:
FL_numtow<-gam((RSSB) ~ s(GSI6, k=5)+s(NAO12, k=5)+s(NAO12_lag2, k=5)+s(AMO6, k=5)+s(Annual_os_bt_anomaly, k=5)+s(month4_sst_anomaly, k=5), family=tw(),method = "REML",data=AGE1ft.df) # Build GAM with all possible variables
summary(FL_numtow) # Find significant variables based on p-value
FL_numtow$aic

FL_numtow<-gam((RSSB) ~ s(GSI6, k=5)+s(NAO12_lag2, k=5)+s(AMO6, k=5)+s(Annual_os_bt_anomaly, k=5), family=tw(),method = "REML",data=AGE1ft.df) # Build GAM with all possible variables
summary(FL_numtow) # Find significant variables based on p-value
FL_numtow$aic

#reduced model:
FL_numtow<-gam((RSSB) ~ s(GSI6, k=5)+s(AMO6, k=5), family=tw(),method = "REML",data=AGE1ft.df) # Build GAM with all possible variables
summary(FL_numtow) # Find significant variables based on p-value
FL_numtow$aic


###Plot GAM
layout(matrix(1:2, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(FL_numtow,AGE1ft.df$GSI6,AGE1ft.df$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on R/SSB",select1=1,title="Environmental Effects on Fall Mean R/SSB",position="bottomleft")
GAM_CURVE_FUN(FL_numtow,AGE1ft.df$AMO6,AGE1ft.df$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on R/SSB",select1=2,title=NULL,position="bottomleft")

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_numtow,pch=20, cex=1.2,cex.lab=1.5)


##### Age 1 recruits (Spring tow) vs. potential environmental influences##########
SP_numtow<-gam((RSSB) ~ s(GSI6, k=4)+s(GSI12, k=4)+s(NAO6, k=4)+s(NAO12, k=4)+s(AMO6, k=4)+s(AMO12, k=4)+s(NAO12_lag2, k=4)+s(month6_bt_anomaly, k=4)+s(Annual_mf_bt_anomaly, k=4)+s(month4_sst_anomaly, k=4), family=tw(),method = "REML",data=AGE1st.df) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic
##full model with duplicates removed:
SP_numtow<-gam((RSSB) ~ s(GSI6, k=5)+s(AMO12, k=5)+s(NAO12_lag2, k=5)+s(month6_bt_anomaly, k=5), family=tw(),method = "REML",data=AGE1st.df) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic
#reduced model:
SP_numtow<-gam((RSSB) ~ s(AMO12, k=5), family=tw(),method = "REML",data=AGE1st.df) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_numtow,pch=20, cex=1.2,cex.lab=1.5)

################################
###Plot GAM
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(SP_numtow,AGE1st.df$AMO12,AGE1st.df$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on R/SSB",select1=1,title="Environmental Effects on Spring Mean R/SSB",position="topleft")

##############
##### Make figures for RMarkdown#####
par(mar=c(4.5,4.5,1.5,1.5))
layout(matrix(1:4, ncol=2, byrow=TRUE))
qq.gam(FL_numtow, rep = 0, level = 0.9, type = "deviance", rl.col = 2, 
       rep.col = "gray80",pch=20, cex=1.2,cex.lab=1.2,main = "Fall QQ")
hist(residuals(FL_numtow, type ="deviance"), xlab = "Residuals", main = "Fall Histogram of residuals",cex.lab=1.2)
qq.gam(SP_numtow, rep = 0, level = 0.9, type ="deviance", rl.col = 2, 
       rep.col = "gray80",pch=20, cex=1.2,cex.lab=1.2,main = "Spring QQ")
hist(residuals(SP_numtow, type ="deviance"), xlab = "Residuals", main = "Spring Histogram of residuals",cex.lab=1.2)


####Combine spring and fall AMO curves
layout(matrix(1:1, ncol=1, byrow=TRUE))
par("mar"=c(4, 5, 1, 1))
plot.gam(FL_numtow, select =2, scale =0,ylab = expression(bold("PE on Mean R/SSB")), xlab = expression(bold("Atlantic Multidecadal Oscillation (°C SST)")), cex.lab=1.5,cex.axis=1.3,col = "#075fb8",shade = TRUE,shade.col=t_col("#075fb8",70,"plot_blt"),lwd = 4, lty=2,rug=FALSE,xlim = c(-0.32,0.32),ylim = c(-1.9,1.45))
rug(AGE1ft.df$AMO6, ticksize = 0.07, side = 1, lwd = 2.7, col = "blue")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot spring
plot(SP_numtow, select =1, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_blue"),lwd = 4,lty=2,xlim = c(-0.32,0.32),ylim = c(-1.9,1.45),rug=FALSE)
rug(AGE1st.df$AMO12, ticksize = 0.05, side = 1, lwd = 2.9, col = "#FF0000")
legend("topleft", inset=0.04, # position
       legend = c("Fall AMO (6-month Mean)","Spring AMO (Annual Mean)"), col = c("#075fb8","#FF0000"),
       cex = 1.3,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
