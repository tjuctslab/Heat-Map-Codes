install.packages('"data.table"')

#Create data.table
library(data.table)
thoracic_data<-data.table(thoracic_data)

#Heart only recipients 
HTx <- thoracic_data[which(thoracic_data$ORGAN == "HR")]

HTd <- thoracic_data$TX_DATE

#Select for First Tx

HTx <- HTx[which(HTx$NUM_PREV_TX == 0)]

#Select for Heart Only Tx 

HTx <- HTx[which(HTx$MULTIORG != "Y")]

#Calculate Time to Follow up

HTx$TTF<- difftime(HTx$PX_STAT_DATE,HTx$TX_DATE,units = "days")


#Retransplant sensorying event 

HTx$PX_STAT_CENSOR <- NA 

HTx$PX_STAT_CENSOR[which(HTx$PX_STAT == "A")]<- 0
HTx$PX_STAT_CENSOR[which(HTx$PX_STAT == "L")]<- 0
HTx$PX_STAT_CENSOR[which(HTx$PX_STAT == "R")]<- 0
HTx$PX_STAT_CENSOR[which(HTx$PX_STAT == "D")]<- 1

#Adult Only
HTx<-HTx[which(HTx$AGE >= 18)]

# calculate weight change
HTx$ListingWeight <- HTx$INIT_WGT_KG_CALC
HTx$RemovalWeight <- HTx$END_WGT_KG_CALC

HTx$WeightChange<- as.numeric(HTx$END_WGT_KG_CALC-HTx$INIT_WGT_KG_CALC)

#calculate BMI change
HTx$BMIChange <- as.numeric(HTx$END_BMI_CALC - HTx$INIT_BMI_CALC)

#BMI at listing
#HTx$BMI_L <- HTx$BMI_TCR
HTx$BMI_L <- as.numeric(HTx$INIT_BMI_CALC)

#BMI at transplant
HTx$BMI_T <- as.numeric(HTx$END_BMI_CALC)


HTx$one_year_surv <- HTx$PX_STAT_CENSOR
#Create blank column and everyone alive beyond 365 is a 'zero'
HTx$one_year_surv[which(HTx$TTF >365)] <- '0'
#everyone with TTF less than 1 year just label deaths as 1 and being alive as 0
HTx$one_year_surv[which(HTx$TTF <=365 & HTx$PX_STAT_CENSOR == '1')] <- '1'
HTx$one_year_surv[which(HTx$TTF <=365 & HTx$PX_STAT_CENSOR == '0')] <- '0'
#Create a new TTF variable so that everyone with a TTF greater than 1 year is labeled for max follow up at 1 year. Then for those with TTF less than 1 year just make sure their TTF goes over to new variable 
HTx$TTF_one_year <- HTx$TTF
HTx$TTF_one_year[which(HTx$TTF > 365)] <- 365
HTx$TTF_one_year[which(HTx$one_year_surv == '1')] <- HTx$TTF[which(HTx$one_year_surv == '1')]
HTx$TTF_one_year[which(HTx$one_year_surv == '0' & HTx$TTF <= 365)] <- HTx$TTF[which(HTx$one_year_surv == 0 & HTx$TTF <=365)]


HTx<- data.table(PT_CODE=HTx$PT_CODE,PX_STAT_CENSOR=HTx$PX_STAT_CENSOR, TTF=HTx$TTF, AGE=HTx$AGE,AGE_DON=HTx$AGE_DON, GENDER=HTx$GENDER,
                 GENDER_DON=HTx$GENDER_DON, HGT_CM_CALC=HTx$HGT_CM_CALC, HGT_CM_DON_CALC=HTx$HGT_CM_DON_CALC, WGT_KG_CALC=HTx$WGT_KG_CALC, 
                 WGT_KG_DON_CALC=HTx$WGT_KG_DON_CALC,BMI_DON_CALC=HTx$BMI_DON_CALC, ETHCAT=HTx$ETHCAT,FUNC_STAT_TCR=HTx$FUNC_STAT_TCR, FUNC_STAT_TRR=HTx$FUNC_STAT_TRR, LIFE_SUP_TCR=HTx$LIFE_SUP_TCR, 
                 VAD_AT_LISTING=HTx$VAD_AT_LISTING, VAD_WHILE_LISTED=HTx$VAD_WHILE_LISTED,THORACIC_DGN=HTx$THORACIC_DGN, REGION=HTx$REGION,MALIG=HTx$MALIG, 
                 ISCHTIME=HTx$ISCHTIME, DAYSWAIT_CHRON=HTx$DAYSWAIT_CHRON,ABO_MAT=HTx$ABO_MAT,CREAT_TRR=HTx$CREAT_TRR, HEMO_CO_TCR = HTx$HEMO_CO_TCR, HEMO_CO_TRR = HTx$HEMO_CO_TRR, 
                 HEMO_PA_MN_TCR=HTx$HEMO_PA_MN_TCR, HEMO_PA_MN_TRR=HTx$HEMO_PA_MN_TRR, HEMO_PCW_TCR=HTx$HEMO_PCW_TCR, HEMO_PCW_TRR=HTx$HEMO_PCW_TRR, 
                 PRIOR_CARD_SURG_TRR=HTx$PRIOR_CARD_SURG_TRR, INOTROPES_TCR=HTx$INOTROPES_TCR, INOTROPES_TRR=HTx$INOTROPES_TRR, 
                 DIAL_AFTER_LIST=HTx$DIAL_AFTER_LIST,DIAG=HTx$DIAG, CEREB_VASC=HTx$CEREB_VASC, CIG_GRT_10_OLD=HTx$CIG_GRT_10_OLD,
                 CIG_USE=HTx$CIG_USE, DIAB=HTx$DIAB, STERNOTOMY_TCR=HTx$STERNOTOMY_TCR, LOS=HTx$LOS, COD=HTx$COD, PTIME=HTx$PTIME, ACUTE_REJ_EPI=HTx$ACUTE_REJ_EPI, FUNC_STAT_TRF=HTx$FUNC_STAT_TRF,
                 PST_DIAL=HTx$PST_DIAL, PST_PACEMAKER=HTx$PST_PACEMAKER, PST_STROKE=HTx$PST_STROKE, HEMO_SYS_TRR=HTx$HEMO_SYS_TRR, HEMO_PA_DIA_TRR=HTx$HEMO_PA_DIA_TRR, HEMO_SYS_TCR=HTx$HEMO_SYS_TCR, HEMO_PA_DIA_TCR=HTx$HEMO_PA_DIA_TCR,
                 ListingWeight=HTx$ListingWeight, RemovalWeight=HTx$RemovalWeight, 
                 WeightChange=HTx$WeightChange, BMIChange=HTx$BMIChange, BMI_L=HTx$BMI_L, BMI_T=HTx$BMI_T, TTF_one_year=as.numeric(HTx$TTF_one_year), one_year_surv = HTx$one_year_surv, TX_DATE = HTx$TX_DATE)

#Filter out weight change = 0.000 
HTx<-HTx[which(HTx$WeightChange > 0.00 | HTx$WeightChange < 0.00)]

#Filter out BMI change = 0.000 
HTx<- HTx[which(HTx$BMIChange > 0.00 | HTx$BMIChange < 0.00)]

#Filter out BMI outliers (1.5xIQR above and below 4th and 1st quartiles, respectively)
#Filter out N/A from BMI @ listing
#df <- HTx[-which(is.na(HTx$BMI_L)),]
df <- HTx

#HTx <- df
#IQR of BMI at listing
BMI_IQR <- IQR(df$BMI_L)
BMI_IQR_outlier <- 1.5*BMI_IQR
lowerlimit <- quantile(df$BMI_L, 0.25)
LL <- as.numeric(lowerlimit - BMI_IQR_outlier)
upperlimit <- quantile(df$BMI_L, 0.75) 
UL <- as.numeric(upperlimit + BMI_IQR_outlier)

df3 <- df[which(df$BMI_L > LL)]
df4 <- df3[which(df3$BMI_L < UL)]


#Filter out weight change outliers (1.5xIQR above and below 3rd and 1st quartiles, respectively)
#IQR of Weight Change
WC_IQR <- IQR(df$WeightChange)
WC_IQR_outlier <- 1.5*WC_IQR
WC_lowerlimit <- quantile(df$WeightChange, 0.25)
WC_LL <- as.numeric(WC_lowerlimit - WC_IQR_outlier)
WC_upperlimit <- quantile(df$WeightChange, 0.75)
WC_UL <- as.numeric(WC_upperlimit + WC_IQR_outlier)

df5 <- df4[which(df4$WeightChange > WC_LL)]
df6 <- df5[which(df5$WeightChange < WC_UL)]

#Filter out BMI change outliers (1.5xIQR above and below 3rd and 1st quartiles, respectively)
BC_IQR <- IQR(df$BMIChange)
BC_IQR_outlier <- 1.5*BC_IQR
BC_lowerlimit <- quantile(df$BMIChange, 0.25)
BC_LL <- as.numeric(BC_lowerlimit - BC_IQR_outlier)
BC_upperlimit <- quantile(df$BMIChange, 0.75) 
BC_UL <- as.numeric(BC_upperlimit + BC_IQR_outlier)

df7 <- df6[which(df6$BMIChange > BC_LL)]
df8 <- df7[which(df7$BMIChange < BC_UL)]


df11<- df8[-which(is.na(df8$one_year_surv)),]
HTx <- df11


## Imputation if possible for your data... 
# HTx_Imp <- HTx
# 
# HTx_Imp<- data.table(
#   PT_CODE=HTx$PT_CODE,
#   PX_STAT_CENSOR=HTx$PX_STAT_CENSOR, TTF=HTx$TTF, PRIM_GFT_FAIL=HTx$PRIM_GFT_FAIL, AGE=HTx$AGE,AGE_DON=HTx$AGE_DON,HGT_CM_CALC=HTx$HGT_CM_CALC, HGT_CM_DON_CALC=HTx$HGT_CM_DON_CALC, WGT_KG_CALC=HTx$WGT_KG_CALC, 
#   WGT_KG_DON_CALC=HTx$WGT_KG_DON_CALC,HEMO_PA_DIA_TRR=HTx$HEMO_PA_DIA_TRR,HEMO_PA_MN_TRR=HTx$HEMO_PA_MN_TRR,HEMO_SYS_TRR=HTx$HEMO_SYS_TRR,
#   HEMO_PCW_TRR=HTx$HEMO_PCW_TRR,ISCHTIME=HTx$ISCHTIME, DAYSWAIT_CHRON=HTx$DAYSWAIT_CHRON,CREAT_TRR=HTx$CREAT_TRR, LOS=HTx$LOS,Trans_Pulm_Grad=HTx$Trans_Pulm_Grad, CM_DRRATION=HTx$CM_DRRATION, DRRatio=HTx$DRRatio)


####Inital Cox Model####
# 
# dat<-HTx
# attach(dat)
# library(survival)
# library(MASS)
# library(epiDisplay)
# 
# f<-stepAIC(
#   coxph(formula=Surv(TTF_one_year,PX_STAT_CENSOR) ~
#           HEMO_PA_MN_TRR+
#           HEMO_PA_DIA_TRR+
#           HEMO_SYS_TRR+
#           CM_DRRATION, data=HTx))
# 
# 
# f<-coxph(formula=Surv(TTF,PX_STAT_CENSOR) ~
#         HEMO_PA_MN_TRR+
#         HEMO_PA_DIA_TRR+
#         HEMO_SYS_TRR+
#         Trans_Pulm_Grad+
#         DRRatio+
#         CM_DRRATION, data=HTx)
# 
# library(MASS)
# attach(dat)
# library(epiDisplay)
# 
# 
# stepAIC(glm(formula=PRIM_GFT_FAIL ~
#               HEMO_PA_MN_TRR+
#               HTx$HEMO_PA_MN_TRR+
#               HEMO_SYS_TRR+
#               Trans_Pulm_Grad+
#               DRRatio+
#               CM_DRRATION, family = binomial,data=HTx))
# 
# f<-glm(formula=PRIM_GFT_FAIL ~
#          HEMO_PA_MN_TRR+
#          DRRatio, data=HTx)
# summary(f)
# 
# exp(cbind(OR = coef(f), confint(f)))
# 
# coef(summary(f))[,'Pr(>|z|)']


#RCS COX --------------------------------------------
library(rms)
dat<-HTx
#mean
Xax_var<-HTx$BMIChange
Yax_var_num<-HTx$TTF_one_year
#Yax_var_cat<-HTx$PX_STAT_CENSOR
Yax_var_cat<-HTx$one_year_surv
stratifier <-HTx$BMI_L   ##change this for BMI listing vs Transplant

Xax_var <- as.numeric(Xax_var)
Yax_var_num <- as.numeric(Yax_var_num)
Yax_var_cat <- as.numeric(Yax_var_cat)


dd <- datadist(Xax_var,stratifier)
options(datadist='dd')

f <- cph(Surv(Yax_var_num, Yax_var_cat) ~ rcs(Xax_var)+stratifier, x=TRUE, y=TRUE)
anova(f) #that's the p value

plot(Predict(f, Xax_var, stratifier, fun=exp), xlab="Weight Change", ylab="Relative Hazard",
     #ylim=c(0,1000000),
     #xlim=c(0,100),
     main = "Weight change transplant strat")


## single RCS plot -- the original RCS code
dd <- datadist(Xax_var)
options(datadist='dd')
f <- cph(Surv(Yax_var_num, Yax_var_cat) ~ rcs(Xax_var, 3), x=TRUE, y=TRUE)  #rcs fxn number = knots
#f <- cph(Surv(Yax_var_num, Yax_var_cat) ~ rcs(Xax_var, 8) + stratifier, x=TRUE, y=TRUE)
AIC(f)  #find the highest AIC to determine the number of knots (last number in rcs fxn) -- not automated so try 3-8ish
anova(f) #that's the p value



#p1 <- ggplot(Predict(f, Xax_var, fun=exp), xlab="Initial Body Temperature (Celsius)", ylab="Relative Hazard of Death", cex.axis = 0.7, cex.lab = 0.9
plot(Predict(f, Xax_var), xlab="BMI at Transplant", ylab="Relative Hazard of Death", cex.axis = 0.7, cex.lab = 0.9
     #plot(Predict(f, Xax_var, stratifier), xlab="ECLS Rewarming Rate (C/h)", ylab="Relative Hazard of Death", cex.axis = 0.7, cex.lab = 0.9
     #fun=exp to change y axis to exponent
     #ylim=c(100,150), 
     #xlim=c(-50,80),
     #main = "Death")
)
### end of single RCS add in


####Determine appropriate AIC#### 
getInfCrit <- function(no, fit, rm_var, add_var_str, ic_fn) {
  new_var <- sprintf(add_var_str, no)
  updt_frml <- as.formula(sprintf(".~.-%s+%s", 
                                  rm_var,
                                  new_var))
  ret <- ic_fn(update(fit, updt_frml))
  names(ret) <- new_var
  return(ret)}



###Survival Tensor Spline  ####
#install.packages("survival")
library(survival)
library(rms)
library(RColorBrewer)
#Spline2<-data.table(HTx$PX_STAT_CENSOR, HTx$WeightChange, HTx$BMI_L, HTx$TTF_one_year)  ## change this line for BMI T vs L
#one year survival

Spline2<-data.table(as.numeric(HTx$one_year_surv), as.numeric(HTx$BMIChange), as.numeric(HTx$BMI_L), as.numeric(HTx$TTF_one_year))  ## change this line for BMI T vs L

V4 <- as.numeric(Spline2$V4)
V1 <- as.numeric(Spline2$V1)
V3 <- as.numeric(Spline2$V3)
V2 <- as.numeric(Spline2$V2)

##find best AIC fit model (lowest AIC value) for number of knots

#number of knots for V3
idx_model <- cph(Surv(V4, V1) ~ rcs(V2,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(V2,3)", add_var_str = "rcs(V2, %d)", ic_fn=AIC)

#number of knots for V2
idx_model <- cph(Surv(V4, V1) ~ rcs(V3,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(V3,3)", add_var_str = "rcs(V3, %d)", ic_fn=AIC)


perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
Srv<-Surv(V4, V1)
f <-cph(Srv ~ rcs (V2,5) * rcs (V3,4), # replace these numbers with the lowest AIC knots for each
        data = Spline2, x = TRUE, y = TRUE)
print (anova ( f) , caption = 'Cubic spline surface',
       size = 'smaller')

#Duplicated plots below bc my project looked at Weight and BMI Change

#Weight Change
png(file="Size_WeightChange.png",pointsize=10,height=5000,width = 5122,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Weight Change (  kg)", ylab = 'BMI at Listing', xlim= c(-20,20),ylim= c(15,40), cex.axis= 1.4, cex.lab=1.4,
       par.box = c(lwd = .1 ))
dev.off()

#BMI Change
png(file="Size_BMIChange.png",pointsize=10,height=5000,width = 5122,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "BMI Change (  kg/m )", ylab = 'BMI at Listing', xlim= c(-6,6),ylim= c(15,40), cex.axis= 2, cex.lab=1.4, 
       par.box = c(lwd = .1 ))
dev.off()


png(file="WeightChange_TEST_BMIT.png",pointsize=10,height=5000,width = 5000,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe, drape = TRUE,
       lwd=.15,
       alpha.regions=.85,
       screen=list(z=20,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Weight Change", ylab = 'BMI at Transplant', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()



#Ignore this part of code if just want to do RCS; this is for summary stats
## Univariat Table Code

dat <- HTx

library(dplyr)
library(magrittr)
dat %<>% mutate_if(is.numeric,as.character)

## do this for all the continuous variables

dat$TTF <- as.numeric(dat$TTF)
dat$AGE <- as.numeric(dat$AGE)
dat$AGE_DON <- as.numeric(dat$AGE_DON)
dat$HGT_CM_CALC <- as.numeric(dat$HGT_CM_CALC)
dat$HGT_CM_DON_CALC <- as.numeric(dat$HGT_CM_DON_CALC)
dat$WGT_KG_CALC <- as.numeric(dat$WGT_KG_CALC)
dat$WGT_KG_DON_CALC <- as.numeric(dat$WGT_KG_DON_CALC)
dat$BMI_DON_CALC <- as.numeric(dat$BMI_DON_CALC)
dat$ISCHTIME <- as.numeric(dat$ISCHTIME)
dat$DAYSWAIT_CHRON <- as.numeric(dat$DAYSWAIT_CHRON)
dat$CREAT_TRR <- as.numeric(dat$CREAT_TRR)
dat$HEMO_CO_TCR <- as.numeric(dat$HEMO_CO_TCR)
dat$HEMO_CO_TRR <- as.numeric(dat$HEMO_CO_TRR)
dat$HEMO_PA_MN_TCR <- as.numeric(dat$HEMO_PA_MN_TCR)
dat$HEMO_PA_MN_TRR <- as.numeric(dat$HEMO_PA_MN_TRR)
dat$HEMO_PCW_TCR <- as.numeric(dat$HEMO_PCW_TCR)
dat$HEMO_PCW_TRR <- as.numeric(dat$HEMO_PCW_TRR)
dat$LOS <- as.numeric(dat$LOS)
dat$PTIME <- as.numeric(dat$PTIME)
dat$ListingWeight <- as.numeric(dat$ListingWeight)
dat$RemovalWeight <- as.numeric(dat$RemovalWeight)
dat$WeightChange <- as.numeric(dat$WeightChange)
dat$BMI_L <- as.numeric(dat$BMI_L)
dat$BMI_T <- as.numeric(dat$BMI_T)
dat$BMIChange <- as.numeric(dat$BMIChange)


dat$HEMO_SYS_TRR <- as.numeric(dat$HEMO_SYS_TRR)
dat$HEMO_SYS_TCR <- as.numeric(dat$HEMO_SYS_TCR)
dat$HEMO_PA_DIA_TRR <- as.numeric(dat$HEMO_PA_DIA_TRR)
dat$HEMO_PA_DIA_TCR <- as.numeric(dat$HEMO_PA_DIA_TCR)


##remove 'stratifier' if you do not want to stratify and find p-value 
#need a Q() for all continuous variables
attach(dat)
library(Publish)
write.csv(
  summary(univariateTable(~
                            
                            Q(HEMO_SYS_TRR) +
                            Q(HEMO_SYS_TCR) +
                            Q(HEMO_PA_DIA_TRR) +
                            Q(HEMO_PA_DIA_TCR) +

                             PX_STAT_CENSOR   +
                             Q(TTF)     +
                             one_year_surv +
                             Q(AGE)     +
                             Q(AGE_DON)  +
                             GENDER     +
                             GENDER_DON  +
                             Q(HGT_CM_CALC) +
                             Q(HGT_CM_DON_CALC) +
                             Q(WGT_KG_CALC)   +
                             Q(WGT_KG_DON_CALC)   +
                             Q(BMI_DON_CALC)   +
                             ETHCAT   +
                             ABO_MAT   +
                             FUNC_STAT_TCR   +
                             FUNC_STAT_TRR    +
                             LIFE_SUP_TCR    +
                             VAD_AT_LISTING  +
                             VAD_WHILE_LISTED  +
                             THORACIC_DGN   +
                             REGION   +
                             MALIG   +
                             Q(ISCHTIME)   +
                             Q(DAYSWAIT_CHRON)   +
                             Q(CREAT_TRR)   +
                             Q(HEMO_CO_TCR)  +
                             Q(HEMO_CO_TRR)   +
                             Q(HEMO_PA_MN_TCR)  +
                             Q(HEMO_PA_MN_TRR)  +
                             Q(HEMO_PCW_TCR)    +
                             Q(HEMO_PCW_TRR)    +
                             PRIOR_CARD_SURG_TRR   +
                             INOTROPES_TCR   +
                             INOTROPES_TRR   +
                             DIAL_AFTER_LIST   +
                             DIAG   +
                             CEREB_VASC   +
                             CIG_USE    +
                             CIG_GRT_10_OLD    +
                             DIAB +
                             STERNOTOMY_TCR  +
                             Q(LOS)    +
                             COD    +
                             Q(PTIME)    +
                             ACUTE_REJ_EPI   +
                             FUNC_STAT_TRF   +
                             PST_DIAL  +
                             PST_PACEMAKER  +
                             PST_STROKE  +
                             Q(ListingWeight) +
                             Q(RemovalWeight)  +
                             Q(WeightChange)   +
                             Q(BMI_L)   +
                             Q(BMI_T)   +
                             Q(BMIChange)
                            
                            
                          
                          ,data=dat)),
  file="UNOS_NewTable1.csv",  #name of output file
  row.names=F
)


wilcox.test(dat$BMI_L, dat$BMI_T, paired = TRUE)
wilcox.test(dat$ListingWeight, dat$RemovalWeight, paired = TRUE)
wilcox.test(dat$HEMO_CO_TCR, dat$HEMO_CO_TRR, paired = TRUE)

# ###Logistic outcome LRM model ####
# Spline2<-data.table(HTx$PX_STAT_CENSOR, HTx$WeightChange, HTx$BMI_L, HTx$TTF_one_year)  ## change this line for BMI T vs L
# 
# library(RColorBrewer)
# Spline2<-data.table(HTx$PRIM_GFT_FAIL,HTx$DRRatio, HTx$HEMO_PA_MN_TRR, HTx$TTF)
# 
# idx_model <- lrm(Spline2$V1 ~ rcs(Spline2$V3,3))
# sapply(3:7, getInfCrit, fit = idx_model,
#        rm_var = "rcs(Spline2$V3,3)", add_var_str = "rcs(Spline2$V3, %d)", ic_fn=AIC)
# 
# 
# perim <- with (Spline2, perimeter(V2, V3,) )
# dd <- datadist (Spline2); options ( datadist ='dd')
# f <-lrm(V1 ~ rcs (V2,4) * rcs (V3,3),
#         data = Spline2, x = TRUE, y = TRUE)
# print (anova (f), caption = 'Cubic spline surface',
#        size = 'smaller')
# 
# png(file="MAP_RVRatio_Levelplot_prime_finalzzz.png",pointsize=40,height=5000,width =5122  ,res=1000, antialias = "default")
# bplot (Predict (f,V2,V3), perim = perim,
#        lfun = levelplot , drape = TRUE,
#        lwd=.1,
#        alpha.regions=.85,
#        #screen=list(z=0,x=-45),
#        col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
#        xlab = "RV D/R Ratio", ylab = 'Mean PA Pressure', ylim=c(0,70),
#        par.box = c(lwd = .1 ))
# dev.off()
# 
# png(file="TPG_RVRatio_wireframe_prime.png",pointsize=10,height=5000,width =5000  ,res=800, antialias = "default")
# bplot (Predict (f,V2,V3), perim = perim,
#        lfun = wireframe , drape = TRUE,
#        lwd=.1,
#        alpha.regions=.85,
#        screen=list(z=12,x=-50),
#        col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
#        xlab = "RV D/R Ratio", ylab = 'Transpulmonary Gradient', xlabrot =17, ylabrot = -69, zlabrot=274,
#        par.box = c(lwd = .1 ))
# dev.off()
# 
# library("writexl")
# 
# df <- data.frame(HTx)
# 
# write_xlsx(df,"C:\\Users\\Melissa\\Desktop\\people.xlsx")
