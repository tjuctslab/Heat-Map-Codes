#Create data.table
thoracic_data<-data.table(thoracic_data)

#Heart only recipients 
HTx <- thoracic_data[which(thoracic_data$ORGAN == "HR")]

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


#Outcomes - not imputated 
HTx$PRIM_GFT_FAIL<-0
HTx$PRIM_GFT_FAIL[which(HTx$GRF_FAIL_CAUSE == "1")] <-1


####calculate D/R Ratio ####
HTx$RVM_Don<-ifelse(HTx$GENDER_DON == "M",11.25*(HTx$AGE_DON^(-.320))*((HTx$HGT_CM_DON_CALC/100)^(1.135))*(HTx$WGT_KG_DON_CALC^(0.315)),10.59*(HTx$AGE_DON^(-.320))*((HTx$HGT_CM_DON_CALC/100)^(1.135))*(HTx$WGT_KG_DON_CALC^(0.315)))
HTx$RVM_Rec<-ifelse(HTx$GENDER == "M",11.25*(HTx$AGE^(-.320))*((HTx$HGT_CM_CALC/100)^(1.135))*(HTx$WGT_KG_CALC^(0.315)),10.59*(HTx$AGE_DON^(-.320))*((HTx$HGT_CM_DON_CALC/100)^(1.135))*(HTx$WGT_KG_DON_CALC^(0.315)))

HTx$DRRatio<-HTx$RVM_Don/HTx$RVM_Rec

#Calculate LV Mass and LV DRRatio 

HTx$LVM_Don<-ifelse(HTx$GENDER_DON == "M",8.17*((HTx$HGT_CM_DON_CALC/100)^(0.561))*(HTx$WGT_KG_DON_CALC^(0.608)),6.82*((HTx$HGT_CM_DON_CALC/100)^(0.561))*(HTx$WGT_KG_DON_CALC^(0.608)))
HTx$LVM_Rec<-ifelse(HTx$GENDER == "M",8.17*((HTx$HGT_CM_CALC/100)^(0.561))*(HTx$WGT_KG_CALC^(0.608)),6.82*((HTx$HGT_CM_DON_CALC/100)^(0.561))*(HTx$WGT_KG_DON_CALC^(0.608)))

HTx$LV_DRRATIO<-HTx$LVM_Don/HTx$LVM_Rec

#Total Cardiac Mass 
HTx$CARMASS_DON<-HTx$RVM_Don + HTx$LVM_Don
HTx$CARMASS_Rec<-HTx$RVM_Rec + HTx$LVM_Rec

HTx$CM_DRRATION<- HTx$CARMASS_DON/HTx$CARMASS_Rec

#Calculate Transpulmonary Pressure 

HTx$Trans_Pulm_Grad<-HTx$HEMO_PA_MN_TRR - HTx$HEMO_PCW_TRR
HTx$Trans_Pulm_Grad[which(is.na(HTx$HEMO_PA_MN_TRR)|is.na(HTx$HEMO_PCW_TRR))] <- NA
HTx<-HTx[which(HTx$Trans_Pulm_Grad>=0)]



HTx<- data.table(PT_CODE=HTx$PT_CODE,PX_STAT_CENSOR=HTx$PX_STAT_CENSOR, TTF=HTx$TTF, PRIM_GFT_FAIL=HTx$PRIM_GFT_FAIL, AGE=HTx$AGE,AGE_DON=HTx$AGE_DON, GENDER=HTx$GENDER,
                 GENDER_DON=HTx$GENDER_DON, HGT_CM_CALC=HTx$HGT_CM_CALC, HGT_CM_DON_CALC=HTx$HGT_CM_DON_CALC, WGT_KG_CALC=HTx$WGT_KG_CALC, 
                 WGT_KG_DON_CALC=HTx$WGT_KG_DON_CALC,HEMO_PA_DIA_TRR=HTx$HEMO_PA_DIA_TRR,HEMO_PA_MN_TRR=HTx$HEMO_PA_MN_TRR,HEMO_SYS_TRR=HTx$HEMO_SYS_TRR,
                 HEMO_PCW_TRR=HTx$HEMO_PCW_TRR,ETHCAT=HTx$ETHCAT,FUNC_STAT_TCR=HTx$FUNC_STAT_TCR, LIFE_SUP_TCR=HTx$LIFE_SUP_TCR, 
                 VAD_AT_LISTING=HTx$VAD_AT_LISTING, VAD_WHILE_LISTED=HTx$VAD_WHILE_LISTED,THORACIC_DGN=HTx$THORACIC_DGN, REGION=HTx$REGION,MALIG=HTx$MALIG, 
                 ISCHTIME=HTx$ISCHTIME, DAYSWAIT_CHRON=HTx$DAYSWAIT_CHRON,ABO_MAT=HTx$ABO_MAT,CREAT_TRR=HTx$CREAT_TRR, PRIOR_CARD_SURG_TRR=HTx$PRIOR_CARD_SURG_TRR,
                 DIAL_AFTER_LIST=HTx$DIAL_AFTER_LIST,DIAG=HTx$DIAG, LOS=HTx$LOS, COD=HTx$COD, CEREB_VASC=HTx$CEREB_VASC, CIG_GRT_10_OLD=HTx$CIG_GRT_10_OLD,
                 CIG_USE=HTx$CIG_USE, DIAB=HTx$DIAB, STERNOTOMY_TCR=HTx$STERNOTOMY_TCR, Trans_Pulm_Grad=HTx$Trans_Pulm_Grad, CM_DRRATION=HTx$CM_DRRATION, DRRatio=HTx$DRRatio)

table(HTx$GENDER)


table(HTx$GENDER_DON)


table(HTx$ETHCAT)

table(is.na(HTx$FUNC_STAT_TCR))
HTx$FUNC_STAT_TCR[which(is.na(HTx$FUNC_STAT_TCR))] <- 000

table(HTx$LIFE_SUP_TCR)
HTx$LIFE_SUP_TCR[which(HTx$LIFE_SUP_TCR=="")]<- 'U'


table(HTx$VAD_AT_LISTING)
HTx$VAD_AT_LISTING[which(HTx$VAD_AT_LISTING==1)]<-"Y"
HTx$VAD_AT_LISTING[which(is.na(HTx$VAD_AT_LISTING))]<-"N"

table(HTx$VAD_WHILE_LISTED)
HTx$VAD_WHILE_LISTED[which(HTx$VAD_WHILE_LISTED==1)]<-"Y"
HTx$VAD_WHILE_LISTED[which(is.na(HTx$VAD_WHILE_LISTED))]<-"N"

table(is.na(HTx$THORACIC_DGN))

table(is.na(HTx$REGION))

table(HTx$MALIG)
HTx$MALIG[which(HTx$MALIG=="")]<- "U"

table(is.na(HTx$ABO_MAT)) 
HTx$ABO_MAT[which(HTx$ABO_MAT=="")]<- "99"

table(HTx$PRIOR_CARD_SURG_TRR)
HTx$PRIOR_CARD_SURG_TRR[which(HTx$PRIOR_CARD_SURG_TRR=="")]<- "U"

table(HTx$DIAL_AFTER_LIST)
HTx$DIAL_AFTER_LIST[which(HTx$DIAL_AFTER_LIST=="")]<- "U"

table(HTx$DIAG)
HTx$COD[which(is.na(HTx$DIAG))]<-0000

table(HTx$COD)
HTx$COD[which(is.na(HTx$COD))]<-0000


table(HTx$CEREB_VASC)
HTx$CEREB_VASC[which(HTx$CEREB_VASC=="")]<- "U"


table(HTx$CIG_GRT_10_OLD)
HTx$CIG_GRT_10_OLD[which(HTx$CIG_GRT_10_OLD=="")]<- "U"

table(HTx$DIAB)
HTx$DIAB[which(is.na(HTx$DIAB))]<-999

table(HTx$CIG_USE) 
HTx$CIG_USE[which(HTx$CIG_USE=="")]<- "U"

table(HTx$STERNOTOMY_TCR)
HTx$STERNOTOMY_TCR[which(is.na(HTx$STERNOTOMY_TCR))] <- 999

HTx_Imp<- data.table(
  PT_CODE=HTx$PT_CODE,
  PX_STAT_CENSOR=HTx$PX_STAT_CENSOR, TTF=HTx$TTF, PRIM_GFT_FAIL=HTx$PRIM_GFT_FAIL, AGE=HTx$AGE,AGE_DON=HTx$AGE_DON,HGT_CM_CALC=HTx$HGT_CM_CALC, HGT_CM_DON_CALC=HTx$HGT_CM_DON_CALC, WGT_KG_CALC=HTx$WGT_KG_CALC, 
  WGT_KG_DON_CALC=HTx$WGT_KG_DON_CALC,HEMO_PA_DIA_TRR=HTx$HEMO_PA_DIA_TRR,HEMO_PA_MN_TRR=HTx$HEMO_PA_MN_TRR,HEMO_SYS_TRR=HTx$HEMO_SYS_TRR,
  HEMO_PCW_TRR=HTx$HEMO_PCW_TRR,ISCHTIME=HTx$ISCHTIME, DAYSWAIT_CHRON=HTx$DAYSWAIT_CHRON,CREAT_TRR=HTx$CREAT_TRR, LOS=HTx$LOS,Trans_Pulm_Grad=HTx$Trans_Pulm_Grad, CM_DRRATION=HTx$CM_DRRATION, DRRatio=HTx$DRRatio)


HTx_noimp<- data.table(PT_CODE=HTx$PT_CODE,GENDER=HTx$GENDER,
                       GENDER_DON=HTx$GENDER_DON, ETHCAT=HTx$ETHCAT,FUNC_STAT_TCR=HTx$FUNC_STAT_TCR, LIFE_SUP_TCR=HTx$LIFE_SUP_TCR, 
                       VAD_AT_LISTING=HTx$VAD_AT_LISTING, VAD_WHILE_LISTED=HTx$VAD_WHILE_LISTED,THORACIC_DGN=HTx$THORACIC_DGN, REGION=HTx$REGION,MALIG=HTx$MALIG, 
                       ABO_MAT=HTx$ABO_MAT, PRIOR_CARD_SURG_TRR=HTx$PRIOR_CARD_SURG_TRR,DIAL_AFTER_LIST=HTx$DIAL_AFTER_LIST,DIAG=HTx$DIAG, COD=HTx$COD, CEREB_VASC=HTx$CEREB_VASC, CIG_GRT_10_OLD=HTx$CIG_GRT_10_OLD,
                       CIG_USE=HTx$CIG_USE, DIAB=HTx$DIAB, STERNOTOMY_TCR=HTx$STERNOTOMY_TCR)


library(visNetwork)
library(bnlearn)

bn_str<-structural.em(HTx_Imp,return.all=)
bn_mod <- bn.fit(bn_str, data = HTx_Imp, method = "mle")
HTx_Imp<- impute(bn_mod,data = HTx_Imp, method = "bayes-lw")
HTx_Imp<-bn_str$imputed
HTx_Imp<-data.table(HTx_Imp)


HTx<-merge(HTx_Imp,HTx_noimp, by="PT_CODE")
###Not used imputation code####
bn_mod <- bn.fit(bn_struct, data = HTx, method = "mle")
imp<- impute(bn_mod,data = HTx, method = "bayes-lw")
qof_followup2_hrae_tier2<-imp
plot.network <- function(structure, ht = "400px", cols = "darkturquoise", labels = nodes(structure)){
  if(is.null(labels)) labels <- rep("", length(nodes(structure)))
  nodes <- data.frame(id = nodes(structure),
                      label = labels,
                      color = cols,
                      shadow = TRUE
  )
  
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = FALSE,
                      shadow = TRUE,
                      color = "black")
  
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}
plot.network(bn_str)

colMeans(is.na(HTx))



#Organize PA presures 




####Inital Cox Model####

dat<-HTx_Imp
attach(dat)
library(survival)
library(MASS)
library(epiDisplay)

f<-stepAIC( 
  coxph(formula=Surv(TTF,PX_STAT_CENSOR) ~ 
          HEMO_PA_MN_TRR+
          HEMO_PA_DIA_TRR+
          HEMO_SYS_TRR+
          Trans_Pulm_Grad+
          DRRatio+
          CM_DRRATION, data=HTx))

f<-coxph(formula=Surv(TTF,PX_STAT_CENSOR) ~ 
        HEMO_PA_MN_TRR+
        HEMO_PA_DIA_TRR+
        HEMO_SYS_TRR+
        Trans_Pulm_Grad+
        DRRatio+
        CM_DRRATION, data=HTx)

library(MASS)
attach(dat)
library(epiDisplay)


stepAIC(glm(formula=PRIM_GFT_FAIL ~
              HEMO_PA_MN_TRR+
              HTx$HEMO_PA_MN_TRR+
              HEMO_SYS_TRR+
              Trans_Pulm_Grad+
              DRRatio+
              CM_DRRATION, family = binomial,data=HTx))

f<-glm(formula=PRIM_GFT_FAIL ~ 
         HEMO_PA_MN_TRR+
         DRRatio, data=HTx)
summary(f)

exp(cbind(OR = coef(f), confint(f)))

coef(summary(f))[,'Pr(>|z|)']


stepAIC(glm(formula=as.factor(PX_STAT_CENSOR) ~
              HEMO_PA_MN_TRR+
              HTx$HEMO_PA_MN_TRR+
              HEMO_SYS_TRR+
              Trans_Pulm_Grad+
              DRRatio+
              CM_DRRATION, family = binomial,data=HTx))

CMRatio 
####Descritize PA Pressure####

HTx$PA_MN_TRR_QUART <- NA
quantile(HTx$HEMO_PA_MN_TRR, c(.33, .67),na.rm = TRUE) 

HTx$PA_MN_TRR_QUART[which(HTx$HEMO_PA_MN_TRR <=23)] <- "1st Tertile"
HTx$PA_MN_TRR_QUART[which(HTx$HEMO_PA_MN_TRR > 23 & HTx$HEMO_PA_MN_TRR < 33)] <- "2nd Tertile"
HTx$PA_MN_TRR_QUART[which(HTx$HEMO_PA_MN_TRR >= 33)] <- "3rd Tertile"

HTx$PA_SYS_TRR_TERT <- NA
quantile(HTx$HEMO_SYS_TRR, c(.33, .67),na.rm = TRUE) 

HTx$PA_SYS_TRR_TERT[which(HTx$HEMO_SYS_TRR <=24)] <- "1st Tertile"
HTx$PA_SYS_TRR_TERT[which(HTx$HEMO_SYS_TRR > 24 & HTx$HEMO_PA_MN_TRR < 48)] <- "2nd Tertile"
HTx$PA_SYS_TRR_TERT[which(HTx$HEMO_SYS_TRR >= 48)] <- "3rd Tertile"

HTx$PA_DIA_TRR_TERT <- NA
quantile(HTx$HEMO_PA_DIA_TRR, c(.33, .67),na.rm = TRUE) 

HTx$PA_DIA_TRR_TERT[which(HTx$HEMO_PA_DIA_TRR <=16)] <- "1st Tertile"
HTx$PA_DIA_TRR_TERT[which(HTx$HEMO_PA_DIA_TRR > 16 & HTx$HEMO_PA_MN_TRR < 24)] <- "2nd Tertile"
HTx$PA_DIA_TRR_TERT[which(HTx$HEMO_PA_DIA_TRR >= 24)] <- "3rd Tertile"




#RCS COX --------------------------------------------
library(rms)
#mean
Xax_var<-HTx$DRRatio
Yax_var_num<-HTx$TTF
Yax_var_cat<-HTx$PX_STAT_CENSOR
stratifier <-HTx$PA_MN_TRR_QUART

Xax_var <- as.numeric(Xax_var)
Yax_var_num <- as.numeric(Yax_var_num)
Yax_var_cat <- as.numeric(Yax_var_cat)


dd <- datadist(Xax_var,stratifier)
options(datadist='dd')

f <- cph(Surv(Yax_var_num, Yax_var_cat) ~ rcs(Xax_var)+stratifier, x=TRUE, y=TRUE)
anova(f) #that's the p value

plot(Predict(f, Xax_var, stratifier, fun=exp), xlab="D/R Ratio", ylab="Relative Hazard",
     #ylim=c(0,1000000),
     #xlim=c(0,100),
     main = "Mean PA Pressure @ Transplant")

#systolic
Xax_var<-HTx$DRRatio
Yax_var_num<-HTx$TTF
Yax_var_cat<-HTx$PX_STAT_CENSOR
stratifier <-HTx$PA_SYS_TRR_TERT

Xax_var <- as.numeric(Xax_var)
Yax_var_num <- as.numeric(Yax_var_num)
Yax_var_cat <- as.numeric(Yax_var_cat)


dd <- datadist(Xax_var,stratifier)
options(datadist='dd')

f <- cph(Surv(Yax_var_num, Yax_var_cat) ~ rcs(Xax_var)+stratifier, x=TRUE, y=TRUE)
anova(f) #that's the p value

plot(Predict(f, Xax_var, stratifier, fun=exp), xlab="D/R Ratio", ylab="Relative Hazard",
     #ylim=c(0,1000000),
     #xlim=c(0,100),
     main = "Systolic PA Pressure @ Transplant")


#diastolic 
Xax_var<-HTx$HEMO_PA_MN_TRR
Yax_var_num<-HTx$TTF
Yax_var_cat<-HTx$PX_STAT_CENSOR
stratifier <-HTx$PA_SYS_TRR_TERT

Xax_var <- as.numeric(Xax_var)
Yax_var_num <- as.numeric(Yax_var_num)
Yax_var_cat <- as.numeric(Yax_var_cat)


dd <- datadist(Xax_var)
options(datadist='dd')

f <- cph(Surv(Yax_var_num, Yax_var_cat) ~ rcs(Xax_var), x=TRUE, y=TRUE)
anova(f) #that's the p value

plot(Predict(f, Xax_var, stratifier, fun=exp), xlab="D/R Ratio", ylab="Relative Hazard",
     #ylim=c(0,1000000),
     #xlim=c(0,100),
     main = "Diastolic PA Pressure @ Transplant")




####Determine appropriate AIC#### 
getInfCrit <- function(no, fit, rm_var, add_var_str, ic_fn) {
  new_var <- sprintf(add_var_str, no)
  updt_frml <- as.formula(sprintf(".~.-%s+%s", 
                                  rm_var,
                                  new_var))
  ret <- ic_fn(update(fit, updt_frml))
  names(ret) <- new_var
  return(ret)}



###3D plot Systolic PA  ####
library(RColorBrewer)
Spline2<-data.table(HTx$PX_STAT_CENSOR, HTx$DRRatio, HTx`$HEMO_PA_MN_TRR, HTx$TTF)

idx_model <- cph(Surv(Spline2$V4, Spline2$V1) ~ rcs(Spline2$V3,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V3,3)", add_var_str = "rcs(Spline2$V3, %d)", ic_fn=AIC)

perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
Srv<-Surv(Spline2$V4, Spline2$V1)
f <-cph(Srv ~ rcs (V2,5) * rcs (V3,4),
        data = Spline2, x = TRUE, y = TRUE)
print (anova ( f) , caption = 'Cubic spline surface',
       size = 'smaller')


png(file="sys_RV_Ratio_levelplot.png",pointsize=10,height=5000,width = 5122,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Systolic PA Presure', ylim= c(0,70),
       par.box = c(lwd = .1 ))
dev.off()


png(file="sys_RV_Ratio_wireframe.png",pointsize=10,height=5000,width = 5000,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe, drape = TRUE,
       lwd=.15,
       alpha.regions=.85,
       screen=list(z=20,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Systolic PA Presure', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()


############

Spline2<-data.table(HTx$PX_STAT_CENSOR, HTx$CM_DRRATION, HTx$HEMO_PA_MN_TRR, HTx$TTF)

idx_model <- cph(Surv(Spline2$V4, Spline2$V1) ~ rcs(Spline2$V3,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V3,3)", add_var_str = "rcs(Spline2$V3, %d)", ic_fn=AIC)

perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
Srv<-Surv(Spline2$V4, Spline2$V1)
f <-cph(Srv ~ rcs (V2,4) * rcs (V3,5),
        data = Spline2, x = TRUE, y = TRUE)
print (anova ( f) , caption = 'Cubic spline surface',
       size = 'smaller')

png(file="mean_Totalmass_Ratio_levelplot_FInalzzz_.png",pointsize=40,height=5000,width = 5122,res=1000, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       #screen=list(z=0,x=-45),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'Mean PA Presure', ylim=c(0,70), xlim=c(0.5,1.7), ps=15,
       par.box = c(lwd = .1 ))
dev.off()


png(file="sys_Totalmass_Ratio_wireframe.png",pointsize=10,height=5000,width = 5000,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       screen=list(z=20,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'Systolic PA Presure', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()



#####3D gif######
png(file="example%03d.png",height=5000,width = 5000 ,res=600,antialias = "subpixel")
for (i in seq(0, 355 ,15)){
  print(bplot (Predict (f,V2,V3), perim = perim,
               lfun = wireframe , drape = TRUE,
               lwd=.1,
               alpha.regions=.85,
               screen = list(z = i, x = -60),
               col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
               xlab = "Total Mass D/R Ratio", ylab = 'Systolic PA Presure',
               par.box = c(lwd = .1 )))
}
dev.off()

# convert pdf to gif using ImageMagick
system("convert -delay 40 *.png animated_3D_plot.gif")
# cleaning up
file.remove(list.files(pattern=".png"))
###3D plot Transpumonary P ####


library(RColorBrewer)
Spline2<-data.table(HTx$PX_STAT_CENSOR,HTx$DRRatio, HTx$Trans_Pulm_Grad, HTx$TTF)

idx_model <- cph(Surv(Spline2$V4, Spline2$V1) ~ rcs(Spline2$V2,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V2,3)", add_var_str = "rcs(Spline2$V2, %d)", ic_fn=AIC)


perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
Srv<-Surv(Spline2$V4, Spline2$V1)
f <-cph(Srv ~ rcs (V2,5) *rcs (V3,4) ,
        data = Spline2, x = TRUE, y = TRUE)
print (anova (f), caption = 'Cubic spline surface',
       size = 'smaller')

png(file="TPG_RVRatio_Levelplot.png",pointsize=10,height=5000,width =5122  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       #screen=list(z=0,x=-45),
       col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Transpulmonary Gradient', ylim=c(-20,50),
       par.box = c(lwd = .1 ))
dev.off()

png(file="TPG_RVRatio_wireframe.png",pointsize=10,height=5000,width =5000  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       screen=list(z=20,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Transpulmonary Gradient', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()

#############
Spline2<-data.table(HTx$PX_STAT_CENSOR,HTx$CM_DRRATION, HTx$Trans_Pulm_Grad, HTx$TTF)

idx_model <- cph(Surv(Spline2$V4, Spline2$V1) ~ rcs(Spline2$V3,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V3,3)", add_var_str = "rcs(Spline2$V3, %d)", ic_fn=AIC)

perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
Srv<-Surv(Spline2$V4, Spline2$V1)
f <-cph(Srv ~ rcs (V2,4) *rcs (V3,5) ,
        data = Spline2, x = TRUE, y = TRUE)
print (anova (f), caption = 'Cubic spline surface',
       size = 'smaller')

png(file="TPG_TotalMass_Ratio_Levelplot_finalzzzz.png",pointsize=40,height=5000,width =5585  ,res=1000, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       #screen=list(z=0,x=-45),
       col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'Transpulmonary Gradient', ylim=c(-5,42)
       ,xlim=c(0.5,1.7),
       par.box = c(lwd = .1 ))
dev.off()

png(file="TPG_Totalmass_Ratio_wireframe.png",pointsize=10,height=5000,width =5000  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       screen=list(z=20,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'Transpulmonary Gradient', xlabrot =17, ylabrot = -69, zlabrot=274, 
       par.box = c(lwd = .1 ))
dev.off()


#####3D gif#####
png(file="example%03d.png", width=5000, heigh=5000, res = 600)
for (i in seq(0, 355 ,15)){
  print(bplot (Predict (f,V2,V3), perim = perim,
               lfun = wireframe , drape = TRUE,
               lwd=.1,
               alpha.regions=.85,
               screen = list(z = i, x = -60),
               col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
               xlab = "Total Mass D/R Ratio", ylab = 'Transpulmonary Gradient',
               par.box = c(lwd = .1 )))
}
dev.off()

# convert pdf to gif using ImageMagick
system("convert -delay 40 *.png animated_3D_plot.gif")
####PCWP####
library(RColorBrewer)
Spline2<-data.table(HTx$PX_STAT_CENSOR,HTx$DRRatio, HTx$HEMO_PCW_TRR, HTx$TTF)

idx_model <- cph(Surv(Spline2$V4, Spline2$V1) ~ rcs(Spline2$V3,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V3,3)", add_var_str = "rcs(Spline2$V3, %d)", ic_fn=AIC)


perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
Srv<-Surv(Spline2$V4, Spline2$V1)
f <-cph(Srv ~ rcs (V2,5) *rcs (V3,3) ,
        data = Spline2, x = TRUE, y = TRUE)
print (anova (f), caption = 'Cubic spline surface',
       size = 'smaller')

png(file="PCWP_RVRatio_Levelplot.png",pointsize=10,height=5000,width =5122  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       #screen=list(z=0,x=-45),
       col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'PCWP', ylim=c(-20,50),
       par.box = c(lwd = .1 ))
dev.off()

png(file="PCWP_RVRatio_wireframe.png",pointsize=10,height=5000,width =5000  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       screen=list(z=12,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'PCWP', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()

#############
Spline2<-data.table(HTx$PX_STAT_CENSOR,HTx$CM_DRRATION, HTx$HEMO_PCW_TRR, HTx$TTF)

idx_model <- cph(Surv(Spline2$V4, Spline2$V1) ~ rcs(Spline2$V2,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V3,3)", add_var_str = "rcs(Spline2$V3, %d)", ic_fn=AIC)

perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
Srv<-Surv(Spline2$V4, Spline2$V1)
f <-cph(Srv ~ rcs (V2,4) *rcs (V3,3) ,
        data = Spline2, x = TRUE, y = TRUE)
print (anova (f), caption = 'Cubic spline surface',
       size = 'smaller')

png(file="PCWP_TotalMass_Ratio_Levelplot.png",pointsize=10,height=5000,width =5585  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       #screen=list(z=0,x=-45),
       col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'PCWP', ylim=c(-20,50),xlim=c(0.5,1.7),
       par.box = c(lwd = .1 ))
dev.off()

png(file="PCWP_Totalmass_Ratio_wireframe.png",pointsize=10,height=5000,width =5000  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       screen=list(z=12,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'PCWP', xlabrot =17, ylabrot = -69, zlabrot=274, 
       par.box = c(lwd = .1 ))
dev.off()


###3D plot Systolic PA - primary graft failure  ####
library(RColorBrewer)
Spline2<-data.table(HTx$PRIM_GFT_FAIL, HTx$DRRatio, HTx$HEMO_SYS_TRR, HTx$TTF)

idx_model <- lrm(Spline2$V1~ rcs(Spline2$V3,3), x=TRUE, y=TRUE)
sapply(3:7, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V3,3)", add_var_str = "rcs(Spline2$V3, %d)", ic_fn=AIC)

perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
f <-lrm(V1 ~ rcs (V2,3) * rcs (V3,3),
        data = Spline2, x = TRUE, y = TRUE)
print (anova ( f) , caption = 'Cubic spline surface',
       size = 'smaller')


png(file="sys_RV_Ratio_levelplot_PRIMGF.png",pointsize=10,height=5000,width = 5122,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Systolic PA Presure', ylim= c(0,100),
       par.box = c(lwd = .1 ))
dev.off()


png(file="sys_RV_Ratio_wirefram_PRIMGF.png",pointsize=10,height=5000,width = 5000,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe, drape = TRUE,
       lwd=.15,
       alpha.regions=.85,
       screen=list(z=12,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Systolic PA Presure', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()


############-primary fraft failure 

Spline2<-data.table(HTx$PRIM_GFT_FAIL, HTx$CM_DRRATION, HTx$HEMO_SYS_TRR, HTx$TTF)

idx_model <- cph(Surv(Spline2$V4, Spline2$V1) ~ rcs(Spline2$V2,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V2,3)", add_var_str = "rcs(Spline2$V2, %d)", ic_fn=AIC)

perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
f <-lrm(V1 ~ rcs (V2,3) * rcs (V3,3),
        data = Spline2, x = TRUE, y = TRUE)
print (anova ( f) , caption = 'Cubic spline surface',
       size = 'smaller')

png(file="sys_Totalmass_Ratio_levelplot_prime.png",pointsize=10,height=5000,width = 5122,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       #screen=list(z=0,x=-45),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'Systolic PA Presure', ylim=c(0,100), xlim=c(0.5,1.7),
       par.box = c(lwd = .1 ))
dev.off()


png(file="sys_Totalmass_Ratio_wireframe_prime.png",pointsize=10,height=5000,width = 5000,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       screen=list(z=12,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'Systolic PA Presure', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()


5
#####3D gif######
png(file="example%03d.png",height=5000,width = 5000 ,res=600,antialias = "subpixel")
for (i in seq(0, 355 ,15)){
  print(bplot (Predict (f,V2,V3), perim = perim,
               lfun = wireframe , drape = TRUE,
               lwd=.1,
               alpha.regions=.85,
               screen = list(z = i, x = -60),
               col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
               xlab = "Total Mass D/R Ratio", ylab = 'Systolic PA Presure',
               par.box = c(lwd = .1 )))
}
dev.off()

# convert pdf to gif using ImageMagick
system("convert -delay 40 *.png animated_3D_plot.gif")
# cleaning up
file.remove(list.files(pattern=".png"))
###3D plot mean PA - primary graft failure ####



Xax_var<-HTx$HEMO_PA_MN_TRR
Yax_var_num<-HTx$TTF
Yax_var_cat<-HTx$PRIM_GFT_FAIL
stratifier <-HTx$PA_MN_TRR_QUART

Xax_var <- as.numeric(Xax_var)
Yax_var_num <- as.numeric(Yax_var_num)
Yax_var_cat <- as.numeric(Yax_var_cat)


dd <- datadist(Xax_var)
options(datadist='dd')

f <- lrm(Yax_var_cat ~ rcs(Xax_var,3), x=TRUE, y=TRUE)
anova(f) #that's the p value

plot(Predict(f, Xax_var, fun=exp), xlab="D/R Ratio", ylab="Relative Hazard",
     #ylim=c(0,1000000),
     #xlim=c(0,100),
     main = "Mean PA Pressure @ Transplant")


library(RColorBrewer)
Spline2<-data.table(HTx$PRIM_GFT_FAIL,HTx$DRRatio, HTx$HEMO_PA_MN_TRR, HTx$TTF)

idx_model <- lrm(Spline2$V1 ~ rcs(Spline2$V3,3))
sapply(3:7, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V3,3)", add_var_str = "rcs(Spline2$V3, %d)", ic_fn=AIC)


perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
f <-lrm(V1 ~ rcs (V2,4) * rcs (V3,3),
        data = Spline2, x = TRUE, y = TRUE)
print (anova (f), caption = 'Cubic spline surface',
       size = 'smaller')

png(file="MAP_RVRatio_Levelplot_prime_finalzzz.png",pointsize=40,height=5000,width =5122  ,res=1000, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       #screen=list(z=0,x=-45),
       col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Mean PA Pressure', ylim=c(0,70),
       par.box = c(lwd = .1 ))
dev.off()

png(file="TPG_RVRatio_wireframe_prime.png",pointsize=10,height=5000,width =5000  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       screen=list(z=12,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Transpulmonary Gradient', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()

#############-primary graft failure 
Spline2<-data.table(HTx$PRIM_GFT_FAIL,HTx$CM_DRRATION, HTx$Trans_Pulm_Grad, HTx$TTF)

idx_model <- cph(Surv(Spline2$V4, Spline2$V1) ~ rcs(Spline2$V2,3), x=TRUE, y=TRUE)
sapply(3:9, getInfCrit, fit = idx_model,
       rm_var = "rcs(Spline2$V2,3)", add_var_str = "rcs(Spline2$V2, %d)", ic_fn=AIC)

perim <- with (Spline2, perimeter(V2, V3,) )
dd <- datadist (Spline2); options ( datadist ='dd')
f <-lrm(V1 ~ rcs (V2,3) * rcs (V3,3),
        data = Spline2, x = TRUE, y = TRUE)
print (anova (f), caption = 'Cubic spline surface',
       size = 'smaller')

png(file="TPG_TotalMass_Ratio_Levelplot_prime.png",pointsize=10,height=5000,width =5122  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       #screen=list(z=0,x=-45),
       col.regions = colorRampPalette(c("blue","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'Transpulmonary Gradient', ylim=c(-20,50),xlim=c(0.5,1.7),
       par.box = c(lwd = .1 ))
dev.off()

png(file="TPG_Totalmass_Ratio_wireframe_prime.png",pointsize=10,height=5000,width =5000  ,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe , drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       screen=list(z=20,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "Total Mass D/R Ratio", ylab = 'Transpulmonary Gradient', xlabrot =17, ylabrot = -69, zlabrot=274, 
       par.box = c(lwd = .1 ))
dev.off()




data = Spline2, x = TRUE, y = TRUE)
print (anova ( f) , caption = 'Cubic spline surface',
       size = 'smaller')


png(file="sys_RV_Ratio_levelplot_PRIMGF.png",pointsize=10,height=5000,width = 5122,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = levelplot, drape = TRUE,
       lwd=.1,
       alpha.regions=.85,
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Systolic PA Presure', ylim= c(0,100),
       par.box = c(lwd = .1 ))
dev.off()


png(file="sys_RV_Ratio_wirefram_PRIMGF.png",pointsize=10,height=5000,width = 5000,res=800, antialias = "default")
bplot (Predict (f,V2,V3), perim = perim,
       lfun = wireframe, drape = TRUE,
       lwd=.15,
       alpha.regions=.85,
       screen=list(z=12,x=-50),
       col.regions = colorRampPalette(c("navy","green4","yellow","red2"))(100),
       xlab = "RV D/R Ratio", ylab = 'Systolic PA Presure', xlabrot =17, ylabrot = -69, zlabrot=274,
       par.box = c(lwd = .1 ))
dev.off()


