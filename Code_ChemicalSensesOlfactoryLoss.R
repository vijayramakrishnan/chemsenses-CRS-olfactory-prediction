##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Project: Code for "Predicting Olfactory Loss In Chronic Rhinosinusitis Using Machine Learning"
## Last Edited: Jul 7, 2021
## Author: Jaron Arbet (edited by Krithika Suresh)
## Description: R code to train models and perform validation described in manuscript
## Inputs: 
## "dataset.xlsx": columns are the predictors, row for each patient in the data set
## "variable_labels.xlsx": 
##      - variable: name of variable in data set
##      - label: label for graphing purposes
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load packages ------------------------------------------------------------
library(arsenal)
library(gtsummary)
library(lubridate)
library(flextable)
library(caret)
library(missForest)
library(pander)
library(scales)
library(multcomp)
library(ggpubr)
library(iml)
library(readxl)
library(tidyverse)
library(ggpubr)
library(iml)

# fisher test function for gtsummary with simulate.pvalue=T
fisher2<- function(data, variable, by,...) {
  result <- list()
  result$p <- stats::fisher.test(data[[variable]], data[[by]],simulate.p.value = T,B=50000)$p.value
  result$test <- "Fisher's exact test"
  result
}

# Prepare data set --------------------------------------------------------
# set directory where data is located
dir0 <- "..."
setwd(dir0)

# variable names for plots 
labels <- read_excel(paste0(dir0,"/variable_labels.xlsx"))

# load in data set
dataset <- read_csv(paste0(dir0,"/dataset.csv"))

# education 
dataset$years_education=as.numeric(ifelse(dataset$EDUCATION=="20+",21,dataset$EDUCATION))
dataset$years_education[which(dataset$years_education==22)]=21

# modify/create variables:
pdata <- dataset %>% 
  mutate(
    HOUSEHOLD_INCOME = factor(HOUSEHOLD_INCOME, levels=c("0-25000","26000-50000","51000-75000","76000-100000","100000+")),
    PREVIOUS_SURGERY = factor(PREVIOUS_SURGERY),
    SMOKER_bin= ifelse(SMOKER==0,0,1),
    ALCOHOL_bin=ifelse(ALCOHOL==0,0,1),
    BSIT_DIAGNOSES = factor(BSIT_DIAGNOSES,levels=c(1,0),labels=c("Yes","No")),
    IMMUNODEF_BIN=factor(ifelse(!is.na(IMMUNODEF)&IMMUNODEF!=0,"Yes","No")),
    CF_or_ciliary_disfunction=factor(ifelse(CILIARY=="Cystic Fibrosis"|CILIARY=="Other","Yes","No")),
    AUTOIMMUNE_bin=factor(ifelse(AUTOIMMUNE!="None","Yes","No")),
    previous_surgeries = fct_recode(PREVIOUS_SURGERY,
                                    "4+" = "4",
                                    "4+" = "5",
                                    "4+" = "6+"
                                    
    ),
    STEROID_bin=factor(ifelse(STEROID!="None","Yes","No")),
    DIABETES_bin=factor(ifelse(DIABETES!="None","Yes","No")),
    insurance2 = fct_recode(INSURANCE,
                            "VA Benefits" = "VA Benefits / Tricare"
    ))

# binary 0/1 vars - 1 always equals yes
bin01vars=c("SEX","AFS","SEPT_DEV","CRS_POLYPS","RAS","HYPER_TURB","MUCOCELE","ASTHMA","ASA_INTOLERANCE","ALLERGY_HISTORY","ALLERGY_TESTING","COPD","DEPRESSION","FIBROMYALGIA","OSA_HISTORY",
            "SMOKER_bin","ALCOHOL_bin","GERD")

pdata %<>%  mutate_each_(funs(factor(.)),bin01vars)

# variables of interest 
outcomes=c("BLN_BSIT_SCORE","BSIT_DIAGNOSES","BSIT_DIAGNOSIS_string")

# don't use qol vars for now
qol_surveys=c("SNOT22_BLN_TOTAL","RSDI_PHYS_BLN_TOTAL","RSDI_FUNCT_BLN_TOTAL", "RSDI_EMOT_BLN_TOTAL" , "RSDI_BLN_TOTAL" ,"SF6D_HUV_BLN","PHQ2_BLN_TOTAL")

# predictors
table1vars=c("SITE","age_at_enrollm"      ,           "SEX"         ,        "RACE"        ,        "ETHNICITY"   ,
             "years_education" ,          "HOUSEHOLD_INCOME" ,   "previous_surgeries" ,   "INSURANCE"  ,
             "insurance2",
             "AFS"       ,          "SEPT_DEV"          ,  "CRS_POLYPS"      ,    "RAS"           ,     
             "HYPER_TURB"             ,        "ASTHMA"        ,     
             "ASA_INTOLERANCE" ,    "ALLERGY_HISTORY"  ,   "ALLERGY_TESTING" ,    "COPD"          ,     
             "DEPRESSION"   ,       "FIBROMYALGIA"     ,   "OSA_HISTORY"      ,   "OSA_TESTING"    ,     "SMOKER", "SMOKER_bin"            ,"ALCOHOL",  "ALCOHOL_bin"         ,   
             "CF_or_ciliary_disfunction"     ,        "AUTOIMMUNE_bin","IMMUNODEF_BIN"       ,   "STEROID_bin"           ,  "DIABETES_bin"         ,  
             "GERD"       ,         "BLN_CT_TOTAL"     ,   "BLN_ENDOSCOPY_TOTAL")

# quantitative vars
qvars=c("years_education","age_at_enrollm","BLN_CT_TOTAL","BLN_ENDOSCOPY_TOTAL")

# ml predictors
mlpredictors=c("SITE","age_at_enrollm"      ,           "SEX"         ,        "RACE"        ,        "ETHNICITY"   ,
               "years_education" ,          "HOUSEHOLD_INCOME" ,   "previous_surgeries" ,   "INSURANCE"    ,      
               "AFS"       ,          "SEPT_DEV"          ,  "CRS_POLYPS"      ,    "RAS"           ,     
               "HYPER_TURB"             ,        "ASTHMA"        ,     
               "ASA_INTOLERANCE" ,    "ALLERGY_HISTORY"  ,   "ALLERGY_TESTING" ,    "COPD"          ,     
               "DEPRESSION"   ,       "FIBROMYALGIA"     ,   "OSA_HISTORY"      ,   "OSA_TESTING"    ,     "SMOKER"            ,"ALCOHOL",     
               "CF_or_ciliary_disfunction"     ,        "AUTOIMMUNE_bin","IMMUNODEF_BIN"       ,   "STEROID_bin"           ,  "DIABETES_bin"         ,  
               "GERD"       ,         "BLN_CT_TOTAL"     ,   "BLN_ENDOSCOPY_TOTAL")

# Bivariate comparisons ---------------------------------------------------
# comparing variables between "smell loss" vs "no smell loss" groups
# fisher's exact test for categorical variables
# Welch's 2 sample t-test for continuous variables
# sorted from smallest to largest q-value (multiple testing adjusted p-value)

pdata2 <- pdata %>% dplyr::select(BSIT_DIAGNOSES,all_of(table1vars)) %>% filter(!is.na(BSIT_DIAGNOSES))

# update names
for(i in 1:ncol(pdata2)){
  ind=which(labels$variable==colnames(pdata2)[i])
  colnames(pdata2)[i]=labels$label[ind]
}

qvars2=vector()
for(i in 1:length(qvars)){
  ind=which(labels$variable==qvars[i])
  qvars2[i]=labels$label[ind]
}

tab1 <- pdata2 %>% 
  tbl_summary(by = `SIT_diagnosis`,missing="no",
              type = list(all_dichotomous() ~ "categorical",all_of(qvars2) ~ 'continuous'),
              digits = list(all_continuous() ~ c(1, 1))) %>% 
  add_n() %>%
  add_p(pvalue_fun = function(x) ifelse(x<0.001, "<0.001",format(round(x,3),nsmall=3)), test = list(all_categorical() ~ "fisher2")) %>%
  add_q() %>% 
  add_overall() %>% 
  bold_labels() %>% 
  bold_p(t = 0.05, q = FALSE) %>% 
  bold_p(t = 0.05, q = T) %>% 
  sort_p(q=T) %>% 
  gtsummary::as_flex_table()

#Table 1 in manuscript
tab1

# Machine Learning --------------------------------------------------------
# Skip running this section if you have already run the models and have saved them in the working directory 

# Models compared:
# Random Forests: allows for non-linear and interaction effects.  Conditional inference trees were used with permutation-based variable importance scores for unbiased variable selection [@strobl2007bias].
# Support vector machine with a radial basis kernel ("SVM-Radial") 
# Step-wise Logistic regression: traditional logistic regression but uses backwards step-wise AIC variable selection ("LogReg-StepAIC")
# LASSO: similar to traditional logistic regression except the regression coefficients are shrunk towards zero to perform variable selection (unimportant variables are given coefficients exactly equal to 0, effectively removing these variables from the model).  Assumes only linear and additive effects (i.e. no interactions)
# MARS: similar to traditional logistic regression but uses stepwise methods for variable selection, and allows for non-linear and interaction effects (unlike LASSO) 

# Evaluating classification accuracy: repeated 10-fold cross validation (5 repeats) was used to tune and evaluate the classification accuracy of each model (AUC, sensitivity, specificity)
# To deal with the fact that the two classes are unbalanced (smell loss vs no smell loss), "down sampling" was used within the cross-validation procedure, as implemented in the caret R package.  Up sampling was also tried but produced equal classification accuracy in terms of AUC.
# Missing Values: For Random Forests, the method of "surrogate splits"[@strobl2009introduction] was used to handle missing values.
# The missForest R package was used to impute missing values for all other models.

# Relative Variable Importance: Random forests used permutation-based variable importance scores to rank variables from most to least important, while LASSO ranks variables based on the absolute value of the standardized regression coefficient.  The IML R package was used to calculate variable importance scores for SVM using a permutation method.  All variable importance scores were scaled from 0 to 100, where the most important variable for the model has a score of 100.

#Note: for a given categorical variable, RF and LASSO calculate separate variable importance scores for each category, whereas the approach used for SVM only calculates an overall score. 

mldata <- pdata %>% dplyr::select(BSIT_DIAGNOSES,all_of(mlpredictors))

seed= 374
mldata=data.frame(mldata[-which(is.na(mldata$BSIT_DIAGNOSES)),])

# down sampling was used to deal with unbalanced classes, I also compared with up sampling but there was negligible difference
ctrl_bin_2c_down <- trainControl(method="repeatedcv", number=10, repeats=5, classProbs = TRUE,
                                 summaryFunction = twoClassSummary,sampling = "down", savePredictions = TRUE) 

# # uncomment if you want to use parallel processing
# library("doSNOW")
# cores=4 #number of cores you want to use; detectCores() from parallel R package to find available # cores
# cl <- makeCluster(cores)
# registerDoSNOW(cl)

#### Random Forest --------------------------
mtry=2:(ncol(mldata)-1)
tunegrid <- expand.grid(.mtry=mtry)
set.seed(seed)
start=Sys.time()
tune_cf_2c_down=train(BSIT_DIAGNOSES~.,data=mldata, method='cforest',
                      tuneGrid=tunegrid, trControl=ctrl_bin_2c_down,na.action = na.pass,metric="ROC")
end=Sys.time()
tune_cf_2c_down
end-start

# ML with imputed data ----------------------------
X=data.frame(mldata[,-which(colnames(mldata)=="BSIT_DIAGNOSES")])
X[sapply(X, is.character)] <- lapply(X[sapply(X, is.character)], 
                                     as.factor)
imputeX=missForest(X)
imputedata=data.frame(BSIT_DIAGNOSES=mldata[,which(colnames(mldata)=="BSIT_DIAGNOSES")],imputeX$ximp)

### MARS ----------------------------
degree=1:5
tunegrid <- expand.grid(.degree=degree)

set.seed(seed)
tune_mars_2c_down=train(BSIT_DIAGNOSES~.,data=imputedata,metric=c("ROC"), method='gcvEarth', tuneGrid=tunegrid, trControl=ctrl_bin_2c_down)
tune_mars_2c_down

### LASSO ----------------------------
X2=model.matrix(BSIT_DIAGNOSES~.,data=imputedata)[,-1]
Y=imputedata$BSIT_DIAGNOSES

cv=cv.glmnet(X2,Y,alpha=1,family="binomial")
lamseq=seq(min(cv$lambda),max(cv$lambda)*2,length.out = 200)

tunegrid <- expand.grid(.lambda=lamseq,.alpha=1)
set.seed(seed)
start=Sys.time()
tune_lasso_2c_down=train(X2,Y, method='glmnet',
                         tuneGrid=tunegrid, trControl=ctrl_bin_2c_down,na.action = na.pass,metric="ROC",preProc=c("center","scale"))
end=Sys.time()
tune_lasso_2c_down
end-start

#### GLM stepwise --------------------------
X=imputedata[,-1]
# down sampling
set.seed(seed)
start=Sys.time()
tune_glmStepAIC_2c_down=train(BSIT_DIAGNOSES~.,data=imputedata, method='glmStepAIC', trControl=ctrl_bin_2c_down,metric="ROC")
end=Sys.time()
tune_glmStepAIC_2c_down
end-start

#### SVM radial --------------------------
# down sampling
set.seed(seed)
start=Sys.time()
tune_svmRadial_2c_down=train(BSIT_DIAGNOSES~.,data=imputedata, method='svmRadialCost',
                             tuneGrid = expand.grid(C = seq(0.000001, 0.5, length = 100)),
                             trControl=ctrl_bin_2c_down,metric="ROC",preProcess = c("center","scale"))
end=Sys.time()
tune_svmRadial_2c_down
end-start

#### save ML results ---------------------
# save(imputedata,X2,Y,mldata,
#      tune_cf_2c_down,
#      tune_mars_2c_down,
#      tune_lasso_2c_down,
#      tune_glmStepAIC_2c_down,
#      tune_svmRadial_2c_down,
# file=paste0(dir0,"/mlmodels.RData"))

# Classification accuracy -------------------------------------------------
load(paste0(dir0, "/mlmodels.RData"))
 
cf_res=tune_cf_2c_down$results[which(tune_cf_2c_down$results$mtry==tune_cf_2c_down$bestTune$mtry),][c("ROC","Sens","Spec")]

lasso_res=tune_lasso_2c_down$results[which(tune_lasso_2c_down$results$lambda==tune_lasso_2c_down$bestTune$lambda),][c("ROC","Sens","Spec")]

mars_res=tune_mars_2c_down$results[which(tune_mars_2c_down$results$degree==tune_mars_2c_down$bestTune$degree),][c("ROC","Sens","Spec")]

glmStep_res=tune_glmStepAIC_2c_down$results[c("ROC","Sens","Spec")]

svmRadial_res=tune_svmRadial_2c_down$results[which(tune_svmRadial_2c_down$results$C==tune_svmRadial_2c_down$bestTune$C),][c("ROC","Sens","Spec")]

resamps <- resamples(list(RF = tune_cf_2c_down,
                          `SVM-Radial` = tune_svmRadial_2c_down,
                          LASSO = tune_lasso_2c_down,
                          `Log Reg-Step`=tune_glmStepAIC_2c_down,
                          MARS=tune_mars_2c_down))

# summary of performance metrics (mean, sd)
auc=resamps$values %>% dplyr::select(contains("ROC"))
sens=resamps$values %>% dplyr::select(contains("Sens"))
spec=resamps$values %>% dplyr::select(contains("Spec"))

temp_auc <- apply(auc,2,function(x) paste0(format(round(mean(x),3),nsmall=3), " (",format(round(sd(x),3),nsmall=3),")"))
temp_sens <- apply(sens,2,function(x) paste0(format(round(mean(x),3),nsmall=3), " (",format(round(sd(x),3),nsmall=3),")"))
temp_spec <- apply(spec,2,function(x) paste0(format(round(mean(x),3),nsmall=3), " (",format(round(sd(x),3),nsmall=3),")"))

tab_metrics <- data.frame(names(temp_auc),temp_auc,temp_sens,temp_spec)
tab_metrics <- tab_metrics[order(tab_metrics[,2],decreasing = TRUE),]
tab_metrics[,1] <- c("SVM-Radial","Random Forest","LASSO","Log Reg-Step","MARS")
colnames(tab_metrics) <- c("Model","Mean AUC (SD)", "Mean Sens (SD)", "Mean Spec (SD)")

# Table 2 in manuscript (Classification accuracy for the different models)
tab_metrics

# create data set for barplots 
tab_summ_full <- data.frame("Mean"=apply(resamps$values[,-1],2,mean), "SD" = apply(resamps$values[,-1],2,sd))
tab_summ_AUC <- tab_summ_full[grepl("ROC", rownames(tab_summ_full)),]
tab_summ_Sens <- tab_summ_full[grepl("Sens", rownames(tab_summ_full)),]
tab_summ_Spec <- tab_summ_full[grepl("Spec", rownames(tab_summ_full)),]
tab_summ_AUC$Model <- sub("\\~.*","", rownames(tab_summ_AUC))
tab_summ_AUC$Model <- factor(tab_summ_AUC$Model,levels=rev(c("MARS","Log Reg-Step","LASSO","RF","SVM-Radial")))

tab_summ_Sens$Model <- sub("\\~.*","", rownames(tab_summ_Sens))
tab_summ_Sens$Model <- factor(tab_summ_Sens$Model,levels=rev(c("MARS","Log Reg-Step","LASSO","RF","SVM-Radial")))

tab_summ_Spec$Model <- sub("\\~.*","", rownames(tab_summ_Spec))
tab_summ_Spec$Model <- factor(tab_summ_Spec$Model,levels=rev(c("MARS","Log Reg-Step","LASSO","RF","SVM-Radial")))

p_auc <- ggplot(tab_summ_AUC) +
  geom_bar( aes(x=Model, y=Mean), stat="identity", alpha=0.7) +
  geom_errorbar( aes(x=Model, ymin=Mean-SD, ymax=Mean+SD), width=0.2, alpha=0.9) + xlab("") + ylab("")+ ylim(c(0,.85)) + theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("AUC") 
p_auc

p_sens <- ggplot(tab_summ_Sens) +
  geom_bar( aes(x=Model, y=Mean), stat="identity", alpha=0.7) +
  geom_errorbar( aes(x=Model, ymin=Mean-SD, ymax=Mean+SD), width=0.2, alpha=0.9) + xlab("") + ylab("")+ ylim(c(0,.85)) + theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Sensitivity")
p_sens

p_spec<- ggplot(tab_summ_Spec) +
  geom_bar( aes(x=Model, y=Mean), stat="identity", alpha=0.7) +
  geom_errorbar( aes(x=Model, ymin=Mean-SD, ymax=Mean+SD), width=0.2, alpha=0.9) + xlab("") + ylab("")+ ylim(c(0,.85)) + theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Specificity")
p_spec

figure_metrics=ggarrange(p_auc,p_sens,p_spec,nrow=1,ncol=3)

# Figure (Classification accuracy for the different models)
figure_metrics
# ggsave("accuracy_wide_barplot.png",units="in",width=8,height=3,dpi=300)

# Comparison of model AUC -------------------------------------------------
# compare models using corrected resample t-test (Nadeau)
# data set of all of the pairwise comparisons  
combdat <- t(combn(1:5,2)) #10 combinations
# new data set containing estimates of differences in AUC
diff_auc <- data.frame("Comp"=NA,"Model1"=NA,"Model2"=NA, "MeanDiff"=NA, "SDDiff"=NA, "CorrectSDDiff"=NA, "Unadj.pval"=NA, "pval"=NA) 
# compute estimate, SD, and p-value for differences in auc for each pairwise comparison
for(i in 1:nrow(combdat)) {
  val1 <- auc[,combdat[i,1]]
  val2 <- auc[,combdat[i,2]]
  diff.name <- paste(names(auc)[combdat[i,1]], ",", names(auc)[combdat[i,2]])
  names1 <- names(auc)[combdat[i,1]]
  names2 <- names(auc)[combdat[i,2]]
  kr <- 10*5 #k*r: k=number of folds, r=number of repeats
  n1 <- 61*9 #n1: number of instances used for training
  n2 <- 61 #n2: number of instances used for testing
  
  mean.diff <- mean(val1-val2)
  sd.diff <- sd(val1-val2)
  correct.sd.diff <- sqrt((1/kr + n2/n1)*sd.diff^2)
  
  t<-mean.diff/sqrt(1/kr*sd.diff^2)
  pval.unadj <- 2*(1-pt(abs(t), df=kr-1))
  
  t <- mean.diff/correct.sd.diff
  pval <- 2*(1-pt(abs(t), df=kr-1))
  diff_auc[i,1:3] <- c(diff.name,names1,names2)
  diff_auc[i,4:8] <- c(mean.diff, sd.diff, correct.sd.diff, pval.unadj, pval)
}

# perform Benjamini-Hochberg adjustment of adjusted pvalue
diff_auc$pval.adjust <- p.adjust(diff_auc$pval, method = "BH")

# Code to create a Figure from "diff_auc" in a matrix plot 
diff_auc_sub <- diff_auc[,c("Model1","Model2","MeanDiff","pval.adjust")]
diff_auc_sub[,1]<- sub("\\~.*", "", diff_auc_sub[,1])
diff_auc_sub[,2]<- sub("\\~.*", "", diff_auc_sub[,2])

sub1 <- subset(diff_auc_sub, Model1=="SVM-Radial"|Model2=="SVM-Radial")
sub1$Model.x <- sub1$Model1
sub1$Model.y <- sub1$Model2
sub2 <- subset(diff_auc_sub, Model1=="SVM-Radial"|Model2=="SVM-Radial")
sub2$Model.x <- sub1$Model2
sub2$Model.y <- sub1$Model1
sub <- rbind(sub1, sub2)
sub$MeanDiff <- ifelse(sub$Model2=="SVM-Radial", -sub$MeanDiff, sub$MeanDiff)
sub$val <-NA
sub$col <- NA
sub$col.text <- NA
sub[sub$Model.x=="SVM-Radial","val"] <- paste0("q=",round(sub[sub$Model.x=="SVM-Radial","pval.adjust"],3))
sub[sub$Model.x=="SVM-Radial","col"] <- 1
sub[sub$Model.x=="SVM-Radial","col.text"] <- ifelse(sub[sub$Model.x=="SVM-Radial","pval.adjust"]<=0.05, "red","black")
sub[sub$Model.y=="SVM-Radial","val"] <- paste0("d=",round(sub[sub$Model.y=="SVM-Radial","MeanDiff"],3))
sub[sub$Model.y=="SVM-Radial","col"] <- 2
sub[sub$Model.y=="SVM-Radial","col.text"] <- ifelse(sub[sub$Model.y=="SVM-Radial","pval.adjust"]<=0.05, "red","black")

sub.RF1 <- subset(diff_auc_sub, Model1=="RF"|Model2=="RF")
sub.RF1$Model.x <- sub.RF1$Model1
sub.RF1$Model.y <- sub.RF1$Model2
sub.RF2 <- subset(diff_auc_sub, Model1=="RF"|Model2=="RF")
sub.RF2$Model.x <- sub.RF1$Model2
sub.RF2$Model.y <- sub.RF1$Model1
sub.RF <- rbind(sub.RF1, sub.RF2)
sub.RF$MeanDiff <- ifelse(sub.RF$Model2=="RF", -sub.RF$MeanDiff, sub.RF$MeanDiff)
sub.RF$val <-NA
sub.RF$col <- NA
sub.RF$col.text <- NA
sub.RF[sub.RF$Model.x=="RF","val"] <- paste0("q=",round(sub.RF[sub.RF$Model.x=="RF","pval.adjust"],3))
sub.RF[sub.RF$Model.x=="RF","col"] <- 1
sub.RF[sub.RF$Model.x=="RF","col.text"] <- ifelse(sub.RF[sub.RF$Model.x=="RF","pval.adjust"]<=0.05, "red","black")
sub.RF[sub.RF$Model.y=="RF","val"] <- paste0("d=",round(sub.RF[sub.RF$Model.y=="RF","MeanDiff"],3))
sub.RF[sub.RF$Model.y=="RF","col"] <- 2
sub.RF[sub.RF$Model.y=="RF","col.text"] <- ifelse(sub.RF[sub.RF$Model.y=="RF","pval.adjust"]<=0.05, "red","black")

sub.LASSO1 <- subset(diff_auc_sub, Model1=="LASSO"|Model2=="LASSO")
sub.LASSO1$Model.x <- sub.LASSO1$Model1
sub.LASSO1$Model.y <- sub.LASSO1$Model2
sub.LASSO2 <- subset(diff_auc_sub, Model1=="LASSO"|Model2=="LASSO")
sub.LASSO2$Model.x <- sub.LASSO1$Model2
sub.LASSO2$Model.y <- sub.LASSO1$Model1
sub.LASSO <- rbind(sub.LASSO1, sub.LASSO2)
sub.LASSO$MeanDiff <- ifelse(sub.LASSO$Model2=="LASSO", -sub.LASSO$MeanDiff, sub.LASSO$MeanDiff)
sub.LASSO$val <-NA
sub.LASSO$col <- NA
sub.LASSO$col.text <- NA
sub.LASSO[sub.LASSO$Model.x=="LASSO","val"] <- paste0("q=",round(sub.LASSO[sub.LASSO$Model.x=="LASSO","pval.adjust"],3))
sub.LASSO[sub.LASSO$Model.x=="LASSO","col"] <- 1
sub.LASSO[sub.LASSO$Model.x=="LASSO","col.text"] <- ifelse(sub.LASSO[sub.LASSO$Model.x=="LASSO","pval.adjust"]<=0.05, "red","black")
sub.LASSO[sub.LASSO$Model.y=="LASSO","val"] <- paste0("d=",round(sub.LASSO[sub.LASSO$Model.y=="LASSO","MeanDiff"],3))
sub.LASSO[sub.LASSO$Model.y=="LASSO","col"] <- 2
sub.LASSO[sub.LASSO$Model.y=="LASSO","col.text"] <- ifelse(sub.LASSO[sub.LASSO$Model.y=="LASSO","pval.adjust"]<=0.05, "red","black")

sub.Log1 <- subset(diff_auc_sub, Model1=="Log Reg-Step"|Model2=="Log Reg-Step")
sub.Log1$Model.x <- sub.Log1$Model1
sub.Log1$Model.y <- sub.Log1$Model2
sub.Log2 <- subset(diff_auc_sub, Model1=="Log Reg-Step"|Model2=="Log Reg-Step")
sub.Log2$Model.x <- sub.Log1$Model2
sub.Log2$Model.y <- sub.Log1$Model1
sub.Log <- rbind(sub.Log1, sub.Log2)
sub.Log$MeanDiff <- ifelse(sub.Log$Model2=="Log Reg-Step", -sub.Log$MeanDiff, sub.Log$MeanDiff)
sub.Log$val <-NA
sub.Log$col <- NA
sub.Log$col.text <- NA
sub.Log[sub.Log$Model.x=="Log Reg-Step","val"] <- paste0("q=",round(sub.Log[sub.Log$Model.x=="Log Reg-Step","pval.adjust"],3))
sub.Log[sub.Log$Model.x=="Log Reg-Step","col"] <- 1
sub.Log[sub.Log$Model.x=="Log Reg-Step","col.text"] <- ifelse(sub.Log[sub.Log$Model.x=="Log Reg-Step","pval.adjust"]<=0.05, "red","black")
sub.Log[sub.Log$Model.y=="Log Reg-Step","val"] <- paste0("d=",round(sub.Log[sub.Log$Model.y=="Log Reg-Step","MeanDiff"],3))
sub.Log[sub.Log$Model.y=="Log Reg-Step","col"] <- 2
sub.Log[sub.Log$Model.y=="Log Reg-Step","col.text"] <- ifelse(sub.Log[sub.Log$Model.y=="Log Reg-Step","pval.adjust"]<=0.05, "red","black")

sub.MARS1 <- subset(diff_auc_sub, Model1=="MARS"|Model2=="MARS")
sub.MARS1$Model.x <- sub.MARS1$Model1
sub.MARS1$Model.y <- sub.MARS1$Model2
sub.MARS2 <- subset(diff_auc_sub, Model1=="MARS"|Model2=="MARS")
sub.MARS2$Model.x <- sub.MARS1$Model2
sub.MARS2$Model.y <- sub.MARS1$Model1
sub.MARS <- rbind(sub.MARS1, sub.MARS2)
sub.MARS$MeanDiff <- ifelse(sub.MARS$Model2=="MARS", -sub.MARS$MeanDiff, sub.MARS$MeanDiff)
sub.MARS$val <-NA
sub.MARS$col <- NA
sub.MARS$col.text <- NA
sub.MARS[sub.MARS$Model.x=="MARS","val"] <- paste0("q=",round(sub.MARS[sub.MARS$Model.x=="MARS","pval.adjust"],3))
sub.MARS[sub.MARS$Model.x=="MARS","col"] <- 1
sub.MARS[sub.MARS$Model.x=="MARS","col.text"] <- ifelse(sub.MARS[sub.MARS$Model.x=="MARS","pval.adjust"]<=0.05, "red","black")
sub.MARS[sub.MARS$Model.y=="MARS","val"] <- paste0("d=",round(sub.MARS[sub.MARS$Model.y=="MARS","MeanDiff"],3))
sub.MARS[sub.MARS$Model.y=="MARS","col"] <- 2
sub.MARS[sub.MARS$Model.y=="MARS","col.text"] <- ifelse(sub.MARS[sub.MARS$Model.y=="MARS","pval.adjust"]<=0.05, "red","black")

fullgrid <- expand.grid("Model1"=(c("SVM-Radial","RF","LASSO","Log Reg-Step","MARS")), "Model2"=rev(c("SVM-Radial","RF","LASSO","Log Reg-Step","MARS")))
fullcomps <- rbind(sub[,c("Model.x","Model.y","val","col","col.text")], 
                   sub.RF[!sub.RF$Model.x%in%c("SVM-Radial")&!sub.RF$Model.y%in%c("SVM-Radial"),c("Model.x","Model.y","val","col","col.text")],
                   sub.LASSO[!sub.LASSO$Model.x%in%c("SVM-Radial","RF")&!sub.LASSO$Model.y%in%c("SVM-Radial","RF"),c("Model.x","Model.y","val","col","col.text")],
                   sub.Log[!sub.Log$Model.x%in%c("SVM-Radial","RF","LASSO")&!sub.Log$Model.y%in%c("SVM-Radial","RF","LASSO"),c("Model.x","Model.y","val","col","col.text")],
                   sub.MARS[!sub.MARS$Model.x%in%c("SVM-Radial","RF","LASSO","MARS")&!sub.MARS$Model.y%in%c("SVM-Radial","RF","LASSO","MARS"),c("Model.x","Model.y","val","col","col.text")])

fullgrid <- merge(fullgrid, fullcomps, by.x=c("Model1","Model2"), by.y=c("Model.x","Model.y"), all.x=TRUE)

#Figure (Comparing classification accuracy)
p_comp <- ggplot(fullgrid, aes(x=Model1,y=Model2,label=val,fill=factor(col))) + 
  geom_tile() + geom_text(aes(Model1, Model2, label = val, color = factor(col.text)), size = 4) +theme_bw() +xlab("") +ylab("") + scale_fill_grey(start=.9,end=.5,na.value="black") + scale_colour_manual(name="col.text",values=c("black","blue"))+ theme(legend.position="none")
p_comp
# ggsave("compare_accuracy_ttest.png",units="in",width=5,height=3,dpi=300)

# Variable Importance -----------------------------------------------------
seed=924
labels2 <- labels[which(labels$variable%in%mlpredictors),]

#### SVM radial --------------------------
set.seed(seed)
X = imputedata[,-1]
predictor = Predictor$new(tune_svmRadial_2c_down, data = X, y = imputedata$BSIT_DIAGNOSES)
imp = FeatureImp$new(predictor, loss =  "ce" ,n.repetitions = 40)

res=tune_svmRadial_2c_down$results[which(tune_svmRadial_2c_down$results$C==tune_svmRadial_2c_down$bestTune$C),][c("ROC","Sens","Spec")]
imp.dat <- imp$results
imp.dat$Feature=imp.dat$feature
imp.dat=imp.dat[order(imp.dat$importance),]
imp.dat$Importance=scales::rescale(imp.dat$importance,to=c(0,100))

vimp2=imp.dat

for(i in 1:nrow(labels2)){
  temp=str_detect(vimp2$Feature,labels2$variable[i])
  ind=which(temp)[1]
  fvar = str_length(vimp2$Feature[ind])!=str_length(labels2$variable[i])
  if(fvar){
    vimp2$Feature=str_replace(vimp2$Feature,labels2$variable[i], paste0(labels2$label[i],"="))
  } else {
    vimp2$Feature=str_replace(vimp2$Feature,labels2$variable[i], paste0(labels2$label[i],""))
  }
}
vimp2$Feature=factor(vimp2$Feature,levels=vimp2$Feature)

vsvmrad=ggplot(vimp2, aes(y = Feature, x = Importance)) +
  geom_bar(stat="identity")+
  xlab("Importance")+ylab("Feature")+ggtitle("SVM-Radial")+
  labs(caption=paste0("Cross-validation AUC=",round(res$ROC,2),", Sensitivity = ",round(res$Sens,2),", Specificity = ",round(res$Spec,2)))+theme(axis.text=element_text(size=7.5))

#Figure (Variable importance for SVM-Radial)
vsvmrad
# ggsave(file="vimp_svmRadial.png",dpi=300,units="in",width=5.5,height=4)

#### Random Forest --------------------------
res=tune_cf_2c_down$results[which(tune_cf_2c_down$results$mtry==tune_cf_2c_down$bestTune$mtry),]
set.seed(seed)
vimp=varImp(tune_cf_2c_down)
vimp2=data.frame(Feature=rownames(vimp$importance),Importance=vimp$importance$Overall)
vimp2=vimp2[order(vimp2$Importance,decreasing = F),]

for(i in 1:nrow(labels2)){
  
  temp=str_detect(vimp2$Feature,labels2$variable[i])
  ind=which(temp)[1]
  fvar = str_length(vimp2$Feature[ind])!=str_length(labels2$variable[i])
  if(fvar){
    vimp2$Feature=str_replace(vimp2$Feature,labels2$variable[i], paste0(labels2$label[i],"="))
  } else {
    vimp2$Feature=str_replace(vimp2$Feature,labels2$variable[i], paste0(labels2$label[i],""))
  }
}
ind=str_detect(vimp2$Feature,"=1$")
ind2=str_detect(vimp2$Feature,"Prior Surgery")
ind3=which(ind==T & ind2==F)
vimp2$Feature[ind3]=str_replace(vimp2$Feature[ind3],"=1","")
vimp2$Feature=str_replace(vimp2$Feature,"=Yes$","")

vimp2$Feature=factor(vimp2$Feature,levels=vimp2$Feature)
cf_vimp=vimp2

vcrf=ggplot(vimp2, aes(y = Feature, x = Importance)) +
  geom_bar(stat="identity")+
  xlab("Importance")+ylab("Feature")+ggtitle("Random Forest")+
  labs(caption=paste0("Cross-validation AUC=",round(res$ROC,2),", Sensitivity = ",round(res$Sens,2),", Specificity = ",round(res$Spec,2)))+theme(axis.text=element_text(size=7))

#### LASSO --------------------------
res=tune_lasso_2c_down$results[which(tune_lasso_2c_down$results$lambda==tune_lasso_2c_down$bestTune$lambda),]
set.seed(seed)
vimp=varImp(tune_lasso_2c_down)
vimp2=data.frame(Feature=rownames(vimp$importance),Importance=vimp$importance$Overall)
vimp2=vimp2[order(vimp2$Importance,decreasing = F),]

for(i in 1:nrow(labels2)){
  
  temp=str_detect(vimp2$Feature,labels2$variable[i])
  ind=which(temp)[1]
  fvar = str_length(vimp2$Feature[ind])!=str_length(labels2$variable[i])
  if(fvar){
    vimp2$Feature=str_replace(vimp2$Feature,labels2$variable[i], paste0(labels2$label[i],"="))
  } else {
    vimp2$Feature=str_replace(vimp2$Feature,labels2$variable[i], paste0(labels2$label[i],""))
  }
}
ind=str_detect(vimp2$Feature,"=1$")
ind2=str_detect(vimp2$Feature,"Prior Surgery")
ind3=which(ind==T & ind2==F)
vimp2$Feature[ind3]=str_replace(vimp2$Feature[ind3],"=1","")
#ind=str_detect(vimp2$Feature,"=Yes$")
vimp2$Feature=str_replace(vimp2$Feature,"=Yes$","")

vimp2$Feature=factor(vimp2$Feature,levels=vimp2$Feature)
lasso_vimp=vimp2

vlasso=ggplot(vimp2, aes(y = Feature, x = Importance)) +
  geom_bar(stat="identity")+
  xlab("Importance")+ylab("Feature")+ggtitle("LASSO")+
  labs(caption=paste0("Cross-validation AUC=",round(res$ROC,2),", Sensitivity = ",round(res$Sens,2),", Specificity = ",round(res$Spec,2)))+theme(axis.text=element_text(size=7))

#Supplementary Figure (Variable importance for Random Forest, LASSO)
figure=ggarrange(vcrf,vlasso, ncol = 2, nrow = 1)
# ggsave(file="vimp_cf_lasso.png",units="in",width=10,height=5,dpi=700)

# Top 10 predictors  ------------------------------------------------------
cf_vimp2=cf_vimp[(nrow(cf_vimp)-9):nrow(cf_vimp),]
vcrf=ggplot(cf_vimp2, aes(y = Feature, x = Importance)) +
  geom_bar(stat="identity")+
  xlab("Importance")+ylab("Feature")+ggtitle("Random Forest")

lasso_vimp2=lasso_vimp[(nrow(lasso_vimp)-9):nrow(lasso_vimp),]
vlasso=ggplot(lasso_vimp2, aes(y = Feature, x = Importance)) +
  geom_bar(stat="identity")+
  xlab("Importance")+ylab("Feature")+ggtitle("LASSO")

#Figure (Top 10 predictors for Random Forest, LASSO)
figure=ggarrange(vcrf,vlasso, ncol = 2, nrow = 1)
# ggsave(file="vimp_cf_lasso_top10.png",units="in",width=10,height=5,dpi=700)
