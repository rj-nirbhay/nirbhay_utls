
options(stringsAsFactors = FALSE)
options(scipen = 10)

# sample try-catch setup
tryCatch({
  cat("\n Exporting", file = log_file)
  write.csv(mtcars1,"testing of trycatch")
},
error= function(e){
  cat("\n Exporting is failed", file = log_file)
  quit(save = "no", status = 1)
}

)

### ===================== This scripts contains user defined functions =============================================

# -----------------------------------------------------------------------------------------------------------------
# Function 1:Lag Transformation
# -----------------------------------------------------------------------------------------------------------------

lagTransformation<- function(ds,n)
{
  # this function creates lag transformation of dataframe
  # args:
    # ds : Dataset
    # n : number of lags
  
  require(dplyr)
  require(purrr)
  if(!(class(ds)=="data.frame" & n%%1==0)) { stop("ds must be a data frame and n must be a integer ")}
  #stopifnot(class(ds)=="data.frame", n%%1==0)  
    
  lags <- seq(n)
  lag_names <- paste("lag", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "")
  lag_functions <- purrr::map(lags, ~ function(x) dplyr::lag(x, .x)) %>% 
    setNames(lag_names)
  ds <- ds %>% 
    dplyr::mutate_at(vars(names(ds)), lag_functions) %>% 
    dplyr::select(contains("_lag"))
  return(ds)
  
}

#----------------------------------------------------------------------------------------------------------------
# Function 2: Log Transformation
# -----------------------------------------------------------------------------------------------------------------

logTransformation<- function(ds)
{
  # this function creats log transformation of dataframe for only varibles which are positive in nature
  # args:
    # ds : Dataset
  
  require(dplyr)
  if(!class(ds)=="data.frame" ) { stop("ds must be a data frame")}
  
  ds <- ds %>% 
    dplyr::select_if(is.numeric)
  #smry<- sapply(ds,summary)
  #varList <- colnames(smry[,smry[1,]>0])
  varList<- names(ds)[sapply(ds, function(x) min(x,na.rm = T))>0]
  ds<- ds %>% 
    dplyr::select(all_of(varList)) %>% 
    dplyr::mutate_at(setNames(varList, paste0(varList,"_log")), log) %>% 
    dplyr::select(contains("_log"))
  return(ds)
}

# -----------------------------------------------------------------------------------------------------------------
# Function 3: Growth Variable Generator
# -----------------------------------------------------------------------------------------------------------------

GrowthGenerator<- function(ds,n, varNameSuffix="Gwth")
{
  # this function creats growth variables for continueous series
  # args : 
    # ds: Dataset
    # n : Growth lag in units
    # varNameSuffix : varible names suffix e.g. quartely, yearly etc depending upon the value of n
  
  require(dplyr)
  if(!(class(ds)=="data.frame" & n%%1==0)) { stop("ds must be a data frame and n must be a integer ")}
  
  ds <- ds %>% 
    dplyr::select_if(is.numeric)
  varList <- names(ds)[sapply(ds, function(x) min(x,na.rm = T)) != 0]
  ds <- ds %>% 
    dplyr::select(varList) %>% 
    dplyr::mutate_at(setNames(varList, paste0(varList,"_",varNameSuffix)), function(x) x/dplyr::lag(x,n)-1) %>% 
    dplyr::select(contains(varNameSuffix))
  return(ds)
  
}

# -----------------------------------------------------------------------------------------------------------------
# Function 4: Difference Variable Generator
# -----------------------------------------------------------------------------------------------------------------

DiffGenerator<- function(ds,n)
{
  # this function generates difference variables
  # args :
    # ds : Dataset
    # n : number of difference
  
  require(dplyr)
  if(!(class(ds)=="data.frame" & n%%1==0)) { stop("ds must be a data frame and n must be a integer ")}
  
  ds <- ds %>% 
    dplyr::select_if(is.numeric)
  diff_names <- paste0("diff", n, sep = "")
  ds <-ds %>% 
    dplyr::mutate_at(setNames(names(ds),paste0(names(ds),"_",diff_names)), function(x) x-dplyr::lag(x,n)) %>% 
    dplyr::select(contains("diff"))
  return(ds)
}

# -----------------------------------------------------------------------------------------------------------------
# Function 5: Mean Absolute Error Calculator and RMSE
# -----------------------------------------------------------------------------------------------------------------

MAPE<-function(actual,predicted){
  # Calculates the Mean absolute error
  # args :
    # actual : vector of actual values
    # predicted : vector of predicted values
  #if((is.numeric(actual) & is.numeric(predicted))) { stop("actual and predicted must be numeric")}
  
  result <- as.matrix(abs((actual-predicted)/actual))*100
  result.mean <- rowMeans(result, na.rm = TRUE)
  #plot(result.mean, type = "l")
  MAPE <- mean(result.mean,na.rm = TRUE)
  return(MAPE)
}

RMSE<-function(actual,predicted){
  # Calculates the Mean absolute error
  # args :
  # actual : vector of actual values
  # predicted : vector of predicted values
  #if((is.numeric(actual) & is.numeric(predicted))) { stop("actual and predicted must be numeric")}
  
  result <- as.matrix((actual-predicted)^2)
  result.mean <- rowMeans(result, na.rm = TRUE)
  #plot(result.mean, type = "l")
  RMSE <- sqrt(result.mean,na.rm = TRUE)
  return(RMSE)
}


# -----------------------------------------------------------------------------------------------------------------
# Function 6: Stationary Test - ADF, KPSS, PP
# -----------------------------------------------------------------------------------------------------------------

stationarity_checks<-function(df)
  # This function summarise the test Results from ADF, KPSS and PP test into list  
{
  require(urca)
  
  if(!class(df)=="data.frame" ) { stop("ds must be a data frame")}
  
  #Adf Test
  adf_zero_mean<-lapply(df,function(x) {summary(ur.df(x,type = "none",selectlags = "BIC") )})
  adf_single_mean<-lapply(df,function(x) {summary(ur.df(x,type = "drift",selectlags = "BIC") )})
  adf_trend<-lapply(df, function(x) {summary(ur.df(x,type = "trend",selectlags = "BIC") )})
  
  #Kpss test
  kpss_trend<- lapply(df,function(x){ summary(ur.kpss(x,type="tau")) })
  kpss_single_mean<- lapply(df,function(x){ summary(ur.kpss(x,type="mu")) })
  
  #PP test
  pp_trend<-lapply(df,function(x){ summary(ur.pp(x,type="Z-tau",model = "trend")) })
  pp_single_mean <- lapply(df,function(x){ summary(ur.pp(x,type="Z-tau",model = "constant")) })
  
  #test-stat
  adf_test_stat<-c(adf_zero_mean[[1]]@teststat, adf_single_mean[[1]]@teststat[1], adf_trend[[1]]@teststat[1])
  kpss_test_stat<-c(0,kpss_single_mean[[1]]@teststat, kpss_trend[[1]]@teststat)
  pp_test_stat<-c(0,pp_single_mean[[1]]@teststat, pp_trend[[1]]@teststat)
  
  
  # critical value
  
  adf_critical_val<-c(adf_zero_mean[[1]]@cval[2], adf_single_mean[[1]]@cval[2], adf_trend[[1]]@cval[2])
  kpss_critical_val<-c(0, kpss_single_mean[[1]]@cval[2], kpss_trend[[1]]@cval[2])
  pp_critical_val<-c(0, pp_single_mean[[1]]@cval[2], pp_trend[[1]]@cval[2])
  
  
  #test stats vs critacal /pvalues
  #adf<-adf_test_stat <= adf_critical_val
  adf_pval<- c(adf_zero_mean[[1]]@testreg[[4]][7], adf_single_mean[[1]]@testreg[[4]][11], adf_trend[[1]]@testreg[[4]][14])
  adf<-adf_pval< 0.05  #if true then staionary
  kpss<- kpss_test_stat <= kpss_critical_val #if true then staionary
  pp<- pp_test_stat >= pp_critical_val  #if true then staionary
  
  #Matrix for test statistics and critcal/p-val
  
  #test
  test_stat <- t(rbind(adf_test_stat,kpss_test_stat,pp_test_stat))
  rownames(test_stat)<-c("Zero_Mean","Single_Mean","Trend")
  
  #critcal/p-value
  critical_val <- t(rbind(adf_pval,kpss_critical_val,pp_critical_val))
  rownames(critical_val)<-c("Zero_Mean","Single_Mean","Trend")
  
  final<-t(rbind(adf,kpss,pp))
  rownames(final)<-c("Zero_Mean","Single_Mean","Trend")
  
  final[final == 'TRUE'] <- "Stationary"
  final[final == 'FALSE'] <- "Non-Stationary"
  
  final[1,2]<-"NA"
  final[1,3]<-"NA"
  print("Test Statistics Matrix is :-")
  print(test_stat)
  print("Critical values Matrix is :-")
  print(critical_val)
  print("Stationary Results")
  print(final)
  #return(final)
  
}

# -----------------------------------------------------------------------------------------------------------------
# Function 7: Linear OLS regression model fit
# -----------------------------------------------------------------------------------------------------------------

mlr_mod<-function(ds,regressors,responseVar,sampleSplit=0.7,var_selection=c("stepwise","None"),stepwiseDirection=c("both", "backward", "forward"),alpha=0.95)
{
  # this functions fits linear regressors with stepwise selection
  # args : 
    # regressors : regressors list as vector
    # responseVar : dependent variable as vector or string
    # ds : dataset
    # sampleSplit : factor to select train data
    # var_selection : variable selection algorithm, use 'stepwise' for stepwise
    # stepwise_direction : direction for stepwise filtering, forward, backward and both can be used
    # alpha : significance level for calculation of lower bound and upper bound, default value 95%
    
  # Example :
  # responseVar<-"x"
  # alpha=0.95
  # var_names <- names(in_frame)
  # regressors <- var_names[-which(var_names == responseVar)] 
  # h<-mlr_mod(regressors,responseVar,in_frame,out_frame,var_selection = "Others")
  
  Packages <- c("MASS","car", "dplyr")
  lapply(Packages, require, character.only = TRUE)
  #if(!(class(ds)=="data.frame" & is.numeric(sampleSplit) & is.numeric(alpha) & sampleSplit<=1 & )) { stop("ds must be a data frame and n must be a integer ")}
  var_selection <- match.arg(var_selection)
  stepwiseDirection <- match.arg(stepwiseDirection)
  #match.arg(var_selection,stepwiseDirection,several.ok=T)
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  RowIndex <- base:: sample(1:nrow(ds), sampleSplit*nrow(ds))  # row indices for training data
  in_frame <- ds[RowIndex, ]  # model training data
  out_frame  <- ds[-RowIndex, ] 
  
  form <- paste(regressors, collapse = '+')
  form_in <- formula(paste(responseVar, '~', form))
  baseModel <- lm(form_in,data = in_frame)
  
  if (var_selection == "stepwise") {
    final_model <- MASS::stepAIC(baseModel,direction = stepwiseDirection,trace = FALSE)
  } else { final_model <- baseModel}
  
  smry<-summary(final_model)
  
  # coefficient estimation
  estimates<- as.data.frame(smry$coefficients)
  colnames(estimates) <- c("Coefficient","StdError","tValue","pValue")
  estimates<-data.frame(Variable=row.names(estimates),estimates)
  estimates$Variable <- as.character(estimates$Variable)
  estimates$stdCoeff<- abs(estimates$Coefficient/estimates$StdError)
  # VIF
  if (nrow(estimates)<=2) {
    cat("model contains fewer than 2 terms, Hence VIF calculation is not possible",'\n')
    vif_init<-data.frame(Variable=c(unique(estimates$Variable)),VIF="NA")}
  else {
    vif_init<-data.frame(VIF=c("(Intercept)"="NA",car::vif(final_model)))
    vif_init<-data.frame(Variable=row.names(vif_init),vif_init)
  }
  vif_init$Variable <- as.character(vif_init$Variable)
  
  # Lower and uper bound calculation at alpha C.I.
  coefBoundMat<-as.data.frame(confint(final_model,level = alpha))
  coefBoundMat<- data.frame(Variable=row.names(coefBoundMat),coefBoundMat)
  colnames(coefBoundMat)<- c("Variable",paste0("LB(",100*alpha,"%C.I.)"),paste0("UB(",100*alpha,"%C.I.)"))
  coefBoundMat$Variable <- as.character(coefBoundMat$Variable)
  # Variable Importance rank calculation based on standard coefficient
  VarImpRank <- estimates %>% 
    dplyr::select("Variable","stdCoeff") %>% 
    dplyr::filter(Variable != "(Intercept)") %>% 
    dplyr::arrange(-stdCoeff) %>% 
    dplyr::mutate(Variable_Importance_Rank=seq(1:(nrow(estimates)-1))) %>% 
    dplyr::select(Variable,Variable_Importance_Rank)
  
  VarImpRank <- rbind(VarImpRank,data.frame(Variable="(Intercept)",Variable_Importance_Rank="NA"))
  VarImpRank$Variable <- as.character(VarImpRank$Variable)
  # model summary
  modelSummary <- list(estimates,VarImpRank,vif_init,coefBoundMat) %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="Variable"), .)
  
  rSquare_inFrame <- data.frame(Index=c("Rsquare","adjRsquare"),Value=c(100*smry$r.squared,100*smry$adj.r.squared))
  rSquare_inFrame$Value<- round(rSquare_inFrame$Value,2)
  fStats_inFrame <- rbind(data.frame(Value=smry$fstatistic), data.frame(Value=pf(smry$fstatistic[1],smry$fstatistic[2],smry$fstatistic[3],lower.tail = F)))
  fStats_inFrame$Index <- c("F-statistics","numdf","dendf","pVal") 
  fStats_inFrame$Value<- round(fStats_inFrame$Value,2)
  
  modelStats = rbind(rSquare_inFrame,fStats_inFrame)
  
  # prediction on train data
  pred_in_frame<-predict(final_model,newdata = in_frame)
  in_frame_MAPE<-data.frame(Index="MAPE_train",Value=MAPE(in_frame %>%dplyr::select(all_of(responseVar)),pred_in_frame))
  
  #prediction on test data
  pred_out_frame<-predict(final_model,newdata = out_frame)
  out_frame_MAPE<-data.frame(Index="MAPE_test",Value=MAPE(out_frame %>%dplyr::select(all_of(responseVar)),pred_out_frame))
  
  overallPred<-rbind(data.frame(Predicted=pred_in_frame),data.frame(Predicted=pred_out_frame))
  overallMAPE<-rbind(in_frame_MAPE,out_frame_MAPE)
  overallMAPE$Value <- round(overallMAPE$Value,2)
  Actuals <- rbind(in_frame,out_frame) %>%dplyr::select(all_of(responseVar))
  overallPredData<-cbind(Actuals,overallPred)
  
  rownames(modelSummary) <- NULL
  rownames(modelStats) <- NULL
  rownames(overallMAPE) <- NULL
  rownames(overallPredData) <- NULL
  
  return(list("ModelSummary" = modelSummary,
              "ModelStats" = modelStats,
              "MAPE" = overallMAPE,
              "FinalDataSet" = overallPredData,
              "ModelEqn" = final_model))
  
}

# -----------------------------------------------------------------------------------------------------------------
# Function 8: Count of missing values for character variables 
# -----------------------------------------------------------------------------------------------------------------

charSummary <- function(df){
  
  num        <- vector(mode = "character")
  char       <- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]]) == "numeric") {
      num    <- c(num, names(df[var]))
    }else if (class(df[[var]]) == "factor" || class(df[[var]]) == "character") {
      char   <- c(char, names(df[var]))
    }
  }
  
  if (length(char)!=0){
    dfchar   <- subset(df, select=char)
    E        <- sapply(dfchar, function(x) as.character(x))
    EE       <- as.data.frame(E)
    n        <- as.data.frame(sapply(EE, function(x) sum(!is.na(x))))
    n        <- data.frame(n)
    colnames(n) <- "n"
    
    n1       <- nrow(df)
    
    #missing value computation
    miss     <- sapply(EE, function(x) sum(is.na(x)))
    miss     <- as.data.frame(miss)
    g3       <- cbind(n, miss)
    perc     <- (miss/n1)*100
    m3       <- cbind(g3, perc)
    colnames(m3)[ncol(m3)] <- "miss%"
    
    #All level count
    All_level <- function(x){
      tbl_x             <- table(x)
      alllevel           <- sort(tbl_x, decreasing = TRUE)[1:ifelse(length(tbl_x) >= 0, yes = length(tbl_x), no = length(tbl_x))]
      alllevelcount <- paste0(names(alllevel), ":", alllevel)
    }
    
    unique     <- sapply(EE, function(x) length(unique(x)))
    unique_val <- sapply(EE, function(x) paste0(All_level(x), collapse = ", "))
    m4         <- cbind.data.frame(m3, unique, "All_level:count" = unique_val)
    
    return(m4)
  }
}

# -----------------------------------------------------------------------------------------------------------------
# Function 9: Opposite of %in%
# -----------------------------------------------------------------------------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

# -----------------------------------------------------------------------------------------------------------------
# Function 10: Left, Right and Mid equivalent of Excel
# -----------------------------------------------------------------------------------------------------------------

left = function(text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# -----------------------------------------------------------------------------------------------------------------
# Function 11: Linear OLS model fit assumption test
# -----------------------------------------------------------------------------------------------------------------

# Function 11(a): Linear OLS model serial correlation test

durbinWatsonTestResult<- function(model,maxLag =5)
{
  
  # this function test Serial Correlation of residuals of OLS model
  # args :
    # model : fitted OLS model
    # maxLag : number of orders 
  
  # Hypothesis of test :
    # Null hypothesis (H0) : There is no correlation among residuals.
    # Alternative hypothesis (H1): Residuals are auto-correlated.
  
  require(car)
  if(!(class(model)=="lm" & maxLag%%1==0)) { stop("model must be a lm type and maxLag must be a integer ")}
  
  set.seed(123)
  dw <- car::durbinWatsonTest(model,max.lag = maxLag)
  dw_result <- setNames(data.frame(matrix(ncol = 3, nrow = maxLag)), c("Order", "DW", "Pr<DW"))
  dw_result <- dw_result  %>% dplyr::mutate(Order = seq(1:maxLag),
                                            DW = unlist(dw$dw),
                                            "Pr<DW" = unlist(dw$p)
                                            )
  return(dw_result) # if p value is near from zero it means one can reject the null.
}

# Function 11(b): Linear OLS model Homoscedasticity test

BreuschPaganTest <- function(model,alpha=0.95)
{
  # this function tests Heteroskedasticity of errors of OLS model
  # args : 
    # model : fitted OLS model
    # alpha : significance level for calculation of lower bound and upper bound, default value 95%
  
  # Hypothesis of test :
    # Null hypothesis : Series is Homoscedastic in nature
    # Alternate hypothesis: Series is Hetroscedastic in nature
  
  require(lmtest)
  require(sandwich)
  if(!(class(model)=="lm" & is.numeric(alpha))) { stop("model must be a lm type and alpha between (0,1)")}
  NeweyWestEst <- NULL
  
  bpTest<-lmtest::bptest(model)
  bpTestResult<-data.frame(Method=bpTest$method,
                           BPTestStatistic=bpTest$statistic,
                           DF=bpTest$parameter,
                           pValue = bpTest$p.value)
  
  if (bpTestResult$pValue<=1- alpha) {
    
    Remark <- paste0("Bp test result: Hetroscedasticity is present. ","Hence estimation of new std error are done based on NeweyWest method")
    
    NeweyWestEst<- lmtest::coeftest(model, vcov=NeweyWest(model,verbose=T))
    NeweyWestEst <- data.frame(NeweyWestEst[,])
    NeweyWestEst<- setNames(data.frame(Variable=row.names(NeweyWestEst),NeweyWestEst), c("Variable","Coefficient","StdError","tValue","pValue"))
    rownames(NeweyWestEst) <- NULL
    
  } else {Remark <- paste0("Bp test result: Homoscedasticity is present. ","Hence no need for Newey West Estimation" )}
  
  return(list(TestResult = Remark, BPTestStats=bpTestResult,NeweyWestEstimates=NeweyWestEst))
  
}

# Function 11(c): Linear OLS model Normality test

mlr_NormalityTest<- function(model, alpha=0.95)
{
  # this function tests Normality of errors of OLS model
  # args : 
    # model : fitted OLS model
    # alpha : significance level for calculation of lower bound and upper bound, default value 95%
  
  # Hypothesis of test :
    # Null hypothesis : Series is non Normal in nature
    # Alternate hypothesis: Series is Normal in nature
  
  require(stats)
  require(goftest)
  require(nortest)
  
  if(!(class(model)=="lm" & is.numeric(alpha))) { stop("model must be a lm type and alpha between (0,1)")}
  y   <- model$residuals
  
  ks  <- stats::ks.test(y, "pnorm", mean(y), sd(y))
  sw  <- stats::shapiro.test(y)
  cvm <- goftest::cvm.test(y)
  ad  <- nortest::ad.test(y)
  
  testResults<- data.frame(Test =c("Kolmogorv-Smirnov", "Shapiro-Wilk","Cramer-von Mises","Anderson-Darling"),
                 statistics=c(ks$statistic,sw$statistic,cvm$statistic,ad$statistic),
                 pValue = c(ks$p.value,sw$p.value,cvm$p.value,ad$p.value),
                 Type =c(ks$method,sw$method,cvm$method[1],ad$method)) %>%
    dplyr::mutate(Result = ifelse(pValue < 1-alpha,"Normal Distn","Non-Normal"))
  
  rownames(testResults) <- NULL
  
  return(testResults)
  
}

# # 11(a): Coefficient sensitivity
# # 11(b): variable sensitivity
# 
# sensitivityTest<- function(model,factor=2)
# {
#   mod<-mlr_mod(regressors,y_var,in_frame,out_frame,var_selection = "Others")
#   existingEst <- mod$ModelSummary %>% dplyr::select(Variable,Coefficient,StdError) %>%
#     mutate(StdError1 = StdError -factor)
#    
# }


# -----------------------------------------------------------------------------------------------------------------
# Function 12: File size calculations
# -----------------------------------------------------------------------------------------------------------------

FileSizeDetails <- function(Path,Pattern= ".",Units= "weeks",Recursive = F){
  # this function gets list of files within a directory and sub directories with Size and date modified
  # args:
    # Path : directory path
    # Pattern : type of files
    # Units : should be one of “auto”, “secs”, “mins”, “hours”, “days”, “weeks”
    # Recursive : TRUE if list down all sub directory files else FALSE
  
  require(dplyr)
  
  files <- list.files(path=Path, pattern=Pattern, full.names=TRUE, recursive=Recursive)
  x <- file.info(files,extra_cols = T)
  x<- data.frame(FileName=row.names(x),x,rownames=F) %>% 
    dplyr::mutate(FileName = as.character(FileName),
                  SizeInMB=round(size/1024,2),
                  LastModified = paste0(round(difftime(Sys.time(),mtime, units = Units),0)," ",Units," ago"),
                  LastAccessed = paste0(round(difftime(Sys.time(),atime, units = Units),0)," ",Units," ago"),
                  UserName = uname,
                  Directory = dirname(FileName),
                  LastModifiedDate = mtime
    ) %>% 
    dplyr::select(FileName,Directory,SizeInMB,UserName,LastModifiedDate,LastModified,LastAccessed) %>% 
    dplyr::arrange(-SizeInMB)
  
  return(x)
}

# -----------------------------------------------------------------------------------------------------------------
# Function 13: add prefix or suffix to column names
# -----------------------------------------------------------------------------------------------------------------

appendDataFrameColumns<-function(ds, prefix="", suffix="",VarsToReplace=names(ds)) { 
  # this function add prefix and suffix to column names 
  # args:
    # ds : dataset
    # prefix : prefix char
    # suffix : suffix char
    # VarsToReplace : list of variables where change is required
  require(dplyr)
  if(!class(ds)=="data.frame") { stop("ds must be a data.fame type ")}
  
  ds <- ds %>% 
    dplyr:: rename_at(vars(all_of(VarsToReplace)), ~ paste0(prefix, . , suffix))  

  return(ds) 
}

# # Old version of same function
# appendDataFrameColumns<-function(df, prefix="", suffix="", sep="") { 
#   colnames(df) <- paste(prefix, colnames(df), suffix, sep=sep) 
#   return(df) 
# }


# -----------------------------------------------------------------------------------------------------------------
# Function 14: Object Size of R environment
# -----------------------------------------------------------------------------------------------------------------

CheckObjectSize <- function(topN = 10){
  
  # this function list top N objects that take the most memories
  # args : 
    # topN = numeric 
  require(dplyr)
  
 data.frame(Object= ls(envir = .GlobalEnv),
             SizeMB = round(unlist(lapply(ls(envir = .GlobalEnv), function(x){
               object.size(get(x, envir = .GlobalEnv))}))/1024/1024,2)) %>% 
    dplyr::arrange(desc(SizeMB)) %>% 
    dplyr::mutate(SizePct = 100*round(SizeMB/sum(SizeMB),2)) %>% 
    head(n=topN)

}

# -----------------------------------------------------------------------------------------------------------------
# Function 15: Month Seq for monthyear format
# -----------------------------------------------------------------------------------------------------------------

MonthSeqGenerator<- function(YearMonth = format(Sys.Date(),"%Y%m"),numMonths=36){
  
  # this function generates yearMonth seq
  # args: 
    # YearMonth = charcater e.g. "202001" for Jan2020
    # numMonths = integer, defines sequence length
  
  require(zoo)
  unlist(lapply(
      numMonths:1,
      FUN = function(x){as.numeric(format(zoo::as.yearmon(as.character(YearMonth), '%Y%m') - (x-1) /12 , '%Y%m'))
      }
    ))
}

# -----------------------------------------------------------------------------------------------------------------
# Function 16: Package Installations and load
# -----------------------------------------------------------------------------------------------------------------

ReqPkgInstallation <- function(list_of_packages){
  
  # this function generates load pkg
  # args: 
    # list_of_packages : list of pkgs

  newPkgs <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(newPkgs)>0) {install.packages(newPkgs,dependencies = TRUE)}
  sapply(list_of_packages, require, character.only = TRUE)
}

# -----------------------------------------------------------------------------------------------------------------
# Function 17: Proportion Calculation
# -----------------------------------------------------------------------------------------------------------------

propCalculation<- function(ds,varList,groupByVars,digits=4){
  
  # this function generates yearMonth seq
    # args: 
    # varList : list of variables
    # groupByVars : Group by variable at which prop is calculated
  require(dplyr)
  if(!class(ds)=="data.frame") { stop("ds must be a data.frame type")}
  
  ds<- ds %>% 
    #dplyr::group_by(!!!groupByVars) %>% 
    dplyr::mutate_at(vars(varList),list("percent" = ~ round(./sum(.),digits))) %>%
    dplyr::ungroup()
  
  return(ds)
}

# -----------------------------------------------------------------------------------------------------------------
# Function 18: Get Common Column names
# -----------------------------------------------------------------------------------------------------------------

GetCommonNames<- function(tbl1, tbl2){
  
  # this function get common names between two dataframes
    # args: 
    # tbl1 , tbl2 : data.frames
  
  require(dplyr)
  if(!(class(tbl1)=="data.frame" & class(tbl2)=="data.frame")) { stop("ds must be a data.frame type")}
  
    tbl1Names<- tbl1 %>% head(2) %>% dplyr::collect() %>% names()
    tbl2Names<- tbl2 %>% head(2) %>% dplyr::collect() %>% names()
    
    return(dplyr::intersect(tbl1Names,tbl2Names))
}

# -----------------------------------------------------------------------------------------------------------------
# Function 19: Get basic stats of numeric column in dataset
# -----------------------------------------------------------------------------------------------------------------

GetSummary<- function(ds){
  
  # this function get summary of data frame
  # args: 
    # ds : data.frames
  
  require(dplyr)
  if(!class(ds)=="data.frame") { stop("ds must be a data.frame type")}

  ds <-ds %>% dplyr::select_if(is.numeric) 
  results<- t(rbind(ParameterName=names(ds),
                           dataType=sapply(lapply(ds, class), "[", 1),
                           NACnt=sapply(ds, function(x) sum(is.na(x))),
                           PercntFill=100*sapply(ds, function(x) 1-(sum(is.na(x))/length(x))),
                           round(sapply(ds,summary),2)))
  
  rownames(results) <- NULL

  return(results)
  # other way for summary
  # results<- data.frame(
  #   ParameterName=names(ds),
  #   dataType=sapply(lapply(ds, class), "[", 1),
  #   NACnt=sapply(ds, function(x) sum(is.na(x))),
  #   PercntFill=sapply(ds, function(x) sum(is.na(x))/length(x)),
  #   min = sapply(ds, function(x) min(x,na.rm = T)),
  #   max = sapply(ds, function(x) max(x,na.rm = T)),
  #   median = sapply(ds, function(x) median(x,na.rm = T)),
  #   sum = sapply(ds, function(x) sum(x,na.rm = T)),
  #   avrg = sapply(ds, function(x) mean(x,na.rm = T))
  #   )
}


# -----------------------------------------------------------------------------------------------------------------
# Function 20: Get Fill by Columns in dataset
# -----------------------------------------------------------------------------------------------------------------

FillCount<- function(ds){
  # this function get datatype and fill count of data frame
    # args: 
    # ds : data.frames
  
  baseSummary<- t(rbind(ParameterName=names(ds),
                        dataType=sapply(lapply(ds, class), "[", 1),
                        NACnt=sapply(ds, function(x) sum(is.na(x))),
                        PercntFill=100*sapply(ds, function(x) 1-(sum(is.na(x))/length(x)))))
  rownames(baseSummary) <- NULL
  
  return(baseSummary)
}

# -----------------------------------------------------------------------------------------------------------------
# Function 21: Fill values from above cell: equivalent to drag down of excel
# -----------------------------------------------------------------------------------------------------------------

FillValuesFromRowsAbove <- function(x, Pattern = is.na) {
  # this function fills values from cell above to the cells present in pattern
  # args: 
    # x : vector
    # pattern : pattern where values to be filled
  if (is.function(Pattern)) {
    isnotPattern <- !Pattern(x)
  } else {
    isnotPattern <- x != Pattern
  }
  # Fill down
  x[which(isnotPattern)][cumsum(isnotPattern)]
}  

# -----------------------------------------------------------------------------------------------------------------
# Function 22: Factor to character datatype
# -----------------------------------------------------------------------------------------------------------------

factToChar<- function(x){
  # this function chnage factor datatype into char 
  # args: 
    # x : data.frames/vector
  fctr.cols = sapply(x, is.factor)
  x[, fctr.cols] = sapply(x[, fctr.cols], as.character)
  return(x)
  
}


# -----------------------------------------------------------------------------------------------------------------
# Function 23: Text Cleaning
# -----------------------------------------------------------------------------------------------------------------

Textprocessing <- function(x)
{
  gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('#\\S+', '', x) ## Remove Hashtags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuations
  gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  gsub(' +',' ',x) ## Remove extra whitespaces
}

# -----------------------------------------------------------------------------------------------------------------
# Function 24: Get First row as headers
# -----------------------------------------------------------------------------------------------------------------

FirstRowAsHeader<- function(ds) {
  # this function gives first row as header 
  # args: 
    # ds : data.frames
  
  names(ds) <- as.character(unlist(ds[1,]))
  return(ds[-1,])
}

# -----------------------------------------------------------------------------------------------------------------
# Function 25: calculate percent distribution for factors
# -----------------------------------------------------------------------------------------------------------------

pct <- function(x){
  # this function to calculate percent distribution for factors
  # arg:
    # x : data.frame
  tbl <- table(x)
  tbl_pct <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(tbl_pct) <- c('Count','Percentage')
  kable(tbl_pct)
}

# -----------------------------------------------------------------------------------------------------------------
# Function 26: normalize using Range
# -----------------------------------------------------------------------------------------------------------------
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# -----------------------------------------------------------------------------------------------------------------
# Function 27: Gives Mode
# -----------------------------------------------------------------------------------------------------------------
Modes <- function(x) {
ux <- unique(x)
ux[which.max(tabulate(match(x, ux)))]
}

ModesAll <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
# -----------------------------------------------------------------------------------------------------------------
# Function 28: Gives data Summary for all data type useful to compare two data sets
# -----------------------------------------------------------------------------------------------------------------

GetDataSummary<- function(ds){
  # This function gives dataset summary:
  # 1. For categorical : unique count
  # 2. For Numeric : Total values
  # 3. For Integer : unique counts
  # 4. For factor : unique levels/counts
  
  # args: 
  # ds : data.frames
  
  stopifnot(any(grepl("data.frame",class(ds))==TRUE))
  require(dplyr)
  
  # Get data types
  
  ds1<- data.frame(DataType=sapply(lapply(ds,class),"[",1))
  ds1<- cbind(FeatureName=row.names(ds1),ds1) %>% data.frame()
  row.names(ds1)<- NULL
  
  # Get all types of vars
  numVars<- ds1[ds1$DataType=="numeric",]
  catVars<- ds1[ds1$DataType=="character",]
  intVars<- ds1[ds1$DataType=="integer",]
  factVars<- ds1[ds1$DataType=="factor",]
  
  # Get Summary for numeric vars
  
  ds2<- ds %>% dplyr::select(all_of(numVars$FeatureName))
  ds2[is.na(ds2)]<- 0
  
  ds2<- data.frame(Total=ds2 %>% dplyr::summarise(across(everything(), sum, na.rm = TRUE)) %>% t())
  ds2<- data.frame(VarnamesNumeric=row.names(ds2),ds2)
  row.names(ds2)<- NULL
  cat("Step1: Numeric summary created","\n")
  # Get Summary for categorical and factor vars
  
  ds3<- ds %>% dplyr::select(all_of(catVars$FeatureName) | all_of(factVars$FeatureName))
  ds3<- data.frame(UniqueCount= ds3 %>% dplyr::summarise(across(everything(),n_distinct)) %>% t())
  ds3<- data.frame(VarnamesCategorical =row.names(ds3),ds3)
  row.names(ds3)<- NULL
  cat("Step2: categorical summary created","\n")
  # Get Integer Summary
  
  ds4<- ds %>% dplyr::select(all_of(intVars$FeatureName))
  ds41<- data.frame(UniqueCount_Int= ds4 %>% dplyr::summarise(across(everything(),n_distinct)) %>% t())
  ds42<- data.frame(Total_Int=ds4 %>% dplyr::summarise(across(everything(), sum, na.rm = TRUE)) %>% t())
  ds4<- cbind(ds41,ds42) 
  ds4<- data.frame(VarnamesInteger=row.names(ds4),ds4)
  rownames(ds4)<- NULL
  cat("Step3: integer summary created","\n")
  # Get master summary
  
  ds1$FeatureName<- as.character(ds1$FeatureName)
  ds2$VarnamesNumeric<- as.character(ds2$VarnamesNumeric)
  ds3$VarnamesCategorical<- as.character(ds3$VarnamesCategorical)
  ds4$VarnamesInteger<-as.character(ds4$VarnamesInteger)
  
  ds<- ds1 %>% 
    dplyr::left_join(ds2,by=c("FeatureName"="VarnamesNumeric")) %>% 
    dplyr::left_join(ds3,by=c("FeatureName"="VarnamesCategorical")) %>% 
    dplyr::left_join(ds4,by=c("FeatureName"="VarnamesInteger")) %>% 
    dplyr::mutate(
      UniqueCount=ifelse(is.na(UniqueCount),UniqueCount_Int,UniqueCount),
      Total=ifelse(is.na(Total),Total_Int,Total)
    ) %>% 
    dplyr::select(-c(UniqueCount_Int,Total_Int))
  
  return(ds)
}

# -----------------------------------------------------------------------------------------------------------------
# Function 29: Get Freq of all cat vars in a data set
# -----------------------------------------------------------------------------------------------------------------

GetFreq<- function(ds,cutoff=20){
  # this function gives freq of character variables upto level defined based on cutoff
  
  # args: 
    # ds : dataset
    # cutoff : number of levels to be captured
  require(dplyr)
  
  ds<- ds %>% dplyr::select_if(is.character)
  
  ds_c<- data.frame(UniqueCount=ds %>% dplyr::summarise(across(everything(),n_distinct)) %>% t())
  ds_c<- cbind(FeatureName=row.names(ds_c),ds_c) %>% 
    data.frame() %>% 
    dplyr::filter(UniqueCount<=cutoff) %>% 
    dplyr::select(FeatureName) 
  
  varList<- as.character(ds_c$FeatureName)
  
  ds<- ds %>% dplyr::select(all_of(varList))
  ds<- lapply(ds, function(x) data.frame(table(x, useNA = "always")))
  ds1<- do.call("rbind",ds)
  names(ds1)<- c("Levels","Freq")
  ds1$FeatureName <- rep(names(ds),sapply(ds,nrow))
  ds1<- ds1 %>% 
    dplyr::arrange(FeatureName, desc(Freq)) %>% 
    dplyr::group_by(FeatureName) %>% 
    dplyr::mutate(Prop=Freq/sum(Freq,na.rm = TRUE)) %>% 
    dplyr::select(FeatureName,Levels,Freq,Prop)
  
  return(ds1)
  
}

# -----------------------------------------------------------------------------------------------------------------
# Function 30: Confusion matrix details
# -----------------------------------------------------------------------------------------------------------------
GetConfusionMatrix<- function(response,predicted, cutoff=0.5){
  # this function gives freq of character variables upto level defined based on cutoff
  
  # args: 
  # response : actual response variable 
  # cutoff : prob value to set predicted
  # predicted : predicted probability
  
  mat<-as.matrix(table(response,predicted >=cutoff))
  
  accurracy= (mat[4]+mat[1])/(mat[1]+mat[2]+mat[3]+mat[4])
  precision= mat[4]/(mat[4]+mat[3])
  recall= mat[4]/(mat[2]+mat[4])
  Fscore= 2*precision*recall/(recall+precision)
  df=data.frame("Value"=rbind(accurracy,precision,recall,Fscore))
  df= cbind("Metric"=row.names(df),df,"cutoff"=cutoff)
  row.names(df)<-NULL
  return(df)
  
}

# -----------------------------------------------------------------------------------------------------------------
# Function 31: Lift chart for binary classification
# -----------------------------------------------------------------------------------------------------------------
lift <- function(response,predicted,grps=10){
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  
  if(is.factor(response)) {response<-as.integer(as.character(response))}
  if(is.factor(predicted)) {predicted<-as.integer(as.character(predicted))}
  
  helper= data.frame(cbind(response,predicted))
  helper[,"bucket"] =ntile(-helper[,"predicted"],grps)
  gaintable = helper %>% 
    dplyr::group_by(bucket) %>% 
    dplyr::summarise_at(vars(response), list(total=n(),totalresp=sum(.,na.rm = TRUE))) %>% 
    dplyr::mutate(
      Cumresp=cumsum(totalresp),
      Gain=Cumresp/sum(totalresp)*100,
      Cumlift=Gain/(bucket*(100/grps))
    )
  
  return(gaintable)
  
}


# -----------------------------------------------------------------------------------------------------------------
# Function 32: Logit regression model fit
# -----------------------------------------------------------------------------------------------------------------

binaryLogitReg_mod<-function(ds,regressors,responseVar,sampleSplit=0.7,
                       var_selection=c("stepwise","None"),stepwiseDirection=c("both", "backward", "forward"),
                       alpha=0.95,prob_cutoff=0.8)
{
  # this functions fits linear regressors with stepwise selection
  # args : 
  # regressors : regressors list as vector
  # responseVar : dependent variable as vector or string
  # ds : dataset
  # sampleSplit : factor to select train data
  # var_selection : variable selection algorithm, use 'stepwise' for stepwise
  # stepwise_direction : direction for stepwise filtering, forward, backward and both can be used
  # alpha : significance level for calculation of lower bound and upper bound, default value 95%
  # prob_cutoff: probability cutoff for predicted values to classify in levels , 0<prob_cutoff<1
  
  # Example :
  # responseVar<-"x"
  # alpha=0.95
  # var_names <- names(in_frame)
  # regressors <- var_names[-which(var_names == responseVar)] 
  # h<-mlr_mod(regressors,responseVar,in_frame,out_frame,var_selection = "Others")
  
  Packages <- c("MASS","car", "dplyr")
  lapply(Packages, require, character.only = TRUE)
  #if(!(class(ds)=="data.frame" & is.numeric(sampleSplit) & is.numeric(alpha) & sampleSplit<=1 & )) { stop("ds must be a data frame and n must be a integer ")}
  var_selection <- match.arg(var_selection)
  stepwiseDirection <- match.arg(stepwiseDirection)
  #match.arg(var_selection,stepwiseDirection,several.ok=T)
  
  ds[[responseVar]]<- as.factor(ds[[responseVar]])
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  RowIndex <- base:: sample(1:nrow(ds), sampleSplit*nrow(ds))  # row indices for training data
  in_frame <- ds[RowIndex, ]  # model training data
  out_frame  <- ds[-RowIndex, ] 
  
  form <- paste(regressors, collapse = '+')
  form_in <- formula(paste(responseVar, '~', form))
  baseModel <- glm(form_in,family = "binomial",data = in_frame)
  
  if (var_selection == "stepwise") {
    final_model <- MASS::stepAIC(baseModel,direction = stepwiseDirection,trace = FALSE)
  } else { final_model <- baseModel}
  
  smry<-summary(final_model)
  
  # coefficient estimation
  estimates<- as.data.frame(smry$coefficients)
  colnames(estimates) <- c("Coefficient","StdError","tValue","pValue")
  estimates<-data.frame(Variable=row.names(estimates),estimates)
  estimates$Variable <- as.character(estimates$Variable)
  estimates$stdCoeff<- abs(estimates$Coefficient/estimates$StdError)
  # VIF
  if (nrow(estimates)<=2) {
    cat("model contains fewer than 2 terms, Hence VIF calculation is not possible",'\n')
    vif_init<-data.frame(Variable=c(unique(estimates$Variable)),VIF="NA")}
  else {
    vif_init<-data.frame(VIF=c("(Intercept)"="NA",car::vif(final_model)))
    vif_init<-data.frame(Variable=row.names(vif_init),vif_init)
  }
  vif_init$Variable <- as.character(vif_init$Variable)
  
  # Lower and upper bound calculation at alpha C.I.
  coefBoundMat<-as.data.frame(confint(final_model,level = alpha))
  coefBoundMat<- data.frame(Variable=row.names(coefBoundMat),coefBoundMat)
  colnames(coefBoundMat)<- c("Variable",paste0("LB_",100*alpha,"percCI"),paste0("UB_",100*alpha,"percCI"))
  coefBoundMat$Variable <- as.character(coefBoundMat$Variable)
  # Variable Importance rank calculation based on standard coefficient
  VarImpRank <- estimates %>% 
    dplyr::select("Variable","stdCoeff") %>% 
    dplyr::filter(Variable != "(Intercept)") %>% 
    dplyr::arrange(-stdCoeff) %>% 
    dplyr::mutate(VarImp_Rank=seq(1:(nrow(estimates)-1))) %>% 
    dplyr::select(Variable,VarImp_Rank)
  
  VarImpRank <- rbind(VarImpRank,data.frame(Variable="(Intercept)",VarImp_Rank="NA"))
  VarImpRank$Variable <- as.character(VarImpRank$Variable)
  # model summary
  modelSummary <- list(estimates,VarImpRank,vif_init,coefBoundMat) %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="Variable"), .)
  
  modelStats_inFrame <- data.frame(
    Index=c("NullDeviance","ResidualDeviance","AIC"),
    Value=c(smry$null.deviance,smry$deviance,smry$aic))
  modelStats_inFrame$Value<- round(modelStats_inFrame$Value,2)
  
  modelStatsDF_inFrame <- data.frame(DegreesOfFreedom=c(smry$df.null,smry$df.residual,"NA"))
  
  modelStats = cbind(modelStats_inFrame,modelStatsDF_inFrame)
  
  # prediction on train data
  pred_in_frame<-predict(final_model,newdata = in_frame,type = "response")
  #in_frame_MAPE<-data.frame(Index="MAPE_train",Value=MAPE(in_frame %>%dplyr::select(all_of(responseVar)),pred_in_frame))
  
  #prediction on test data
  pred_out_frame<-predict(final_model,newdata = out_frame,type = "response")
  #out_frame_MAPE<-data.frame(Index="MAPE_test",Value=MAPE(out_frame %>%dplyr::select(all_of(responseVar)),pred_out_frame))
  
  overallPred<-rbind(data.frame(Predicted=pred_in_frame),data.frame(Predicted=pred_out_frame))
  Actuals <- rbind(in_frame,out_frame) %>% dplyr::select(all_of(responseVar))
  
  modelPerformance_inframe<-GetConfusionMatrix(in_frame[[responseVar]],pred_in_frame,cutoff =prob_cutoff) %>% dplyr::mutate("DataType"="in-frame")
  modelPerformance_outframe<-GetConfusionMatrix(out_frame[[responseVar]],pred_out_frame,cutoff =prob_cutoff) %>% dplyr::mutate("DataType"="out-frame")
  modelPerformance_overall<-GetConfusionMatrix(Actuals[[responseVar]],overallPred,cutoff =prob_cutoff) %>% dplyr::mutate("DataType"="overall")
  
  modelPerformance<-rbind(modelPerformance_inframe,modelPerformance_outframe,modelPerformance_overall) %>% dplyr::select(DataType,everything())
  
  overallPredData<-cbind(Actuals,overallPred)
  
  rownames(modelSummary) <- NULL
  rownames(modelStats) <- NULL
  rownames(modelPerformance) <- NULL
  rownames(overallPredData) <- NULL
  
  return(list("ModelSummary" = modelSummary,
              "ModelStats" = modelStats,
              "modelPerformance" = modelPerformance,
              "FinalDataSet" = overallPredData,
              "ModelEqn" = final_model))
  
}

# -----------------------------------------------------------------------------------------------------------------
# Function 33: Load All csv to R environment
# -----------------------------------------------------------------------------------------------------------------


temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
# or 
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))



