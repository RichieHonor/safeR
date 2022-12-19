

#Helper function to allow for indexing within dplyr easily
BooleanPaste <- function(values,boolean,replaceFalseWithNA = T){
  boolean[!boolean] <- NA
  out <- values[boolean]
  return(out)
}



#Function to extract the formula from a glmmtmb object
formX <- function(fit){
out <- as.character(fit$call)[2]
return(out)
}



#Scientific Notation Formatter FUnction
SciNot <- function(x,Digits = 2,Type ){
  
  n <- formatC(x, format = "e", digits = Digits)
  
  prior = gsub("e.*","",n)
  
  latter <-gsub(".*e","",n) 
  
  # out <- paste(prior,"%*%10^{",latter,"}",sep="")
  
  if(Type=="former"){
    out <- prior
  }
  if(Type == "latter"){
    out <- latter
  }
  
  return(out)
}

pToText <- function(p){
  out <- p
  
  p[is.na(p)]<-403
  out[p >= 0.05] <- paste("p = ",formatC(p[p >= 0.05],digits = 2, format = "f"),sep="")
  out[p < 0.05] <- "p < 0.05"
  out[p < 0.01] <- "p < 0.01"
  out[p < 0.001] <- "p < 0.001"
  return(out)
}



#returns the larger number between in two vectors
SelectLS <- function(x,y,Type="Larger"){
  #this will remember what is NA and what is not. NA is not going to be read in which is greater, 
  # replaced with zero. if one is not NA, then this value will take predence. If both are NA, then we will convert back to NA at the end. 
  NAMemory_x <- is.na(x)
  NAMemory_y <- is.na(y)
  
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  
  if(Type=="Larger"){
    newVec <- c()
    newVec[x>y] <-x[x>y] 
    newVec[y>x] <-y[y>x] 
    newVec[y==x] <-y[y==x] 
    
    
    
    newVec[NAMemory_x&NAMemory_y] <- NA
    
    
  }
  return(newVec)
  
}




#Creating a function to extract the values out of a linear regression.
lmExtract <- function(x,y,ret = "Est"){
  
  if (all(is.na(y) | all(is.na(x)))) {
    out <- NA
  }
  
  else{
    
    fit <- lm(y~x)
    
    if(ret=="Int"){
      out <- summary(fit)[[4]][1,"Estimate"]
    }
    
    if(ret=="Est"){
      out <- summary(fit)[[4]][2,"Estimate"]
      
    }
    
    if(ret=="p"){
      out <- summary(fit)[[4]][2,"Pr(>|t|)"]
    }
    
    if(ret=="CILow"){
      Est <- summary(fit)[[4]][2,"Estimate"]
      
      SE <- summary(fit)[[4]][2,"Std. Error"]
      
      out <- Est-1.96*SE
      
    }
    
    
    if(ret=="CIHigh"){
      Est <- summary(fit)[[4]][2,"Estimate"]
      
      SE <- summary(fit)[[4]][2,"Std. Error"]
      
      out <- Est+1.96*SE
    }
    
  }
  
  return(out)
}

#Function to adjust p vlaues with benjemini hogeberg adjustment
##Paper for adjusted p values Selective inference in complex research
newP <- function(oldP){
  
  if (all(is.na(oldP))) {
    
    out <- rep(NA,length(oldP))
  }
  
  else{
    
    #Ordering p values
    Porder <- data.frame(p=oldP) %>% 
      #Making a column to identify the original order 
      mutate(Org = 1:length(p)) %>%  
      arrange(p) %>%
      mutate(n = 1:length(p)) %>% 
      #putting the data back into its original order. 
      arrange(Org)
    
    #Developing new p value
    Porder <- Porder %>% mutate(
      newP = (p*nrow(Porder %>% drop_na(p)))/n
    )
    
    out <- Porder$newP
  }
  
  return(out)
}


#formula to take the average standard deviation wit unequal sample size 
Ave_STD <- function(sd,n,k){
  num <- sum((n-1)*sd^2)
  
  denom <-sum(n)-k
  
  w.e. <- (num/denom)
  
  final <- sqrt(w.e.)
  
  return(final)
}


#Function to convert to numeric 
ToNumb<-function(x){
  
  #removing periods and commas from string
  NoperiodComma <- gsub("[\\,\\.]","",x)
  
  #converting to numberic
  suppressWarnings(
    Out <- as.numeric(gsub("[^0123456789\\.]","",x))
  )
  
  #Finding any alpha and spaces separating numbers
  AS <- grepl("[[:digit:]]+[[:alpha:][:space:]]+[[:digit:]]+",x)
  
  if(any(AS)){
    stop(
      "Error: Letters or spaces seperating numbers causing inflation"
    )
  }
  
  #anything separating a number
  ANY <- grepl("[[:digit:]]+[[:alpha:][:space:][:punct:]]+[[:digit:]]+",NoperiodComma)
  
  if(any(ANY)){
    stop(
      "Error: Punctuation seperating numbers causing inflation"
    )
  }
  
  #Multiple periods
  MP <- grepl("[[:digit:]]+[\\.]{2,}[[:digit:]]+",x)
  if(any(MP)){
    stop(
      "Error: multiple periods causing NA in as.numeric; potential for inflation when fixed"
    )
  }
  
  #One period or comma separating number
  OP <- grepl("([[:digit:]]+[\\.]{1}[[:digit:]]+){2,}",x)
  if(any(OP)){
    stop(
      "Error: Multiple periods causing NA in as.numeric; potential for inflation when fixed"
    )
  }
  
  #Checking to see if any were not able to be converted to numeric and printing them with a warning.
  OutTest <- Out
  Testx <- x
  Testx[is.na(Testx)] <- "NA Identifier"
  OutTest[is.na(OutTest)] <- "NA Identifier"
  
  TestX <- Testx=="NA Identifier"
  OutTest <- OutTest=="NA Identifier"
  
  Nas_Generated <- x[TestX!=OutTest]
  
  
  
  if(length(Nas_Generated)>0){
    warning(
      print("As numberic has converted certain values to NA. Here they are:"),
      print(tibble("NasGenerated"=Nas_Generated))
    )
  }
  
  
  return(Out)
}


#Remove ouliers function
outlier_limit<-function(x,return.bool=F,return.limit=F,return.excluded=F,return.proportion=F){
  
  limit<-mean(x,na.rm=T)+sd(x,na.rm=T)*3
  
  limit<-round(limit,0)
  
  if(return.bool){
    x<-x<=limit
    x[is.na(x)]<-F
  }
  else if(return.limit){
    x<-limit
  }
  else if(return.excluded){
    x<- x[x>limit]
    x<-x[!is.na(x)]
  }
  
  else if(return.proportion){
    x<-x[!is.na(x)]
    x<- length(x[x>limit])/length(x)
  }
  else{
    x<-x[x<=limit]
  }
  
  return(x)
  
}

#Function to remove NA's when counting the unique things. 
n_Unique<-function(x){
  return(sum(!is.na(unique(x))))
}


# Safe join to indicate potential problems with the join
safe_leftJ <- function(x, y, by = NULL, outputCheck = F, ...) {
  new <- left_join(x, y, by, ...)
  
  #Exploding Join Error
  if (nrow(new) > nrow(x)) {
    errors <- y %>% 
      group_by(.data[[by]]) %>% 
      summarize(n = n()) %>% 
      filter(n > 1)
    
    print(errors)
    stop(print("duplicates in y causing exploding join"))
  }
  
  #Demonstrating where there are missing matches and throwing a warning if there are
  n_U_X <- n_Unique(x[[by]])
  n_U_Y <- n_Unique(y[[by]])
  
  U_X <- unique(x[[by]])
  U_Y <- unique(y[[by]])
  
  if (n_U_X != n_U_Y) {
    
    # "Responses in X not in Y:",
    U_X2 <- U_X[!U_X %in% U_Y]
    
    # "Responses in Y not in X:",
    U_Y2 <- U_Y[!U_Y %in% U_X]
    
    #Functionality for building tibble. need equal length of X and Y
    if (length(U_X2) < length(U_Y2)) {
      extraNA <- length(U_Y2) - length(U_X2)
      U_X2 <- U_X2 %>% append(rep(NA, extraNA))
    }
    
    if (length(U_X2) > length(U_Y2)) {
      extraNA <- length(U_X2) - length(U_Y2)
      U_Y2 <- U_Y2 %>% append(rep(NA, extraNA))
    }
    
    #Creating output check table
    outputCheckTBL <- tibble("In_X_not_Y"=U_X2,"In_Y_not_X"=U_Y2)
    
    warning(
      print(
        "one dataframe has more unique responses than the other\n"
      ),
      
      #If not output check, then through ths is in the warning
      if (!outputCheck){
        print(
          outputCheckTBL
        )
      }
      
    )
  }
  
  if(outputCheck){
    return(list(outputCheckTBL,new))
  }
  
  else{
    return(new)
  }
}

nin5CI<-function(num,denom){
  percent<-num/denom
  upper<-percent+qnorm(0.975)*sqrt((percent*(1-percent))/denom)
  lower<-percent-qnorm(0.975)*sqrt((percent*(1-percent))/denom)
  ci<-qnorm(0.975)*sqrt((percent*(1-percent))/denom)
  
  #Setting lower limit at zero. 
  lower[lower<0]<-0
  
  
  #Formatting the printing option
  printCI <-
    paste(
      formatC(percent * 100, format = "f", digits = 2),
      " (",
      formatC(lower * 100, format = "f", digits = 2),
      ", ",
      formatC(upper * 100, format = "f", digits = 2),
      ")",
      sep = ""
    )
  
  #Adding a return NA if either of the inputs are NA
  printCI[is.na(num)]<-NA
  
  return(printCI)
}



#Creating a function to summarize data
StatDat<-function(x,GiveDenom=NULL,CI=F,percent_as_Character=F){
  
  #creating dataframe
  y<-data.frame(table(x))
  
  #Filtering out those that did not respond
  y<-y %>% filter(x!="")
  
  #Finding denominator (if givedenom is T then denominator is this. )
  if(is.null(GiveDenom)){
    denominator<-sum(y$Freq)
  }
  if(!is.null(GiveDenom)){
    denominator<-GiveDenom
  }
  
  
  #adding percent column
  y$Percent<-y$Freq/denominator*100 
  
  #ordering data frame according to percent
  y<-y[order(y$Percent,decreasing=T),]
  
  #Formatting - should be output be a character with 2 decimal points retaining trailing zeros?
  if(percent_as_Character){
    y$Percent<-formatC(y$Percent,format="f",digits = 2)
  }
  
  if(!percent_as_Character){
    y$Percent<- round(y$Percent,2)
    
  }
  
  #Changing col names
  colnames(y)<-c(deparse(substitute(x)),"Freq","Percent")
  
  #adding denominator column
  y$Respondents<-denominator
  
  
  #Adding in a CI column if this is needed
  if(CI){
    y$Percent<-nin5CI(y$Freq,denominator)
  }
  
  #If there is both CI and percent as character are T send an error. 
  if(CI & percent_as_Character){
    stop("only one of CI or percent_as_Charcter may be true. Which would you like to output, CI or a percent?")
  }
  
  return(y)
}

labelmaker <- function(x, denom = NULL) {
  if (is.null(denom)) {
    percent <- x / sum(x) * 100
  }
  else{
    percent <- x / denom * 100
  }
  sum <- round(x, 0)
  Percent <- formatC(round(percent, 2), format = 'f', digits = 2)
  sum2 <- broman::add_commas(sum)
  z <- paste(sum2, " (", Percent, "%)", sep = "")
}


multianswer_Table<-function(Data,UniqueAnswers,DataName,Overide=F){
  
  #Creating data frame with "rawdat" as the first column
  dat <- data.frame(rawDat = Data)
  
  #Adding a new column for each unique answer with a boolean to indicate whether the respondent selected it
  for (i in UniqueAnswers) {
    dat[grepl(i, dat$rawDat, fixed = T), i] <- T
    dat[!grepl(i, dat$rawDat, fixed = T), i] <- F
  }
  
  
  #Removing non-responses (to get the true denominator)
  dat <- dat[dat$rawDat != "", ]
  
  #Creating a denominator variable
  denom <- nrow(dat)
  
  
  #Summarizing data and pivoting to a longer format to obtain column totals, lastly making a percent column
  dat_Summarized <- dat %>% 
    summarize_if(is.logical, sum) %>%
    pivot_longer(everything(), names_to = "Response", values_to = "n") %>%
    mutate("percent" = n / denom * 100)
  
  
  #ordering data in table
  dat_Summarized <-
    dat_Summarized[order(dat_Summarized$n, decreasing = T), ]
  
  #adding a line to the beginning to show sample size and the question. 
  dat_Summarized <-
    tibble(
      Response = DataName,
      n = denom,
      percent = 100,
      n_total = denom
    ) %>% 
    bind_rows(dat_Summarized)
  
  #Changing percentage to percentage + CI
  dat_Summarized$percent <- nin5CI(dat_Summarized$n, denom)
  
  
  #Checking if any are equal to zero (i.e. no respondents selecte it) this probably means that an error occured in response matching with grepl
  if(any(dat_Summarized$n==0)){
    
    if(!Overide){
      stop(paste("Check to ensure that matching occured correctly - no responses were detected for:\n",dat_Summarized$Response[dat_Summarized$n==0]))
    }
    else{
      warning(paste("Check to ensure that matching occured correctly - no responses were detected for:\n",dat_Summarized$Response[dat_Summarized$n==0]))
    }
    
  }
  
  return(dat_Summarized)
  
}






