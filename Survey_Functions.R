

#Helper function to allow for indexing within dplyr easily
BooleanPaste <- function(values,boolean,replaceFalseWithNA = T){
   boolean[!boolean] <- NA
   out <- values[boolean]
   return(out)
}

#This function allows boolean assignment using two vectors. For use inside of dplyr
#Replacement is a vector or a single value which will replace where the boolean indicates.
BooleanPaste2 <- function(focalVector,boolean,replacement,replaceNAWithFalse = T){

   if(replaceNAWithFalse){
      boolean[is.na(boolean)] <- F
   }

   #Vector replacement
   if(length(replacement)==length(focalVector)){
      focalVector[boolean] <- replacement[boolean]
   }

   #Replacing vector with single value
   else if(length(replacement )==1){
      focalVector[boolean] <- replacement
   }

   else(
      stop("Replacement is not of length 1 or of length equal to the length of the focal vector")
   )

   return(focalVector)
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


   if(any(grepl("[^0123456789\\.]",x))){
      warning(
         print("We have removed non-numberic characters from certain strings and may have converted certain values to NA. Here they are: "),
         print(
            tibble(
               "Input values" = x[grepl("[^0123456789\\.]", x)],
               "Function Output" = Out[grepl("[^0123456789\\.]",  x)]
            )
         )
      )

   }


   return(Out)
}




#Safe join to indicate potential problems with the join
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
   U_X <- unique(x[[by]])
   U_Y <- unique(y[[by]])

   if (any(!U_X%in%U_Y)|any(!U_Y%in%U_X)) {

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
            "Not all values have been matched!"
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


nin5CI<-function(num,denom,digits=1){
   percent<-num/denom
   upper<-percent+qnorm(0.975)*sqrt((percent*(1-percent))/denom)
   lower<-percent-qnorm(0.975)*sqrt((percent*(1-percent))/denom)
   ci<-qnorm(0.975)*sqrt((percent*(1-percent))/denom)

   #Setting lower limit at zero.
   lower[lower<0]<-0


   #Formatting the printing option
   printCI <-
      paste(
         formatC(percent * 100, format = "f", digits = digits),
         " (",
         formatC(lower * 100, format = "f", digits = digits),
         ", ",
         formatC(upper * 100, format = "f", digits = digits),
         ")",
         sep = ""
      )

   #Adding a return NA if either of the inputs are NA
   printCI[is.na(num)]<-NA

   return(printCI)
}



#Creating a function to summarize data
StatDat<-function(x,GiveDenom=NULL,CI=F,percent_as_Character=F,digits =1){

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
      y$Percent<-formatC(y$Percent,format="f",digits = digits)
   }

   if(!percent_as_Character){
      y$Percent<- round(y$Percent,digits)

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

labelmaker <- function(x, denom = NULL,digits=1) {
   if (is.null(denom)) {
      percent <- x / sum(x) * 100
   }
   else{
      percent <- x / denom * 100
   }
   sum <- round(x, 0)
   Percent <- formatC(percent, format = 'f', digits = digits)
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


   #Checking if any are equal to zero (i.e. no respondents select it) this probably means that an error occurred in response matching with grepl
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


widen_multianswer <- function(Vector, UniqueAnswers){

   #Creating data frame with "rawdat" as the first column. The second column is called checkDat, in this, responses will be removed as they are accounted for to see if any were missed.
   dat <- data.frame(rawDat = Vector,checkDat = Vector)

   #Adding a new column for each unique answer with a boolean to indicate whether the respondent selected it
   for (i in UniqueAnswers) {
      dat[grepl(i, dat$rawDat, fixed = T), i] <- "Yes"
      dat[!grepl(i, dat$rawDat, fixed = T), i] <- "No"

      dat$checkDat <- gsub(i,"",dat$checkDat,fixed = T)

      #Is no answers are detected, then print for which were not detected.
      if(!any(dat[[i]]=="Yes")){
         print(
            paste("No Responses Detected for",i)
         )
      }
   }

   #Double Checking
   if(dat %>% filter(grepl("[[:alpha:]]",checkDat)) %>% nrow()>0){
      stop(

         print(
            paste("error, you have ommited one of the responses",dat %>% filter(grepl("[[:alpha:]]",checkDat)) %>% select(checkDat))

         )
      )
   }
   return(dat)
}

#function to perform test
ContinuousTests <- function(x,y,return.method=F){

   var.equal <- HomoTest(x,y)

   #Anova
   if(n_Unique(x)>2){
      method <- "anova"
      if(var.equal){
         out <- oneway.test(y~x,var.equal = T)[[3]] #extracts p val

      }

      if(!var.equal){
         out <- oneway.test(y~x,var.equal = F)[[3]]
      }

   }

   #T test
   if(n_Unique(x)==2){

      method <- "t.test"
      if(var.equal){
         out <- t.test(y~x,var.equal = T)[[3]] #extracts p val

      }

      if(!var.equal){
         out <- t.test(y~x,var.equal = F)[[3]]

      }
   }

   if(return.method ){
      out <- method
   }

   return(out)
}



#function to perform test on ordinal data
OrdinalTests <- function(x,y,return.method=F){

   #Kruskall wallis (like anova)
   if(n_Unique(x)>2){
      method <- "KW.test"

      out <- kruskal.test(y~x)[[3]] #extracts p val

   }

   #MAnn whitney u test (like t.test)
   if(n_Unique(x)==2){

      method <- "U.test"
      out <- wilcox.test(y~x)[[3]] #extracts p val

   }

   if(return.method ){
      out <- method
   }

   return(out)
}


#Assessing equal variances among all groups.
HomoTest <- function(x,y){

   df <- leveneTest(y~x) %>% as.data.frame()

   p <- df %>% select("Pr(>F)")%>% drop_na() %>% pull()

   #making the return the response to var.equal. T means null accepted. F means null rejected (unqual variance)
   if(p<0.05){
      p <- F
   }
   if(p>=0.05){
      p <- T
   }

   return(p)
}



BenjaminiNewP <- function(oldP){

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


#Perform stats on these combination
postHoc_continuous <- function(y_data,x_val){

   Var_Equal <- HomoTest(x=x_val,y=y_data)
   main.p <- ContinuousTests(x=x_val,y=y_data)
   method <- ContinuousTests(x=x_val,y=y_data,return.method = T)

   #Unique Combinations
   testFrame <- crossing(a = x_val,b = x_val) %>%
      filter(a!=b,order(a)<order(b)) %>%
      rowwise() %>%
      mutate(
         y_vals=list(y_data[x_val==a|x_val==b]),
         x_vals=list(x_val[x_val==a|x_val==b]),
         postHoc_P_unadjusted = ContinuousTests(x=x_vals,y=y_vals),
         main.p = main.p,
         method = method,
         a_mean = mean(y_data[x_val==a],na.rm=T),
         b_mean = mean(y_data[x_val==b],na.rm=T)
      ) %>%
      ungroup() %>%
      select( - y_vals, -x_vals)

   if(Var_Equal){
      testFrame <- testFrame %>%
         mutate(
            postHoc_P_Adj = p.adjust(postHoc_P_unadjusted,method = "bonferroni")
         )
   }
   if(!Var_Equal){
      testFrame <- testFrame %>%
         mutate(
            postHoc_P_Adj = p.adjust(postHoc_P_unadjusted,method = "BH")
         )
   }

   return(testFrame)
}





#Perform stats on these combination
postHoc_ordinal <- function(y_data,x_val){

   main.p <- OrdinalTests(x=x_val,y=y_data)
   method <- OrdinalTests(x=x_val,y=y_data,return.method = T)

   #Unique Combinations
   testFrame <- crossing(a = x_val,b = x_val) %>%
      filter(a!=b,order(a)<order(b)) %>%
      rowwise() %>%
      mutate(
         y_vals=list(y_data[x_val==a|x_val==b]),
         x_vals=list(x_val[x_val==a|x_val==b]),
         postHoc_P_unadjusted = OrdinalTests(x=x_vals,y=y_vals),
         main.p = main.p,
         method = method,
         a_mean = mean(y_data[x_val==a],na.rm=T),
         b_mean = mean(y_data[x_val==b],na.rm=T),
         mean_dif = abs(a_mean-b_mean)
      ) %>%
      ungroup() %>%
      select( - y_vals, -x_vals) %>%
      mutate(
         postHoc_P_Adj = p.adjust(postHoc_P_unadjusted,method = "BH")
      )

   #Must adjust p values with bonferroni or benjamini!
   return(testFrame)

}



#Generating function to perform Chi-sq test with post hoc
CategoricalStats <- function(x,y){

   #Removing non-responses
   Notna_x <- !is.na(x)
   Notna_y <- !is.na(y)
   x <- x[Notna_x&Notna_y]
   y <- y[Notna_x&Notna_y]

   #Abandoning testing if there are less than two responses
   if(n_Unique(y)<2){
      p <- NA
      outFrame <- tibble(omnibusP = p,PostHocFocalTestVar = "Not enough levels in y for chiSq test",postHoc_P_unadjusted = NA)
   }


   #Performing chi sq test if there are more than two responses
   if(n_Unique(y)>=2){

      #determining p value from main chisq test
      p <- chisq.test(x = x, y = y)[[3]]

      #If significant, perform postHoc
      if(p<0.01){

         postHoc_P_unadjusted <- c()
         PostHocFocalTestVar <- c()
         var_proportion <- c()

         #Reorganize variable to perform test on each one vs all others.
         vars <- unique(x)
         for(i in 1:n_Unique(vars)){
            #modifying x data to perform test on only one variable.
            newx <- x
            newx[newx!=vars[i]] <- "Other_Internal"

            #Determining frequency of response
            proportion <- length(newx[newx==vars[i]&y=="Yes"])/length(newx[newx==vars[i]])

            #Building data frame
            postHoc_P_unadjusted[i] <- chisq.test(x = newx, y = y)[[3]]

            PostHocFocalTestVar[i] <- vars[i]

            var_proportion[i] <- proportion
         }

         #Building data frame
         outFrame <- tibble(PostHocFocalTestVar,postHoc_P_unadjusted) %>%
            mutate(
               postHoc_P_Adj = p.adjust(postHoc_P_unadjusted,method = "BH"),
               omnibusP = p,
               var_proportion = var_proportion
            )
      }
      if(p>=0.01){
         postHoc_P_unadjusted <- NA
         PostHocFocalTestVar <- NA
         var_proportion <- NA

         outFrame <- tibble(omnibusP = p,PostHocFocalTestVar,postHoc_P_unadjusted)
      }
   }


   return(outFrame)
}
