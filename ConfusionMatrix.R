#Confusion matrix

#Set the threshold to 0.50 and compute the confusion matrix of the classifier.

library(caret)
score <-c(0.2,0.25,0.3,0.5,0.6,0.7,0.6,0.5,0.9,0.8)

threshold <- 0.5
actual <-c(0,0,1,1,0,1,1,0,1,1)

#Provide the confusion matrix

pred <- as.numeric(score >=threshold)

cm<-confusionMatrix(data = factor(pred,levels=c("1","0")),
                    reference = factor(actual,levels=c("1","0")),
                    positive = "1")
cm$table

#tpr and fpr
#fpr = fp/fp+tn
#tpr = tp / tp+fn


tpr<-cm$table[1,1]/(cm$table[1,1]+cm$table[2,1])
fpr<-cm$table[1,2]/(cm$table[2,2]+cm$table[1,2])

# create functions tpr fpr
fpr<-function(thres){
  pred <-as.numeric(score>=thres)
  cm<-confusionMatrix(data = factor(pred,levels=c("1","0")),
                        reference = factor(actual,levels=c("1","0")),
                        positive = "1")
  f<- cm$table[1,2]/(cm$table[2,2]+cm$table[1,2])
  return(f)
}

tpr<-function(thres){
  pred <-as.numeric(score>=thres)
  cm<-confusionMatrix(data = factor(pred,levels=c("1","0")),
                      reference = factor(actual,levels=c("1","0")),
                      positive = "1")
  t<- cm$table[1,1]/(cm$table[1,1]+cm$table[2,1])
  return(t)
}

# use the functions to make a roc curve
#d)
thres<-unique(score)
data<- data.frame(fpr = sapply(thres,fpr),
                  tpr = sapply(thres,tpr))
plot(data$fpr,data$tpr,type="b")



#MY SOLUTION

predicted <- factor( ifelse(score >= threshold, 1, 0) )

actual <-factor(c(0,0,1,1,0,1,1,0,1,1))

Cmatrix <-confusionMatrix( reference = actual , data = predicted , positive = "1" )
Cmatrix


TPR <- Cmatrix$byClass[['Sensitivity']]
FPR <- 1 - Cmatrix$byClass[['Specificity']]

tpr<- function(x){
  
  score <-c(0.2,0.25,0.3,0.5,0.6,0.7,0.6,0.5,0.9,0.8)
  
  predicted <- factor( ifelse(score >= x, 1, 0) )
  
  actual <-factor(c(0,0,1,1,0,1,1,0,1,1))
  
  A <-confusionMatrix( reference = actual , data = predicted , positive = "1" )
  
  # TPR = Sensitivity
  TruePositiveRate = A$byClass[['Sensitivity']]
  return(TruePositiveRate)
  
}


fpr<- function(x){
  
  score <-c(0.2,0.25,0.3,0.5,0.6,0.7,0.6,0.5,0.9,0.8)
  
  predicted <- factor( ifelse(score >= x, 1, 0) )
  
  actual <-factor(c(0,0,1,1,0,1,1,0,1,1))
  
  A <-confusionMatrix( reference = actual , data = predicted , positive = "1" )
  
  # FPR = 1-Specificity
  spec = A$byClass[['Specificity']]
  FalsePositiveRate = 1 - spec
  return(FalsePositiveRate)
  
}

tpr(0.3) #1
fpr(0.3) #0.5

tpr(0.4) #0.8333 same as threshold 0.5
fpr(0.4) #0.5

tpr(0.5) #0.83333
fpr(0.5) # 0.5


tpr(0.55) #0.666667
fpr(0.55) # 0.25

tpr(0.6) #0.6667 same as threshold 0.55
fpr(0.6) #0.25

tpr(0.7)# 0.5
fpr(0.7)# 0

tpr(0.75) #0.3333333
fpr(0.75) #0

tpr(0.85) #0.1666667 sane as  0.9
fpr(0.85) # 0

tpr(0.9) # 0.166667
fpr(0.9) # 0

tprvalues <- c(1,1,0.8333333,0.6666667,0.5,0.3333333,0.1666667)
fprvalues <- c(0.75 ,0.5,0.5,0.25,0,0,0)


df <- data.frame(tprvalues,fprvalues)
df


plot(df$fprvalues, df$tprvalues, type = "b", 
     xlim = c(0,1), ylim = c(0,1), 
     main = 'ROC Curve',
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     col = "blue")
abline(a = 0, b = 1, lty=2, col = "grey")





