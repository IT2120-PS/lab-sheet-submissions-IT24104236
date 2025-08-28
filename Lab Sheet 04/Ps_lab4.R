setwd("C:\\Users\\IT24104236\\Desktop\\IT24104236")

data<-read.table("DATA 4.txt",header=TRUE,sep =" ")

fix(data)

attach(data)


boxplot(X1,main="Box plot for Team Attendence",outline=TRUE,outpch=8,horizontal=TRUE)
boxplot(X2,main="Box plot for TEAm Salary",outline=TRUE,outpch=8,horizontal=TRUE)
boxplot(X3,main="Box plot for Years",outline=TRUE,outpch=8,horizontal=TRUE)

hist(X1,ylab="Frequency",xlab="Team Attendence",main="Histogram for Team Attendence")
hist(X2,ylab="Frequency",xlab="Team Salary",main="histogram for Team Salary")
hist(X3,ylab="Frequency",xlab="Years",main="Histogram for Years")

stem (X1)
stem(X2)
stem(X3)


mean(X1)
mean(X2)
mean(X3)

median(X1)
median(X2)
median(X3)


sd(X1)
sd(X2)
sd(X3)


summary(X1)
summary(X2)
summary(X3)


quantile(X1)

quantile(X1)[2]

quantile(X1)[4]


IQR(X1)
IQR(X2)
IQR(X3)


get.mode<-function (y) {
  counts<-table(X3)
  names(counts[counts == max(counts)])
}
get.mode(X3)

table(X3)

max(counts)

counts == max(counts)

counts[counts == max(counts)]

names(counts[counts == max(counts)])

get.outliers<-function(z){
  q1 quantile (z) [2] 
  q3 quantile (z) [4]
  iqr <- q3 -q1
  
  ub <-q3 + 1.5*iqr
  1b <- q1-1.5 *iqr
  print(paste ("Upper Bound = ", ub))
  print (paste ("Lower Bound", 1b))
  print (paste ("Outliers:", paste (sort (z[z<lb z>ub]), collapse = ",")))
}

get.outliers (X1)
get.outliers (X2)
get.outliers (X3)

print(paste("Upper Bound=", ub))
print(paste("Lower Bound", 1b))

print(paste ("Outliers:", paste(sort(z[z<lb|z>ub]),collapse = ",)))




## 1 ##

branch_data<-read.table("Exercise.txt",header=TRUE,sep=",")
fix(branch_data)

attach(branch_data)

## 2 ##

str(branch_data)

#Branch<-Integer(Nominal(this scale categorizes data without any inherent order or ranking))
#Sales_X1<-Numeric(Ratio(properties of interval scales,plus a true point))
#Advertising_X2<-Numeric(ratio)
#Years_X3<-Integer(Ratio)



## 3 ##
boxplot(branch_data$Sales_X1,main ="Boxplot for sales",outline = TRUE,outpch=8,horizontal=TRUE)


## 4 ##

#Five number summary
summary(Advertising_X2)

#obtaining IQR
IQR(Advertising_X2)


## 5 ##


get.outliers <- function(z) {
  q1<-quantile(z,0.25)
  q3<-quantile(z,0.75)
  iqr <- q3 - q1        
  
  ub <- q3 + 1.5 * iqr      
  lb <- q1 - 1.5 * iqr     
  
  print(paste("Upper Bound = ", ub))
  print(paste("Lower Bound = ", lb))
  
  outliers <- z[z < lb | z > ub]
  if (length(outliers) == 0) {
    print("No outliers found")
  } else {
    print(paste("Outliers: ", paste(sort(outliers), collapse = " , ")))
  }
}


get.outliers(branch_data$Years_X3) 




SD
