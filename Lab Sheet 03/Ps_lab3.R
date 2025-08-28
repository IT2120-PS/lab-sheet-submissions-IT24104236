setwd("C:\\Users\\IT24104236\\Desktop\\IT24104236")
data <- read.csv("DATA 2.csv", header = TRUE)
fix(data)
names(data) <- c("Age", "Gender", "Accommodation")


data$Gender <-factor(data$Gender,c(1,2),c("Male" ,"Female" ))
data$Accommodation <- factor (data$Accommodation, c(1,2,3) ,c("Home", "Boarded", "Lodging"))
attach(data)


#part 2 
gender.freq <- table(Gender)
acc.freq <- table(Accommodation)

barplot(gender.freq, main = "bar chart for gender " ,ylab ="frequency" ,xlab = "gender")
barplot(acc.freq,main = "bar accommodoation " , ylab = "frequncy" , xlab = "accommodation")


pie(gender.freq, main = "Pie Chart for Gender")
pie(acc.freq, main = "Pie Chart for Accommodation")

hist(Age, main = "Histogram for Age")
boxplot(Age, main = "Box Plot for Age", horizontal = TRUE, outline = TRUE)

#part 3
gender_acc.freq <- table(Gender, Accommodation)

barplot(gender_acc.freq, beside = FALSE, main = "Gender & Accommodation", legend = rownames(gender_acc.freq), xlab = "Accommodation", ylab = "Frequency")

barplot(gender_acc.freq, beside = TRUE, main = "Gender & Accommodation", legend = rownames(gender_acc.freq), xlab = "Accommodation", ylab = "Frequency")


#part 4 

boxplot(Age ~ Gender, main = "Boxplots for Age by Gender", xlab = "Gender", ylab = "Age")
boxplot(Age ~ Accommodation, outpch = 8, main = "Boxplots for Age by Accommodation", xlab = "Accommodation", ylab = "Age", outline = TRUE)

#part 5

xtabs(Age ~ Gender + Accommodation) / gender_acc.freq
detach(data)

#EXERCISE 

student_data <- read.csv("Exercise.csv", header = TRUE)

summary(student_data$X1)
hist(student_data$X1, main = "Histogram for Age")

table(student_data$X2)
barplot(table(student_data$X2), main = "Bar Chart for Gender", xlab = "Gender", ylab = "Frequency")

boxplot(student_data$X1 ~ student_data$X3, main = "Age by Accommodation", xlab = "Accommodation", ylab = "Age")

