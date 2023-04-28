#read in the data
data = read.csv("C:\\Users\\anish\\Desktop\\games.csv")
par(mfrow=c(1,1))
# get dimensions
dim(data)

#check missing entries
check = sapply(data,is.na)
length(which(check==TRUE))>0

# White_rating column analysis
sorted_rating = sort(data$white_rating)
hist(sorted_rating,breaks=20,col="green",xlab="Ratings (sorted)")
qqnorm(sorted_rating,pch=1,frame=FALSE)
summary(sorted_rating)

#turns column analysis
hist(data$turns,breaks=60,col="green",xlab="turns")
summary(data$turns)

# increment_code columns analysis
hist(data$increment_code)
summary(data$increment_code)






#Question 1
par(mfrow=c(1,1))
intervals <- function(rating) {
  
  if (rating >= 2700) {
    num=10
  } else if (rating >= 2500 & rating < 2700) {
    num=9
  } else if (rating >= 2400 & rating < 2500) {
    num=8
  }
  else if (rating >= 2300 & rating < 2400) {
    num=7
  }
  else if (rating >= 2200 & rating < 2300) {
    num=6
  }
  else if (rating >= 2000 & rating < 2200) {
    num=5
  }
  else if (rating >= 1800 & rating < 2000) {
    num=4
  }
  else if (rating >= 1600 & rating < 1800) {
    num=3
  }
  else if (rating >= 1400 & rating < 1600) {
    num=2
  }
  else if (rating >= 1200 & rating < 1400) {
    num=1
  }
  else {
    num = 0
  }
  return(num)
}

# ratings to be used in calculating intervals
tenratings <- data[ which(data$increment_code=='10+0'),'white_rating']
# turns (moves) to be used in anova
tenturns <- data[ which(data$increment_code=='10+0'),'turns']
# change the ratings to a discrete interval form
i = lapply(tenratings,intervals)
groups = unlist(i)
#results
boxplot(tenturns~groups,col=rainbow(length(unique(groups))),xlab="Rating Intervals",ylab="Moves",main="Turns and rating group boxplot (blitz)")

# comapre data
anova(lm(groups~tenturns))




#Question 2

#Assumptions
hist(data$turns,col='cyan')
qqnorm(data$turns)
#Results
cor.test(data$turns,data$white_rating)








#Question3

#processing
tendata <- data[ which(data$increment_code=='10+0'),'turns']
fivedata <- data[ which(data$increment_code=='5+5'),'turns']
#summaries
summary(tendata)
summary(fivedata)

#Assumption check
sd(tendata)
sd(fivedata)
par(mfrow=c(1,2))
hist(fivedata, type="l", col="green" )
hist(tendata, type="l", col="yellow" )
# uncomment below to see qqnorm and comment above two lines
#qqpnorm(fivedata,pch=1,frame="false")
#qqpnorm(tendata,pch=1,frame="false")

#Results
t.test(fivedata,tendata,var.equal = T)