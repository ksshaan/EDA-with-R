#==========================================================================================
# TRY SOME IMPROVISATIONs
#==========================================================================================

#SUGGESTION 1:
#Often we are not required the graphs for all the numeric variables. Try to
#improve the codeby adding an additional parameter "variable" that can take
#a vector of variable index and return the graphs for only those variables.

#Example: Graphs(Boston, var=c(1,3,4))
#Will generate the graphics for only the numerical variables among the 
#variables 1,3 & 4 in the data Boston

library(MASS)
GraphS1(Boston,c(1,3,4))
GraphS1 <- function(data,cols)
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in cols)
  {
    if(is.numeric(data[,i]))
    {
      
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "Frequency", col = "lightgreen", border=F)
      
      
      
      dev.off()  #NOTE this step
      
    }
    
  }
}

GraphS1(cars,c(2,3,4,5,6,7,8,9))

dim(Boston)
GraphS1(Boston,c(1,3,4))

names(cars)

summary(Boston)



#SUGGESTION 2:
#Improve the code in suggestion 1 such that if the argument variable is 
#ignored then it will return the graphs of all the numeric variables 
#in the data by default.

Graphs2 <- function(data,cols=c(1:ncol(data)))
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in cols)
  {
    if(is.numeric(data[,i]))
    {
      print(i)
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "Frequency", col = "lightgreen", border=F)
      
      
      
      dev.off()  #NOTE this step
      
    }
    
  }
}

Graphs2(cars,c(2,3))
Graphs2(Boston)
names(cars)


#SUGGESTION 3:
#We ignored the cateorical variables in our discussion. Make some 
#improvement in your codes in suggestion 2 such that the function will take
#the argument "data" and "variable" and will return boxplots & 
#histograms for the numerical variables and barplots and pie charts for
#the categorical variables.

#Example:
#Graphs(mtcars)
#will get the necessary graphics for all numeric variables and categorical variables in the
#data

#Suggestion 3

Graphs3 <- function(data,cols=c(1:ncol(data)))
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in cols)
  {
    if(is.numeric(data[,i]))
    {
      #print(i)
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "Frequency", col = "lightgreen", border=F)
      
      
      
      dev.off()  #NOTE this step
      
    }
    else
    {
      print(i)
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(1,2))
      tt=table(data[,i])
      w = as.data.frame(tt)
      #barplot(w[,2],names.arg = w[,1])
      barplot(w[,2],names.arg = w[,1],col = "darkred",main = paste("Barplot of", names(data)[i]))
      
      #barplot(data[,i],main = paste("Barplot of", names(data)[i]),ylab =names(data)[i],
      #        col = "darkred",horiz = TRUE)
      
      lbls = w[,1] 
      pct = round(w[,2]/sum(w[,2])*100)
      lbls = paste(lbls,pct)
      pie(w[,2],labels = lbls, col=rainbow(length(lbls)),
          main="Pie Chart ")  
      
      dev.off()  #NOTE this step
      
      
      
    }
  }
}

Graphs3(iriss)








#SUGGESTION 4:
#Probably you need not want to mess up your working directory with so many 
#image files...Create an additional argument for the function "dir" 
#(directory), such that the function exports all the files to your 
#specified folder (which need not necessaryly be your working directory).

#Example:
#Graphs(Boston, Variable = c(1,3,4), 
#dir = ".../Praxis/LearntSometingNew/Graphs")
#will generate the necessary graphics for the variables 1, 3 and 4 in 
#the specified location
#in your system i.e. ".../Praxis/LearntSometingNew/Graphs"

#SUggestion 4 :

Graphs4 <- function(data,cols=c(1:ncol(data)),folder='C:\\Users\\hp\\Documents\\R\\Rscripts\\images\\')
{ 
  setwd(folder)
  x = paste("Test",runif(1, min=0, max=100), Sys.Date(), sep = "_")
  dir.create(x)
  #dir.create(x)
  setwd(paste(folder,'\\',x,sep = ""))
  
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  for(i in cols)
  {
    if(is.numeric(data[,i]))
    {
      #print(i)
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(2,1))
      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]), 
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)
      
      hist(data[,i], main = paste("Histogram of", names(data)[i]), 
           xlab = names(data)[i], ylab = "Frequency", col = "lightgreen", border=F)
      
      
      
      dev.off()  #NOTE this step
      
    }
    else
    {
      print(i)
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step
      
      par(mfrow=c(1,2))
      tt=table(data[,i])
      w = as.data.frame(tt)
      #barplot(w[,2],names.arg = w[,1])
      barplot(w[,2],names.arg = w[,1],col = "darkred",main = paste("Barplot of", names(data)[i]))
      
      #barplot(data[,i],main = paste("Barplot of", names(data)[i]),ylab =names(data)[i],
      #        col = "darkred",horiz = TRUE)
      
      lbls = w[,1] 
      pct = round(w[,2]/sum(w[,2])*100)
      lbls = paste(lbls,pct)
      pie(w[,2],labels = lbls, col=rainbow(length(lbls)),
          main="Pie Chart ")  
      
      dev.off()  #NOTE this step
      
      
      
    }
  }
}

Graphs4(iriss,c(1:6),'C:\\Users\\hp\\Documents\\R\\Rscripts\\Image_Folder\\')
Graphs4(iriss,c(2:4))

#########################################################

Function 2: chi square test for dataframes

cols = c(2,3)
cols[1]
chisqtest_dfs <- function(data,cols)
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  print(paste("Chi square test b/n",names(data)[cols[1]],names(data)[cols[2]],sep=",")) 
  k=chisq.test(data[,cols[1]], data[,cols[2]],simulate.p.value = TRUE)
#The issue is that the chi-square approximation to the distribution 
#of the test statistic relies on the counts being roughly normally 
#distributed. If many of the expected counts are very small, 
#the approximation may be poor.It uses simulations to find the p value
  
  if (k$p.value < 0.05)
  { 
    print(paste("The two variables relate to each other",k$p.value))
  }
  else
  {
    print(paste("The two variables are independent as p-value  =",k$p.value))  
    }
  
}


chisqtest_dfs(mtcars,c(7,8))

arranged_data = movemydfs_categorical_numeric(Boston)
data=as.data.frame(arranged_data)
chisqtest_dfs(data,c(13,14))

arranged_data = movemydfs_categorical_numeric(mtcars)
data=as.data.frame(arranged_data)
chisqtest_dfs(data,c(8,9))
chisqtest_dfs(data,c(9,10))
chisqtest_dfs(data,c(8,10))
chisqtest_dfs(data,c(10,11))



cols =c(7,8)
data =mtcars
names(mtcars)
k=chisq.test(mtcars[,cols[1]], data[,cols[2]])
k$p.value
class(k)
j=as.data.frame(k)


Function 3: t test for dataframes

run_ttest_indsample <- function(data,cols)
{
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  
  if (is.numeric(data[,cols[1]]) & is.numeric(data[,cols[2]]))
  {
    print(paste("t test of ind samples where where y1 and y2 are numeric",names(data)[cols[1]],names(data)[cols[2]],sep=",")) 
    k=t.test(data[,cols[1]],data[,cols[2]], var.equal = FALSE)
    
  }
  else
  {
    print(paste("t test of ind samples where y is numeric and x is a binary factor",names(data)[cols[1]],names(data)[cols[2]],sep=",")) 
    k=t.test(data[,cols[1]]~data[,cols[2]], var.equal = FALSE)
    #Independent-samples T-test where y1 and y2 are numeric
    #testing the difference between the samples when the variances 
    #of two normal distributions are not known.
    #We use it to determine whether there is a  
    #difference between the means of two groups.   
  }
  
  if (k$p.value < 0.05)
  { 
    print(paste("The means two cols do differ significantly as p-value = ",k$p.value))
  }
  else
  {
    print(paste("The means two cols do not differ significantly as p-value = ",k$p.value))  
  }
  
}






#Function 4: rearrange the columns so that they are grouped 
#according to datatype nemeric vs categorical.

movemydfs_categorical_numeric <-function(data)
{
  temp <- numeric()
  temp <-c()
  t = ncol(data)
  
  
  for(i in 1:ncol(data))
  {
    
    tt=table(data[,i])#Cross tabulation
    w = as.data.frame(tt)
    if (dim(w)[1]<=10)#Checking if there are <10factors
    { 
      print(paste("am categorical",names(data)[i],sep="="))
      data[,ncol(data)+1] =data[,i] 
      
      #Converting the categorical columns to Factors
      data[,ncol(data)] <- as.factor(data[,ncol(data)]) 
      temp<-append(temp,i)
    }
    
  } 
  
  j=1 #Creating a new categorical column at the end
  for (i in temp)
  {
    names(data)[t+j]= names(data)[i]  
    j=j+1
  }
  
  data = data[-temp] #deleting the duplicate categorical cols
  #View(data)
  q=length(temp)
  print(paste("Total cols =",ncol(data),"categorical cols = ",q,"categorical at the end"))
  #print(class(data))
  return (data)
}

library(MASS)
data("Boston")
class(Boston)
arranged_data = data.frame()
arranged_data = movemydfs_categorical_numeric(Boston)
data=as.data.frame(arranged_data)
View(Boston)
View(data)

arranged_data = movemydfs_categorical_numeric(mtcars)
data=as.data.frame(arranged_data)
View(mtcars)
View(data)

iiriss = read.csv("iris.csv")



class(data)
View(data)
chisqtest_dfs(data,c(13,14))
chisqtest_dfs(data,c(7,8))

arranged_data = movemydfs_categorical_numeric(iiriss)
data=as.data.frame(arranged_data)
for (i in 1:ncol(data)){
  print(is.numeric(data[,i]))
}

movemydfs_categorical_numeric(mtcars)

fram = read.csv("framingham.csv")

arranged_data = movemydfs_categorical_numeric(fram)
data=as.data.frame(arranged_data)
chisqtest_dfs(data,c(8,15))



arranged_data = movemydfs_categorical_numeric(mtcars)
data=as.data.frame(arranged_data)
View(data)
cols=c(4,8)
run_ttest_indsample(data,cols)




arranged_data = movemydfs_categorical_numeric(mtcars)
data=as.data.frame(arranged_data)
View(data)
cols=c(1,2)
run_ttest_indsample(data,cols)

# independent 2-group t-test
#t.test(y~x) # where y is numeric and x is a binary factor
cols=c(1,12)
chisqtest_dfs(data,cols)
#t.test(data[,cols[1]]~data[,cols[2]], var.equal = FALSE)
run_ttest_indsample(data,cols)


===============================
#Main Function :
  chisqtest_dfs <- function(data,cols)
  {
    if(!is.data.frame(data))
      stop("The given object is not a data frame")
    
    print(paste("Chi square test b/n",names(data)[cols[1]],names(data)[cols[2]],sep=",")) 
    k=chisq.test(data[,cols[1]], data[,cols[2]],simulate.p.value = TRUE)
    #The issue is that the chi-square approximation to the distribution 
    #of the test statistic relies on the counts being roughly normally 
    #distributed. If many of the expected counts are very small, 
    #the approximation may be poor.It uses simulations to find the p value
    
    if (k$p.value < 0.05)
    { 
      print(paste("The two variables relate to each other",k$p.value))
    }
    else
    {
      print(paste("The two variables are independent as p-value  =",k$p.value))  
    }
    
  }

  run_ttest_indsample <- function(data,cols)
  {
    if(!is.data.frame(data))
      stop("The given object is not a data frame")
    
    if (is.numeric(data[,cols[1]]) & is.numeric(data[,cols[2]]))
    {
      print(paste("t test of ind samples where where y1 and y2 are numeric",names(data)[cols[1]],names(data)[cols[2]],sep=",")) 
      k=t.test(data[,cols[1]],data[,cols[2]], var.equal = FALSE)
      
    }
    else
    {
      print(paste("t test of ind samples where y is numeric and x is a binary factor",names(data)[cols[1]],names(data)[cols[2]],sep=",")) 
      k=t.test(data[,cols[1]]~data[,cols[2]], var.equal = FALSE)
      #Independent-samples T-test where y1 and y2 are numeric
      #testing the difference between the samples when the variances 
      #of two normal distributions are not known.
      #We use it to determine whether there is a  
      #difference between the means of two groups.   
    }
    
    if (k$p.value < 0.05)
    { 
      print(paste("The means two cols do differ significantly as p-value = ",k$p.value))
    }
    else
    {
      print(paste("The means two cols do not differ significantly as p-value = ",k$p.value))  
    }
    
  }

  
check_t_chisquare <- function(data,cols)
  {
    if(!is.data.frame(data))
      stop("The given object is not a data frame")
    
    if (!is.numeric(data[,cols[1]]) & !is.numeric(data[,cols[2]]))
    {
      chisqtest_dfs(data,cols)  
    }
    else
    {
      run_ttest_indsample(data,cols)
    }
  }

#DATA:
fram = read.csv("framingham.csv")
arranged_data = movemydfs_categorical_numeric(fram)
data=as.data.frame(arranged_data)
View(data)
View(fram)
#CALL FUNCTIONS
cols = c(1,15)
cols = c(14,15)
check_t_chisquare(data,cols)
