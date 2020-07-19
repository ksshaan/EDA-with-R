# EDA-with-R
This Repository has R scripts having functions used for EDA
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

#SUGGESTION 2:
#Improve the code in suggestion 1 such that if the argument variable is 
#ignored then it will return the graphs of all the numeric variables 
#in the data by default.

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

#########################################################

Function 2: chi square test for dataframes




Function 3: t test for dataframes




#Function 4: rearrange the columns so that they are grouped 
#according to datatype nemeric vs categorical.

