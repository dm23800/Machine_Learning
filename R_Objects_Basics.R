getwd()

#Logical
d <- TRUE
class(d)

#Numeric
 d<- 23.9
class(d) 

#Integer
d<- 23L
class(d)

#Comples
d <- 3+5i
class(d)

#Vectors : Most Basic data type. It can contain one or more elements of the same datatype

#Factors are R objects which are created using a vector
#It stores the vector as well as distinct values of the elements of the vector as labels
#irrespective of whether it is numeric or character or boolean
#Useful in statistical modeling

colors <- c ("Red","Green","Yellow","Red")
class(colors)
colors_factor <- factor(colors)
class(colors_factor)

colors_factor <- factor(colors,levels =c("Red","Green","Yellow"))

class(colors_factor)

colors_factor

#A Matrix is a table with all datatypes same
#It can be created by giving a vector input to a matrix function


#List can contain many different datatypes including a list itself

#Arrays are like matrices except that they arent confined to 2 dimensions
a <- array (c("yes","no"),dim=c(3,3,2))
print(a)


#Dataframe : It is a tabular data object with equally sized vectors of different datatypes
name_gender_age_income <- data.frame()
name_gender_age_income
class(name_gender_age_income)
class(name_gender_age_income$Name)

#Function
sample =c(1,2,3,4,5)
mean(sample)
sd(sample)


#Write a function to double the input
sqfunction = function (a) {
  return (a*a)
  
}

# Add 5 and then square
editvec = function (x) {
  y=(x+5)*(x+5)
  y
}

editvec
class(editvec)

editvec(1:20)

newvec <-editvec(1:20)
class(newvec)

# Fix it up a little and make more generic
editvec1 <- function(x,shift=5,power=2){
  if (!numeric(x)) return (NULL)
  (x+shift)
}

editvec(1:20,shift=3.5,power=.5)

?apply
samplesq=apply(sample,c(1,2),sqfunction)
#Write a vector od length 20 and initialize with any value using for loop




