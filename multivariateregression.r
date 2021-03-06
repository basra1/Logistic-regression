setwd ("C:\\Users\\MYPC\\Desktop\\acadgild")
getwd ()

cars_retail <- read.csv ("cars_retail.csv", header=TRUE)
head (cars_retail)

cars_retail_new <- cars_retail

str (cars_retail)

#Summary Statistics
summary (cars_retail)

#Checking for Missing Values

for (i in 1:12)
{
  print(sum(is.na(cars_retail[,i])))
}

#Variable transformation
cars_retail$Cruise <- as.factor (cars_retail$Cruise)
cars_retail$Sound <- as.factor (cars_retail$Sound)
cars_retail$Leather <- as.factor (cars_retail$Leather)

#Outlier detection
stats_out <- boxplot.stats(cars_retail$Price)$out 
stats_out
boxplot (cars_retail$Price, horizontal=T)
#Plotting Price~Mileage
plot (cars_retail$Price~cars_retail$Mileage)
#Subsetting dataset with Price > 52000
cars_r_out <- cars_retail[cars_retail$Price>52000,]
cars_r_out
#Outlier values- Cars with Price>52000 are on the expensive side and can't be considered as outliers
plot (cars_r_out$Price~cars_r_out$Mileage)

#Creating a simple Linear Regression Model with Price as DV and Mileage as IV

cars_r_lm1 <- lm (Price~Mileage,data=cars_retail) 
cars_r_lm1
summary(cars_r_lm1)# R-sq : 0.02046 or 2%

#Creating a simple Linear Regression Model with Price as DV and Mileage as IV for cars with Price>52000
cars_r_lm2 <- lm (Price~Mileage, data=cars_r_out)
cars_r_lm2
summary(cars_r_lm2)# R-sq : 0.986 or 98.6%

#Create dummy variables for Make variable
class (cars_retail$Make)
Make_factor <- model.matrix(~cars_retail$Make )
head(Make_factor)

#Variable with k levels require only k-1 dummy variables ##need to ask
Make_factor <- data.frame(Make_factor)#Converting from matrix to data.frame
Make_factor
factor_length <- length(names(Make_factor))-1
cars_retail_new <- cbind (cars_retail_new,Make_factor[1:factor_length] )
head (cars_retail_new)

#Multiple Linear Regression
cars_r_mlm1 <- lm (Price~Mileage+Cylinder+Liter+Doors+Cruise+Sound+Leather+
                     +cars_retail.MakeCadillac+
                     cars_retail.MakeChevrolet+cars_retail.MakePontiac+cars_retail.MakeSAAB,
                   data=cars_retail_new)
cars_r_mlm1

summary(cars_r_mlm1)

#Create dummy variables for Type Variable and including in the next iteration of MLR
class (cars_retail_new$Type)
Type_factor <- model.matrix(~cars_retail_new$Type -1)
head(Type_factor)

#Variable with k levels require only k-1 dummy variables
Type_factor <- data.frame(Type_factor)#Converting from matrix to data.frame
factor_length1 <- length(names(Type_factor))-1
cars_retail_new <- cbind (cars_retail_new,Type_factor[1:factor_length1] )
head (cars_retail_new)

#Multiple Linear Regression
cars_r_mlm2 <- lm (Price~Mileage+Liter+Doors+
                  cars_retail.MakeCadillac+
                     cars_retail.MakeSAAB+cars_retail_new.TypeConvertible+
                     cars_retail_new.TypeCoupe+cars_retail_new.TypeHatchback+
                     cars_retail_new.TypeSedan,
                   data=cars_retail_new)
cars_r_mlm2
summary(cars_r_mlm2)

#Second iteration of MLR with added Type variable has resulted in 
#better R-sq and Adj R-sq
#All variables included are significant in predicting the Price

#Predicted value from MLR cars_r_mlm2
cars_retail_p <- predict (cars_r_mlm2)
#Residual value
cars_retail_res <- resid(cars_r_mlm2)

#Take the log of Price and generate the regression equation
#Multiple Linear Regression ## Why Log
cars_r_mlm3 <- lm (log(Price)~Mileage+Liter+Doors+
                     cars_retail.MakeCadillac+
                     cars_retail.MakeSAAB+cars_retail_new.TypeConvertible+
                     cars_retail_new.TypeHatchback+
                     cars_retail_new.TypeSedan,
                   data=cars_retail_new)
cars_r_mlm3
summary(cars_r_mlm3)
#Log(Price) is giving a better R-sq and Adj R-sq over the above model
#Therefore we can finalise on this model
#All IV's are significant in predicting the log (Price)

#Predicted value from MLR cars_r_mlm2
cars_retail_pl <- predict(cars_r_mlm3)
#Residual value
cars_retail_resl <- resid(cars_r_mlm3)

plot (cars_retail_new$Price,cars_retail_resl,xlab="Price",ylab="Residuals")

plot (cars_retail_pl,cars_retail_resl,xlab="Pred Price",ylab="Residuals",abline (0,0))
#Distribution above and below are not skewed and near symmetric 
#So we can rule out heteroscedacity

#Actual vs Predicted Price Plot
plot (cars_retail_new$Price, col="green",type="l")
par (new=TRUE)
plot (cars_retail_pl,col="red",type="l")
#The actual and predicted price are comparable and close

par(mfrow=c(2,2))
plot (cars_r_mlm3)

#Comparing the distribution of Residual of Price and Log(Price)
hist (cars_retail_res)
hist (cars_retail_resl)
#Residual of Log(Price) is showing a better normal distribution that residual of Price without 
#log transformation