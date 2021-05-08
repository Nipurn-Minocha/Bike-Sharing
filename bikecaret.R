myUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv"
d <- read.table(file=myUrl,header = T,sep = ",")
names(d) <- c("Date","RentedBikeCount","Hour","Temperature","Humidity","WindSpeed","Visibility",
              "DewPointTemp","Solar","Rainfall","Snowfall","Seasons","Holiday","FunctioningDay")
str(d)
d <- cbind(d,Weekday=weekdays(as.POSIXlt(d$Date),abbreviate=FALSE))
d <- cbind(d,Month=months(as.POSIXlt(d$Date),abbreviate=FALSE))
d$Year <- format(as.Date(d$Date, format="%d/%m/%Y"),"%Y")
d <- d[,c(2,1,3:16)]
names(d)[1] <- "y"
str(d)
d$Date <- as.Date(d$Date, format="%d/%m/%Y")
library(caret)
dummies <- dummyVars(y ~ ., data = d)            # create dummies for Xs
ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
d <- cbind(d$y, ex)                              # combine target var with Xs
names(d)[1] <- "y"                               # name target var 'y'
rm(dummies, ex)                                  # clean environment
d$Date <- as.Date(d$Date,origin = "1970-01-01")

# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(d[,3:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    # summarize the cors

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff. Find them and remove them
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- d[,3:ncol(d)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)                  # calculate a new cor matrix
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update dataset by removing those filtered vars that were highly correlated
d <- cbind(d$y,d$Date,filteredDescr)
names(d)[1] <- "y"
names(d)[2] <- "Date"

rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up


y <- d$y
dates <- d$Date

# create a column of 1s. This will help identify all the right linear combos
d <- cbind(rep(1, nrow(d)), d[3:ncol(d)])
names(d)[1] <- "ones"

# identify the columns that are linear combos
comboInfo <- findLinearCombos(d)
comboInfo

# remove columns identified that led to linear combos
d <- d[, -comboInfo$remove]

# remove the "ones" column in the first column
d <- d[, c(2:ncol(d))]

# Add the target variable back to our data.frame
d <- cbind(y,dates,d)

rm(y, comboInfo)  # clean up

nzv <- nearZeroVar(d, saveMetrics = TRUE)
d <- d[, c(TRUE,!nzv$zeroVar[3:ncol(d)])]

preProcValues <- preProcess(d[,2:ncol(d)], method = c("range"))
d <- predict(preProcValues, d)

set.seed(1234) # set a seed so you can replicate your results


# identify records that will be used in the training set. Here we are doing a
# 70/30 train-test split. You might modify this.
inTrain <- createDataPartition(y = d$y,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
# create your partitions
train <- d[inTrain,]  # training data set
test <- d[-inTrain,]  # test data set

ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=10,        # k number of times to do k-fold
                     #classProbs = T,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)

myModel1 <- train(y ~ .,               # model specification
                  data = train,        # train set used to build model
                  method = "glm",      # type of model you want to build
                  trControl = ctrl,    # how you want to learn
                  #family = "binomial", # specify the type of glm
                  metric = "Rsquared"       # performance measure
)
myModel1
