h2o.init(nthreads=1, max_mem_size="4g")

data <- h2o.importFile("https://archive.ics.uci.edu/ml/machine-learning-databases/00560/SeoulBikeData.csv",header = NA)
y <- "RentedBikeCount"                                # target variable to learn
xdf <-   as.data.frame(data)           # feature variables are all other columns
xdf2 <- xdf[-1,]
xdf2$Hour <- as.factor(xdf2$Hour)
#plot(10:1)
names(xdf2) <- c("Date","RentedBikeCount","Hour","Temperature","Humidity","WindSpeed",
                 "Visibility","DewPointTemperature","SolarRadiation","Rainfall","Snowfall",
                 "Seasons","Holiday","FunctioningDay")
x <- c("Hour","Temperature","Humidity","WindSpeed",
       "Visibility","SolarRadiation","Rainfall","Snowfall",
       "Seasons","Holiday","FunctioningDay")
xdf2 <- cbind(xdf2,Weekday=weekdays(as.POSIXlt(xdf2$Date),abbreviate=FALSE))
xdf2 <- cbind(xdf2,Month=months(as.POSIXlt(xdf2$Date),abbreviate=FALSE))
xdf2$Year <- format(as.Date(xdf2$Date, format="%d/%m/%Y"),"%Y")
parts <- h2o.splitFrame(as.h2o(xdf2), 0.8, seed=99) # randomly partition data into 80/20
train <- parts[[1]]                         # random set of training obs
test <- parts[[2]]                          # random set of testing obs

m <- h2o.deeplearning(x, y, train)
m1 <- h2o.glm(x,y,train)
aml <- h2o.automl(x, y 
                  , training_frame = train
                  , validation_frame = test
                  , max_runtime_secs = 300    # max time to run in seconds
                  , max_models = 10           # max num of models
                  , seed = 123                # for reproducibility.
)
lb <- aml@leaderboard
print(lb, n = nrow(lb))
pred <- h2o.predict(aml@leader, test)
h2o.r2(aml@leader,valid = TRUE)


model_path <- h2o.saveModel(object = aml@leader
                            , path = getwd()
                            , force = TRUE  #indicates how to deal with files that already exist
)
print(model_path)