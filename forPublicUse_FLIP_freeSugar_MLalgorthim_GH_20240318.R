##################################################################### 
###### Load data ######

# Load datasets
df2017 <- fread("/Users/load_your_data.csv") # dataset to built model FLIP2017
df2020 <- fread("/Users/load_your_data.csv") # dataset to predict FLIP2020

# Variables used
df2017 <- df2017[,c("ID","Brand", "TRA_Cat","TRA_Item","FREESUG100",
                    "KCAL100","FAT100","SAT100","TRANS100","CHO100","FIBRE100","SUG100",           
                    "PRO100" ,"NA100","CHOL100" ,"VITA100","VITC100","CALCIUM100" ,"IRON100")]
df2020 <- df2020[,c("ID","Brand", "TRA_Cat","TRA_Item","FREESUG100",
                    "KCAL100","FAT100","SAT100","TRANS100","CHO100","FIBRE100","SUG100",                
                    "PRO100" ,"NA100","CHOL100" ,"VITA100","VITC100","CALCIUM100" ,"IRON100")]

#####################################################################

#####################################################################
###### Filter NA and Outliers ######
## FLIP2017 filter no freeSugar 100
df2017 <- df2017 %>% dplyr::filter(!is.na(FREESUG100))
df2017 <- df2017[df2017$FREESUG100 <= 100,]
df2017 <- df2017[complete.cases(df2017)==T,]

## FLIP2017 and FLIP2020 filter NA in nutrients
dfall <- rbind(df2017, df2020)
dfall$id <- rownames(dfall)
colnames(dfall)
dfall <- dfall[!(is.na(dfall$KCAL100) & is.na(dfall$FAT100)& is.na(dfall$SAT100)& is.na(dfall$TRANS100)& is.na(dfall$CHOL100)& is.na(dfall$NA100)& is.na(dfall$CHO100)& is.na(dfall$FIBRE100)& is.na(dfall$SUG100)& is.na(dfall$PRO100)),]

###### Select TRA Category ######
table(dfall$TRA_Cat)
df3 <- dfall[dfall$TRA_Cat == "W",] # change TRA category here

###### Select Variables for Algorithm ######
data <- df3[df3$date == "2017",]
data <- data[,c("KCAL100", "FAT100", "SAT100","TRANS100","CHOL100","NA100",
                 "CHO100","FIBRE100","SUG100","PRO100","date","id","FREESUG100")]

###### True Value Stats ####
df3a <- df3[df3$date == "2017",]
describe(df3a$FREESUG100)
summary(df3a$FREESUG100)
#####################################################################

#####################################################################
###### !!! KNN - FLIP2017 data for free sugar model (train and test) ######

## set the seed to make your partition reproducible
set.seed(1234)
## random select training data 70%
smp_size <- floor(0.7 * nrow(data))
train_id <- sample(seq_len(nrow(data)), size = smp_size)
train_data <- data[train_id, ]
test_data <- data[-train_id, ]
summary(train_data$FREESUG100)

colnames(train_data)
train_data_x <-train_data[, -c(11:13)]
train_data_x = scale(train_data_x)[,]
train_data_x[is.na(train_data_x)] <- 0
train_data_y = train_data[,13]
train_data_y<-data.matrix(train_data_y)

test_data_x <-test_data[, -c(11:13)]
test_data_x = scale(test_data_x)[,]
test_data_y = test_data[,13]
test_data_y<-data.matrix(test_data_y)


#####################################################################
###### !!! Build KNN reg model to predict ######

require(caret)
knnregmodel = knnreg(train_data_x, train_data_y, k=5) # number of nerbourhood
knnregmodel
str(knnregmodel)

pred_y = predict(knnregmodel, data.frame(test_data_x))
test2017=data.frame(test_data_y, pred_y)

#####################################################################
###### !!! See performance and plot and stats ######

mse = mean((test_data_y - pred_y)^2)
mae = caret::MAE(test_data_y, pred_y)
rmse = caret::RMSE(test_data_y, pred_y)
r2 = cor(test_data_y, pred_y) ^ 2

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse, " R2: ", r2)


#####################################################################
###### !!! Stats ######
describe(train_data_y)
describe(test_data_y)
describe(pred_y)


#####################################################################
###### !!! Apply KNN reg model and predict Free Sugar FLIP2020 ######
### load FLIP2020 raw data

data2020 <- df3[df3$date == "2020",]
data2020 <- data2020[,c("KCAL100", "FAT100", "SAT100","TRANS100","CHOL100","NA100",
                "CHO100","FIBRE100","SUG100","PRO100","date","id","FREESUG100")]

data2=data2020
data2 <- data2[, -c(11:13)]
data2_x = scale(data2)[,]
data2_x[is.na(data2_x)] = 0

pred_y2 = predict(knnregmodel, data.frame(data2_x))
pred_y2 <- as.data.frame(pred_y2)
describe(pred_y2)

predict2020=data.frame(pred_y2) ### output
predict2020$FLIP_ID = "NA"
predict2020$FLIP_ID = df3b$ID
