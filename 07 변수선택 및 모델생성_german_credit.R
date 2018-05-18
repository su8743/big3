#==================================================
# 4. 모델생성
#==================================================
german_credit.csv
# 4.1 Train Set & Test Set ---------------------------------------------------------
#Sampling Indexes
credit <- read.csv("german_credit.csv", na.strings=c("","NA","Unknown"))

str(credit)
credit$Creditability <- as.factor(credit$Creditability)
credit$Account.Balance <- as.factor(credit$Account.Balance)
credit$Payment.Status.of.Previous.Credit <- as.factor(credit$Payment.Status.of.Previous.Credit)
credit$Purpose <- as.factor(credit$Purpose)
credit$Value.Savings.Stocks <- as.factor(credit$Value.Savings.Stocks)
credit$Instalment.per.cent <- as.factor(credit$Instalment.per.cent)
credit$Sex...Marital.Status <- as.factor(credit$Sex...Marital.Status)
credit$Guarantors <- as.factor(credit$Guarantors)
credit$Most.valuable.available.asset <- as.factor(credit$Most.valuable.available.asset)
credit$Concurrent.Credits <- as.factor(credit$Concurrent.Credits)
credit$Type.of.apartment <- as.factor(credit$Type.of.apartment)
credit$No.of.Credits.at.this.Bank <- as.factor(credit$No.of.Credits.at.this.Bank)
credit$Occupation <- as.factor(credit$Occupation)
credit$No.of.dependents <- as.factor(credit$No.of.dependents)
credit$Telephone <- as.factor(credit$Telephone)
credit$Foreign.Worker <- as.factor(credit$Foreign.Worker)

indexes <- sample(1:nrow(credit), size=0.3*nrow(credit))

# Split data
test <- credit[indexes,]
train <- credit[-indexes,]
nrow(test)

# 데이터 들여다보기
# 주요 변수의 비율 비교 : titanic.0, train, test


# 4.2 변수 선택하기 : 전진선택법 ---------------------------------------------------
glm.base <- glm(Creditability~1, family=binomial, data = train)
str(train)
forward.aic <- step(glm.base
                    , Creditability ~
                      Account.Balance +
                      Payment.Status.of.Previous.Credit +
                      Purpose +
                      Value.Savings.Stocks +
                      Instalment.per.cent +
                      Sex...Marital.Status +
                      Guarantors +
                      Most.valuable.available.asset +
                      Concurrent.Credits +
                      Type.of.apartment +
                      No.of.Credits.at.this.Bank +
                      Occupation +
                      No.of.dependents +
                      Telephone +
                      Foreign.Worker +
                      Duration.of.Credit..month.+
                      Credit.Amount +
                      Length.of.current.employment +
                      Duration.in.Current.address +
                      Age..years.
                    ,direction = "forward")

# Survived ~ Sex + Pclass + Age + Family
# 내가선택된 변수 ==>  Sex + Pclass + Age + Embarked

# 4.3 머신러닝 알고리즘 : 로지스틱회귀 ---------------------------------------------

#모델생성
train.glm.1 <- glm(Creditability ~ Account.Balance + Duration.of.Credit..month. + 
                     Value.Savings.Stocks + Payment.Status.of.Previous.Credit + 
                     Purpose + Sex...Marital.Status + Guarantors + Instalment.per.cent + 
                     Foreign.Worker + Type.of.apartment
                   , family = binomial
                   , data = train)


#예측
head(test)
fit.results.1 <- predict(train.glm.1, newdata=test[,-1], type='response')

fit.results.1

#==================================================
# 5. 검증
#==================================================

# 5.1 ROC(Receiver Operating Curve) --------------

install.packages("Epi")
require(Epi)

ROC(fit.results.1,test$Creditability  )

# 5.2 Cut-off value
# 0.657
# 0.598
# 1,0으로 바꿔주기 
fit.results.New.1 <- ifelse(fit.results.1 >= 0.597,1,0)

# 5.1 Confusion matrix ------------------------------------------------------------
if(!require(caret)) install.packages("caret")
library(caret)
if(!require(e1071)) install.packages("e1071")
library(e1071)

# 수정된 부분 : confusionMatrix(예측값, 실제값)으로 실행
confusionMatrix(fit.results.New.1,test$Creditability)


# ------------
if(!require(kernlab)) install.packages("kernlab")
library(kernlab)

fit.svm <- ksvm(Creditability ~ Account.Balance + Duration.of.Credit..month. + 
                  Value.Savings.Stocks + Payment.Status.of.Previous.Credit + 
                  Purpose + Sex...Marital.Status + Guarantors + Instalment.per.cent + 
                  Foreign.Worker + Type.of.apartment
                , data=train)

train$Creditability<- as.integer(train$Creditability)
test$Creditability <- as.integer(test$Creditability)
result.svm <- predict(fit.svm,test[,-1])

test$Survived <- as.factor(test$Survived)
ROC(result.svm, test$Creditability  )

# 1,0으로 바꿔주기 

confusionMatrix(result.svm,test$Creditability)
confusionMatrix(result.svm.1,test$Survived)
confusionMatrix(result.svm.11,test$Survived)
class(fit.results.New.1)
result.svm.11 <- as.numeric(result.svm.1)

str(test)
test$Survived

