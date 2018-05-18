#==================================================
# 4. 모델생성
#==================================================

# 4.1 Train Set & Test Set ---------------------------------------------------------
head(titanic.0)
#Sampling Indexes
titanic.0 <- read.csv("titanic.1.csv")
str(titanic.0)
titanic.0$Pclass <- as.factor(titanic.0$Pclass)
titanic.0$Survived <- as.factor(titanic.0$Survived)

?sample
sample(1:100, size = 10)
nrow(titanic.0)

indexes <- sample(1:nrow(titanic.0)
                  , size=0.3*nrow(titanic.0))
indexes
# Split data
test <- titanic.0[indexes,]
train <- titanic.0[-indexes,]
nrow(test)

# 데이터 들여다보기
# 주요 변수의 비율 비교 : titanic.0, train, test
table(titanic.0$Sex)

prop.table(table(titanic.0$Sex))
prop.table(table(train$Sex))
prop.table(table(test$Sex))

prop.table(table(titanic.0$Survived))
prop.table(table(train$Survived))
prop.table(table(test$Survived))

barplot(table(titanic.0$AgeGroup))
barplot(table(train$AgeGroup))
barplot(table(test$AgeGroup))

?step

# 4.2 변수 선택하기 : 전진선택법 ---------------------------------------------------
glm.base <- glm(Survived~1, family=binomial, data = train)
str(train)
forward.aic <- step(glm.base
                    , Survived~Age+Embarked+Fare+Sex+Pclass+SibSp+Parch+Family+AgeGroup
                    ,direction = "forward")

# Survived ~ Sex + Pclass + Age + Family
# 내가선택된 변수 ==>  Sex + Pclass + Age + Embarked

# 4.3 머신러닝 알고리즘 : 로지스틱회귀 ---------------------------------------------

#모델생성
train.glm.1 <- glm(Survived ~ Sex + Pclass + Age + Family
                   , family = binomial
                   , data = train)

train.glm.2 <- glm(Survived ~ Sex + Pclass + Age + Embarked
                   , family = binomial
                   , data = train)

#예측
head(test)
fit.results.1 <- predict(train.glm.1, newdata=test[,-3], type='response')
fit.results.2 <- predict(train.glm.2, newdata=test[,-3], type='response')
fit.results.1

#==================================================
# 5. 검증
#==================================================

# 5.1 ROC(Receiver Operating Curve) --------------

install.packages("Epi")
require(Epi)

ROC(fit.results.1,test$Survived  )
ROC(fit.results.2,test$Survived  )

# 5.2 Cut-off value
# 0.657
# 0.598
# 1,0으로 바꿔주기 
fit.results.New.1 <- ifelse(fit.results.1 >= 0.657,1,0)
fit.results.New.2 <- ifelse(fit.results.2 >= 0.598,1,0)

# 5.1 Confusion matrix ------------------------------------------------------------
if(!require(caret)) install.packages("caret")
library(caret)
if(!require(e1071)) install.packages("e1071")
library(e1071)

# 수정된 부분 : confusionMatrix(예측값, 실제값)으로 실행
confusionMatrix(fit.results.New.1,test$Survived)
confusionMatrix(fit.results.New.2,test$Survived)

# SVM비교
# 4.3 머신러닝 알고리즘 : SVM ---------------------------------------------
if(!require(kernlab)) install.packages("kernlab")
library(kernlab)

fit.svm <- ksvm(Survived ~ Sex + Pclass + Age + Family + Embarked
                , data=train)

train$Survived<- as.integer(train$Survived)
test$Survived <- as.integer(test$Survived)
result.svm <- predict(fit.svm,test[,-3])

test$Survived <- as.factor(test$Survived)
ROC(result.svm,test$Survived  )

# 1,0으로 바꿔주기 
result.svm.1 <- ifelse(result.svm >= 1.052,1,0)
confusionMatrix(result.svm,test$Survived)
confusionMatrix(result.svm.1,test$Survived)
confusionMatrix(result.svm.11,test$Survived)
class(fit.results.New.1)
result.svm.11 <- as.numeric(result.svm.1)

str(test)
test$Survived
