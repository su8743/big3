##############################
# 연습문제 : 변수선택 및 모델생성
##############################

# 전처리가 끝난 titanic 데이테셋을 이용하여 다음 연습문제를 푸시오.
rm(list=ls())

# titanic.0 <- read.csv("titanic.0.csv", na.strings = "NA")
# titanic.0$Survived <- as.factor(titanic.0$Survived)
# titanic.0$Pclass <- as.factor(titanic.0$Pclass)
# str(titanic.0)

titanic.0$Title <- as.factor(titanic.0$Title)

# 1. 데이터셋을 train set과 test set을 7:3로 나누시오

indexes <- sample(1:nrow(titanic.0), size=0.3*nrow(titanic.0))
# Split data
test <- titanic.0[indexes,]
train <- titanic.0[-indexes,]

# 2. 전진선택법으로 변수를 선택하시오
glm.base <- glm(Survived~1, family=binomial, data = train)
str(train)
colSums(is.na(titanic.0))
forward.aic <- step(glm.base, Survived ~ Age+Embarked+Fare+Sex+Pclass+SibSp+Parch
                                         +Family+Mother+AgeGroup
                    ,direction = "forward")

#Survived ~ Sex + Pclass + Age + SibSp


# 3. 로지스틱 회귀분석 모델 두가지를 생성하시오
#    1) EDA를 통해서 선택된 독립변수들을 이용한 모델 : EDA -
#    2) 전진선택법으로 선택된 독립변수들을 이용한 모델 : Step - 



# 4. 3번에서 생성된 모델들로 test set을 이용하여 각각 예측결과를 저장하시오.



# 5. ROC 커브를 각각 그려보세요.



# (옵션) 두 모델의 AUC를 구하여 비교하시오.



# 6. 각각, 최적의 cut-off value를 찾으시오.



# 7. 최적의 cut-off value를 적용하여 4번의 예측 결과를 1, 0로 나누시오.



# 8. confusionMatrix를 이용하여 성능을 비교하시오.


bank.0 <- read.csv("titanic.0.csv")


