##################################################
# 데이터 전처리
##################################################

# NA 처리 ------------------------------
grep('aud', mpg$manufacturer)
mpg[grep('aud', mpg$manufacturer),]

gsub('aud', 'Aud', mpg$manufacturer)

titanic.0$Name

# Age : 호칭이 같은 사람의 평균Age로 채우기
# ① 이름에서 타이틀(호칭)을 분리하자. 그리고 별도 칼럼으로 만들자.

master_vector <- grep("Master[.]",titanic.0$Name)
miss_vector <- grep("Miss[.]", titanic.0$Name)
mrs_vector <- grep("Mrs[.]", titanic.0$Name)
mr_vector <- grep("Mr[.]", titanic.0$Name) 
dr_vector <- grep("Dr[.]", titanic.0$Name)


#빈 값으로 채워진 열(변수) 추가
titanic.0$Title <- ''
head(titanic.0)

titanic.0[master_vector,]$Title <-"Master"
titanic.0[miss_vector,]$Title <-"Miss"
titanic.0[mrs_vector,]$Title <-"Mrs"
titanic.0[mr_vector,]$Title <-"Mr"
titanic.0[dr_vector,]$Title <-"Dr"

head(titanic.0)


# 칼럼 삭제는? 직관적이지는 않음.
titanic.0$Title <- NULL
head(titanic.0)

## ② 호칭의 평균 나이를 계산한다.
titanic.0$Title == "Master"
titanic.0$Age[titanic.0$Title == "Master"]

master_age <- titanic.0$Age[titanic.0$Title == "Master"]
master_age

master_age_mean <- mean(master_age, na.rm = TRUE)
master_age_mean

round(master_age_mean,digits = 2)

master_age <- round(mean(titanic.0$Age[titanic.0$Title == "Master"], na.rm = TRUE), digits = 2)
miss_age <- round(mean(titanic.0$Age[titanic.0$Title == "Miss"], na.rm = TRUE), digits =2)
mrs_age <- round(mean(titanic.0$Age[titanic.0$Title == "Mrs"], na.rm = TRUE), digits = 2)
mr_age <- round(mean(titanic.0$Age[titanic.0$Title == "Mr"], na.rm = TRUE), digits = 2)
dr_age <- round(mean(titanic.0$Age[titanic.0$Title == "Dr"], na.rm = TRUE), digits = 2)


## ③ 나이가 NA인 사람의 호칭을 보고 평균나이를 넣어준다. 

titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Master","Age"] <-  master_age
titanic.0$Age
titanic.0[,"Age"]
names(titanic.0)
titanic.0[,6]

titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Miss","Age"] <-  miss_age
titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Mrs","Age"] <-  mrs_age
titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Mr","Age"] <-  mr_age
titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Dr","Age"] <-  dr_age

sum(is.na(titanic.0$Age))

# 두건의 승선지역 NA값은 가장 많은 S로 처리하자.
# 하지만, 더 좋은 방법은 승선지역을 종속변수로 두고 예측하는 것!
titanic.0$Embarked
sum(is.na(titanic.0$Embarked))
table(titanic.0$Embarked)
is.na(titanic.0$Embarked)
titanic.0$Embarked[is.na(titanic.0$Embarked)] <- 'S'
table(titanic.0$Embarked)

# 데이터 채우기2 범주형으로 변환 ---------------------------------------------
titanic.0$Age
cut(titanic.0$Age, breaks = c(0,10,20,30,40,50,60,70,80,90,100))
library(dplyr)
a <- mutate(titanic.0, AgeGroup = cut(titanic.0$Age
                                     , breaks = c(0,10,20,30,40,50,60,70,80,90,100)))
head(a)
head(a[,c("Age", "AgeGroup")])
a1<- mutate(titanic.0, AgeGroup = cut(titanic.0$Age, breaks = c(0,10,20,30,40,50,60,70,80,90,100)
                                      , labels = c("Age0_10","Age11_20","Age21_30","Age31_40"
                                                   ,"Age41_50","Age51_60","Age61_70","Age71_80"
                                                   ,"Age81_90","Age91_100")))
head(a1[,c("Age", "AgeGroup")])
titanic.0 <- mutate(titanic.0
                    , AgeGroup = cut(titanic.0$Age
                                     , breaks = c(0,10,20,30,40,50,60,70,80,90,100)
                                     , labels = c("Age0_10","Age11_20","Age21_30","Age31_40"
                                                  ,"Age41_50","Age51_60","Age61_70","Age71_80"
                                                  ,"Age81_90","Age91_100")))

str(titanic.0)
head(titanic.0)

# 변수 추가 생성하기 -----------------------------------------------------------

titanic.0 <- titanic.0 %>% 
  mutate(Family = SibSp + Parch + 1)

titanic.0 <- mutate(titanic.0, Family = SibSp + Parch + 1)

head(titanic.0)

# 가변수화 -----------------------------------------------
install.packages("dummies")
library(dummies)

dia <- data.frame(diamonds)
head(dia)
dum.cut <-dummy(dia$cut, sep = ".")
dia.new <- cbind(dia, dum.cut )
head(dia.new)
?merge
