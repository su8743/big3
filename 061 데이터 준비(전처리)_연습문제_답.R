##############################
# 연습문제2
##############################


# titanic 데이테셋을 이용하여 다음 연습문제를 푸시오.

# 1. 호칭으로 구성된 Title 칼럼(변수)을 만드시오.

str(titanic.0)
master_vector <- grep("Master[.]",titanic.0$Name)
miss_vector <- grep("Miss[.]", titanic.0$Name)
mrs_vector <- grep("Mrs[.]", titanic.0$Name)
mr_vector <- grep("Mr[.]", titanic.0$Name) 
dr_vector <- grep("Dr[.]", titanic.0$Name)

#빈 값으로 채워진 열(변수) 추가
titanic.0$Title <- NULL
titanic.0$Title <- ''
head(titanic.0)

titanic.0[master_vector,]$Title <-"Master"
titanic.0[master_vector,"Title"] <-"Master"

titanic.0[miss_vector,]$Title <-"Miss"
titanic.0[mrs_vector,]$Title <-"Mrs"
titanic.0[mr_vector,]$Title <-"Mr"
titanic.0[dr_vector,]$Title <-"Dr"

# 2. Age를 소수점 1자리에서 반올림하여 자연수로 된 Age2 칼럼(변수)을 만드시오.
titanic.0 <- mutate(titanic.0, Age2 = round(Age))

str(titanic.0)

# 3. 각 호칭의 나이들의 최빈값을 구하시오.
mode <- function(x) {
  t <- table(x)
  as.integer(names(t)[which.max(t)])
}

group_by(titanic.0, Title) %>%
  summarise(Md = mode(Age2))


master_Mode <- mode(titanic.0$Age2[titanic.0$Title == "Master"])
miss_Mode <- mode(titanic.0$Age2[titanic.0$Title == "Miss"])
mrs_Mode <- mode(titanic.0$Age2[titanic.0$Title == "Mrs"])
mr_Mode <- mode(titanic.0$Age2[titanic.0$Title == "Mr"])
dr_Mode <- mode(titanic.0$Age2[titanic.0$Title == "Dr"])

master_Mode
mr_Mode

# 4. 각 호칭의 나이가 NA인 값을 위 3번의 최빈값으로 변경하시오.

titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Master","Age"] <-  master_Mode
titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Miss","Age"] <-  miss_Mode
titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Mrs","Age"] <-  mrs_Mode
titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Mr","Age"] <-  mr_Mode
titanic.0[is.na(titanic.0$Age) & titanic.0$Title == "Dr","Age"] <-  dr_Mode

sum(is.na(titanic.0$Age))

# 5. 나이를 ~ 20세, 21~40세, 41~60세, 61세~ 총 4개의 범주로 데이터를 변환하여 AgeGroup에 저장하시오.
titanic.0 <- mutate(titanic.0
                    , AgeGroup2 = cut(titanic.0$Age
                                              , breaks = c(0,20,40,60,100)))

head(titanic.0)
