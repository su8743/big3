## KNN 

# 1. 개요 ------------------------------------------------------------------------

## 사례 : 유방암 종양 촬영 데이터를 이용하여, 악성과 양성 분류하기
## 569개 관측치, 32개 변수
## 주요변수 : Radius, Texture, Perimeter, Area, Smoothness, Compactness, Concavity
##          , Concave points, Symmetry, Fractal dimension 등
    

# 2. 준비-----------------------------------------------------------------

setwd("C:\\Users\\Gilbert Han\\Documents\\2.회사업무\\D_plus\\2.강의\\데이터진흥원_유통빅데이터과정_R")
wbcd <- read.csv("wisc_bc_data.csv", header = T, stringsAsFactors = FALSE)

head(wbcd)

### 1) ID 값 제거

wbcd <- wbcd[-1]
table(wbcd$diagnosis)


### 2) 종속변수 factor형으로 변경.
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

### 3) 주요변수 기초 통계량
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])


### 4) 독립변수 정규화 하기

# 값 정규화
normalize <- function(x){
    return ((x - min(x))/(max(x)-min(x)))
}

# 정규화 테스트
normalize(c(1,2,3,4,5))

# 정규화하여 데이터 셋 구성하기
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
head(wbcd)
head(wbcd_n)

### 5) train, test data set : 값은 무작위로 입력되어 있으므로 row number로 분리

wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

# 종속변수(label)로 구성된 별도 데이터셋 만들기

head(wbcd_n)
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]


# 3 모델 생성     ---------------------------------------------------------

library(class)
 
### k값 결정
#보통 데이터 건수의 제곱근으로 부터 시작
sqrt(469)
#21.65641

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels
                      , k = 21)

wbcd_test_pred1 <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels
                      , k = 5)

wbcd_test_pred2 <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels
                       , k = 40)


# 4. 값 검증 -------------------------------------------------------------------
library(caret)

# confusionMatrix
confusionMatrix(wbcd_test_pred,wbcd_test_labels)
# confusionMatrix(wbcd_test_pred2,wbcd_test_labels)
# confusionMatrix(wbcd_test_pred3,wbcd_test_labels)



