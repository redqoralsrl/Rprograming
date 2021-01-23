# chapter05~08
# 데이터 프레임 만들기
history <- c(100,90,80) # 역사점수 생성
math <- c(98,75,60) # 수학점수 생성
science <- c(60,80,20) # 과학점수 생성
# 변수 합해서 데이터프레임 만들기
data_result　 <- data.frame(history,math,science)
data_result
mean(data_result$history)
mean(data_result$math)
mean(data_result$science)
summary(data_result)
# 외부 데이터 불러오기
# 엑셀 데이터 불러오기
install.packages("readxl")
library(readxl)
data_excel <- read_excel("finalexam.xlsx", sheet = 1,col_names = T)
data_excel
# 엑셀을 불러와서 변수로 만든다
# sheet는 엑셀의 몇번째 데이터 시트를 가져오는지(엑셀이 sheet로 되어있다)
# col_names는 데이터를 불러올때 컬리이름까지 가져올꺼야? True 혹은 False
# 즉, 제일 위에 있는 id, class, math, history, english를 가져온다는 말
# F로 쓰면 ...1 ...2 ...3 ...4 ...5로 변수명이 온다
summary(data_excel)
mean(data_excel$math)
mean(data_excel$history)
mean(data_excel$english)
# csv파일 불러오기(,로 구별되어 있다)
# 설치 안해도 됨
data_csv <- read.csv("csv_exam.csv", header = T)
data_csv
write.csv(data_excel,file = "hi.csv")
#####################################
exam <- read.csv("csv_exam.csv")
head(exam) # 앞에서부터 6행까지 출력
tail(exam) # 밑에서부터 6행까지 출력
View(exam) # 뷰어 창에서 데이터 확인하기
dim(exam) # 몇 행 몇 열로 구성되어 있는지 보기
str(exam) # 데이터 속성 확인
summary(exam) # 요약통계량 출력
#####################################
# mpg 데이터 파악하기
# ggplot2의 mpg 데이터를 데이터 프레임 형태로 불러오기
# ggplot2의 패키지 안에서 mpg 데이터만 쏙 뽑아오기
mpg <- as.data.frame(ggplot2::mpg)
# 패키지 안에 mpg 데이터만 사용하겠다 (::)
mpg
head(mpg)
tail(mpg)
View(mpg)
dim(mpg)
str(mpg)
summary(mpg)
####################################
# 데이터 수정하기 - 변수명 바꾸기
install.packages("dplyr")
library(dplyr)
# 데이터 프레임 생성
df_raw <- data.frame(test1 = c(1,2,3), test2 = c(2,3,2))
df_raw
# 데이터 수정 작업 시 복사본을 만들어 백업본을 만든다
df_new <- df_raw
df_new
# 변수명 바꾸기
# rename이라는 함수를 쓰는데 dplyr 안에 있는 것이다
df_new <- rename(df_new,var1 = test1, var2 = test2)
df_new
###################################
# 문제
# mpg 데이터의 변수명은 긴 단어를 짧게 줄인 축약어로 되어 있습니다
# cty 변수는 도시 연비, hwy 변수는 고속도로 연비를 의미합니다
# 변수명을 이해하기 쉬운 단어로 바꾸려고 합니다
# mpg 데이터를 이용해서 아래 문제를 해결해 보세요.
# 1) ggplot2패키지의 mpg 데이터를 사용할 수 있도록 불러온 뒤 복사본을 만드세요
# 2) 복사본 데이터를 이용해서 cty는 city로 hwy는 highway로 변수명을 수정하세요
# 3) 데이터 일부를 출력해서 변수명이 바뀌었는지 확인하는데 위에서 6번째
# 줄만 나오게 출력하세요.

#1)
install.packages("dplyr")
library(dplyr)
mpg <- as.data.frame(ggplot2::mpg)
mpg
mpg_new <- mpg
mpg_new
#2)
mpg_new <- rename(mpg_new, city = cty, highway = hwy)
#3)
head(mpg_new)

################################
# 파생 변수 만들기

# 데이터프레임 생성
df <- data.frame(test = c(10,20,30), test1 = c(20,40,60))
df
# 파생변수 생성
df$test_sum <- df$test + df$test1
df
df$test_evg <- (df$test + df$test1) / 2
df
# mpg 통합 연비 변수 만들기
mpg <- as.data.frame(ggplot2::mpg)
mpg$total <- (mpg$cty + mpg$hwy) / 2
head(mpg)
mean(mpg$total)
# 조건문을 활용해 파생변수 만들기
# 기준값 정하기
summary(mpg$total)
hist(mpg$total)
# 조건문으로 합격 판정 변수 만들기
# 20이상이면 pass 아니면 fail을 넣어라
mpg$test <- ifelse(mpg$total >= 20, "pass","fail")
head(mpg,10)
# 빈도표로 판정 자동차 수 살펴보기
table(mpg$test)
# 막대그래프로 빈도 표현하기
install.packages(ggplot2)
library(ggplot2)
qplot(mpg$test)
# 중첩 조건문 활용하기 - 연비 등급 변수 만들기
mpg$grade <- ifelse(mpg$total >= 30, "A", ifelse(mpg$total >= 20, "B", "C"))
head(mpg,20)
table(mpg$grade)
qplot(mpg$grade)

###################################
# 정리하기
# 1. 데이터 준비, 패키지 준비
mpg <- as.data.frame(ggplot2::mpg)
library(dplyr) # dplyr 로드
library(ggplot2) # ggplot2 로드
# 2. 데이터 파악
head(mpg) # Raw 데이터 앞부분
tail(mpg) # Raw 데이터 뒷부분
View(mpg) # Raw 데이터 뷰어창에서 확인
dim(mpg) # 차원
str(mpg) # 속성
summary(mpg) # 요약 통계량
# 3. 변수명 수정
mpg <- rename(mpg, company = manufacturer)
# 4. 파생변수 생성
mpg$total <- (mpg$cty + mpg$hwy) / 2 # 변수 조합
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail") # 조건문 활용
# 5. 빈도 확인
table(mpg$test) # 빈도표 출력
qplot(mpg$test) # 막대 그래프 생성
##################################
# 문제
# ggplot2 패키지에는 미국 동부중부 437개 지역의 인구통계 정보를 담은 midwest
# 라는 데이터가 포함되어 있습니다. midwest 데이터를 사용해 데이터 분석
# 문제를 해결 해보세요.
# Q1)ggplot2의 midwest 데이터를 데이터 프레임 형태로 불러와서 데이터 특성을
# 파악하세요
# Q2)poptotal(전체 인구)를 total로, popasian(아시아 인구)를 asian으로 변수명을
# 저장하세요
# Q3)total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수를
# 만들고, 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세요.
# Q4)아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large" 그외에는
# small을 부여하는 파생변수를 만들어 보세요.
# Q5)"large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도표와 빈도 막대
# 그래프를 만들어 확인해 보세요.
library(dplyr)
library(ggplot2)
#Q1)
midwest <- as.data.frame(ggplot2::midwest)
head(midwest,2)
tail(midwest,2)
dim(midwest)
View(midwest)
str(midwest)
summary(midwest)
midwest_test <- midwest
head(midwest_test,2)
#Q2)
midwest_test <- rename(midwest_test,total = poptotal,asian = popasian)
head(midwest_test,1)
#Q3)
midwest_test$total_asian <- (midwest_test$asian / midwest_test$total) * 100
head(midwest_test,2)
hist(midwest_test$total_asian)
#Q4)
mean(midwest_test$total_asian)
midwest_test$evg <- ifelse(midwest_test$total_asian > 0.4872462,"large","small")
head(midwest_test,2)
table(midwest_test$evg)
qplot(midwest_test$evg)
##################################
# 데이터 전처리 - 원하는 형태로 데이터 가공하기
# dplyr 패키지
# filter() 행 추출
# select() 열 추출
# arrange() 정렬
# mutate() 변수 추가
# summarise() 통계치 산출
# group_by() 집단별로 나누기
# left_join() 데이터 합치기(열)
# bind_rows() 데이터 합치기(행)
##################################
# 조건에 맞는 데이터만 추출하기
# 행을 출력하기
library(dplyr)
exam <- read.csv("csv_exam.csv")
exam
# exam에서 class가 1인 경우만 추출하여 출력
exam %>% filter(class == 1)
# %>% 파이프 연산자 혹은 체인 연산자라고 한다
exam %>% filter(class == 2)
# 1반이 아닌 경우
exam %>% filter(class != 1)
# 3반이 아닌 경우
exam %>% filter(class != 3)
# 수학 점수가 50점 초과한 경우
exam %>% filter(math > 50)
# 수학 점수가 50점 미만인 경우
exam %>% filter(math < 50)
# 여러 조건을 충족하는 행 추출하기
# 1반이면서 수학점수가 50점 이상인 경우
exam %>% filter(class == 1 & math >= 50)
# 2반이면서 영어점수가 80점 이상인 경우
exam %>% filter(class == 2 & english >= 80)
# 수학점수가 90점 이상이거나 영여 점수가 90점 이상인 경우
exam %>% filter(math >= 90 | english >= 90)
# 영어점수가 90점 미만이거나 과학점수가 50점 미만인 경우
exam %>% filter(english < 90 & science < 50)
# 목록에 해당되는 행 추출하기
# 1, 3, 5반에 해당되면 추출
exam %>% filter(class == 1 | class == 3 | class == 5)
# %in% 기호 이용하기 매치 오퍼레이터라고 한다
# 여러개의 조건을 간략하게 적을 수 있다
exam %>% filter(class %in% c(1,3,5))
# 추출한 행으로 데이터 만들기
# class 가 1인 데이터를 class1에 저장하고  class1의 수학 점수 평균을 구하라
class1 <- exam %>% filter(class == 1)
mean(class1$math)
# class가 2인 데이터를 class2에 저장하고 class2의 수학 점수 평균을 구하라
class2 <- exam %>% filter(class == 2)
mean(class2$math)
#################################
# 논리 연산자
# <     작다
# <=    작거나 같다
# >   크다
# >=    크거나 같다
# ==    같다
# !=    같지 않다
# |     또는
# &     그리고
# %in%  매칭확인
#################################
# 산술 연산자
# +     더하기
# -     빼기
# *     곱하기
# /     나누기
# ^, ** 제곱
# %/%   나눗셈의 몫
# %%    나눗셈의 나머지
#################################
# 문제
# mpg 데이터를 이용해 분석 문제를 해결해 보세요.
# Q1)자동차 배기량에 따라 고속도로 연비가 다른지 알아보려고 합니다.
# displ(배기량)이 4 이하인 자동차와 5 이상인 자동차 중 어떤 자동차의 hwy(고속
# 도로 연비)가 평균적으로 더 높은지 알아보세요.
# Q2) 자동차 제조회사에 따라 도시 연비가 다른지 알아보려고 합니다.
# "audi"와 "toyota" 중 어느 manufacturer(자동차 제조 회사)의 cty(도시 연비)가
# 평균적으로 더 높은지 알아보세요.
# Q3) "chevrolet", "ford", "honda" 자동차의 고속도로 연비 평균을 알아보려고
# 합니다. 이 회사들의 자동차를 추출한 뒤 hwy 전체 평균을 구해보세요.
# Q1)
mpg <- as.data.frame(ggplot2::mpg)
mpg_data <- mpg
head(mpg_data)
mpg_displ4 <- mpg %>% filter(displ <= 4)
mpg_displ5 <- mpg %>% filter(displ >= 5)
mean(mpg_displ4$hwy)
mean(mpg_displ5$hwy)
#Q2)
mpg_audi <- mpg %>% filter(manufacturer == "audi")
mpg_toyota <- mpg %>% filter(manufacturer == "toyota")
mean(mpg_audi$cty)
mean(mpg_toyota$cty)
#Q3)
mpg_hwy <- mpg %>% filter(manufacturer %in% c("chevrolet","ford","honda"))
mean(mpg_hwy$hwy)
##############################
# 필요한 변수만 추출하기
# 열만 추출하기
exam %>% select(math) # math 추출
exam %>% select(english) # english 추출
# 여러 변수 추출하기
exam %>% select(class, math, english) # class, math, english 변수 추출
# 변수 제외하기
exam %>% select(-math) # math 제외
# 여러 변수 제외하기
exam %>% select(-math, -english) # math, english 제외 
# dplyr 함수 조합하기
# class가 1인 행만 추출한 다음 english 추출
exam %>% filter(class == 1) %>% select(english)
# 가독성 있게 줄 바꾸기
exam %>%
  filter(class == 1) %>%
  select(english)
# 일부만 출력하기
exam %>%
  select(id, math) %>% # id, math 추출
  head(10) # 앞부분 10행까지 추출 

# 문제
# mpg 데이터를 이용해서 분석 문제를 해결해보세요
# Q1. mpg데이터는 11개 변수로 구성되어 있습니다. 이 중 일부만 추출해서
# 분석에 활용하려고 합니다. mpg 데이터에서 class(자동차 종류), cty(도시연비)
# 변수를 추출해 새로운 데이터를 만드세요. 새로 만든 데이터의 일부를 출력해서
# 두 변수로만 구성되어 있는지 확인하세요.
# Q2. 자동차 종류에 따라 도시 연비가 다른지 알아보려고 합니다. 앞에서 추출한
# 데이터를 이용해서 class(자동차 종류)가 다른 "suv"인 자동차와 "compact"인
# 자동차 중 어떤 자동차의 cty(도시 연비)가 더 높은지 알아보세요.
# Q1
mpg <- as.data.frame(ggplot2::mpg)
mpg_data <- mpg %>% select(class,cty)
mpg_data
# Q2
mpg_suv <- mpg_data %>% filter(class == "suv")
mpg_compact <- mpg_data %>% filter(class == "compact")
mean(mpg_suv$cty)
mean(mpg_compact$cty)
#######################
# 데이터 정렬하기
# 오름차순으로 정렬하기
exam %>% arrange(math) # math 오름차순 정렬
# 내림차순으로 정렬하기 desc() 함수 사용
exam %>% arrange(desc(math)) # math 내림차순 정렬
# 정렬 기준 변수 여러개 지정
# class 기준으로 정렬하고 반에서 math로 오름차순 정렬
exam %>% arrange(class, math)
# class 기준으로 정렬하고 반에서 math로 내림차순 정렬
exam %>% arrange(class, desc(math))
#######################
# 문제
# mpg 데이터를 이용해서 분석 문제를 해결해보세요.
# "audi"에서 생산한 자동차 중에 어떤 자동차 모델의 hwy(고속도로 연비)가
# 높은지 알아보려고 합니다. "audi"에서 생산한 자동차 중 hwy가 1~5위에 해당하는
# 자동차의 데이터를 출력하세요.
mpg <- as.data.frame(ggplot2::mpg)
head(mpg,2)
mpg %>%
  filter(manufacturer == "audi") %>%
  arrange(desc(hwy)) %>%
  head(5)
#######################
# 파생변수 추가하기
exam <- read.csv("csv_exam.csv")
# mutate()는 내장함수 추가하는 거랑 다르다
# 데이터 프레임 $가 없다
# 내장함수는 데이터프레임$변수명을 적지만
# mutate()는 훨씬 코드가 간결해진다
library(dplyr) # %>% 함수를 포함
exam %>%
  mutate(total = math + english + science) %>% # 총합 변수 추가
  head # 일부 추출
exam %>%
  mutate(total = math + english + science, # 총합 변수 추가
         mean = (math + english + science) / 3) %>% # 총평균 변수 추가
  head # 일부 추출
# 추가한 변수를 dplyr 코드에 바로 활용하기
exam %>%
  mutate(total = math + english + science) %>%
  arrange(total) %>% # 총합 변수 기준 정렬
  head # 일부 추출
###############################
# 문제
# mpg 데이터를 이용해서 분석 문제를 해결해보세요
# mpg 데이터는 연비를 나타내는 변수가 hwy(고속도로 연비), cty(도시 연비)
# 두 종류로 분리되어 있습니다 두 변수를 각각 활용하는 대신 하나의 통합 연비
# 변수를 만들어 분석하려고 합니다
# Q1) mpg 데이터 복사본을 만들고 cty와 hwy를 더한 '합산 연비 변수'를 추가하세요
# Q2) 앞에서 만든 '합산 연비 변수'를 2로 나눠 '평균 연비 변수'를 추가하세요
# Q3) '평균 연비 변수'가 가장 높은 자동차 3 종의 데이터를 출력하세요
# Q4) 1~3번 문제를 해결할 수 있는 하나로 연결된 dplyr 구문을 만들어 출력하세요
# 데이터는 복사본 대신 mpg 원본을 사용하세요
# Q1)
mpg <- as.data.frame(ggplot2::mpg)
mpg_copy <- mpg
mpg_copy <- mpg_copy %>%
  mutate(total = cty + hwy)
head(mpg_copy,2)
# Q2)
mpg_copy <- mpg_copy %>%
  mutate(evg = total / 2)
head(mpg_copy,2)
# Q3)
mpg_copy %>%
  arrange(desc(evg)) %>%
  head(3)
# Q4)
mpg %>%
  mutate(total = cty + hwy,
         evg = total / 2) %>%
  arrange(desc(evg)) %>%
  head(3)
# Q4는 분석이 끝나고 나서 또 활용할때는 다시해야되는 불편함이 있다
# 하지만 Q1~3은 데이터를 계속 활용할 꺼 같으면 쓰는게 좋고
# 한번만 쓰거나 별로 쓸일이 없을때는 그냥 Q4처럼 저장안하고 그때마다
# 사용하는 것도 좋을때도 있다 각기 장단점이 있다
##########################
# 집단 별로 데이터 요약하기

# 요약하기
exam %>% summarise(mean_math = mean(math)) # math 평균 산출출
exam %>%
  group_by(class) %>% # class 별로 분리
  summarise(mean_math = mean(math)) # math 평균 산출
# 여러 요약통계량 한번에 산출하기
exam %>%
  group_by(class) %>% # class 별로 분리
  summarise(mean_math = mean(math), # math 평균
            sum_math = sum(math), # math 합계
            median_math = median(math), # math 중간 값
            n = n()) # 학생 수
# 자주 사용하는 요약 통계량 함수
# mean() 평균
# sd() 표준편차
# sum() 합계
# median() 중앙값
# min() 최솟값
# max() 최댓값
# n() 빈도
###########################
#각 집단별로 다시 집단 나누기
mpg <- as.data.frame(ggplot2::mpg)
mpg %>%
  group_by(manufacturer, drv) %>% # 회사별로 구별한뒤 거기서 구동방식별 분리
  summarise(mean_cty = mean(cty)) %>% # cty 평균 산출
  head(10) # 일부 출력
###########################
# 문제
# mpg 데이터를 이용해서 분석 문제를 해결해보세요
# Q1. mpg 데이터의 class는 "suv", "compact" 등 자동차를 특징에 따라 일곱 종류로
# 분류한 변수입니다 어떤 차종의 연비가 높은지 비교해보려고 합니다 class별
# cty 평균을 구해보세요
# Q2. 앞 문제의 출력 결과는 class 값 알파벳 순으로 정렬되어 있습니다 어떤
# 차종의 도시 연비가 높은지 쉽게 알아볼 수 있도록 cty 평균이 높은 순으로
# 정렬해 출력하세요
# Q3. 어떤 회사 자동차의 hwy(고속도로 연비)가 가장 높은지 알아보려고 합니다
# hwy 평균이 가장 높은 회사 세 곳을 출력하세요
# Q4. 어떤 회사에서 "compact(경차)" 차종을 가장 많이 생산하는지 알아보려고
# 합니다 각 회사별 "compact" 차종 수를 내림차순으로 정렬해 출력하세요
# Q1)
mpg <- as.data.frame(ggplot2::mpg)
mpg %>%
  group_by(class) %>%
  summarise(evg = mean(cty))
# Q2)
mpg %>%
  group_by(class) %>%
  summarise(evg = mean(cty)) %>%
  arrange(desc(evg))
# Q3)
mpg %>%
  group_by(manufacturer) %>%
  summarise(evg = mean(hwy)) %>%
  arrange(desc(evg)) %>%
  head(3)
# Q4)
mpg %>%
  filter(class == "compact") %>%
  group_by(manufacturer) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
###########################
# 데이터 합치기

# 가로로 합치기
# 데이터 생성
# 중간고사 데이터 생성
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,80,70,90,85))
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70,83,65,95,100))
test1
test2
total <- left_join(test1,test2,by="id") # id 기준으로 합쳐 total에 할당당
total # total 출력
# 다른 데이터 활용해 변수 추가하기
# 반 별 담임교사 명단 생성
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim","lee","park","choi","jung"))
exam <- read.csv("csv_exam.csv")
school <- left_join(exam,name,by="class")
school

#세로로 합치기
# 데이터 생성
group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60,80,70,90,85))
group_b <- data.frame(id = c(6,7,8,9,10),
                      test = c(70,83,65,95,80))
group_all <- bind_rows(group_a,group_b) # 데이터 합쳐서 group_all에 할당
group_all
##########################
# 문제
# mpg 데이터를 이용해서 분석 문제를 해결해 보세요
# mpg 데이터의 f1 변수는 자동차에 사용하는 연료(fuel)를 의미합니다
# 아래는 자동차 연료별 가격을 나타낸 표 입니다
# fl    연료종류    가격(갤런당 USD)
# c     CNG           2.35
# d     diesel        2.38
# e     ethanol E85   2.11
# p     premium       2.76
# r     regular       2.22
# 우선 이 정보를 이용해서 연료와 가격으로 구성된 데이터 프레임을 만들어 보세요
fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringAsFactors = F)
fuel # 출력
#   fl price_fl stringAsFactors
# 1  c     2.35           FALSE
# 2  d     2.38           FALSE
# 3  e     2.11           FALSE
# 4  p     2.76           FALSE
# 5  r     2.22           FALSE
# Q1. mpg 데이터에는 연료 종류를 나타낸 fl 변수는 있지만 연료 가격을 나타낸
# 변수는 없습니다. 위에서 만든 fuel 데이터를 이용해 mpg 데이터에 price_fl
# (연료 가격) 변수를 추가하세요
# Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 model, fl, price_fl
# 변수를 추출해 앞부분 5행을 출력해 보세요
######################################
# Q1)
mpg <- as.data.frame(ggplot2::mpg)
fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl = c(2.35,2.38,2.11,2.76,2.22),
                   stringAsFactors = F)
fuel
mpg <- left_join(mpg,fuel,by="fl")
# Q2)
mpg %>%
  select(model,fl,price_fl) %>%
  head(5)
######################################
# 분석 도전 문제)
# 미국 동북중부 437개 지역의 인구통계 정보를 담고 있는 midwest 데이터를 사용해
# 데이터 분석 문제를 해결해 보세요 midwest는 ggplot2 패키지에 들어 있습니다
# Q1) popadults 는 해당 지역의 성인 인구, poptotal은 전체 인구를 나타냅니다
# midwest 데이터에 '전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요
# Q2) 미성년 인구 백분율이 가장 높은 상위 5개 county(지역)의 미성년 인구
# 백분율을 출력하세요
# Q3) 분류표의 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의
# 지역이 있는지 알아보세요
# 분류    기준
# large   40% 이상
# middle  30%~40% 미만
# small   30% 미만
# Q4) popasian은 해당 지역의 아시아인 인구를 나타냅니다 '전체 인구 대비 아시아인
# 인구 백분율' 변수를 추가하고 하위 10개 지역의 state(주), county(지역명),
# 아시아인 인구 백분율을 출력하세요
# Q1)
midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>%
  mutate(popkids = (poptotal - popadults) / poptotal * 100)
# Q2)
midwest %>%
  select(county,popkids) %>%
  arrange(desc(popkids)) %>%
  head(5)
# Q3)
midwest <- midwest %>%
  mutate(kids = ifelse(popkids >= 40, "large", ifelse(popkids >= 30, "middle", "small")))
table(midwest$kids)
# Q4)
midwest %>%
  mutate(asain = popasian / poptotal * 100) %>%
  arrange(asain) %>%
  select(state,county,asain) %>%
  head(10)
