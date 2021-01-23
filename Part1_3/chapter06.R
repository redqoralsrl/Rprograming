exam <- read.csv("csv_exam.csv")
head(exam) # 앞에서부터 6행까지 출력
head(exam, 10) # 앞에서부터 10행까지 출력
tail(exam) # 뒤에서부터 6행까지 출력
tail(exam, 10) # 뒤에서부터 10행까지 출력
View(exam) # 뷰여 창에서 데이터 확인하기 
dim(exam) # 몇 행 몇 열로 구성되어 있는지 보기
str(exam) # 데이터 속성 확인
summary(exam) # 요약통계량 출력
# mpg데이터 파악하기
# ggplot2의 mpg 데이터를 데이터 프레임 형태로 불러오기
# ggplot2패키지 안에서 mpg데이터만 쏙 뽑아올때
mpg <- as.data.frame(ggplot2::mpg)
# 패키지 안에 mpg데이터만 사용하겠다 (::)
mpg
head(mpg) # Raw 데이터 앞부분확인
tail(mpg) # Raw 데이터 뒷부분확인
View(mpg) # Raw 데이터 뷰어 창 확인
dim(mpg) # 행 열 출력
str(mpg) # 데이터 속성 확인
summary(mpg) # 요약통계량 출력

# 데이터 수정하기 - 변수명 바꾸기 
install.packages("dplyr") # 이미 해서 생략
library(dplyr)
# 데이터 프레임 생성
df_raw <- data.frame(var1 = c(1,2,1), var2 = c(2,3,2))
df_raw
# 데이터 수정 작업 시 복사본 즉, 백업본을 만든다
df_new <- df_raw # 데이터 프레임 복사본 만들기
df_new
# 변수명 바꾸기
# rename이라는 함수를 쓰는데 dplyr 안에 있는 것이다
df_new <- rename(df_new, v2 = var2) # var2를 v2로 수정정
df_new

# 문제
# mpg 데이터의 변수명은 긴 단어를 짧게 줄인 축약어로 되어 있습니다
# cty 변수는 도시 연비, hwy 변수는 고속도로 연비를 의미합니다
# 변수명을 이해하기 쉬운 단어로 바꾸려고 합니다
# mpg 데이터를 이용해서 아래 문제를 해결해 보세요.
# 1) ggplot2패키지의 mpg 데이터를 사용할 수 있도록 불러온 뒤 복사본을 만드세요
# 2) 복사본 데이터를 이용해서 cty는 city로 hwy는 highway로 변수명을 수정하세요
# 3) 데이터 일부를 출력해서 변수명이 바뀌었는지 확인하는데 위에서 6번째
# 줄만 나오게 출력하세요.
install.packages(dplyr)
library(dplyr)
# 1)
mpg_raw <- as.data.frame(ggplot2::mpg)
mpg_new <- mpg_raw
mpg_raw
mpg_new
# 2)
mpg_new <- rename(mpg_new, city = cty, highway = hwy)
# 3)
head(mpg_new)

# 파생 변수 만들기

# 데이터 프레임 생성
df <- data.frame(var1 = c(4,3,8), var2 = c(2,6,1))
df
# 파생변수 생성
df$var_sum <- df$var1 + df$var2 # var_sum 파생변수 생성
df
df$var_min <- (df$var1 + df$var2) / 2 # var_min 만들어보기
df
# mpg 통합 연비 변수 만들기
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
head(mpg,20)
# 빈도표로 합격 판정 자동차 수 살펴보기
table(mpg$test) # 연비 합격 빈도표 생성
#막대 그래프로 빈도 표현하기
install.packages(ggplot2) # 설치 되어 있으니 생략
library(ggplot2) # qplot을 쓰기 위해 적용하기
qplot(mpg$test) # 연비 합격 빈도 막대 그래프 생성
#중첩 조건문 활용하기 - 연비 등급 변수 만들기
mpg$grade <- ifelse(mpg$total >= 30, "A", ifelse(mpg$total>=20,"B","C"))
head(mpg,20)
table(mpg$grade)
qplot(mpg$grade)

# 정리하기
# 1. 데이터 준비, 패키지 준비
mpg <- as.data.frame(ggplot2::mpg) # 데이터 불러오기
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
install.packages(ggplot2)
library(dplyr)
library(ggplot2)
#1)
midwest <- as.data.frame(ggplot2::midwest)
head(midwest,5)
tail(midwest,5)
dim(midwest)
str(midwest)
View(midwest)
summary(midwest)
midwest_new <- midwest
head(midwest_new,3)
#2)
midwest_new <- rename(midwest_new,total=poptotal,asian=popasian)
head(midwest_new,3)
#3)
midwest_new$total_asian <- (midwest_new$asian / midwest_new$total) * 100
head(midwest_new$total_asian)
hist(midwest_new$total_asian)
#4)
mean(midwest_new$total_asian)
midwest_new$asian_evg <- ifelse(midwest_new$total_asian > 0.4872462, "large","small")
#5)
table(midwest_new$asian_evg)
qplot(midwest_new$asian_evg)