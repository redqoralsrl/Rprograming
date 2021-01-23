# Chapter09~11
# 변수 간 관계를 표현하는 산점도
# 1단계 배경설정(축)
# 2단계 그래프 추가(점,막대,선)
# 3단계 설정 추가(축 범위, 색, 표식)

# 산점도 만들기
# 산점도 : 데이터를 x축과 y축에 점으로 표현한 그래프
# 나이와 소득처럼, 연속 값으로 된 두 변수의 관계를 표현할 때 사용
install.packages("ggplot2")
# 1.배경 설정하기
library(ggplot2)
# x축 displ, y축 hwy로 지정해 배경생성
ggplot(data = mpg, aes(x = displ, y = hwy))
# 2.그래프 추가하기
# 배경에 산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
# 3.축 범위를 조정하는 설정 추가하기
# x축 범위 3~6으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3,6)
# y축 범위 10~30으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3,6) +
  ylim(10,30)
# qplot() : 전처리 단계 데이터 확인용 문법 간단, 기능 단순
# ggplot() : 최종 보고용, 색, 크기, 폰트 등 세부 조작 가능
##################
# 문제
# mpg데이터와 midwest 데이터를 이용해서 분석 문제를 해결해 보세요
# Q1. mpg데이터의 cty(도시연비)와 hwy(고속도로 연비)간에 어떤 관계가 있는지
# 알아보려고 합니다 x축은 cty, y축은 hwy로 된 산점도를 만들어 보세요
# Q2. 미국 지역별 인구통계 정보를 담은 ggplot2 패키지의 midwest 데이터를
# 이용해서 전체 인구와 아시아인 인구간에 어떤 관계가 있는지 알아보려고 합니다
# x축은 poptotal(전체 인구), y축은 popasian(아시아인 인구)으로 된 산점도를
# 만들어 보세요 전체 인구는 50만명 이하, 아시아인 인구는 1만명 이하인 지역만
# 산점도에 표시되게 설정하세요
# Q1)
library(ggplot2)
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()
# Q2)
ggplot(data = midwest, aes(x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0,500000) +
  ylim(0,10000)
########################
# 막대 그래프 - 집단 간 차이 표현하기
# 막대 그래프(Bar Chat) : 데이터의 크기를 막대의 길이로 표현한 그래프
# 성별 소득 차이처럼 집단 간 차이를 표현할 때 주로 사용

# 평균 막대 그래프 만들기
# 1.집단별 평균표 만들기
library(dplyr)
library(ggplot2)
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))
df_mpg
# 2.그래프 생성하기
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()
# 알파벳 순으로 나온다
# 3.크기 순으로 정렬하기
# mean_hwy 순으로 정렬하는데 -가 있으니 내림차순으로 정렬하라
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) +
  geom_col()
# 4.빈도 막대 그래프
# 값의 개수(빈도)로 막대의 길이를 표현한 그래프
# x축 범주 변수, y축 빈도
ggplot(data = mpg, aes(x = drv)) + geom_bar()
# x축 연속 변수, y축 빈도
ggplot(data = mpg, aes(x = hwy)) + geom_bar()
#######################
# 문제
# mpg 데이터를 이용해서 분석 문제를 해결해 보세요
# Q1. 어떤 회사에서 생산한 "suv" 차종의 도시 연비가 높은지 알아보려고 합니다
# "suv" 차종을 대상으로 평균 cty(도시연비)가 가장 높은 회사 다섯 곳을 막대
# 그래프로 표현해 보세요 막대는 연비가 높은 순으로 정렬하세요
# Q2. 자동차 중에서 어떤 class(자동차 종류)가 가장 많은지 알아보려고 합니다
# 자동차 종류별 빈도를 표현한 막대 그래프를 만들어 보세요
# Q1)
library(dplyr)
library(ggplot2)
mpg_data <- mpg %>%
  filter(class == "suv") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)
ggplot(data=mpg_data, aes(x = reorder(manufacturer,-mean_cty), y = mean_cty)) +
  geom_col()
#Q2)
ggplot(data=mpg, aes(x = class)) + geom_bar()
################
# 시간에 따라 달라지는 데이터를 표현하는 선 그래프
# 선 그래프(Line Chart) : 데이터를 선으로 표현한 그래프
# 시계열 그래프(Time Series Chat) : 일정 시간 간격을 두고 나열된 시계열
# 데이터(Time Series Chat)를 선으로 표현한 그래프, 환율, 주가지수 등
# 경제 지표가 시간에 따라 어떻게 변하는지 표현할 때 활용
library(dplyr)
library(ggplot2)
# 시계열 그래프 만들기
ggplot(data = economics, aes(x = date, y =unemploy)) + geom_line()
##########
# 문제
# economics 데이터를 이용해서 분석 문제를 해결해 보세요
# Q1) psavert(개인 저축률)가 시간에 따라서 어떻게 변해왔는지 알아보려고 합니다
# 시간에 따른 개인 저축률의 변화를 나타낸 시계열 그래프를 만들어 보세요
# Q1)
head(economics)
ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()
##########
# 집단 간 분포 차이를 표현하는 상자 그림
# 상자 그림(Box Plot) : 데이터의 분포(퍼져 있는 형태)를 직사각형 상자 모양으로
# 표현한 그래프
# 분포를 알 수 있기 때문에 평균만 볼 때보다 데이터의 특성을 좀 더 자세히 이해
# 할 수가 있다
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()
# 상자를 구성하고 있는 선분(줄)들은 요약통계량을 이용해서 상자그림을 그리게
# 된다
# 가로 선분은 중앙값이다 상자의 밑면은 1분위 수이다(하위 25%)
# 상자의 윗면는 3분위 수(상위 25%)이다 상자 밖에 위 아래로 선은 밑은 최솟값
# 위는 최댓값으로 보면 된다
# 하지만 점들이 있는데 극단적으로 크거나 작은게 있으면 표현한다
# 1분위에서 3분위 사이의 거리를 구한다음(rqr)에 곱하기 1.5를 해서 
# 1.5를 곱한 1.5rpr을 구한 다음에 밑면은 1분위수 - 1.5rqr 밖에 있으면
# 점을 찍는다 윗면은 3분위 수 + 1.5rqr 밖에 있으면 점을 찍는다
# 상자 높이에 평균이 높은지 낮은지 알 수 있다
# 상자 높낮이는 클수록 다양성이 크다라고 볼 수 있다
# 상자가 작으면 비슷한 성질의 차들을 생성한다고 볼 수 있다
# 상자를 보면은 오른쪽으로 뒤집으면
# 상자가 가운데에 있다고 생각하면 정상분포가 있는 형태로 볼 수 있고
# 아래쪽은 왼쪽에 치우쳐진 그래프가 만들어진다
# 윗쪽은 오른쪽에 치우쳐진 그래프가 만들어진다
##############
# 문제
# mpg 데이터를 이용해서 분석 문제를 해결해보세요
# Q1. class(자동차 종류)가 "compact", "subcompact", "suv"인 자동차의 cty(도시
# 연비)가 어떻게 다른지 비교해보려고 합니다 세 차종의 cty를 나타낸 상자
# 그림을 만들어보세요
# Q1)
mpg <- as.data.frame(ggplot2::mpg)
mpg1 <- mpg %>%
  filter(class %in% c("compact","subcompact","suv"))
ggplot(data = mpg1, aes(x = class, y = cty)) + geom_boxplot()
############
# 결측치 정제하기

# 실무에서는 데이터가 100% 완전하지 못하다
# 그래서 빠진 데이터나 이상한 수치의 데이터는 제거해야 한다

# 1. 빠진 데이터를 찾아라! - 결측치 정제하기
# 결측치(Missing Value)
# - 누락된 값, 비어있는 값
# - 함수 적용 불가, 분석 결과 왜곡
# - 제거 후 분석 실시

# 결측치 찾기

# 결측치 만들기
# 결측지 표기 - 대문자 NA
df <- data.frame(sex = c("M","F",NA,"M","F"),
                 score = c(5,4,3,4,NA))
df
# 결측치 확인하기
is.na(df)
# 결측치 빈도 출력
table(is.na(df))
# 변수별로 결측치 확인하기
table(is.na(df$sex)) # 성별 결측치 빈도 출력
table(is.na(df$score)) # 점수 결측치 빈도 출력
# 결측치 포함된 상태로 분석
mean(df$score)
sum(df$score)
# 결측치 제거하기
# 결측치 있는 행 제거하기
library(dplyr)
df %>%
  filter(is.na(score)) # score가 NA인 데이터만 출력
df %>%
  filter(!is.na(score)) # score 결측치 제거만 출력
# 하지만 데이터는 지워지지 않고 보여주기만 하는 것이다
# 제거하려면 새로 넣어야 한다

# 결측치 제외한 데이터로 분석하기
df_nomiss <- df %>% filter(!is.na(score)) # score 결측치 제거거
mean(df_nomiss$score) # 잘되는 것을 볼 수 있다다
sum(df_nomiss$score)


# 여러 변수 동시에 결측치 없는 데이터 추출하기
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss
# 하지만 변수가 많으면 위와 같이 적으면 불편하다
# 결측치가 하나라도 있으면 제거하는 함수
df_nomiss2 <- df %>% na.omit(df) # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2
# 하지만 분석에 필요한 데이터까지 손실 될 가능성이 있어서
# 사용을 안함

# 함수의 결측치 제외 기능 이용하기
mean(df$score, na.rm = T) # 결측치 제외하고 평균 산출
sum(df$score, na.rm = T) # 결측치 제외하고 합계 산출
# summarise()에서 na.rm = T 사용하기
# 결측치 생성
exam <- read.csv("csv_exam.csv") # 데이터 불러오기
exam[c(3,8,15),"math"] <- NA # 3, 8, 15행의 math에 NA 할당
# 평균 구하기
exam %>%
  summarise(mean_math = mean(math)) # 그냥하면 NA때문에 NA가 나온다
exam %>%
  summarise(mean_math = mean(math, na.rm = T)) # 결측치 제외하고 평균 산출
# 결측치 대체하기(결측치 대체법)
# - 결측치 많을 경우 모두 제외하면 데이터 손실 큼
# - 대안 : 다른 값 채워넣기
# 결측치 대체법(Imputation)
# - 대표값(평균, 최빈값 등)으로 일괄 대체
# - 통계분석 기법 적용, 예측값 추정해서 대체

# 평균 값으로 결측치 대체하기
# 평균 구하기
mean(exam$math, na.rm = T) # 결측치 제외하고 math 평균 산출
# 평균으로 대체하기
exam$math <- ifelse(is.na(exam$math), 55, exam$math) # math가 NA면 55로 대체
table(is.na(exam$math))
exam
mean(exam$math)
##########
#문제
# mpg 데이터를 이용해서 분석문제를 해결해 보세요
# mpg 데이터 원본에는 결측치가 없습니다 우선 mpg 데이터를 불러와 몇개의 값을
# 결측치로 만들겠습니다 아래 코드를 실행하면 다섯 행의 hwy 변수에 NA가 할당
# 됩니다
# mpg <- as.data.frame(ggplot2::mpg)
# mpg[c(65,124,131,153,212),"hwy"] <- NA
# 결측치가 들어있는 mpg 데이터를 활용해서 문제를 해결해보세요
# Q1.drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고
# 합니다 분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 합니다
# drv변수와 hwy변수에 결측치가 몇 개 있는지 알아보세요
# Q2.filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy
# 평균이 높은지 알아보세요 하나의 dplyr구문으로 만들어야 합니다
library(dplyr)
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65,124,131,153,212),"hwy"] <- NA
# Q1)
table(is.na(mpg$drv))
table(is.na(mpg$hwy))
# Q2)
mpg %>%
  filter(!is.na(hwy)) %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))
###########
library(dplyr)
# 이상치 정제하기
# - 정상 범주에서 크게 벗어난 값
# - 이상치 포함시 분석 결과 왜곡
# - 결측 처리 후 제외하고 분석
# - 논리적으로 존재할 수 없는 값, 논리적으로 존재하나 극단적으로 크거나 작음

# 논리적으로 존재 할 수 없는 결과를 결측 처리 후 분석시 제외
# 이상치 포함된 데이터 - sex 3, score 6
outlier <- data.frame(sex = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,6))
outlier
# 이상치 확인하기
table(outlier$sex)
table(outlier$score)
# 결측 처리하기
# sex가 3이면 NA 할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
# score가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier
# 결측지 제외하고 분석
outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))
###########
# 극단적인 값 제거하기

# 상자그림으로 극단치 기준 정해서 제거하기
# 상자그림 생성
mpg <- as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy)
# 상자그림 통계치 출력
boxplot(mpg$hwy)$stats # 상자그림 통계치 출력력
# [1,]   12 - 밑에 선분
# [2,]   18 - 상자 밑면
# [3,]   24 - 상자 중앙 선분
# [4,]   27 - 상자 윗면
# [5,]   37 - 위에 선분

# 결측 처리하기
# 12~37 벗어나면 NA할당
mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy >37, NA, mpg$hwy)
table(is.na(mpg$hwy))
# 결측치 제외하고 분석하기
mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy,na.rm = T))
#########
# 문제
# mpg 데이터를 이용해서 분석 문제를 해결해 보세요
# 우선 mpg 데이터를 불러와서 일부러 이상치를 만들겠습니다 drv(구동방식) 변수의
# 값은 4(사륜구동), f(전륜구동), r(후륜구동) 세 종류로 되어있습니다 몇 개의
# 행에 존재할 수 없는 값 k를 할당하겠습니다 cty(도시연비) 변수도 몇 개의 행에
# 극단적으로 크거나 작은 값을 할당하겠습니다
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(10,14,58,93),"drv"] <- "k"
mpg[c(29,43,129,203),"cty"] <- c(3,4,39,42)
# 이상치가 들어있는 mpg 데이터를 활용해서 문제를 해결해보세요
# 구동방식별로 도시 연비가 다른지 알아보려고 합니다 분석을 하기 전에 우선
# 두 변수에 이상치가 있는지 확인하려고 합니다
# Q1) drv에 이상치가 있는지 확인하세요 이상치를 결측 처리한 다음 이상치가
# 사라졌는지 확인하세요 결측처리 할 때는 %in%기호를 활용하세요
# Q2) 상자 그림을 이용해서 cty에 이상치가 있는지 확인하세요 상자 그림의
# 통계치를 이용해 정상 범위를 벗어난 값을 결측 처리한 후 다시 상자 그림을
# 만들어 이상치가 사라졌는지 확인하세요
# Q3)두 변수의 이상치를 결측처리 했으니 이제 분석할 차례입니다 이상치를 
# 제외한 다음 drv별로 cty평균이 어떻게 다른지 알아보세요 하나의 구문으로 만드세요
# Q1)
table(mpg$drv)
mpg$drv <- ifelse(mpg$drv %in% c("4","f","r"), mpg$drv, NA)
table(mpg$drv)
# Q2)
boxplot(mpg$cty)
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)
# Q3)
mpg %>%
  filter(!is.na(drv) & !is.na(cty)) %>%
  group_by(drv) %>%
  summarise(mean_cty = mean(cty))

##########
# <프로젝트>
# 한국복지패널 데이터를 활용한 한국인의 삶 분석
# 1. 성별에 따른 소득
# 2. 나이와  소득의 관계
# 3. 연령대에 따른 소득
# 4. 연령대 및 성별에 따른 소득

# 준비하기
# foreign 패키지 설치
install.packages("foreign") # spss 파일을 불러오는 패키지가 있다
# 패키지 
library(foreign)
library(dplyr)
library(ggplot2)
# 데이터 불러오기
raw_welfare <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
# 데이터 copy
welfare <- raw_welfare

# 데이터 검토
dim(welfare)
str(welfare)
head(welfare)
summary(welfare)
View(welfare)
# 데이터가 크기에 확인이 되지 않는다

# 변수명
welfare <- rename(welfare,
                  sex = h0901_4, # 성별
                  birth = h0901_5, # 태어난 년도
                  income = h09_din) # 소득득

# 분석 1 : 성별에 따른 소득
# 절차
# 1. 변수 검토 및 정제 - 성별
# - 1-1 변수 검토, 수정
# - 1-2 정제 - 이상치 확인 및 결측 처리
# 2. 변수 검토 및 정제 - 소득
# - 2-1 변수 검토, 수정
# - 2-2 정제 - 이상치 확인 및 결측 처리
# 3. 성별 소득 평균 분석
# - 성별 소득 평균표 생성
# - 그래프 생성

# 1. 변수 검토 및 정제 - 성별
# 1-1. 변수 검토, 수정
class(welfare$sex) # 데이터의 속성을 알려준다 numeric은 숫자 그러니 문자로 바꿔야 한다
summary(welfare$sex)
# 코딩북을 봐야된다 성별 변수는 1은 남자 2 는 여 모르면 9로 응답함이라고 나온다
table(welfare$sex)
# 9라고 응답한 사람은 없으니 오류가 없다
# 만약 이상치가 있다면 아래와 같이 하면 된다
# table(welfare$sex) # 만약 이상치가 9가 나오면
# welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) # NA값으로 변경
# table(is.na(welfare$sex)) # 잘 바꼈나 확인
# 변수 값 변경
welfare$sex <- ifelse(welfare$sex == 1, "male","female")
table(welfare$sex)
qplot(welfare$sex)
# 2. 변수 검토 및 정제 - 소득
# 2-1. 변수 검토, 수정
class(welfare$income)
summary(welfare$income)
# 코딩북을 보면서 확인해야 한다 무응답은 9999로 되어 있다
qplot(welfare$income)
qplot(welfare$income) + xlim(0,10000)
# 2-2. 이상치 확인 및 결측처리
table(is.na(welfare$income))
# 딱히 없어서 패스

# 3. 성별 소득 평균 분석
sex_income <- welfare %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))
sex_income
# 그래프 생성
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()
# 하지만 가구주의 소득 평균이다
# 한국이 가부장적 국가다보니 남자를 대답하게 된다
# 가구주가 여성의 데이터는 1인가구나 홀부모가족이라 소득이 낮을
# 가능성이 있다 한번 검토해보자

# 분석 2 : 나이와 소득의 관계
# 절차
# 1. 변수 검토 및 정제 - 나이
# - 1-1. 태어난 연도 변수 검토
# - 1-2. 정제 - 이상치 확인 및 결측처리
# - 1-3. 나이 변수 생성
# 2. 변수 검토 및 정제 - 소득
# - 앞에서 이미 완료됨
# 3. 나이별 소득 평균 분석
# - 나이별 소득 평균표 생성
# - 그래프 생성

# 1. 변수 검토 및 정제 - 나이
# 1-1 태어난 연도 변수 검토
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth) # 노령화 데이터가 지나치게 높다 조사할때 노년층을 주로 했다
# 1-2 정제 - 이상치 확인 및 결측 처리
# 나이는 무응답이 9999로 되어있다
# 만약 있다면 아래와 같이 입력하면 된다
# welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth)
# table(is.na(welfare$birth))
# 1-3 나이 변수 생성
welfare$age <- 2014 - welfare$birth + 1 # 현재나이로 만들어주자
summary(welfare$birth)
qplot(welfare$birth)
# 2. 변수 검토 및 정제 - 소득
# 앞에서 완료함
# 3. 나이별 소득 평균 분석
age_income <- welfare %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
age_income
# 그래프 생성
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_point()
# 하지만 위는 문제가 있다
# 만약에 제일 작은 값인 13살이 한명이면 걔 한명 가지고 데이터가 된다 그러면
# 다른 나이에 여러명이면 그 나이에 대표성이 없다
# 대표성을 확보해야한다
# 연령별로 구별지으면 된다
# 청년, 성년 등으로 구별 지어서 하면 오히려 괜찮은 데이터가 된다

# 분석 3. 연령대에 따른 소득
# 절차
# 1. 변수 검토 및 정제 - 연령대
# - 1-1. 연령대 변수 생성
# 2. 변수 검토 및 정제 - 소득 
# - 2-2. 앞에서 완료됨
# 3. 연령대별 소득 평균 분석
# - 연령대별 소득 평균표 생성
# - 그래프 생성

# 1. 변수 검토 및 정제 - 연령대
# - 1-1 연령대 변수 생성
# 범주     기준
# 초년    30세 미만
# 중년    30~59세
# 노년    60세 이상

welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young", ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)
# 위에 그림은 보면 young은 빼고 분석하는게 좋을 것 같다

# 2. 변수 검토 및 정제 - 소득
# 앞에서 완료

# 3. 연령대별 소득 평균 분석
# 초년 빈도 적으므로 제외
welfare_income <- welfare %>%
  filter(ageg != "young") %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
welfare_income
# 그래프 만들기
ggplot(data = welfare_income, aes(x = ageg, y = mean_income)) + geom_col()

# 분석 4 : 연령대 및 성별에 따른 소득
# 1. 연령대 및 성별 소득 평균표 생성
# 2. 그래프 만들기
# 초년은 제외
sex_income <- welfare %>%
  filter(ageg != "young") %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col()
# fill = 변수 # 변수 별로 칠해달라는 함수
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") # position 변경(기본값 = "stack")
############
