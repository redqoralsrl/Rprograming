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
library(ggplot2)
mpg_test <- mpg %>%
  filter(class %in% c("compact","subcompact","suv"))
ggplot(data = mpg_test, aes(x = class, y = cty)) + geom_boxplot()
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
# mpg <- as.data.fram(ggplot2::mpg)
# mpg[c(65,124,131,153,212),"hwy"] <- NA
# 결측치가 들어있는 mpg 데이터를 활용해서 문제를 해결해보세요
# Q1.drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고
# 합니다 분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 합니다
# drv변수와 hwy변수에 결측치가 몇 개 있는지 알아보세요
# Q2.filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy
# 평균이 높은지 알아보세요 하나의 dplyr구문으로 만들어야 합니다
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
  






