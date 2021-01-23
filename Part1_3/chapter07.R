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
# 수학 점수가 50 점 초과한 경우
exam %>% filter(math > 50)
# 수학 점수가 50점 미만인 경우
exam %>% filter(math < 50)
# 영어 점수가 80점 이상인 경우
exam %>% filter(english >= 80)
# 영어 점수가 80점 이하인 경우
exam %>% filter(english <= 80)
# 여러 조건을 충족하는 행 추출하기
# 1반이면서 수학 점수가 50점 이상인 경우
exam %>% filter(class == 1 & math >= 50)
# 2반이면서 영어점수가 80점 이상인 경우
exam %>% filter(class == 2 & english >= 80)
# 수학 점수가 90점 이상이거나 영어 점수가 90점 이상인 경우
exam %>% filter(math >= 90 | english >= 90)
# 영어 점수가 90점 미만이거나 과학 점수가 50점 미만인 경우
exam %>% filter(english < 90 | science < 50)
# 목록에 해당되는 행 추출하기
# 1, 3, 5 반에 해당되면 추출
exam %>% filter(class == 1 | class == 3 | class == 5)
# %in% 기호 이용하기 매치 오퍼레이터라고 한다
# 여러개의 조건을 적을 수 있다
exam %>% filter(class %in% c(1,3,5))
# 추출한 행으로 데이터 만들기
# class 가 1인 데이터를 class1에 저장하고  class1의 수학 점수 평균을 구하라
class1 <- exam %>% filter(class == 1)
class1
mean(class1$math)
# class 가 2인 데이터를 class2에 저장하고 class2의 수학 점수 평균을 구하라
class2 <- exam %>% filter(class == 2)
class2
mean(class2$math)
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
# 산술 연산자
# +     더하기
# -     빼기
# *     곱하기
# /     나누기
# ^, ** 제곱
# %/%   나눗셈의 몫
# %%    나눗셈의 나머지

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
#Q1)
mpg <- as.data.frame(ggplot2::mpg)
head(mpg,2)
mpg_4 <- mpg %>% filter(displ <= 4)
mpg_5 <- mpg %>% filter(displ >= 5)
mean(mpg_4$hwy)
mean(mpg_5$hwy)
#Q2)
mpg_audi <- mpg %>% filter(manufacturer == "audi")
mpg_toyota <- mpg %>% filter(manufacturer == "toyota")
mean(mpg_audi$cty)
mean(mpg_toyota$cty)
#Q3)
mpg_hwy <- mpg %>% filter(manufacturer %in% c("chevrolet","ford","honda"))
mean(mpg_hwy$hwy)

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
# Q1)
mpg <- as.data.frame(ggplot2::mpg)
head(mpg,2)
mpg_new <- mpg
head(mpg_new,2)
mpg_data <- mpg %>% select(class,cty)
mpg_data
# Q2)
mpg_suv <- mpg_data %>% filter(class == "suv")
mpg_compact <- mpg_data %>% filter(class == "compact")
mean(mpg_suv$cty)
mean(mpg_compact$cty)

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













