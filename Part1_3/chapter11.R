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
