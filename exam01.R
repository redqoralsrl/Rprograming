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
####################
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
###################
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
#####################
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

install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
raw_welfare <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
welfare <- raw_welfare
welfare <- rename(welfare,
                  sex = h0901_4, # 성별
                  birth = h0901_5, # 태어난 년도
                  income = h09_din) # 소득득
# 분석 1 : 성별에 따른 소득
class(welfare$sex)
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
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0,10000)
table(is.na(welfare$income))
# 3. 성별 소득 평균 분석
sex_income <- welfare %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()
# 분석 2 : 나이와 소득의 관계
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
welfare$age <- 2014 - welfare$birth + 1
summary(welfare$birth)
qplot(welfare$birth)
age_income <- welfare %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
age_income
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_point()
# 분석 3. 연령대에 따른 소득
# 범주     기준
# 초년    30세 미만
# 중년    30~59세
# 노년    60세 이상
welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young", ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)
welfare_income <- welfare %>%
  filter(ageg != "young") %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
welfare_income
ggplot(data = welfare_income, aes(x = ageg, y = mean_income)) + geom_col()
# 분석 4 : 연령대 및 성별에 따른 소득
sex_income <- welfare %>%
  filter(ageg != "young") %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col()
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge")
#######