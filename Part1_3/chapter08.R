# 파생변수 추가하기
exam <- read.csv("csv_exam.csv")
# mutate()는 내장함수 추가하는 거랑 다르다
# 데이터 프레임 $가 없다
# 내장함수는 데이터프레임$변수명을 적지만
# mutate()는 훨씬 코드가 간결해진다
exam %>%
  mutate(total = math + english + science) %>% # 총합 변수 추가
  head # 일부 추출
exam %>%
  mutate(total = math + english + science, # 총합 변수 추가
         mean = (math + english + science) / 3) %>% # 총평균 변수 추가
  head # 일부 추출
# mutate()에 ifelse() 적용하기
exam %>%
  mutate(test = ifelse(science >= 60, "pass", "fail")) %>%
  head
# 추가한 변수를 dplyr 코드에 바로 활용하기
exam %>%
  mutate(total = math + english + science) %>% # 총합 변수 추가
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
mpg_copy <- mpg_copy %>% mutate(sum_ctyhwy = cty + hwy)
# Q2)
mpg_copy <- mpg_copy %>% mutate(evg_ctyhwy = sum_ctyhwy / 2)
# Q3)
mpg_copy %>% arrange(desc(evg_ctyhwy)) %>% head(3)
# Q4)
mpg %>%
  mutate(sum_ctyhwy = cty + hwy,
         evg_ctyhwy = sum_ctyhwy / 2) %>%
  arrange(desc(evg_ctyhwy)) %>%
  head(3)
# Q4는 분석이 끝나고 나서 또 활용할때는 다시해야되는 불편함이 있다
# 하지만 Q1~3은 데이터를 계속 활용할 꺼 같으면 쓰는게 좋고
# 한번만 쓰거나 별로 쓸일이 없을때는 그냥 Q4처럼 저장안하고 그때마다
# 사용하는 것도 좋을때도 있다 각기 장단점이 있다

# 집단 별로 데이터 요약하기

# 요약하기
exam %>% summarise(mean_math = mean(math)) # math 평균 산출
exam %>%
  group_by(class) %>% # class 별로 분리
  summarise(mean_math = mean(math)) # math 평균 산출
# 여러 요약통계량 한 번에 산출하기
exam %>%
  group_by(class) %>% # class 별로 분리
  summarise(mean_math = mean(math), # math 평균
            sum_math = sum(math), # math 합계
            median_math = median(math), # math 중앙 값
            n = n()) # 학생 수
# 자주 사용하는 요약 통계량 함수
# mean() 평균
# sd() 표준편차
# sum() 합계
# median() 중앙값
# min() 최솟값
# max() 최댓값
# n() 빈도

# 각 집단별로 다시 집단 나누기
mpg <- as.data.frame(ggplot2::mpg)
mpg %>%
  group_by(manufacturer, drv) %>% # 회사별, 구별 방식별 분리
  summarise(mean_cty = mean(cty)) %>% # cty 평균 산출
  head(10) # 일부 출력
##################
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
  summarise(evg_cty = mean(cty)) %>%
  head(7)
# Q2)
mpg %>%
  group_by(class) %>%
  summarise(evg_cty = mean(cty)) %>%
  arrange(desc(evg_cty)) %>%
  head(7)
# Q3)
mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
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
# 기말고사 데이터 생성
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(70,83,65,95,100))
test1
test2
total <- left_join(test1,test2,by = "id") # id 기준으로 합쳐 total에 할당당
total # total 출력
# 다른 데이터 활용해 변수 추가하기
# 반 별 담임교사 명단 생성
name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim","lee","park","choi","jung"))
name
exam <- read.csv("csv_exam.csv")
school <- left_join(exam,name,by="class")
school

#세로로 합치기
# 데이터 생성
group_a <- data.frame(id = c(1,2,3,4,5),
                      test = c(60,80,70,90,85))
group_b <- data.frame(id = c(6,7,8,9,10),
                      test = c(70,83,65,95,80))
group_a
group_b
group_all <- bind_rows(group_a,group_b) # 데이터 합쳐서 group_all에 할당
group_all
################
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
# Q2. 연료 가격 변수가 잘 추가됐는지 확인하기 위해서 mode, fl, price_fl 변수를
# 추출해 앞부분 5행을 출력해 보세요
######################################
# Q1)
mpg <- as.data.frame(ggplot2::mpg)
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
# Q2) 미성년 인구 백분율이 가장 높은 상위 5개 country(지역)의 미성년 인구
# 백분율을 출력하세요
# Q3) 분류표의 기준에 따라 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의
# 지역이 있는지 알아보세요
# 분류    기준
# large   40% 이상
# middle  30%~40% 미만
# small   30% 미만
# Q4) popasian은 해당 지역의 아시아인 인구를 나타냅니다 '전체 인구 대비 아시아인
# 인구 백분율' 변수를 추가하고 하위 10개 지역의 state(주), country(지역명),
# 아시아인 인구 백분율을 출력하세요
# Q1)
midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>%
  mutate(popkids = (poptotal - popadults) / poptotal * 100)
# Q2)
midwest %>%
  select(county, popkids) %>%
  arrange(desc(popkids)) %>%
  head(5)
# Q3)
midwest$percent <- ifelse(midwest$popkids >= 40, "large", ifelse(midwest$popkids>= 30, "middle", "small"))
table(midwest$percent)
# Q4)
midwest %>%
  mutate(asain = popasian / poptotal * 100) %>%
  arrange(asain) %>%
  select(state,county,asain) %>%
  head(10)
