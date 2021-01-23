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
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3,6)
# y축 범위 10~30으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3,6) +
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
ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()
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
library(ggplot2)
mpg_re <- mpg %>%
  filter(class == "suv") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)

ggplot(data = mpg_re, aes(x = reorder(manufacturer, -mean_cty),
                          y = mean_cty )) + geom_col()
# Q2)
ggplot(data = mpg, aes(x = class)) + geom_bar()






