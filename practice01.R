# chapter01~04
install.packages(dplyr)
install.packages(ggplot2)
library(dplyr)
library(ggplot2)
####################################
head(mpg) # 자동차 데이터를 위에서부터 나열
dim(mpg) # 데이터 정보의 크기를 행과 열로 나열
str(mpg) # 데이터 속성이 무엇인지 알려줌
summary(mpg) # 속성과 데이터의 각각의 평균을 구하는데 숫자만 계산이 됨
View(mpg) # mpg 안에 데이터와 정보들을 나열(정보가 많으면 비추천)
####################################
#1)회사별 연비 높은 순 정렬을 해보자
mpg %>% # mpg 데이터
  group_by(manufacturer) %>% # 제조사별로 나눠라
  summarise(mean.hwy=mean(hwy)) %>% # 고속도로 연비의 계산을 요약해라
  arrange(desc(mean.hwy)) # 내림차순으로 연비를 나열하라
####################################
#2)특정 회사의 연비를 정렬해보자
mpg %>% # mpg 데이터
  filter(manufacturer=="ford") %>% # 제조사중에 ford로 걸러달라
  group_by(model) %>% # 모델별로 분류해라
  arrange(desc(hwy)) # 내림차순으로 연비를 나열하라
####################################
#3)배기량과 연비에 미치는 영향을 회귀분석해보자
lm.mpg <- lm(data=mpg, hwy ~ displ) # 회귀 분석 -> 계산을 해준다
# 회귀 분석한 결과를 lm.mpg의 변수에 담는다
summary(lm.mpg) # 계산을 요약정리해서 출력해달라
# displ 1당 -3.5인 것을 보아 그렇게는 영향은 없지만 점점 줄어는 든다
####################################
#4)배기량과 연비 관계 그래프 만들기
qplot(data=mpg, x=displ, y=hwy) # x축은 배기량 y축은 연비
####################################
#함수
head(mpg) # 데이터 위에서 부터 보기
mean(mpg$hwy) # 데이터에 연비 평균을 계산하라
max(mpg$hwy) # 데이터에 연비가 가장 큰 값
min(mpg$hwy) # 데이터에 연비가 가장 작은 값
hist(mpg$hwy) # 히스토그램으로 연비를 보여달라
#####################################
#변수 활용
a<-1
a
b<-2
b
a+b
a+b-a
3/a
d<-c(1,2,3,4,5)
d
e<-c(6:10)
e
d + 2 # 각각 2씩 더해진다
d + e # 각각 d와 e의 합으로 나온다
f<-seq(1,5)
f
g<-seq(1,10,by=2) # 2씩 증가해서 저장
g
a1<-"a"
a1
a2<-c("a","b","c")
a2
a3<-c("Hello","World","is","good")
a3
a<-c(1,2,3,4)
mean(a) # 평균을 구하라
max(a) # 최댓값을 구하라
min(a) # 최솟값을 구하라
a<-c("a","a","b","c")
qplot(a) # 그래프를 만들어라
ab<-c("hello","my","name") 
paste(ab, collapse = " ") # 데이터를 하나로 띄어쓰기로 합쳐준다
####################################
qplot(data=mpg,x=hwy) # 그래프로 보여달라
qplot(data=mpg,x=cty) # 그래프로 보여달라
qplot(data=mpg,x=drv,y=hwy,geom="point") # 그래프를 점으로 표현하라
qplot(data=mpg,x=drv,y=hwy,geom="boxplot") # 그래프를 박스로 표시하라
qplot(data=mpg,x=drv,y=hwy,geom="boxplot",colour=drv) # 그래프를 색상도 표현하라
####################################
# 사용법을 모를 때 qplot의 예제를 복사해서 붙여서 실행하자
?qplot
qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)
