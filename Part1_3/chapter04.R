#패키지 = 함수 꾸러미
#패키지 설치와 로드
#Rstudio 실행할 때마다!
#내장 함수는 X
#어플깔듯 입맛대로 설치
#필요한 기능에 따라 최신 분석 기법 수시로 업데이트
#ggplot2는 시각화 패키지 qplot2(),geom_histogram(),geom_line().....
library(ggplot2)
qplot(b)
head(mpg)
qplot(data = mpg, x = hwy) # 2가지 파라미터를 이용했다 데이터와 고속연비
qplot(data = mpg, x = cty)
qplot(data = mpg, y = hwy, x = drv, geom = "point") # drv 구동 방식
qplot(data = mpg, y = hwy, x = drv, geom = "boxplot")
qplot(data = mpg, y = hwy, x = drv, geom = "boxplot", colour = drv)
#구동 방식 별로 색을 추가해 달라
?qplot

qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)







