# 데이터 프레임 만들기
history <- c(90,80,60,70) # 역사점수 생성
history
math <- c(50,60,100,20) # 수학점수 생성
math
# 변수 합해서 데이터프레임 만들기
df_midterm <- data.frame(history,math)
df_midterm
# 반 추가하기
class <- c(1,1,2,2)
class
df_midterm <- data.frame(history,math,class)
df_midterm
mean(df_midterm$history)
mean(df_midterm$math)

# 외부 데이터 불러오기
# 엑셀 데이터 불러오기
install.packages("readxl")
library(readxl)
df_finalexam <- read_excel("finalexam.xlsx",sheet = 1, col_names = T)
# 엑셀을 불러와서 변수로 만든다
# sheet는 엑셀의 몇번째 데이터 시트를 가져오는지(엑셀이 sheet로 되어있다)
# col_names는 데이터를 불러올때 컬리이름까지 가져올꺼야? True 혹은 False
# 즉, 제일 위에 있는 id, class, math, history, english를 가져온다는 말
# F로 쓰면 ...1 ...2 ...3 ...4 ...5로 변수명이 온다
df_finalexam # 데이터를 확인
mean(df_finalexam$math)
mean(df_finalexam$history)
mean(df_finalexam$english)
# csv파일 불러오기(,로 구별되어 있다)
# 설치 안해도 됨
read.csv("csv_exam.csv", header = T)
# csv파일은 sheet가 아니다
# header는 col_names와 같은 기능이다
# 변수에 넣지 않아서 불러올 수 는 없다
# 저장하는 방법이 있다
write.csv(df_finalexam, file = "output_newdata.csv")












