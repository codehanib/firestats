# 파일럿 프로젝트 2
# 전국 산불 통계 자료
# install.packages("leaflet")
# install.packages("htmltools")

library(leaflet)
library(htmltools)

setwd("C:/R/pilot")
getwd()

ffire <-read.csv("산림청_산불통계데이터_20250911.csv")

# ----------------------전처리----------------------------

# 데이터 확인
head(ffire)
head(ffire, 50)
unique(ffire$f.gugun)
unique(ffire$f.city)

# 단어가 포함된 데이터 찾기
ffire[grepl("포항", ffire$f.gugun),]
ffire[grepl("충남", ffire$f.city),]

# 결측값 확인
colSums(is.na(ffire))

# 결측값에 값넣기
ffire$f.cause[ffire$f.cause == ""] <-"기"

# 컬럼명 변경
names(ffire)[names(ffire)=="발생일시_년"] <-"f.year"
names(ffire)[names(ffire)=="발생일시_월"] <-"f.month"
names(ffire)[names(ffire)=="발생일시_일"] <-"f.day"
names(ffire)[names(ffire)=="발생일시_요일"] <-"f.week"
names(ffire)[names(ffire)=="진화종료시간_년"] <-"f.out.year"
names(ffire)[names(ffire)=="진화종료시간_월"] <-"f.out.month"
names(ffire)[names(ffire)=="진화종료시간_일"] <-"f.out.day"
names(ffire)[names(ffire)=="발생장소_시도"] <-"f.city"
names(ffire)[names(ffire)=="발생장소_시군구"] <-"f.gugun"
names(ffire)[names(ffire)=="발생원인_구분"] <-"f.cause"
names(ffire)[names(ffire)=="발생원인_기타"] <-"f.detail"
names(ffire)[names(ffire)=="피해면적_합계"] <-"f.area"

# 필요없는 컬럼 제거
ffire <-ffire[,-11]

# 지울 데이터 위치 확인
ffire[,11]

# 특정 컬럼의 값 변경
ffire$f.city[grepl("충남", ffire$f.city)] <-"충남"
ffire$f.gugun[grepl("포항", ffire$f.gugun)] <-"포항"
ffire$f.gugun[grepl("창원", ffire$f.gugun)] <-"창원"
ffire$f.gugun[grepl("수원", ffire$f.gugun)] <-"수원"
ffire$f.gugun[grepl("용인", ffire$f.gugun)] <-"용인"
ffire$f.gugun[grepl("안산", ffire$f.gugun)] <-"안산"

# --------------------------------------------------------

# 1. 연도별 산불 발생 추이 선그래프
library(ggplot2)
year.cnt <- aggregate(ffire$f.day,by=list(year=ffire$f.year),FUN='length')
year.cnt

ggplot(year.cnt, aes(x=year, y=x))+
  geom_line(lwd=1, color="red")+
  geom_point(size=4, shape=19, alpha=0.5, color="red")+
  ggtitle('(2022~2025)연도별 산불 발생 추이')+
  ylim(0,800)+
  geom_label(
    aes(label = x),
    size = 4,
    vjust = -0.5,
    show.legend = FALSE
  )+
  labs(x = "연도", y = "발생 횟수")+
  theme(
    plot.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  )

# --------------------------------------------------------
# 2. 월별 화재 발생 추이(4년)
month.cnt <- aggregate(ffire$f.day,by=list(year=ffire$f.year,
                                           month=ffire$f.month),FUN='length')

month.cnt

# 그래프
ggplot(month.cnt, aes(x=month, y=x, colour=factor(year),group=year))+
  geom_line(lwd=1)+
  geom_point(size=4, shape=17, alpha=0.5)+
  ggtitle('2022~2025(~9월) 산불 발생 통계')+
  ylim(0,250)+
  scale_x_continuous(breaks = 1:12)+
  labs(x = "월", y = "발생 횟수", colour = "year")

# --------------------------------------------------------
# 3. 지역별 산불 발생 횟수
loc.cnt <-aggregate(ffire$f.day,by=list(loc=ffire$f.city),FUN = 'length')
loc.cnt <-loc.cnt[order(loc.cnt$x, decreasing = TRUE),]
rownames(loc.cnt) <- 1:nrow(loc.cnt)

# 상위 10개지역
top10.cnt<- loc.cnt[1:10,]
top10.cnt

# 막대그래프
ggplot(top10.cnt, aes(x=reorder(loc, -x), y=x, fill=x))+
  geom_bar(stat = 'identity')+
  ggtitle('2022~2025 지역별 산불 발생 통계')+
  geom_col()+
  scale_fill_gradient(
    high = "red",
    low = "darkgray"
  )+
  ylim(0,450)+
  geom_text(
    aes(label = x),
    vjust = -0.5)+
  labs(x ="", y="발생 횟수", fill="")+
  theme(
    plot.title = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14)
  )

# 경기도에서 산불이 가장 많이 났던 지역
# install.packages('plotrix')
# library(plotrix)

fire.gg<-subset(ffire, f.city == "경기")
fire.gg<-aggregate(fire.gg$f.day, by=list(loc=fire.gg$f.gugun),length)
fire.gg<-fire.gg[order(fire.gg$x, decreasing = TRUE),]
rownames(fire.gg) <- 1:nrow(fire.gg)
fire.gg
fire.gg.5 <- fire.gg[1:5,]

fire.gg.5
# --------------------------------------------------------
# 4. 피해 면적 10순위
area.top <-aggregate(ffire,by=list(area=ffire$f.area),FUN =max)
area.top <-ffire[order(ffire$f.area, decreasing = TRUE),]
area.10.top <-area.top[1:10,]
area.10.top
# 5순위 그래프
area.5.top <-area.top[1:6,]
area.5.top<-aggregate(area.5.top$f.area,by=list(loc=area.5.top$f.gugun),FUN=sum)
area.5.top <-area.5.top[order(area.5.top$x,decreasing = TRUE),]
rownames(area.5.top) <- 1:nrow(area.5.top)

area.5.top

pie3D(x=area.5.top$x,
      labels =area.5.top$loc,
      main='2022~2025 산불 피해 면적',
      col=c('brown','red','tomato','orange','gray'),
      explode = 0.1,
      radius = 1.0,
      labelcex = 1.2,
      cex.main = 1.5)

# --------------------------------------------------------
# 5. 발생 원인 워드클라우드

head(ffire$f.detail,20)

#install.packages("udpipe")

library(udpipe)
library(wordcloud)
library(RColorBrewer)

ud_model <- udpipe_load_model(
  "korean-gsd-ud-2.5-191206.udpipe"
)

# 텍스트
text <- fire.text


# 형태소 분석
result <- udpipe_annotate(
  ud_model,
  x = text
)

result <- as.data.frame(result)

# 명사 추출
nouns <- subset(
  result,
  upos %in% c("NOUN", "PROPN")
)

# 단어
words <- nouns$token

# 조사 제거
words <- gsub(
  "(으로부터|로부터|에서부터|에게서|한테서|으로는|로는|에서는|은|는|이|가|을|를|과|와|에|에서|의|도|로|으로|에게|한테|께|부터|까지|만|조차|마저|밖에|처럼|같이)$",
  "",
  words
)

# 빈 문자열 제거
words <- words[words != ""]

# 2글자 이상
words <- words[nchar(words) >= 2]

# 불용어
stop_words <- c(
  "것", "수", "등", "때", "곳",
  "정말", "많은", "있는",
  "하는", "되는", "대한"
)

words <- words[
  !words %in% stop_words
]

# 단어 빈도
word_freq <- sort(
  table(words),
  decreasing = TRUE
)

# 빈도 확인
print(word_freq)

# 워드클라우드
wordcloud(
  words = names(word_freq),
  freq = as.numeric(word_freq),
  min.freq = 1,
  random.order = FALSE,
  colors = brewer.pal(8, "Set1"),
  scale = c(8,0.1)
)
