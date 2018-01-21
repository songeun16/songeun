cow_data=read.csv("cow_data.csv",stringsAsFactors = T)
head(cow_data)
str(cow_data)
sum(is.na(cow_data))
cow_data = na.omit(cow_data)

cow_data$price=as.numeric(cow_data$price)
cow_data$grade=as.factor(cow_data$grade)
cow_data$gender=as.factor(cow_data$gender)
cow_data$slaughter_house=as.character(cow_data$slaughter_house)
cow_data$address=as.character(cow_data$address)
cow_data$slaughter_date=as.character(as.factor(cow_data$slaughter_date))



#문제 1번 
cow_data$is_edible=NA
myfunction<- function(a) {
  ifelse(cow_data$age >= 50 & (cow_data$grade=="3" | cow_data$grade=="등외"),"폐기용","식용")
}
cow_data$is_edible <- myfunction(cow_data)

#문제2번 
aa <- cow_data[cow_data$grade=="1++",c(5,9,11)]
head(aa)
aa2<- with(aa,summary(aa$address))
aa2<- as.data.frame(aa,sort=T)

b <- subset(cow_data,grade=="1++",select =c(address,grade),sort=T)


c <- by(cow_data[,5],cow_data[,9],summary)
c1 <- as.data.frame(c$`1++`)
c2 <- subset(c1,c$`1++`>=29& c$`1++`<=46)
head(c2)


c1 <- as.list(c$`1++`)
c2 <- as.data.frame(c1)


attach(cow_data)
as.character(address)->a
strsplit(a," ")->b
c<-list()
for (i in 1:length(b))
  (ifelse (length(b[[i]])==4,paste(b[[i]][1],b[[i]][2]),NA))->c[i]
unlist(c)->d
as.data.frame(cow_data$grade)->grade1
cow1<-as.data.frame(cbind(d,grade1))
names(cow1)<-c("address1","grade1")
cow2<-na.omit(cow1[cow1$grade1=="1++",])
head(cow2)
cow3<-with(cow2,summary(address1))
cow_data2 <- head(as.data.frame(cow3),n=3)
detach(cow_data)

cow_data$price %>% 
  table() %>%
  sort(decreasing = T) %>%
  head(4)

#3번
strsplit(cow_data$address," ")->b
c<-list()
for (i in 1:length(b))
  (ifelse (length(b[[i]])==4,paste(b[[i]][1],b[[i]][2]),NA))->c[i]
unlist(c)->d
price <- data.frame(as.numeric(cow_data$price))
cow_data3 <- as.data.frame(cbind(d,price))
colnames(cow_data3) <- c("address","price")
y1 <- subset(cow_data3,(address=="전라북도 정읍시" | address=="전라남도 고흥군" | address=="경기도 안성시"))
y1%>%
  group_by(address) %>%
  summarise(mean(price))

#4번
cow_data$slaughter_date = substr(cow_data$slaughter_date,5,6)
cow_data4 <- as.data.frame(cbind(d,cow_data$slaughter_date))
colnames(cow_data4) <- c("address","date")
head(cow_data4)
y2 <- subset(cow_data4,(address=="전라북도 정읍시" | address=="전라남도 고흥군" | address=="경기도 안성시"))
head(y2,10)
y22 <- table(y2)
barplot(y22,beside=T,col=c("red","blue","green"))

