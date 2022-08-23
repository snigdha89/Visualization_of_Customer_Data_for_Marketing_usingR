#reading the data
df <- read.csv(file.choose(),header = T)
#seeing first 6 rows of the data
head(df)

#checking dimensions of data
dim(df)

#feature names
colnames(df)

#checking column datatypes
sapply(df,class)
datatable(df)

# Objective

# Data Preprocessing

library("tidyverse")
library("plotly")
df$Income<-parse_number(df$Income)
df$Education[df$Education=='Basic']<-'High.school'
df$Education[df$Education=='Graduation']<-'Under.graduate'
df$Education[df$Education=='2n Cycle']<-'Master'
df$Education<-factor(df$Education,ordered=T,levels=c('High school','Under.graduate','Master','PhD'))
df$Marital_Status<-factor(df$Marital_Status)
df$Country<-factor(df$Country)
df$Kidhome<-factor(df$Kidhome)
df$Teenhome<-factor(df$Teenhome)
df$Complain<-factor(df$Complain)
df$Response<-factor(df$Response)
df$AcceptedCmp1<-factor(df$AcceptedCmp1)
df$AcceptedCmp2<-factor(df$AcceptedCmp2)
df$AcceptedCmp3<-factor(df$AcceptedCmp3)
df$AcceptedCmp4<-factor(df$AcceptedCmp4)
df$AcceptedCmp5<-factor(df$AcceptedCmp5)

colSums(is.na(df))
df2<-df[complete.cases(df),] #removing NAs
colSums(is.na(df2))

outliers<- function(column){
  lower.bound<-quantile(column,0.25) - 1.5*IQR(column)
  upper.bound<-quantile(column,0.75) + 1.5*IQR(column)
  return(column[column<lower.bound | column>upper.bound])
}
outliers(df2$Income)
which(grepl(666666, df2$Income))
#remove row 514 (specifying data.frame, row index)
df2 <- df2[-c(514),]

outliers(df2$MntWines)
outliers(df2$MntFishProducts)
outliers(df2$MntGoldProds)
outliers(df2$MntMeatProducts)
outliers(df2$MntSweetProducts)
outliers(df2$MntFruits)
outliers(df2$NumDealsPurchases)
outliers(df2$NumCatalogPurchases)
outliers(df2$NumStorePurchases)
outliers(df2$NumWebPurchases)

df2$Age<-2021-df2$Year_Birth
#checking outliers in age
outliers(df2$Age)
which(grepl(128, df2$Age))
which(grepl(122, df2$Age))
which(grepl(121, df2$Age))
df2 <- df2[-c(495,797,2149),]

df2$Total.expenditure<-df2$MntWines+df2$MntFruits+df2$MntGoldProds+df2$MntMeatProducts+df2$MntSweetProducts+df2$MntFishProducts
head(df2)

## Number of kids and Total expenditure

plot_ly (df2,x=df2$Total.expenditure[df2$Kidhome=='0'],y = ~df2$Kidhome[df2$Kidhome=='0'], type="box", name = '0 kids',color=I("greenyellow")) %>%
add_trace(x=df2$Total.expenditure[df2$Kidhome=='1'],y = ~df2$Kidhome[df2$Kidhome=='1'], name = '1 kid',color=I("orange")) %>%
add_trace(x=df2$Total.expenditure[df2$Kidhome=='2'],y = ~df2$Kidhome[df2$Kidhome=='2'],name = '2 kids',color=I("hotpink")) %>%
layout(xaxis = list(title = 'Total expenditure')) %>%
layout(yaxis = list(title = 'Kids'))%>%
layout(title = 'Boxplot for total expenditure based on number of kids at home',plot_bgcolor='#e5ecf6', 
       xaxis = list( 
         zerolinecolor = '#ffff', 
         zerolinewidth = 2,
         gridcolor = 'ffff',range=c(0,3000)), 
       yaxis = list( 
         zerolinecolor = '#ffff', 
         zerolinewidth = 2, 
         gridcolor = 'ffff')) -> p
p

df20<-df2[df2$Kidhome==0,]
df21<-df2[df2$Kidhome==1,]
df22<-df2[df2$Kidhome==2,]
fig1 <- plot_ly(x = df20$Total.expenditure, type = 'histogram',color=I("greenyellow"),name="0 kids",opacity=0.5)%>%
  layout(xaxis=list(range=c(0,3000)),yaxis=list(range=c(0,300)) )
fig2 <- plot_ly(x=df21$Total.expenditure, type ='histogram',color=I("orange"),name="1 kid",opacity=0.5) %>%
  layout(xaxis=list(range=c(0,3000)),yaxis=list(range=c(0,300)) )
fig3 <- plot_ly(x=df22$Total.expenditure, type='histogram',color=I("hotpink"),name="2 kids",opacity=0.5) %>%
  layout(xaxis=list(range=c(0,3000)),yaxis=list(range=c(0,300)) )
fig <- subplot(fig1, fig2, fig3, nrows = 3) %>% 
  layout(title = list(text = "Histograms for total expenditure based on number of kids at home"),
         plot_bgcolor='#e5ecf6' 
         ) 
fig
df2%>% group_by(Kidhome)%>% summarise(sd(Total.expenditure))
median(df2$Total.expenditure[df2$Kidhome=='0'])
mean(df2$Total.expenditure[df2$Kidhome=='0'])

df2%>% group_by(Kidhome)%>% summarise(sd(Total.expenditure))
median(df2$Total.expenditure[df2$Kidhome=='1'])
mean(df2$Total.expenditure[df2$Kidhome=='1'])

df2%>% group_by(Kidhome)%>% summarise(sd(Total.expenditure))
median(df2$Total.expenditure[df2$Kidhome=='2'])
mean(df2$Total.expenditure[df2$Kidhome=='2'])

## Number of teenagers and Total expenditure

plot_ly(df2,x=df2$Total.expenditure[df2$Teenhome=='0'],y = ~df2$Teenhome[df2$Teenhome=='0'], type="box", name = '0 teens',color=I("greenyellow")) %>%
  add_trace(x=df2$Total.expenditure[df2$Teenhome=='1'],y = ~df2$Teenhome[df2$Teenhome=='1'], name = '1 teens',color=I("orange")) %>%
  add_trace(x=df2$Total.expenditure[df2$Teenhome=='2'],y = ~df2$Teenhome[df2$Teenhome=='2'],name = '2 teens',color=I("hotpink")) %>%
  layout(xaxis = list(title = 'Total expenditure')) %>%
  layout(yaxis = list(title = 'Teenagers'))%>%
  layout(title = 'Boxplot for total expenditure based on number of teenagers at home',plot_bgcolor='#e5ecf6', 
       xaxis = list( 
         zerolinecolor = '#ffff', 
         zerolinewidth = 2,
         gridcolor = 'ffff',range=c(0,3000)), 
       yaxis = list( 
         zerolinecolor = '#ffff', 
         zerolinewidth = 2, 
         gridcolor = 'ffff')) -> p2
p2

df20b<-df2[df2$Teenhome==0,]
df21b<-df2[df2$Teenhome==1,]
df22b<-df2[df2$Teenhome==2,]
fig1b <- plot_ly(x = df20b$Total.expenditure, type = 'histogram',color=I("greenyellow"),name="0 teens",opacity=0.5)%>%
  layout(xaxis=list(range=c(0,3000)),yaxis=list(range=c(0,400)) )
fig2b <- plot_ly(x=df21b$Total.expenditure, type ='histogram',color=I("orange"),name="1 teens",opacity=0.5) %>%
  layout(xaxis=list(range=c(0,3000)),yaxis=list(range=c(0,400)) )
fig3b <- plot_ly(x=df22b$Total.expenditure, type='histogram',color=I("hotpink"),name="2 teens",opacity=0.5) %>%
  layout(xaxis=list(range=c(0,3000)),yaxis=list(range=c(0,400)) )
figb <- subplot(fig1b, fig2b, fig3b, nrows = 3) %>% 
  layout(title = list(text = "Histograms for total expenditure based on number of teens at home"),
         plot_bgcolor='#e5ecf6') 
figb

## Country wise Expenditure

req = aggregate(df2$Total.expenditure, by=list(Category=df2$Country), FUN=sum)
req = as.data.frame.matrix(req) 
req$Category = factor(req$Category, levels = req$Category[order(req$x, decreasing = TRUE)])

req %>% plot_ly(x = ~Category, y = ~x, type = 'bar',opacity=0.5,color = I("lightslategrey")) %>% layout(title = "Barplot for country wise expenditure",
                    xaxis = list(title = "Country"),
                    yaxis = list(title = "Expenditure",range=c(0,700000)))


## Country wise Expenditure on various products

fig1c <- plot_ly(x = df2$Country, y =df2$MntFishProducts, type = 'bar', opacity=0.5,color = I("greenyellow"),name="Fish")

fig2c <- plot_ly(x = df2$Country, y = df2$MntFruits, type = 'bar', opacity=0.5,color = I("purple"),name="Fruits")%>%
  layout(yaxis=list(range=c(0,30000))) 

fig3c <- plot_ly(x = df2$Country, y=df2$MntGoldProds, type = 'bar', opacity=0.5,color = I("orange"),name="Gold")%>%
  layout(yaxis=list(range=c(0,50000))) 

fig4c <- plot_ly(x =df2$Country, y=df2$MntMeatProducts, type = 'bar', opacity=0.5,color = I("magenta"), name="Meat")%>%
  layout(yaxis=list(range=c(0,200000))) 

fig5c <- plot_ly(x = df2$Country, y=df2$MntSweetProducts, type = 'bar', opacity=0.5,color = I("yellow"),name="Sweets")

fig6c <- plot_ly(x =df2$Country, y=df2$MntWines, type = 'bar', opacity=0.5,color = I("salmon"), name="Wines")%>%
  layout(yaxis=list(range=c(0,400000))) 


figc <- subplot(fig1c, fig2c, fig3c, fig4c, fig5c, fig6c, nrows = 3) %>%
  layout(title = list(text = "Barplots for expenditure of countries on different products"),
         plot_bgcolor='#e5ecf6')
figc

#Grouped barplot
df2$Country <- factor(df2$Country, levels = unique(df2$Country)[order(df2$MntWines, decreasing = FALSE)])
fig1d <- plot_ly(data=df2, x = ~Country, y = ~MntFishProducts, type = 'bar', opacity=0.6,color = I("greenyellow"),name="Fish")
fig2d <- fig1d %>% add_trace(y = ~MntFruits, name = 'Fruits', opacity=0.6,color = I("purple"))
fig3d <- fig2d %>% add_trace(y = ~MntGoldProds, name = 'Gold', opacity=0.6,color = I("orange"))
fig4d <- fig3d %>% add_trace(y = ~MntMeatProducts, name = 'Meat', opacity=0.6,color = I("magenta"))
fig5d <- fig4d %>% add_trace(y = ~MntSweetProducts, name = 'Sweets', opacity=0.6,color = I("yellow"))
fig6d <- fig5d %>% add_trace(y = ~MntWines, name = 'Wines', opacity=0.6,color = I("salmon"))
fig7d <- fig6d %>% layout(title = list(text = "Grouped Barplot for expenditure of countries on different products"),
                          plot_bgcolor='#e5ecf6',yaxis = list(title = 'Expenditure',range=c(0,350000)), barmode = 'grouped')
fig7d


## Level of Education and marital status of customers

#Pie chart for education status and marital status
fig <- plot_ly(mai = c(0, 0, 0, 0),marker = list(colors=c("peachpuff","lightsteelblue", "pink","paleturquoise", "lightgray","plum","mediumslateblue", "gray","lavenderblush")))
fig <- fig %>% add_pie(data = count(df2, Education), labels = ~Education,
                       values = ~n,name = "Education",domain = list(x = c(0, 0.4), y = c(0.2, 1)))
fig <- fig %>% add_pie(data = count(df2, Marital_Status), labels = ~Marital_Status,
                       values = ~n, name = "Marital_Status", domain = list(x = c(0.6, 1), y = c(0.2, 1)))
fig <- fig %>% layout(title = "Pie Charts for distribution of data based on Education and Marital Status", showlegend = T,strip.white=FALSE,xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig

## Number of Web, In store purchases and Education status

dfsd1 <-unique(df2$Education)
dfsd2<-df2[df2$Education== 'Under.graduate',]
dfsd3<-df2[df2$Education=='Master',]
dfsd4<-df2[df2$Education=='PhD',]

Education <- c("Under.graduate", "Master", "PhD")
NumWebPurchases <- c(sum(dfsd2$NumWebPurchases), sum(dfsd3$NumWebPurchases), sum(dfsd4$NumWebPurchases))
NumStorePurchases <- c(sum(dfsd2$NumStorePurchases), sum(dfsd3$NumStorePurchases) , sum(dfsd4$NumStorePurchases))
df <- data.frame(Education, NumWebPurchases, NumStorePurchases)
df$Education <- factor(df$Education, levels = unique(df$Education)[order(df$NumStorePurchase, decreasing = FALSE)])
p <- plot_ly(df, x = ~Education, y = ~NumWebPurchases, type = 'bar', name = 'Number of WebPurchases',color=I("lightpink")) %>%
  add_trace(y = ~NumStorePurchases, name = 'Num of StorePurchases',opacity=0.4,color=I('greenyellow')) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p


## Number of Web, In store purchases based on  Marital Status

dfms1 <-unique(df2$Marital_Status)
dfms2<-df2[df2$Marital_Status== 'Divorced',]
dfms3<-df2[df2$Marital_Status=='Single',]
dfms4<-df2[df2$Marital_Status=='Married',]
dfms5<-df2[df2$Marital_Status=='Together',]
dfms6<-df2[df2$Marital_Status=='Widow',]
dfms7<-df2[df2$Marital_Status=='YOLO',]
dfms8<-df2[df2$Marital_Status=='Alone',]
dfms9<-df2[df2$Marital_Status=='Absurd',]

Marital_Status <- c("Divorced", "Single" , " Married" , "Together","Widow" , "YOLO" , "Alone" , "Absurd")
NumWebPurchasesms <- c(sum(dfms2$NumWebPurchases), sum(dfms3$NumWebPurchases),sum(dfms4$NumWebPurchases) ,sum(dfms5$NumWebPurchases) ,sum(dfms6$NumWebPurchases) ,sum(dfms7$NumWebPurchases),sum(dfms8$NumWebPurchases),sum(dfms9$NumWebPurchases))
NumStorePurchasesms <- c(sum(dfms2$NumStorePurchases), sum(dfms3$NumStorePurchases) , sum(dfms4$NumStorePurchases),sum(dfms5$NumStorePurchases) ,sum(dfms6$NumStorePurchases),sum(dfms7$NumStorePurchases),sum(dfms8$NumStorePurchases),sum(dfms9$NumStorePurchases))
dfMS <- data.frame(Marital_Status, NumWebPurchasesms, NumStorePurchasesms)
dfMS$Marital_Status <- factor(dfMS$Marital_Status, levels = unique(dfMS$Marital_Status)[order(dfMS$NumStorePurchasesms, decreasing = FALSE)])
p_marr_status <- plot_ly(dfMS, x = ~Marital_Status, y = ~NumWebPurchasesms, type = 'bar', name = 'Number of WebPurchases',color=I("darkgray")) %>%
  add_trace(y = ~NumStorePurchasesms, name = 'Num of StorePurchases',opacity=0.6,color=I("salmon")) %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p_marr_status



# Sampling Techniques
## Simple Random Sampling , Systematic Sampling, Stratified

#Sampling
library("sampling")
#Plotting histogram for original data
set.seed(123)
fig8 <- plot_ly(x = df2$Income, type = "histogram",histnorm = "probability",color=I("darkgray"),name="Histogram for all data",opacity=0.8)%>% layout( xaxis=list(zerolinecolor = '#fff',zerolinewidth = 2,gridcolor = 'ffff',range=c(0,180000),title="Income"),yaxis=list(range=c(0,0.040),title="Density")) 
fig8%>%layout(title="Histogram for all data")

#Simple Random Sampling with replacement
set.seed(123)
N <- nrow(df2)
n <- 80
s<-srswr(n,N)
#s
#s[s!=0]
rows<-rep((1:nrow(df2))[s!=0],s[s!=0])
srs<-df2[rows,]
fig8a <- plot_ly(x = srs$Income, type = "histogram",histnorm = "probability",color=I("greenyellow"),name="Srswr",opacity=0.5)%>% layout(xaxis=list(zerolinecolor = '#fff',zerolinewidth = 2,gridcolor = 'ffff',range=c(0,180000),title="Income"),yaxis=list(range=c(0,0.25),title="Density")) 


#Simple Random Sampling without replacement
set.seed(123)
s2<-srswor(n,N)
#s2
#s2[s2!=0]
rows2<-(1:nrow(df2))[s2!=0]
srs2<-df2[rows2,]
fig8b<- plot_ly(x = srs2$Income, type = "histogram",histnorm = "probability",color=I("orange"),name="Srswor",opacity=0.8)%>% layout(xaxis=list(zerolinecolor = '#fff',zerolinewidth = 2,gridcolor = 'ffff',title="Income",range=c(0,180000)),yaxis=list(range=c(0.25),title="Density")) 


#Systematic Sampling
set.seed(123)
k<-ceiling(N/n)
#k
r<-sample(k,1)
#r
sys<-seq(r,by=k,length=n)
#sys
s3<-df2[sys,]
#s3
fig8c <- plot_ly(x = s3$Income, type = "histogram",histnorm = "probability",color=I("pink"),opacity=0.5,name="Systematic Sampling")%>% layout(xaxis=list(zerolinecolor = '#fff',zerolinewidth = 2,gridcolor = 'ffff',title="Income",range=c(0,180000)),yaxis=list(title="Density",range=c(0,0.25))) 

#Systematic Sampling (Unequal probabilities)
set.seed(123)
pick<-inclusionprobabilities(df2$Total.expenditure,80)
s3b<-UPsystematic(pick)
s3c<-df2[s3b!=0,]
fig8c2 <- plot_ly(x = s3c$Income, type = "histogram",histnorm = "probability",color=I("blue"),opacity=0.5,name="Systematic Sampling (unequal prob)")%>% layout(xaxis=list(zerolinecolor = '#fff',zerolinewidth = 2,gridcolor = 'ffff',title="Income",range=c(0,180000)),yaxis=list(range=c(0.25),title="Density")) 

#Stratified Sampling
set.seed(123)
df2$Education<-factor(df2$Education,ordered=T,levels=c('Under.graduate','Master','PhD'))
freq<-table(df2$Education)
st.sizes<-80*(freq/sum(freq))  #calculating sample sizes for unequal strata
#st.sizes
s4<-sampling::strata(df2,stratanames=c("Education"),method="srswor",size=st.sizes)
#s4
d<-getdata(df2,s4)
fig8d <- plot_ly(x = d$Income, type = "histogram",histnorm = "probability",color=I("magenta"),name="Stratified sampling",opacity=0.5)%>% layout(xaxis=list(zerolinecolor = '#fff',zerolinewidth = 2,gridcolor = 'ffff',title="Income",range=c(0,180000)),yaxis=list(title="Density",range=c(0,0.25)))

s <- subplot(fig8a, fig8b, fig8c, fig8c2, fig8d, nrows = 3, shareY = TRUE)%>%layout(title='Sampling techniques')
s
mean(df2$Income) #population mean
mean(srs$Income) #srswr
mean(srs2$Income) #srswor
mean(s3$Income) #systematic 
mean(s3c$Income) #systematic with unequal probabilities
mean(d$Income) #stratified

sd(df2$Income) #population standard deviation
sd(srs$Income) #srswr
sd(srs2$Income) #srswor
sd(s3$Income) #systematic 
sd(s3c$Income) #systematic with unequal probabilities
sd(d$Income) #stratified

## Linear Regression Model

pal <- c("peachpuff","deeppink","darkmagenta","blueviolet","yellow","darkolivegreen")
dfexp <- df2[,c(29,5,30)]
cor(df2$Total.expenditure,df2$Income) #r
mdl<-lm(df2$Total.expenditure~df2$Income)
summary(mdl) #R2=0.6244

dfexp %>%
  plot_ly(x = ~Income, y = ~Total.expenditure,colors = pal) %>%
  add_markers(color = ~Age) %>%
  add_lines(x = ~Income, y = fitted(mdl))

fitted(mdl) #predicted values
resid(mdl) #residuals of the model (difference between actual and predicted values)

rmse<-sqrt(mean(sum((df2$Total.expenditure-fitted(mdl))^2)))
cat("RMSE is", rmse)
mae<-mean(abs(df2$Total.expenditure-fitted(mdl)))
cat("MAE is", mae) #mean absolute error


### Hypothesis test to check for linear relation between income and expenditure

#### Step 1: Stating the hypothesis and selecting alpha level
#### Step 2: Decide test statistic

t=B1/(sqrt(sum(sq(actual y-predicted y))/n-2) / sqrt(sum(sq(actual x-mean x))))

degree.of.freedom<-nrow(df)-2
degree.of.freedom
 
#### Step 3: Decision rule
#### Step 4: Compute test statistic
summary(mdl)
#### Step 5: Conclusion
#### Confidence intervals: 
confint(mdl,level=0.95)

# Central Limit Theorem
#hist(df2$Age)
agedf <- data.frame(df2$Age)
#agedf
sample23 <- 250

xbars1 = numeric(250)
xbars2 = numeric(250)
xbars3 = numeric(250)
xbars4 = numeric(250)
for (i in 1:250) {
  xbars1[i] <- mean(sample(df2$Age, 
                           size = 10, replace = TRUE))
}
agedf1 <- data.frame(xbars1)
#agedf1
p1 <- plot_ly(agedf1, x= ~agedf1$xbars1, type = 'histogram',histnorm = 'probability',color=I("blue"),opacity=0.4,name="Sample size:10")%>%layout(xaxis=list(range=c(40,65)),yaxis=list(title="Density",range=c(0,0.15)))

for (i in 1:250) {
  xbars2[i] <- mean(sample(df2$Age, 
                           size = 20, replace = TRUE))
}
agedf2 <- data.frame(xbars2)
p2 <- plot_ly(agedf2, x= ~agedf2$xbars2, type = 'histogram',histnorm = 'probability',color=I("salmon"),name="Sample size:20")%>%layout(xaxis=list(range=c(40,65)),yaxis=list(range=c(0,0.15)))


for (i in 1:250) {
  xbars3[i] <- mean(sample(df2$Age, 
                           size = 30, replace = TRUE))
}
agedf3 <- data.frame(xbars3)
p3 <- plot_ly(agedf3, x= ~agedf3$xbars3, type = 'histogram',histnorm = 'probability',color=I("greenyellow"),opacity=0.6, name="Sample size:30")%>%layout(xaxis=list(range=c(40,65)),yaxis=list(title="Density",range=c(0,0.15)))


for (i in 1:250) {
  xbars4[i] <- mean(sample(df2$Age, 
                           size = 40, replace = TRUE))
}
agedf4 <- data.frame(xbars4)
p4 <- plot_ly(agedf4, x= ~agedf4$xbars4, type = 'histogram',color=I("pink"),histnorm = 'probability',name="Sample size:40")%>%layout(xaxis=list(range=c(40,65)),yaxis=list(range=c(0,0.15)))

p <- subplot(p1, p2, p3, p4, nrows = 2, shareY = TRUE)%>%layout(title="Central Limit Theorem")
p
cat("Sample Size = ", 10, " Mean = ", mean(xbars1),
    " SD = ", sd(xbars1))
cat("Sample Size = ", 20, " Mean = ", mean(xbars2),
    " SD = ", sd(xbars2))
cat("Sample Size = ", 30, " Mean = ", mean(xbars3),
    " SD = ", sd(xbars3))
cat("Sample Size = ", 40, " Mean = ", mean(xbars4),
    " SD = ", sd(xbars4))

As we can see, the mean of the sampling distribution remains almost the same as the overall population mean, whereas the standard deviation decreases as sample size is increased.

## Number of Deals and Age

Purchase-based targeting (PBT) goes after consumers who have a purchase history with your offering. Statistics show that those who bought once are more likely to buy again. PBT is highly productive and so this question will help to know people of what age have opted for more deals than others and this will ultimately help strategize PBT of consumers.
plot_ly(df2,x=df2$NumDealsPurchases,y = ~df2$Age, type="box",opacity=0.6,color=I("darkcyan"))%>%layout(title="Boxplot for Number of deals and Age of Customer", xaxis=list(title="Number of deals",range=c(0,16)),yaxis=list(title="Age"))

df2 %>%
  plot_ly(x = ~Age, y = ~NumDealsPurchases,opacity=0.6,color=I("darkcyan")) %>%
  add_markers(size = ~Income) %>% layout(title="Number of deals and Age",yaxis=list(title="Number of deals",range=c(0,16)),xaxis=list(title="Age"))


## Number of kids, teens and Web purchases
#Stacked bar plot for kids and teens and the no of web purchases

g1 <- plot_ly(df2, x = ~NumWebPurchases, y = ~Teenhome, color=I("lightslategrey"),type = 'bar',opacity=0.6, name = 'Teens') %>%
  add_trace(y = ~Kidhome, color=I("grey"),opacity=0.5,name = 'Kids') %>%
  layout(title="Teens,kids and web purchases",yaxis = list(title = 'Teens/Kids'), barmode = 'stack',xaxis=list(title="Web purchases",range=c(0,14000)))
g1

## Number of kids, teens and In store purchases
#Stacked bar plot for kids and teens on the no of in store purchases
s1 <- plot_ly(df2, x = ~NumStorePurchases, color=I("lightslategrey"),opacity=0.6, y = ~Teenhome, type = 'bar', name = 'Teens') %>%
  add_trace(y = ~Kidhome, color=I("grey"),opacity=0.5,name = 'Kids') %>%
  layout(title="Teens,kids and in store purchases",yaxis = list(title = 'Teens/Kids'), barmode = 'stack',xaxis=list(title="In store purchases",range=c(0,20000)))
s1
df2%>%group_by(Kidhome)%>%summarise(sum(NumWebPurchases))
df2%>%group_by(Kidhome)%>%summarise(sum(NumStorePurchases))
df2%>%group_by(Teenhome)%>%summarise(sum(NumWebPurchases))
df2%>%group_by(Teenhome)%>%summarise(sum(NumStorePurchases))

## Types of purchases based on number of kids and number of teenagers.

#All kinds of Purchases based on number of kids as a Grouped Bar Chart
fig1d <- plot_ly(data=df2, x = ~Kidhome, y = ~NumStorePurchases, type = 'bar', opacity=0.5,color = I("greenyellow"),name="No of In Store Purchases")
fig2d <- fig1d %>% add_trace(y = ~NumWebPurchases, name = 'Web Purchases', opacity=0.5,color = I("magenta"))
fig3d <- fig2d %>% add_trace(y = ~NumDealsPurchases, name = 'Deals Purchased', opacity=0.6,color = I("orange"))
fig4d <- fig3d %>% add_trace(y = ~NumCatalogPurchases, name = 'Catalog Purchases', opacity=0.5,color = I("salmon"))
fig5d <- fig4d %>% add_trace(y = ~NumWebVisitsMonth, name = 'Web Visits/Month', opacity=0.4,color = I("blue"))
fig6d <- fig5d %>% layout(title="Types of purchases based on number of kids",yaxis = list(title = 'Purchases',range=c(0,30000)),xaxis=list(title="Kids"), barmode = 'stack')
fig6d

#All kinds of Purchases based on number of teens as a stacked bar chart
fig1d <- plot_ly(data=df2, x = ~Teenhome, y = ~NumStorePurchases, type = 'bar', opacity=0.5,color = I("greenyellow"),name="No of In Store Purchases")
fig2d <- fig1d %>% add_trace(y = ~NumWebPurchases, name = 'Web Purchases', opacity=0.5,color = I("magenta"))
fig3d <- fig2d %>% add_trace(y = ~NumDealsPurchases, name = 'Deals Purchased', opacity=0.6,color = I("orange"))
fig4d <- fig3d %>% add_trace(y = ~NumCatalogPurchases, name = 'Catalog Purchases', opacity=0.5,color = I("salmon"))
fig5d <- fig4d %>% add_trace(y = ~NumWebVisitsMonth, name = 'Web Visits/Month', opacity=0.4,color = I("blue"))
fig6d <- fig5d %>% layout(title="Types of purchases based on number of teens",yaxis = list(title = 'Purchases',range=c(0,25000)), barmode = 'stack',xaxis=list(title="Teens"))
fig6d

## Number of Web visits per month
df2 %>%
  separate(Dt_Customer, sep="/", into = c("month","day","year"))->df3
df3%>%filter(year=="12")%>%group_by(month)%>%summarise(Webvisits=sum(NumWebVisitsMonth))->a
a$month<-as.numeric(a$month)
a<-a%>%arrange(month)
df3%>%filter(year=="13")%>%group_by(month)%>%summarise(Webvisits=sum(NumWebVisitsMonth))->b
b$month<-as.numeric(b$month)
b<-b%>%arrange(month)
df3%>%filter(year=="14")%>%group_by(month)%>%summarise(Webvisits=sum(NumWebVisitsMonth))->c
c$month<-as.numeric(c$month)
c<-c%>%arrange(month)
fig9 <- plot_ly(a, x = ~month, y = ~Webvisits, type = 'scatter', name="Year 2012",mode = 'lines',line=list(color="greenyellow",width=4))%>%layout(xaxis=list(range=c(1,12)),yaxis=list(range=c(0,800)))
fig9b <- plot_ly(b, x = ~month, y = ~Webvisits, type = 'scatter', name="Year 2013", mode= 'lines',line=list(color="orange",width=4))%>%layout(xaxis=list(range=c(1,12)),yaxis=list(range=c(0,800)))
fig9c<-plot_ly(c, x = ~month, y = ~Webvisits, type = 'scatter', name="Year 2014", mode = 'lines',line=list(color="pink",width=4))%>%layout(xaxis=list(range=c(1,12)),yaxis=list(range=c(0,800)))
fig9d <- subplot(fig9, fig9b, fig9c, nrows = 3) %>% 
  layout(title = list(text = "Lineplots for number of web visits per month"),
         plot_bgcolor='#e5ecf6') 
fig9d
