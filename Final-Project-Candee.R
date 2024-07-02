#1. Use	read_csv() to	read	your	CSV	file	(https://intro-datascience.s3.us-east2.amazonaws.com/Resort01.csv) into	a	dataframe	called	bookings.	Describe	the	variables	in	the	dataframe	using	descriptive	statistics	– add	a	brief	comment	to explain	what	you	see.	How	many	observations	and	variables	does	the	dataframe	have?	Be	sure	to	comment	your	code	and	describe	the results you	found.	[1	point]

library(tidyverse)
bookings <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")
#The data frame contains information regarding hotel bookings. The data frame has 40060 observations and it has 20 variables. The columns have variables such as adults, cancellations, and if a guess is repeated for a stay at the hotel, with that being said the columns have categorical and even numerical variables too. 

#2. Check	the	numerical	variables	for	missing	values	using	the	is.na().	Determine what to	do	with	any	NAs. [1	point]

sum(is.na(bookings))
#According to our is.na function there are no missing values. 

#3. Generate	tables	(using	the	table() function)	for	any	3	categorical	response	variables	(e.g.,	Meal),	and	write	a	sentence	for	each,	describing	what	you	see.	[1	point]

table(bookings$ReservedRoomType)
#The table shows the type of room guests reserved for their hotel reservation. It seems that most guests reserve room type A which has 23399.

table(bookings$IsRepeatedGuest)
#According to the table, 38383 reservations are not repeating guests while 1778 reservations are repeating guests of the hotel. 

table(bookings$IsCanceled)
#According to the table, 28938 bookings were not canceled while 11122 bookings were canceled. 

#4. Create	three	boxplots	for	IsCanceled,	using	each	of	these	grouping	variables:	LeadTime,	StaysInWeekNights,	PreviousCancellations.	Add	a	comment	explaining	each	boxplot. [1	point]
library(ggplot2)

boxplot(LeadTime ~ IsCanceled, data=bookings)
#The time from when a customer books the reservation to the time they stay at the hotel results in less cancellations. In other words, the higher number of days in LeadTime results in less cancellations. 
boxplot(StaysInWeekNights ~ IsCanceled, data=bookings)
#According to the box plot, the more days you stay in on week nights which is monday to friday results in more cancellations. 
boxplot(PreviousCancellations ~ IsCanceled, data=bookings)
#The box plot indicates, that the more a customer cancels in the past they are more likely to cancel now.  


#5. Create	histograms	for	any	3	variables not	explored	in	step	3. For	each	attribute, compare	the	histogram	of	when	the	reservation	was	cancelled	to	when	it	was not	cancelled. Write	a	sentence	about	each	attribute,	describing	your	findings. [1	point]

bookings %>% filter(bookings$IsCanceled==1) ->CancelledBookings
bookings %>% filter(bookings$IsCanceled==0) ->NotCancelledBookings

hist(CancelledBookings$Children)
hist(NotCancelledBookings$Children)
#According to both histograms for children and if bookings were cancelled or not, the bookings with children are cancelled more often. 

hist(CancelledBookings$Adults)
hist(NotCancelledBookings$Adults)
#For the cancelled histogram it seems that most adults in the party of 5 or fewer cancel their bookings, then according to the not cancelled histogram it seems if you are in a party of two you are less likely to cancel the booking. 

hist(CancelledBookings$Babies)
hist(NotCancelledBookings$Babies)
#There isn't a big difference in the histogram graphs between cancelled and not cancelled bookings for babies. 

#6. Add	a	comment	listing	the	country	with	the	largest	number	of	bookings	and	show	the	code	you	used	to	identify	it. [1	point]

which.max(table(bookings$Country))
#Portugal is the country with the largest number number of bookings. 


#7. Currently,	the	data	is	at	the	individual	booking	level.	Create	a	new	dataframe	called	country based	on	the	Country and	IsCanceled variables) to	show	the	total	number	of	cancellations	for	each	country (hint:explore	the	aggregate() or	summarize() functions).	Possible	names	for	the	resulting	two	columns	in	country are	name and	numCanceled.	Display	the	country	with	the	highest	number	of	cancellations. [1	point]

country <- aggregate(IsCanceled ~ Country, data = bookings, sum)
names(country)[names(country) == "Country"] <- 'name'
names(country)[names(country) == "IsCanceled"] <- 'NotCancelledBookings'
which.max(country$numCanceled)
country
#Portugal also has the highest number of cancellations. 


#8. Create	a	color	gradient	world	map	where	the	color	of	each	country	indicates	its	total	number	of	cancellations.	Assuming	your	dataframe	is	called	country,	here	is	code	to	do	this: install.packages("rworldmap") library(rworldmap) sPDF <- joinCountryData2Map(country, joinCode="ISO3", nameJoinColumn="name") map<-mapCountryData(sPDF, nameColumnToPlot='numCanceled', catMethod="logFixedWidth") Make	sure	“name” in	the	nameJoinColumn	=	“name” parameter	corresponds	to	the	name	of	the	column	in	your	country dataframe	that	contains	the	3-letter	country	abbreviations,	and	“numCanceled” in	nameColumnToPlot=‘numCanceled’ is the	same	as	the	country variable	name	containing	the	total	number	of	cancellations	per	country.	If	that	is	the	case,	you	should	be	able	to	see	a	map	after	running	these	commands.	Write	a	comment	about	what	you	see	in	the	map. [2 points]

install.packages("rworldmap")
library(rworldmap)
sPDF <-
  joinCountryData2Map(country, joinCode="ISO3", nameJoinColumn="name")
map<-mapCountryData(sPDF, nameColumnToPlot='NotCancelledBookings',
                    catMethod="logFixedWidth")

#Portugal, United Kingdom and Spain have the highest density of not canceling. 

#9. Returning	to	the	full	data	set	(bookings),	convert	some	of	the	fields	into	factor variables	and	then	use	Association	Rules	Mining to	see	if	there	are	patterns	of attributes	that	connect	with	a	canceled	booking. Here’s	a	line	of	code	that	converts	the	main	dataframe	into	a	new	dataframe	that	only	contains	factor	variables: bookCat <- data.frame(meal=as.factor(bookings$Meal), marketSegment=as.factor(bookings$MarketSegment), isRepeatedGuest=as.factor(bookings$IsRepeatedGuest), assignedRoom=as.factor(bookings$AssignedRoomType), customerType=as.factor(bookings$CustomerType), bookingChanges=as.factor(bookings$BookingChanges>0), canceled=as.factor(bookings$IsCanceled)) Using	the	itemFrequencyPlot() function,	inspect	the	variables	in	bookCat and	include	a	comment	on	what	you	see. [1	point]

bookCat <- data.frame(meal=as.factor(bookings$Meal),
                      marketSegment=as.factor(bookings$MarketSegment),
                      isRepeatedGuest=as.factor(bookings$IsRepeatedGuest),
                      assignedRoom=as.factor(bookings$AssignedRoomType),
                      customerType=as.factor(bookings$CustomerType),
                      bookingChanges=as.factor(bookings$BookingChanges>0),
                      canceled=as.factor(bookings$IsCanceled))

library(arules)
install.packages(c("arules", "arulesViz"))
library("arules", "arulesViz")
bookCat1 <- as(bookCat, "transactions")
itemFrequencyPlot(bookCat1)

#10. Remember	that	you	can’t	use	the	bookCat dataframe	as	input	to	apriori() directly	–you	will	first	have	to	coerce	it	to	the	transactions	class.	Identify	at	least	two	high confidence	rules	that	connect	with	someone	canceling	their	stay.	Describe	your analysis	and	these	high	confidence	rules	in	a	few	sentences	– what do	the	support,confidence,	and	lift	values	of	these	rules	mean? [2 points]

rules <- apriori(bookCat,parameter=list(supp=0.01, conf=0.5), control = list(verbose=F), appearance = list(default="lhs", rhs=("canceled=1")))

#PreviousCancelations is a rule that states if the customer cancelled in the past that they will most likely cancel in the future as well.

#StaysInWeekNights is a rule that shows us that their is a relationship between people cancelling when they are staying from monday to friday. Referring back to question #4 it supports the rule of people cancelling and people staying from monday to friday. 


#11. Next,	we	will	turn	to	supervised	machine	learning to	try	and	predict	cancellations	using	Support	Vector	Machines and Trees.	To	use	these models,	we	first	need	to focus	on	the	numerical	variables	in	our	dataset.	Run	the	following	command	before	you	proceed: book <- data.frame(leadTime=bookings$LeadTime, staysWeekend=bookings$StaysInWeekendNights, staysWeek=bookings$StaysInWeekNights, adults=bookings$Adults, children=bookings$Children, babies=bookings$Babies, prevCancellations=bookings$PreviousCancellations, specialRequests=bookings$TotalOfSpecialRequests, canceled=as.factor(bookings$IsCanceled)) Using	the	createDataPartition() function	from	the	caret package,	partition	book	into	a	trainSet and	a	testSet,	where	the	trainSet is	.7	of	the	entire	data	and	y=book$canceled. [1	point]

book <- data.frame(leadTime=bookings$LeadTime,
                   staysWeekend=bookings$StaysInWeekendNights,
                   staysWeek=bookings$StaysInWeekNights,
                   adults=bookings$Adults,
                   children=bookings$Children,
                   babies=bookings$Babies,
                   prevCancellations=bookings$PreviousCancellations,
                   specialRequests=bookings$TotalOfSpecialRequests,
                   canceled=as.factor(bookings$IsCanceled))

library(caret)
trainList <- 
  createDataPartition(y=book$canceled, p=.70,list = FALSE)

trainSet <- book[trainList,]
testSet <- book[trainList,]



#12. You	are	now	ready	to	create	a	support	vector	machine and tree models.	You	can	use the	same	parameters	we	used	in	the	HW – of	course,	don’t	forget	to	change	the	name	of	the	variable	you	are	predicting	to	canceled.	Remember,	you	need	to	create	two	models	(one	using	SVM	and	using	rpart). Once	your	models are trained	(the	SVM	model may	take	about	10	minutes	since	it’s a	big	dataset),	use	the	predict() function	to	see	how	well	your	model	performs	on	the	testSet (for	each	model). Finally,	using	table() or	confusionMatrix(),	create	a	confusion	matrix	and	calculate	the	error	of	for	both models.Which	model	is	better	(or	are	they	the	same)?	Explain	how	you	arrived at	your conclusion. [4 points]

install.packages("e1071")
library(e1071)
library(rpart)
library(rpart.plot)
library(arules)
library(kernlab)
library(caret)

svm <- ksvm(canceled ~ ., data=trainSet, method="svmRadial", prepProc=c("center","scale"))
svm
predict(svm)
svmPred <- predict(svm, testSet, type="response")
confusionMatrix(testSet$canceled,svmPred)
table(testSet$canceled,svmPred)


library(rpart)
model <- rpart(canceled ~., data = testSet, method = 'class', parms=list(split = 'information'), minsplit = 2, minbucket = 1)

model
install.packages('rpart.plot')
library(rpart.plot)

rpart.plot(model)
#The rpart plot is better because you can see different situation outcomes. The tree model shows different variable outcomes such as LeadTime and special requests which is used to give specific information. With that being said, the rmodel is a better representation of instances that can occur, while the confusion matrix gives us general statements. 

#13. Write	a	paragraph,	to	be	sent	to	the	CEO	of	the	hotel	company, summarizing any actionable insights	you	found in	your	analysis.	In	other	words,	based	on	your	analysis,	what	would	you suggest	to	the	CEO? [3 points]

#Dear CEO, after going over the data provided on your hotel company I have some actionable insights about the analysis of the data you provided. To start after going over the data I found that reservations with people of 5 or more tend to cancel their reservation more often that groups of less than 5. A suggestion I would add is to put down a larger security deposit for the larger reservations so you as a company don't lose more money. Another suggestion I would add is that past customers who have stayed with you tend to cancel their future bookings. This is a big piece of data the shows that customers don't want to stay with your company anymore. I would suggest you add a cancellation fee for past guests who have previously canceled so that way the company is cramming to fill in another room for a given nights. My last suggestion would be to upgrade your hotel internationally. According to the data, you have the least amount of canceled bookings in the country of Portugal. I don't know specifically what attracts guests to the Portugal hotel, it could be the amenities, hotel location, or just the luxuries of the hotel. Whatever attracts guests to Portugal is making your company money. I would suggest you whatever attracts them their you implement into your other hotels so that guests cancel less frequently like the hotels in Portugal. With that being said I hope you take my suggestions into consideration to make your hotel company a better place for people to stay!


#Best,
#Rebecca



