CPS = read.csv("CPSData.csv")

#find the number of interviewees
summary(CPS)
#or
str(CPS)

#find the most common industry of employment

summary(CPS) # orders the levels of a factor variable from large to small

#sort the table and find state with fewest intervieweees and largest number of interviewees

sort(table(CPS$State))

#find proportion of US citizens
table(CPS$Citizenship)
#123,712/131302 = 0.942

#find which race has at least 250 interviewees of hispanic  ethnicity

table(CPS$Race,CPS$Hispanic)
left off page 4

