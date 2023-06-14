###Birds of conservation concern - adding to british birds list

list = read.csv('data/BOU_British_List_threatenedstatus.csv')
bocc = read.csv('data/BTO_BoCC_2023.csv')

head(list)
head(bocc)
matches = bocc[bocc$Common.Name %in% list$British..English..vernacular.name,]
#66 matches

#get names to match - remove all capital letters
bocc$CommonName = tolower(bocc$Common.Name)
list$CommonName = tolower(list$British..English..vernacular.name)
matches = bocc[bocc$CommonName %in% list$CommonName,]
#157 - there are 15 species that don't match
notmatch = bocc[!(bocc$CommonName %in% list$CommonName),]
notmatch$CommonName
#redpoll - which one?
#leach's storm-petrel changed to leach's petrel
#bean goose - which one?
#what is a turnstone kestrel?
#common whitethroat changed to whitethroat
#common redstart changed to redstart
#european storm-petrel changed to Storm petrel
#northern fulmar changed to fulmar
#which guillemot?

#now 167 match
notmatch$CommonName
#montagu's harrier - no idea
#redpoll - lesser redpoll is most common - red list. 
#common redpoll is just a winter visitor and arctic very rarely. according to RSPB common is amber list.

#changed to lesser redpoll

#bean goose
#taiga bean goose mostly seen
#some organisations see tundra and taiga as the same species. so i will include them both here
#there is no such thing as a turnstone kestrel. they are two different species oops
#guilletmot changed to common guillemot, black guillemot was already included in the list.

#now 174 match yay

library(dplyr)

bocclist = left_join(list,bocc[,-1], by = 'CommonName')

table(bocc$BOCC.category)
table(bocclist$BOCC.category)

#now schedule 1 species
#schedule 1 birds listed directly from the wildlife and countryside act on legislation.gov.uk
s1 = read.csv('data/Schedule1_2023.csv')
s1$CommonName = tolower(s1$CommonName)
matches = s1[s1$CommonName %in% list$CommonName,]
#4 species no match
notmatch = s1[!(s1$CommonName %in% list$CommonName),]
notmatch$CommonName
#mainly adding hyphens. scarlet rosefinch changed to common rosefinch.

bocclist$Schedule1 = ifelse(bocclist$CommonName %in% s1$CommonName, 1, 0)

#add bats?
bats = read.csv('data/UKBats_annex2.csv')

names(bocclist)
threatlist = bocclist[,c(9,3,4,10,11)]
names(threatlist)
threatlist$Annex2 = NA
threatlist$group = 'bird'
names(bats)[2] = 'Scientific.name'
names(bats)[3] = 'Annex2'
bats$Category = NA
bats$BOCC.category = NA
bats$Schedule1 = NA

bats2 = bats[,c(1,2,4,5,6,3)]
bats2$group = 'bat'

threatlist = rbind(threatlist, bats2)


write.csv(threatlist, 'data/threatenedspecies.csv', row.names = F)

threatlist = read.csv('data/threatenedspecies.csv')
#make one column which describes protection status
threatlist$protection = 'None' 
threatlist$protection[threatlist$BOCC.category=='Amber'& threatlist$Schedule1==0] = 'AmberBOCC'
threatlist$protection[threatlist$BOCC.category=='Red'&threatlist$Schedule1==0] = 'RedBOCC'
threatlist$protection[threatlist$Schedule1==1] = 'Schedule1'
threatlist$protection[threatlist$Annex2==1] = 'Annex2'
table(threatlist$protection)

write.csv(threatlist, 'data/threatenedspecies.csv', row.names = F)
