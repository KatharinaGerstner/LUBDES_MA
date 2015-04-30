# Purpose: To extract zotero citation data into excel.
# Version: 1.0
# Platform: Written on R 2.11, Win 7, Zotero 2.01, but should work on all platforms.#
###############################################################################

############################
## The following has variables you need to replace for your setup
##########################

# Replace with the path to your Zotero sqlite file
dbZot <- "~/.mozilla/firefox/rhhh8jil.Default User/zotero/zotero.sqlite"

# Put a character of the top level folders you want to extract
# This will include subfolders as well
vTopFolders <- c("rawWOSimport")

##########################

#libraries
require(sqldf)
require(gdata)
require(reshape2)
require(reshape)
require(plyr)
# SQL query that gets basic data.
sqlC <- "SELECT

items.itemID,
collections.collectionID,
collections.collectionName,
collections.parentCollectionID,
fields.fieldName,
itemDataValues.value,
creatorData.firstName,
creatorData.lastName,
itemNotes.title,
itemNotes.note
FROM
collectionItems
INNER JOIN items ON (collectionItems.itemID = items.itemID)
INNER JOIN collections ON (collectionItems.collectionID = collections.collectionID)
INNER JOIN itemCreators ON (items.itemID = itemCreators.itemID)
INNER JOIN itemData ON (itemData.itemID = items.itemID)
INNER JOIN itemDataValues ON (itemData.valueID = itemDataValues.valueID)
LEFT OUTER JOIN itemNotes ON (items.itemID = itemNotes.sourceItemID)
INNER JOIN creators ON (itemCreators.creatorID = creators.creatorID)
INNER JOIN creatorData ON (creators.creatorDataID = creatorData.creatorDataID)
INNER JOIN fields ON (itemData.fieldID = fields.fieldID)"

# This sends query to sqlite db
# Notice bug, stringAsFactor=F results in 0 vars, but T with method="raw" gets desired outcome
##MB: this is really stupid! I had to look for the very first entry (using head on the imported dfData) and change it within Zotreo so that the first values (page issue etc) is a chr and no int - otherwise R will remove all titles ....
dfData <- sqldf(sqlC,, stringsAsFactors = F, dbname=dbZot, method="raw")

#This filter the data from sqlite and extracts the ID of the top level folder
#vParentID <- unique(dfData[dfData$collectionName %in% vTopFolders,"collectionID"])

#This extracts a mapping of parent-child IDs
#dfParentChildID <-  unique(dfData[vParentID %in% dfData$parentCollectionID,c("itemID","collectionID","parentCollectionID","collectionName")])

# Self join
#dfParentChildID2 <- merge(dfParentChildID,dfParentChildID,by.x=c("itemID","collectionID"),by.y=c("itemID","parentCollectionID"),all.x=TRUE)

# Creates a field that concat top folder - sub folder
#dfParentChildID2 <- transform(dfParentChildID2, top.sub.folder  = paste(collectionName.x,collectionName.y,sep="-"))

# Creates a unique ID
#dfParentChildID2$ID <- with(dfParentChildID2,paste(itemID,collectionID.1,collectionID,sep="-"))

# Creates another unique ID for the original dataset from sqlite
dfData$ID <- with(dfData,paste(itemID,sep="-"))

# merge the datasset
#dfData2 <- merge(dfData,dfParentChildID2,by=c("ID"),all=TRUE)

# Get rid of the NA row in data and extract some key variables
dfData3 <- dfData[c("ID","collectionName","fieldName", "value", "firstName", "lastName", "title")]

#rename vars to prevent name conflict later on
dfData3 <- rename.vars(dfData3, from="title",to="notetitle")

# now need to reshape data, but trick was how to aggregate text. Other functions caused issue, but
# max seems to work for some reason.
# This take the fieldName which is the field in zotero that has vars like author, year, etc
# we want to turn this into wide form so that they become columns
dfData4 <- cast(data = dfData3, ID + collectionName + firstName + lastName + notetitle ~ fieldName, value = "value", fun.aggregate=max)

# Make author last name only
dfData4$author <- with(dfData4, lastName)
# Alternative: both names dfData4$author <- with(dfData4, paste(lastName,firstName,sep=","))

dfData6 <- unique(dfData4[,c("ID", "author", "publicationTitle","title","bookTitle", "date")])


# Create essential a group_concat on author names.
# TODO: Fix so that authors are reversed.
dfNames <- ddply(dfData6, .(ID), summarise, authors = paste(author,sep=";",collapse=";"))

# Merge new names back into dataset
dfData5 <- merge(dfData4,dfNames)

# Make the prior data frame unique and extract the vars of interest
dfData6 <- unique(dfData5[,c("ID", "author", "authors", "publicationTitle","title","bookTitle", "date", "notetitle")])

# Extract the year from the data
dfData6$date <- substr(as.character(dfData6$date),1,4)

#order by top folder
#dfData6 <- dfData6[order(dfData6$top.sub.folder),]

#convert all factor to character
dfData6 <- as.data.frame(lapply(dfData6,as.character),stringsAsFactors=F)

#Combine book-article so that I know what book an article or chapter came from.
dfData6$bookjournal <- with(dfData6, ifelse(is.na(bookTitle), title,paste(bookTitle,title,sep="::")))

# Drop unnecessary Var
#dfData7 <- dfData6[,-7]

write.csv(dfData6,"/tmp/ZoteroCitations.csv")
# TODO: find some excel package that doesn't have 256 character limit because
# some abstracts are really long....
[/code]