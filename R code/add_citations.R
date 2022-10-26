# Library to join based on regex.
library("fuzzyjoin")

# Read in citations from the paper.
citations <- read.csv('citations.csv', header = FALSE)
citations <- cbind(citations, 1:nrow(citations))
colnames(citations) <- c('Title', 'Citation_Number')
dim(citations)

# Read in annotation file including selected articles.
annotations_all <- read.csv("../Data/article_charting_101922.csv", skip = 1)
annotations <- annotations_all %>% filter(Include_FR_SY == 1)
dim(annotations)

# Merge the two files
my_citations <- citations %>% regex_right_join(annotations,
                                               by = c(Title = "Title"))
dim(my_citations)

# Remove title from citations files and rename title form annotation file.
my_citations<- select(my_citations, - Title.x)
my_citations<- rename(my_citations, Title = Title.y)
colnames(my_citations)
head(my_citations)

# Save the file.
write.csv(my_citations, "../Data/article_charting_101922_with_citations.csv")


# test <- read.csv("../Data/article_charting_101722_with_citations.csv")
 # head(test)
#dim(my_citations)
