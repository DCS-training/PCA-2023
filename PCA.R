library(tidyverse)
library(rlang)

install.packages("rlang")
install.packages("ggcorrplot")
library(ggcorrplot)
DataSet <- read_csv("data/simd2020_withinds_toTest.csv")

#Let's Look at it
#Some data are weird e.g. Percentages has a % symbol and NUll value are encoded as *

#Let's Clean But first subset for only the columns we are interested in 
SubsetDataSet<-DataSet[,c(3,4,18,20,29,32,33,38,39,43,49)]

#Step 1 Remove % and * and re-encode as Continuous values +factors
SubsetDataSetCleaned <- mutate_if(SubsetDatabase, 
                             is.character, 
                             str_replace_all, 
                             pattern = "[%*]", 
                             replacement = "")

SubsetDataSetCleaned <- na_if(SubsetDataSetCleaned, '')

SubsetDataSetCleaned <- SubsetDataSetCleaned %>% 
  mutate_at(c(2:11), as.numeric)

#Step 1 Remove Null Values 
# Check for Columns with Null Values
colSums(is.na(SubsetDataSetCleaned))

#Council_area, Total_population, income_rate, employment_rate, overcrowded_rate, nocentralheating_rate, drive_primary are ok 

#not_participating, University, and Broadband are okish we can just remove those Attendance is around 2% so we should discuss but for now let's just remove all null values

DatasetNoNull <-na.omit(SubsetDataSetCleaned)

#Check again
colSums(is.na(DatasetNoNull))

library(tidyverse)
#Group by constituency
Costituencies <- DatasetNoNull%>% 
  group_by(Council_area)%>% 
  summarise(Total_population = sum(Total_population),
            income_rate=mean(income_rate),
            employment_rate=mean(employment_rate),
            Attendance=mean(Attendance),
            not_participating=mean(not_participating),
            University=mean(University),
            overcrowded_rate=mean(overcrowded_rate),
            nocentralheating_rate=mean(nocentralheating_rate),
            drive_primary=mean(drive_primary),
            broadband=mean(broadband),)


#Subset only numeric
numerical_data <- Costituencies[,2:11]

head(numerical_data)





#now we normalise 
data_normalized <- scale(numerical_data)
head(data_normalized)

#Plot Matrix
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)

data_pca <- princomp(corr_matrix)
summary(data_pca)


data_pca$loadings[, 1:2]

install.packages("factoextra")
install.packages("cli")
library(factoextra)
#factoextra
fviz_eig(data_pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data_pca, col.var = "black")



fviz_cos2(data_pca, choice = "var", axes = 1:2)


fviz_pca_var(data_pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

#Compute the PCS
pcs <- prcomp(numerical_data)
plot(pcs)
print(pcs)
#let's add the PC1 and PC2 info to the  dataset
pcDataset <- data_normalized
pc1 <- as.data.frame(cbind(pcs$x[,1], pcs$x[,2]))
pcDataset$pc2 <- pcs$x[,2]
str(pcDataset)

pcDataset <-cbind(pc1, Costituencies)


pcs$x[,1]
pcs$x[,2]
#Finally let's plot it 
ggplot(pcDataset, aes(x=V1, y=V2, color=Council_area, label=Council_area)) + geom_point(size=8, alpha=0.5)+theme_bw()+ labs(title = "Council")+geom_text(size=3)+ theme(legend.position = "none")  

ggplot(pcIris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris") 

#Better see the clusters
library(plyr)

find_hull <- function(pcDataset) pcDataset[chull(pcDataset$V1, pcDataset$V2), ]
hulls <- ddply(pcDataset,"Council_area", find_hull)

ggplot(pcDataset, aes(x=V1, y=V2, color=Council_area)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Try")+ geom_polygon(data=hulls, alpha=.2, aes(fill=Council_area))

#Plot the variance percentage impact of PC1 and PC2
pcDataset <- subset(pcDataset, select=c(V1:Council_area))
percentage <- round((pcs$sdev*pcs$sdev) / sum((pcs$sdev*pcs$sdev)) * 100, 2)
percentage <- paste( colnames(pcDataset), "(",paste(as.character(percentage), "%", ")", sep="") )

ggplot(pcDataset, aes(x=V1, y=V2, color=Council_area)) + geom_point(size=6, alpha=0.5)+theme_bw()+ labs(title = "Iris")+ xlab(percentage[1]) + ylab(percentage[2])

#other option
install.packages("ggfortify")
library(ggfortify)
autoplot(pcs, data=iris, size=4, alpha=0.5, colour='Species', loadings=TRUE,loadings.label = TRUE )+theme_bw()

