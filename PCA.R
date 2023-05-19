#Mount the libraries we need 
library(tidyverse)
library(ggcorrplot)
library(factoextra)
library(plyr)

# Data set 1:SIMD Data ######
## Import the First Data set ========
SIMD <- read_csv("data/simd2020_withinds_toTest.csv")

#Let's Look at it
#Some data are weird e.g. Percentages has a % symbol and Null value are encoded as *

## Data Wrangling and Cleaning ========

#Let's Clean But first subset for only the columns we are interested in 
SIMDSubset<-SIMD[,c(3,18,20,29,32,33,38,39,49)]# selecting by position 

#Step 1 Remove % and * and re-encode as Continuous values
SIMDCleaned <- mutate_if(SIMDSubset, #change if is character
                         is.character, 
                         str_replace_all, 
                         pattern = "[%*]", #What I am searching
                         replacement = "")#What I am replacing with

#Now that we have removed all the weird symbols we can transform the columns containing numeric values into proper numeric ones 
SIMDCleaned <- SIMDCleaned %>% 
  mutate_at(c(2:9), as.double)

## Getting set for PCA (Remove null values and subset by numeric) =====
#Step 1 Remove Null Values 
# Check for Columns with Null Values
colSums(is.na(SIMDCleaned))

#Council_area, income_rate, employment_rate, overcrowded_rate, nocentralheating_rate, are ok 

#not_participating, University, and Broadband are ok-ish we can just remove those Attendance is around 2% so we should discuss but for now let's just remove all null values

SIMDNoNull <-na.omit(SIMDCleaned)#This will blankly remove all row when you have at least one null value (nb do it after you already drop all the columns you do not want to use or you will drop rows you should not)

#Check again
colSums(is.na(SIMDNoNull))

#University is the only positive one so we actually may want to flip it so we get % of people that did not go to University rather than the percentage that went 
SIMDNoNull$University <-(100-SIMDNoNull$University)

#Since we want to look at by authority/constituency lets
#Group by constituency
Costituencies <- SIMDNoNull%>% 
  dplyr::group_by(Council_area)%>% 
  dplyr::summarise(income_rate=mean(income_rate),
            employment_rate=mean(employment_rate),
            Attendance=mean(Attendance),
            not_participating=mean(not_participating),
            University=mean(University),
            overcrowded_rate=mean(overcrowded_rate),
            nocentralheating_rate=mean(nocentralheating_rate),
            broadband=mean(broadband))


#Subset only continuous variables
SIMDNumerical <- Costituencies[,2:9]#selecting all rows and only columns 2-9
#Let's check them 
head(SIMDNumerical)

#now we normalise. Scale them on the average of the variable so we look at deviation from the mean. standardize the numerical data by subtracting the mean and dividing by the standard deviation 
SIMDNormalized <- scale(SIMDNumerical)
head(SIMDNormalized)


#Plot Matrix to check correlation between variables
SIMDCorrMatrix <- cor(SIMDNormalized)
ggcorrplot(SIMDCorrMatrix)

SIMDpca <- princomp(SIMDCorrMatrix)#Perform PCA on the correlation matrix
summary(SIMDpca)#let see how much each PC count for 
SIMDpca$loadings[, 1:2]#Lets look at loadings (impact of each variables, our ingredients)

## Visualise the results ====

fviz_eig(SIMDpca, addlabels = TRUE)#Let's check the scree plot (of the eigen values) for the % of PC1 and PC2 

# Graph of the variables
fviz_pca_var(SIMDpca, col.var = "black")#check impact of each variables
#First, all the variables that are grouped together are positively correlated to each other
#Then, the higher the distance between the variable and the origin, the better represented that variable is. 
#Finally, variables that are negatively correlated are displayed to the opposite sides of the biplot’s origin. 
fviz_cos2(SIMDpca, choice = "var", axes = 1:2)# Calculate how much each values impacted in PCA lower values means not very well represented
fviz_pca_var(SIMDpca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)#See it in relation to their correlation

#Compute the PCS in a different way to check if we can see patterns against factor variables 
pcsSIMD <- prcomp(SIMDNormalized)#do PCA on our normalised data. This time because we want to see each eigenvalue rather thant the total loadings we do it on the normalised data rather than the correlation matrix 
print(SIMDpca)
#let's add the PC1 and PC2 info to the  data set
pc1_2 <- as.data.frame(cbind(PC1=pcsSIMD$x[,1], PC2=pcsSIMD$x[,2]))
pcSIMDData <-cbind(pc1_2, Costituencies)

#Finally let's plot it 
ggplot(pcSIMDData, aes(x=PC1, y=PC2, color=Council_area, label=Council_area)) +
  geom_point(size=8, alpha=0.5)+
  theme_bw()+
  labs(title = "Council")+geom_text(size=3)+
  theme(legend.position = "none")+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

#last check does it cluster better than by using two of the variable alone
# Let see for example income_rate and employment_rate
ggplot(pcSIMDData, aes(x=income_rate, y=not_participating, color=Council_area, label=Council_area)) +
  geom_point(size=8, alpha=0.5)+
  theme_bw()+
  labs(title = "Council")+geom_text(size=3)+
  theme(legend.position = "none")+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)
#Now play around with this last graph and see if the partial pattern we saw is visible across only two variables

# Data set 2: Authority data ######
## Import the Second Data set =======
CostOfLiving <- read_csv("data/AuthorityData.csv",locale = locale(encoding = "WINDOWS-1252"))

## Data Wrangling and Cleaning =========
#Let's Look at it
#There are a lot of empty rows and the first one is actually the total across Scotland so let's remove the first row and all rows past 33
CostOfLiving<- CostOfLiving[2:33,]#All column but only row 2:33

#Some data are weird e.g. Percentages has a % symbol and there are £ symbols in there 

#Remove % and £ and re-encode as Continuous values
CostOfLivingCleaned <- mutate_if(CostOfLiving, 
                                 is.character, 
                                 str_replace_all, 
                                 pattern = "[%£]", 
                                 replacement = "")

#Some columns have , rather than . as decimal point 
CostOfLivingCleaned <- mutate_if(CostOfLivingCleaned, 
                                 is.character, 
                                 str_replace_all, 
                                 pattern = "[,]", 
                                 replacement = ".")
#Now re-encode numerical columns as such
CostOfLivingCleaned <- CostOfLivingCleaned %>% 
  mutate_at(c(4:23), as.double)

#We want to look at Change rather than general number so we want to look at increase % But first let's see where the null values are 
colSums(is.na(CostOfLivingCleaned))

#Since we have few records lets only use what we have all data from so 
#Food insecurity
#House prices
#Welfare applications
#Homeless applications
#Rent
#Because some of these will be connected to population we want to look at % increase/decrease rather than the values themselves
#So the first step is to generate 6 new columns that will calculate this % of increase
Evolution <-CostOfLivingCleaned%>%
  mutate(FoodInsecurity=round(((`food_insecurity2018-2022`-`food_insecurity2017-2021`)/`food_insecurity2017-2021`)*100,1),
         HousePrices=round(((house_price_jul_22-house_price_jul_21)/house_price_jul_21)*100,1),
         WelfareApp=round(((`welfare_applications2021-2022`-`welfare_applications2020-2021`)/`welfare_applications2020-2021`)*100,1),
         Rent=round(((average_rent_2022-average_rent_2021)/average_rent_2021)*100,1),
         Homeless=round(((`homelessness_applications 2021-2022`-`homelessness_applications2020-21`)/`homelessness_applications2020-21`)*100,1))%>%
  select(authority,location,region,FoodInsecurity,HousePrices,WelfareApp,Rent,Homeless)#Select only the textual columns and the newly created ones

## Getting Set for PCA =====
#Check again
colSums(is.na(Evolution))

#Extract the numerical variables
numericalEvolution<- Evolution[,4:8]
head(numericalEvolution)
#Now we normalise. Scale them on the average of the variable so we look at deviation from the mean. standardize the numerical data by subtracting the mean and dividing by the standard deviation 
EvolutionNormalised <- scale(numericalEvolution)
head(EvolutionNormalised)
#Plot Matrix to check correlation between variables
EveolCorrMatrix<- cor(EvolutionNormalised)
ggcorrplot(EveolCorrMatrix)

EvolPCA <- princomp(EveolCorrMatrix)
EvolPCA$loadings[, 1:2]

## Visualise the results ====
fviz_eig(EvolPCA, addlabels = TRUE)#Let's check the scree plot (of the eigen values) for the % of PC1 and PC2 not ideally but hopefully still significant

# Graph of the variables
fviz_pca_var(EvolPCA, col.var = "black")#check impact of each variables so we can see if some of them are not very well portrayed in our analysis 
#First, all the variables that are grouped together are positively correlated to each other
#Then, the higher the distance between the variable and the origin, the better represented that variable is. 
#Finally, variables that are negatively correlated are displayed to the opposite sides of the biplot’s origin. 
fviz_cos2(EvolPCA, choice = "var", axes = 1:2)# Calculate how much each values impacted in PCA lower values means not very well represented
fviz_pca_var(EvolPCA, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

#Compute the PCS again but this time on the single records rather than on the correlation matrix
EvolPCS <- prcomp(EvolutionNormalised)
print(EvolPCS)
#let's add the PC1 and PC2 info to the  dataset
pc1_2 <- as.data.frame(cbind(PC1=EvolPCS$x[,1], PC2=EvolPCS$x[,2]))
EvolPCSData <-cbind(pc1_2, Evolution)
#Finally let's plot it 
ggplot(EvolPCSData, aes(x=PC1, y=PC2, color=location, label=authority)) +
  geom_point(size=8, alpha=0.5)+
  theme_bw()+ 
  labs(title = "Authority")+
  geom_text(size=3)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept=0)

#Better see the clusters
find_hull <- function(EvolPCSData) EvolPCSData[chull(EvolPCSData$PC1, EvolPCSData$PC2), ]
hulls <- ddply(EvolPCSData,"location", find_hull)

ggplot(EvolPCSData, aes(x=PC1, y=PC2, color=location,label=authority)) +
  geom_point(size=6, alpha=0.5)+
  theme_bw()+
  labs(title = "Try")+
  geom_polygon(data=hulls, alpha=.2, aes(fill=location))+
  geom_text(size=3)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept=0)
#Exercise try to do the same but using region as categorical value 

#Does single variables explain more #last check does it cluster better than by using two of the variable alone
# Let see for example Rent and Welfare Applications
ggplot(EvolPCSData, aes(x=Rent, y=WelfareApp, color=location, label=authority)) +
  geom_point(size=8, alpha=0.5)+
  theme_bw()+
  labs(title = "Authority")+
  geom_text(size=3)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)
