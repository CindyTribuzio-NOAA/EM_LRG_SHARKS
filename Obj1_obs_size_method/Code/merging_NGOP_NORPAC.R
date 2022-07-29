### Set the directory:

# setwd( dir )
#setwd("C:/Users/diver/Documents/EM")
getwd()
norpac <- read.csv("norpac_catch_report.csv")
ngop <- read.csv("NPGOP_PSS_length.csv", col.names = c("Cruise","Permit","Year","Observer","Vessel","Gear","Date","Haul","Size_est","Visual_cue","Total_haul_PSS","Census","Sex","Photo_taken","Wt","PCL","Specimen_YN","Specimen_num","Notes","Notes2"))
##I don't know why, but it wouldn't import the name "Cruise" correctly, so I had to set all columns manually.
norpac1PSS <- which(norpac$Sample.Number == 1)
norpac1PSSonly <-norpac[norpac1PSS,]
norpac1PSSonlywithweights<-norpac1PSSonly[norpac1PSSonly$Sample.Weight..kg. !=0,]
mean(norpac1PSSonlywithweights$Sample.Weight..kg.)
median(norpac1PSSonlywithweights$Sample.Weight..kg.)


head(norpac)
head(ngop)

str(norpac)
str(ngop)

## Both datasets look good, and they have the variables that
## we're interested in.

## The only thing to do, at the moment, is to convert
## the 'Haul' variable (in the NGOP data) to an integer type.
## Currently, it's a factor variable...

ngop$Haul <- as.integer( as.character(ngop$Haul) )


### Now, we can merge the twod dataframes together by their
### Permit IDs and Haul IDs (as well as):

df <- merge(x = ngop, y = norpac,
            by = c("Permit", "Haul", "Cruise", "Year"),
            all.x = T)
head(df)
tail(df)
str(df)

## Looks good. I think(?)

## Save the merged dataset into a CSV file:
#write.csv(df, "merged_NGOP_NORPAC_7-18-2022.csv", row.names = F)

dfnew <- data.frame(df$Date,df$Size_est, df$Total_haul_PSS, df$Wt, df$PCL, df$Official.Total.Catch..mt., df$Sample.Size, df$Sample.Number, df$Sample.Weight..kg.)
# Focusing on more relevant columns

only1PSS <- which(dfnew$df.Sample.Number == 1)
#including only the hauls which had 1 PSS (so hopefully less extrapolated weight data)
dfonly1PSS <- dfnew[only1PSS,]
dfonly1PSSwithweights <- dfonly1PSS[dfonly1PSS$df.Sample.Weight..kg. !=0,]
#Excluding those which had weights of 0- I'm not sure why that happens, even.


S_sharks <- data.frame(dfonly1PSSwithweights[which(dfonly1PSSwithweights$df.Size_est == "S"),])
M_sharks <- data.frame(dfonly1PSSwithweights[which(dfonly1PSSwithweights$df.Size_est == "M"),])
L_sharks <- data.frame(dfonly1PSSwithweights[which(dfonly1PSSwithweights$df.Size_est == "L"),])

mean(S_sharks$df.Sample.Weight..kg.)
mean(M_sharks$df.Sample.Weight..kg.)
mean(L_sharks$df.Sample.Weight..kg.)
median(S_sharks$df.Sample.Weight..kg.)
median(M_sharks$df.Sample.Weight..kg.)
median(L_sharks$df.Sample.Weight..kg.)

Sharkweights <- rbind(S_sharks,M_sharks, L_sharks)

boxplot(S_sharks$df.Sample.Weight..kg., xlab = "Small Sharks", ylab = "kg")
abline(h= mean(S_sharks$df.Sample.Weight..kg.), col = "blue", lwd=3)

boxplot(M_sharks$df.Sample.Weight..kg.)
abline(h= mean(M_sharks$df.Sample.Weight..kg.), col = "blue", lwd=3)

boxplot(L_sharks$df.Sample.Weight..kg.)
abline(h= mean(L_sharks$df.Sample.Weight..kg.), col = "blue", lwd=3)

Sharkweights$df.Size_est <- factor(Sharkweights$df.Size_est, levels=c("S","M","L"))
boxplot(df.Sample.Weight..kg.~df.Size_est, data=Sharkweights,main="Weight Assigned to Sharks by Size Category", xlab="Size Class",ylab="Weight(in kg)", col="lightblue", border= "black")

NORPAC_avg_weight <- norpac$Sample.Weight..kg./norpac$Sample.Number #sample weights divided by sample numbers
NORPAC_avg_weight <- NORPAC_avg_weight[NORPAC_avg_weight !=0] #removing all the 0s
NORPAC_avg_weight <- NORPAC_avg_weight[!is.na(NORPAC_avg_weight)] #noooow removing all the NAs
NORPAC_avg_weight
mean(NORPAC_avg_weight)
median(NORPAC_avg_weight)

norpac_longline <- norpac[c(norpac$Gear.Description=="LONGLINER"),] #norpac data but only with longline vessels
longline_avg_weight <- norpac_longline$Sample.Weight..kg./norpac_longline$Sample.Number #sample weights divided by sample numbers
longline_avg_weight <- longline_avg_weight[longline_avg_weight !=0] #removing all the 0s
longline_avg_weight <- longline_avg_weight[!is.na(longline_avg_weight)] #noooow removing all the NAs
mean(longline_avg_weight)
median(longline_avg_weight)
