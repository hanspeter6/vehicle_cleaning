# libraries
library(tidyverse)

## reading in relevant sets:

# for newspapers
newspapers_engagement_02 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/newspapers_engagement_02.rds")
newspapers_engagement_05 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/newspapers_engagement_05.rds")
newspapers_engagement_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/newspapers_engagement_08.rds")
newspapers_engagement_10 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/newspapers_engagement_10.rds")
newspapers_engagement_12 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/newspapers_engagement_12.rds")
newspapers_engagement_14 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/newspapers_engagement_14.rds")

# check how many and which are not the same between 02 and 05
names(newspapers_engagement_05)[which(!names(newspapers_engagement_05) %in% names(newspapers_engagement_02))]
names(newspapers_engagement_02)[which(!names(newspapers_engagement_02) %in% names(newspapers_engagement_05))]

# check how many and which are not the same between 05 and 08
names(newspapers_engagement_08)[which(!names(newspapers_engagement_08) %in% names(newspapers_engagement_05))]
names(newspapers_engagement_05)[which(!names(newspapers_engagement_05) %in% names(newspapers_engagement_08))]

# check how many and which are not the same between 08 and 10
names(newspapers_engagement_10)[which(!names(newspapers_engagement_10) %in% names(newspapers_engagement_08))]
names(newspapers_engagement_08)[which(!names(newspapers_engagement_08) %in% names(newspapers_engagement_10))]

# check how many and which are not the same between 08 and 10
names(newspapers_engagement_12)[which(!names(newspapers_engagement_12) %in% names(newspapers_engagement_10))]
names(newspapers_engagement_10)[which(!names(newspapers_engagement_10) %in% names(newspapers_engagement_12))]

# check how many and which are not the same between 08 and 10
names(newspapers_engagement_14)[which(!names(newspapers_engagement_14) %in% names(newspapers_engagement_12))]
names(newspapers_engagement_12)[which(!names(newspapers_engagement_12) %in% names(newspapers_engagement_14))]

# for magazines
magazines_engagement_02_all <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/magazines_engagement_02_all.rds")
magazines_engagement_05_all <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/magazines_engagement_05_all.rds")
magazines_engagement_08_all <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/magazines_engagement_08_all.rds")
magazines_engagement_10_all <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/magazines_engagement_10_all.rds")
magazines_engagement_12_all <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/magazines_engagement_12_all.rds")
magazines_engagement_14_all <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/magazines_engagement_14_all.rds")

## identify which titles in any of the periods had more than 5% response:

# function to extract names with more than 5% response:
big_mags <- function(x, cut = 0.05) {# x = dataframe; cut = cutoff with default of 5%
        temp1 <- ifelse(x != 0,1,0)
        temp2 <- colSums(temp1)
        ind <- which(temp2 >= cut * nrow(x))
        names(data.frame(x[,ind]))
}

# create list of target objects
mags_list <- list(magazines_engagement_02_all,
                  magazines_engagement_05_all,
                  magazines_engagement_08_all,
                  magazines_engagement_10_all,
                  magazines_engagement_12_all,
                  magazines_engagement_14_all)

# identify all unique mag names with more than 5% response in any of the years
all_big_mags <- unique(unlist(lapply(mags_list, big_mags)))

## create set for each period that contains only these magazines, placing the mean of all the others in an "other.mags" variable

# function to create new set with only desired variables and the mean of the rest grouped into other.mags
with_others <- function(x) {# x = dataframe
        ind_keep <- which(names(x) %in% all_big_mags)
        other.mags <- as.vector(apply(x[,-ind_keep], 1, mean))
        cbind(x[,ind_keep], other.mags)
}

magazines_engagement_02_other <- with_others(magazines_engagement_02_all)
magazines_engagement_05_other <- with_others(magazines_engagement_05_all)
magazines_engagement_08_other <- with_others(magazines_engagement_08_all)
magazines_engagement_10_other <- with_others(magazines_engagement_10_all)
magazines_engagement_12_other <- with_others(magazines_engagement_12_all)
magazines_engagement_14_other <- with_others(magazines_engagement_14_all)

# describe or identify important changes year on year as in newspapers above
names(magazines_engagement_05_other)[which(!names(magazines_engagement_05_other) %in% names(magazines_engagement_02_other))]
names(magazines_engagement_02_other)[which(!names(magazines_engagement_02_other) %in% names(magazines_engagement_05_other))]

names(magazines_engagement_08_other)[which(!names(magazines_engagement_08_other) %in% names(magazines_engagement_05_other))]
names(magazines_engagement_05_other)[which(!names(magazines_engagement_05_other) %in% names(magazines_engagement_08_other))]

names(magazines_engagement_10_other)[which(!names(magazines_engagement_10_other) %in% names(magazines_engagement_08_other))]
names(magazines_engagement_08_other)[which(!names(magazines_engagement_08_other) %in% names(magazines_engagement_10_other))]

names(magazines_engagement_12_other)[which(!names(magazines_engagement_12_other) %in% names(magazines_engagement_10_other))]
names(magazines_engagement_10_other)[which(!names(magazines_engagement_10_other) %in% names(magazines_engagement_12_other))]

names(magazines_engagement_14_other)[which(!names(magazines_engagement_14_other) %in% names(magazines_engagement_12_other))]
names(magazines_engagement_12_other)[which(!names(magazines_engagement_12_other) %in% names(magazines_engagement_14_other))]

# for radio
radio_engagement_02 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/radio_engagement_02.rds")
radio_engagement_05 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/radio_engagement_05.rds")
radio_engagement_08 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/radio_engagement_08.rds")
radio_engagement_10 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/radio_engagement_10.rds")
radio_engagement_12 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/radio_engagement_12.rds")
radio_engagement_14 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/radio_engagement_14.rds")

## identify which titles in any of the periods had more than 5% response:

# function to extract names with more than 5% response:
big_radios <- function(x, cut = 0.05) {# x = dataframe; cut = cutoff with default of 5%
        temp1 <- ifelse(x != 0,1,0)
        temp2 <- colSums(temp1)
        ind <- which(temp2 >= cut * nrow(x))
        names(data.frame(x[,ind], check.names = FALSE))
}

# create list of target objects
radios_list <- list(radio_engagement_02,
                  radio_engagement_05,
                  radio_engagement_08,
                  radio_engagement_10,
                  radio_engagement_12,
                  radio_engagement_14)

# identify all unique mag names with more than 5% response in any of the years
all_big_radios <- unique(unlist(lapply(radios_list, big_radios)))

## create set for each period that contains only these radios, placing the mean of all the others in an "other.radio" variable

# function to create new set with only desired variables and the mean of the rest grouped into other.mags
with_others <- function(x) {# x = dataframe
        ind_keep <- which(names(x) %in% all_big_radios)
        other.radio <- as.vector(apply(x[,-ind_keep], 1, mean))
        cbind(x[,ind_keep], other.radio)
}

radio_engagement_02_other <- with_others(radio_engagement_02)
radio_engagement_05_other <- with_others(radio_engagement_05)
radio_engagement_08_other <- with_others(radio_engagement_08)
radio_engagement_10_other <- with_others(radio_engagement_10)
radio_engagement_12_other <- with_others(radio_engagement_12)
radio_engagement_14_other <- with_others(radio_engagement_14)

# describe or identify important changes year on year as in newspapers above
names(radio_engagement_05_other)[which(!names(radio_engagement_05_other) %in% names(radio_engagement_02_other))]
names(radio_engagement_02_other)[which(!names(radio_engagement_02_other) %in% names(radio_engagement_05_other))]

names(radio_engagement_08_other)[which(!names(radio_engagement_08_other) %in% names(radio_engagement_05_other))]
names(radio_engagement_05_other)[which(!names(radio_engagement_05_other) %in% names(radio_engagement_08_other))]

names(radio_engagement_10_other)[which(!names(radio_engagement_10_other) %in% names(radio_engagement_08_other))]
names(radio_engagement_08_other)[which(!names(radio_engagement_08_other) %in% names(radio_engagement_10_other))]

names(radio_engagement_12_other)[which(!names(radio_engagement_12_other) %in% names(radio_engagement_10_other))]
names(radio_engagement_10_other)[which(!names(radio_engagement_10_other) %in% names(radio_engagement_12_other))]

names(radio_engagement_14_other)[which(!names(radio_engagement_14_other) %in% names(radio_engagement_12_other))]
names(radio_engagement_12_other)[which(!names(radio_engagement_12_other) %in% names(radio_engagement_14_other))]

# for tv & internet: should be pretty straight forward


# save magazine and radio objects:

saveRDS(magazines_engagement_02_other,"magazines_engagement_02_other.rds")
saveRDS(magazines_engagement_05_other,"magazines_engagement_05_other.rds")
saveRDS(magazines_engagement_08_other,"magazines_engagement_08_other.rds")
saveRDS(magazines_engagement_10_other,"magazines_engagement_10_other.rds")
saveRDS(magazines_engagement_12_other,"magazines_engagement_12_other.rds")
saveRDS(magazines_engagement_14_other,"magazines_engagement_14_other.rds")

saveRDS(radio_engagement_02_other,"radio_engagement_02_other.rds")
saveRDS(radio_engagement_05_other,"radio_engagement_05_other.rds")
saveRDS(radio_engagement_08_other,"radio_engagement_08_other.rds")
saveRDS(radio_engagement_10_other,"radio_engagement_10_other.rds")
saveRDS(radio_engagement_12_other,"radio_engagement_12_other.rds")
saveRDS(radio_engagement_14_other,"radio_engagement_14_other.rds")