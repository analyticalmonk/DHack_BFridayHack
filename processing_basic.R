### functions for basic data processing
# 1. Removing redundant columns
# 2. Converting two-element columns to binary
# 3. Removing duplicate columns
# 4. Removing duplicate rows


## loading libraries
library(plyr)


## function for removing constant columns
remove_redundant_columns <- function()
{
    count_unique <- lapply(X_train, function(k){length(unique(k))})
    constant_columns <- names(count_unique[count_unique == 1])
    
    if (length(constant_columns) > 0)
    {
        X_train <<- X_train[,!names(X_train) %in% constant_columns]
        X_test <<- X_test[,!names(X_test) %in% constant_columns]
        
        cat(length(constant_columns), "redundant columns were removed from data\n")
    }else
    {
        cat("No redundant columns found\n")
    }
    
    cat("\n")
}


## function for converting two-element columns to binary
convert_binary_columns <- function()
{
    count_unique <- lapply(X_train, function(k){length(unique(k))})
    binary_columns <- names(count_unique[count_unique == 2])
    
    changes <- 0
    
    for (i in which(colnames(X_train) %in% binary_columns))
    {
        if (all(unique(X_train[,i]) %in% c(0,1)) != T)
        {
            X_test[,i] <<- as.numeric(factor(X_test[,i], levels=X_train[,i])) - 1
            X_test[is.na(X_test[,i]),i] <<- -1
            
            X_train[,i] <<- as.numeric(as.factor(X_train[,i])) - 1
            
            changes <- changes+1
            
            cat("Column", colnames(X_train)[i], "converted to binary column\n")
        }
    }
    
    if (changes == 0)
    {
        cat("No binary columns found\n")
    }
    
    cat("\n")
}


## function for removing duplicate columns
remove_duplicate_columns <- function()
{
    dups <- sum(duplicated(lapply(X_train,c)))
    
    if (dups > 0)
    {
        X_train <<- X_train[!duplicated(lapply(X_train,c))]
        X_test <<- X_test[,colnames(X_test) %in% colnames(X_train)]
        
        cat(dups, "duplicate columns removed from data\n")
    }else
    {
        cat("No duplicate columns found\n")
    }
    
    cat("\n")
}


## function for removing duplicate rows
remove_duplicate_rows <- function()
{
    if (length(which(duplicated(X_train))) > 0)
    {
        X_train <<- X_train[!duplicated(X_train),]
        
        cat(length(which(duplicated(X_train))), "duplicate rows removed from data\n")
    }else
    {
        cat("No duplicate rows found\n")
    }
}
