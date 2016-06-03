Convert.to.transactions <- function(dataset_){
        
        print("Starting to convert dataset into transactions data")
        # Arrange columns
        dataset_ = dataset_[,order(names(dataset_))]
        
        # Get row numbers
        rowNumbers <- seq(nrow(dataset_))
        
        # Group row numbers as per each column and category
        row.numbers.grouped <- lapply(dataset_, function(x) tapply(rowNumbers, x, eval, simplify = FALSE))
        
        # Each column categories
        factors.names <- unlist(lapply(row.numbers.grouped, names), use.names = FALSE)
        
        # Each column names times their categories
        column.initials <- rep(names(row.numbers.grouped), sapply(row.numbers.grouped, length))

        # Simplifiy the list by converting it to simple vectors with index
        row.numbers.unlisted <- unlist(row.numbers.grouped, recursive = FALSE, use.names = FALSE)
        
        # Count length of each elements in list. basically it is a support for each column and particular factor value
        support.per.column <- sapply(row.numbers.unlisted, length)
        
        # Simply it to column numbers only
        row.numbers.unlisted <- unlist(row.numbers.unlisted, use.names = FALSE)
        
        # Initialize transaction matrix with proper dimensions
        trans.matrix = matrix(0,nrow = dim(dataset_)[1],ncol = length(support.per.column))
        
        
        column = 1
        x1 = row.numbers.unlisted
        
        # Iterate over row numbers vector based on support value
        # If I have 430 support then I will iterate row numbers vector from 1:430 and remove these elements from next iteration
        # Every time, we will have to filter data from 1st element
        for(i in support.per.column)
        {
                counter = 0
                temp = x1[1:i]
                #print(temp)
                for(j in temp)
                {
                        #print(j)
                        trans.matrix[j,column] = 1
                }
                x1 = x1[-(1:i)]
                column = column + 1
        }
        
        
        trans.df = as.data.frame(trans.matrix)
        names(trans.df) = paste(column.initials, factors.names, sep = "=")
        print("Succesfully converted to transactions data")
        
        return(trans.df)
}

# Calculate support
calc.support <- function(items_,transactions_){
        
        support.vector = NULL
        text = ""
        
        for(j in 1:length(items_))
        {
                text = paste(text,items_[j])
                if(is.null(support.vector))
                {
                        support.vector = transactions_[,items_[j]]
                }else{
                        support.vector = support.vector * transactions_[,items_[j]]
                }
        }
        support = sum(support.vector)/nrow(transactions_)
        return(support)
        
}

#Choose method
generate.itemsets <- function(transactionData_,minsup_=0,methodType_=1){
        
        if(methodType_ == 2){
                print(paste0("Generating frequent and candidate itemsets using fk-1 x fk-1 method, min support: ",minsup_))
                cand.and.fre.itemsets = generate.itemsets.k_1xk_1(transactionData_,minsup_)
                
                print(paste0("frequent and candidate itemsets are successfully generated using fk-1 x fk-1 method, min support: ",minsup_))
        }else{
                print(paste0("Generating frequent and candidate itemsets using fk-1 x f1 method, min support: ",minsup_))
                cand.and.fre.itemsets = generate.itemsets.k_1x1(transactionData_,minsup_)
                
                print(paste0("frequent and candidate itemsets are successfully generated using fk-1 x f1 method, min support: ",minsup_))
        }
        
        
        return(cand.and.fre.itemsets)
        
}

#Handle directory

manage.dir <- function(datasetName_,methodtype_,itemsetType_){

        if(!dir.exists(paste0(outputDir,datasetName_)))
        {
                dir.create(paste0(outputDir,datasetName_))
        }
        if(!dir.exists(paste0(outputDir,datasetName_,"/",itemsetType_))){
                dir.create(paste0(outputDir,datasetName_,"/",itemsetType_))
                
        }
        if(!dir.exists(paste0(outputDir,datasetName_,"/",itemsetType_,"/",methodtype_))){
                dir.create(paste0(outputDir,datasetName_,"/",itemsetType_,"/",methodtype_))
        }
        
        final.dir =  paste0(outputDir,datasetName_,"/",itemsetType_,"/",methodtype_,"/")
        return(final.dir)
}


# Save results
write.list <- function(input_list_,datasetName_,methodtype_,itemsetType_,outputDir_){
        
        names(input_list_) = seq(1:length(input_list_))
        
        final.dir = manage.dir(datasetName_,methodtype_,itemsetType_)
                
        output = sapply(names(input_list_), 
               function (x) write.csv(input_list_[[x]], file=paste0(final.dir,"itemset_",x, ".csv")))
        
}

write.custom.csv <- function(input_df_,datasetName_,methodtype_,itemsetType_,outputDir_){
        
        final.dir = manage.dir(datasetName_,"rules",itemsetType_)
        
        write.csv(input_df_,paste0(final.dir,methodtype_,".csv"))
        
}