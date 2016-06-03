# Fk-1 x F1 method to generate combinations
generate.combs.k_1x1 <- function(k.min.1.data_,one.fre.itemsets_){
        one.fre.itemsets_ = as.character(one.fre.itemsets_[,1])
        a = NULL
        #print(k.min.1.data_)
        for(i in 1:(nrow(k.min.1.data_)-1))
        {
                temp.pairs = one.fre.itemsets_
                for(k in 1:length(k.min.1.data_)){
                        startCharacter = strsplit(as.character(k.min.1.data_[i,k]),"=")[[1]][1]
                        index = which(temp.pairs == as.character(k.min.1.data_[i,k]))+1
                        temp.pairs = temp.pairs[index:length(temp.pairs)]
                        temp.data = as.character(temp.pairs[!startsWith(temp.pairs,startCharacter)])
                        temp.data = temp.data[complete.cases(temp.data)]
                }
                #print("**********************")
                #print(temp.data)
                for (j in temp.data) {
                        value = c(as.character(unlist(k.min.1.data_[i,])),j)
                        if(is.null(a))
                        {
                                a = value
                        }
                        else{
                                a = rbind(a,value)
                        }
                        
                }
        }
       
        a.df = as.data.frame(a,row.names = F,stringsAsFactors = F)
        a.df = a.df[complete.cases(a.df),]
        
        #print("----------------")
        #print(a.df)
        if(nrow(a.df)>1){
               
                #print("-----------------")
                #print(a.df)
                names(a.df) = c(names(k.min.1.data_),paste0("item",length(a.df)))
                return(a.df)
              
        }else{
                return(NULL)
        }
        
        
}


# Generate itemsets
generate.itemsets.k_1x1 <- function(transactions.data_,minsup_ = 0.2){
        
        # Find one frequent itemsets  
        n.transactions = nrow(transactions.data_)
        transactions.support = colSums(transactions.data_)
        
        one.itemsets.df_ = data.frame(item1 = names(transactions.support),support = transactions.support/n.transactions,stringsAsFactors = F,row.names = seq(1:length(transactions.support)))
        
        candidate.itemsets = list(one.itemsets.df_)
        one.fre.itemsets.df_ = one.itemsets.df_ %>% filter(support > minsup_)
        
        currentData_ = data.frame(item1 = one.fre.itemsets.df_[,1],stringsAsFactors = F)
        frequent.itemsets = list(one.fre.itemsets.df_)
        fre.count = nrow(currentData_)
        cand.count = nrow(currentData_)
        
        while(T)
        {
                temp.itemset.df = generate.combs.k_1x1(currentData_,one.fre.itemsets.df_)
                
                if(is.null(temp.itemset.df)){
                        break
                }
                cand.count = cand.count + nrow(temp.itemset.df)
                
                temp.itemset.df$support = apply(temp.itemset.df,1,function(x) calc.support(x,transactions.data_))
                candidate.itemsets[length(candidate.itemsets)+1] = list(temp.itemset.df)
                
                temp.itemset.df = temp.itemset.df %>% filter(support > minsup_)
                #print(head(temp.itemset.df))
                if(nrow(temp.itemset.df) < 2){
                        break
                }
                fre.count = fre.count + nrow(temp.itemset.df)
                
                frequent.itemsets[length(frequent.itemsets)+1] = list(temp.itemset.df)
                
                currentData_ = temp.itemset.df[,1:(length(temp.itemset.df)-1)]
                
        }
        
        print(paste0("Number of candidate itemsets: ",cand.count))
        print(paste0("Number of frequent itemsets: ",fre.count))
        
        return(list(candidate.itemsets,frequent.itemsets))
        
}

find.closed.or.maximal.fre.itemsets <- function(frequent.itemsets.df_){
        closedItemsets = NULL
        maximalItemsets = NULL
        #print("Finding closed and maximal itemsets")
        num.closed = 0
        num.maximal = 0
        #frequent.itemsets.df = frequent.itemsets
        for(fre.itemset.index in 1:(length(frequent.itemsets.df_)-1))
        {
                parentData = frequent.itemsets.df_[[fre.itemset.index]]
                #print(parentData)
                childData = frequent.itemsets.df_[[fre.itemset.index+1]]
                closedItemset.df = parentData[1,]
                maximalItemset.df = parentData[1,]
                for(i in 1:nrow(parentData))
                {
                        child.by.parent = childData[apply(childData,1,function(x) mean(parentData[i,1:(length(parentData)-1)] %in% x) == 1),]
                        if(!(parentData[i,length(parentData)] %in% unique(child.by.parent$support))){
                                closedItemset.df = rbind(closedItemset.df,parentData[i,])
                        }
                        
                        if(nrow(child.by.parent) == 0){
                                maximalItemset.df = rbind(maximalItemset.df,parentData[i,])
                        }
                        
                }
                
                closedItemsets[length(closedItemsets)+1] = list(closedItemset.df[-1,])
                maximalItemsets[length(maximalItemsets)+1] = list(maximalItemset.df[-1,])
                num.closed = num.closed + nrow(closedItemset.df) - 1
                num.maximal = num.maximal + nrow(maximalItemset.df) - 1
        }
        
        if(length(frequent.itemsets.df_)<= (fre.itemset.index+1)){
                num.maximal = num.maximal + nrow(frequent.itemsets.df_[[fre.itemset.index+1]])
                num.closed = num.closed + nrow(frequent.itemsets.df_[[fre.itemset.index+1]])
        }
       
        print("Closed and maximal itemsets successfully generated")
        print(paste0("Number of closed itemsets: ",num.closed))
        print(paste0("Number of maximal itemsets: ",num.maximal))
        
        return(list(closedItemsets,maximalItemsets))
}


# Fk-1 x F1 method to generate combinations
generate.2.fre.itemset <- function(k.min.1.data_,one.fre.itemsets_){
        one.fre.itemsets_ = as.character(one.fre.itemsets_[,1])
        a = NULL
        for(i in 1:(nrow(k.min.1.data_)-1))
        {
                temp.pairs = one.fre.itemsets_
                for(k in 1:length(k.min.1.data_)){
                        startCharacter = strsplit(as.character(k.min.1.data_[i,k]),"=")[[1]][1]
                        index = which(temp.pairs == as.character(k.min.1.data_[i,k]))+1
                        temp.pairs = temp.pairs[index:length(temp.pairs)]
                        temp.data = as.character(temp.pairs[!startsWith(temp.pairs,startCharacter)])
                }
                
                for (j in temp.data) {
                        value = c(as.character(unlist(k.min.1.data_[i,])),j)
                        if(is.null(a))
                        {
                                a = value
                        }
                        else{
                                a = rbind(a,value)
                        }
                        
                }
        }
        
        a.df = as.data.frame(a,row.names = F,stringsAsFactors = F)
        a.df = a.df[complete.cases(a.df),]
        if(nrow(a.df)>1)
        {
                names(a.df) = c(names(k.min.1.data_),paste0("item",length(a.df)))
                
        }
        return(a.df)
}

generate.combs.k_1xk_1 <- function(k_1Data1_){
        
        k_1Data1_ = k_1Data1_[,1:(length(k_1Data1_)-1)]
        a = NULL
        for(i in 1:(nrow(k_1Data1_)-1))
        {
                filtered.rows = apply(k_1Data1_,1,function(x) identical(as.character(k_1Data1_[i,(1:length(k_1Data1_)-1)]),as.character(x[(1:length(x))-1])))
                temp.pairs = k_1Data1_[filtered.rows,length(k_1Data1_)]
                
                if(length(temp.pairs) == 0)
                        next
                
                for(k in 1:length(k_1Data1_)){
                        
                        startCharacter = strsplit(as.character(k_1Data1_[i,k]),"=")[[1]][1]
                        any.duplicates = temp.pairs == as.character(k_1Data1_[i,k])
                        if(sum(any.duplicates) != 0 )
                        {
                                index = which(temp.pairs == as.character(k_1Data1_[i,k])) + 1
                                temp.pairs = temp.pairs[index:length(temp.pairs)]
                                temp.data = as.character(temp.pairs[!startsWith(temp.pairs,startCharacter)])
                                temp.data = temp.data[complete.cases(temp.data)]
                        }
                }
                
                for (j in temp.data) {
                        value = c(as.character(unlist(k_1Data1_[i,])),j)
                        if(is.null(a))
                        {
                                a = value
                                names(a) = c(names(k_1Data1_),paste0("item",length(a)))
                                
                        }
                        else{
                                a = rbind(a,value)
                        }
                        
                }
        }
        
        #print("-------------")
        #print(class(a))
        if(class(a) != "character"){
                a.df = as.data.frame(a,row.names = F,stringsAsFactors = F)
                a.df = a.df[complete.cases(a.df),]  
        }else{
                #print(a)
                a.df = data.frame(as.list(a))
                #print(a.df)
        }
        
  
        #print(a.df)
        #print("----------------")
        #print(a.df)
        if(nrow(a.df)>1){
                
                #print("-----------------")
                #print(a.df)
                names(a.df) = c(names(k_1Data1_),paste0("item",length(a.df)))
                return(a.df)
                
        }else{
                return(NULL)
        }
        
}


# Generate itemsets
generate.itemsets.k_1xk_1 <- function(transactions.data_,minsup_ = 0.1){
        
                #transactions.data_ = carData.transactions
        # Find one frequent itemsets  
        n.transactions = nrow(transactions.data_)
        transactions.support = colSums(transactions.data_)
        
        one.itemsets.df_ = data.frame(item1 = names(transactions.support),support = transactions.support/n.transactions,stringsAsFactors = F,row.names = seq(1:length(transactions.support)))
        
        candidate.itemsets = list(one.itemsets.df_)
        one.fre.itemsets.df_ = one.itemsets.df_ %>% filter(support > minsup_)
        
        currentData_ = data.frame(item1 = one.fre.itemsets.df_[,1],stringsAsFactors = F)
        frequent.itemsets = list(one.fre.itemsets.df_)
        fre.itemsets = generate.2.fre.itemset(currentData_,one.fre.itemsets.df_)
        
        fre.itemsets$support = apply(fre.itemsets,1,function(x) calc.support(x,transactions.data_))
        
        fre.itemsets = fre.itemsets %>% filter(support > minsup_)
        frequent.itemsets[length(frequent.itemsets)+1] = list(fre.itemsets)
        fre.count = nrow(one.fre.itemsets.df_) + nrow(fre.itemsets)
        cand.count = nrow(one.fre.itemsets.df_) + nrow(fre.itemsets)
        
        while(T)
        {
                #print(head(fre.itemsets))
                temp.itemset.df = generate.combs.k_1xk_1(fre.itemsets)
                
                if(is.null(temp.itemset.df)){
                        break
                }
                cand.count = cand.count + nrow(temp.itemset.df)
                temp.itemset.df$support = apply(temp.itemset.df,1,function(x) calc.support(x,transactions.data_))
                candidate.itemsets[length(candidate.itemsets)+1] = list(temp.itemset.df)
                
                temp.itemset.df = temp.itemset.df %>% filter(support > minsup_)
                #print(head(temp.itemset.df))
                if(nrow(temp.itemset.df) < 2){
                        break
                }
                fre.count = fre.count + nrow(temp.itemset.df)
                frequent.itemsets[length(frequent.itemsets)+1] = list(temp.itemset.df)
                
                fre.itemsets = temp.itemset.df
                
              
        }
        
        print(paste0("Number of candidate itemsets: ",cand.count))
        print(paste0("Number of frequent itemsets: ",fre.count))
        
        return(list(candidate.itemsets,frequent.itemsets))
        
}