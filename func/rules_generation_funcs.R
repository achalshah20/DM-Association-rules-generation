#Function to get the support value
extract.support <- function(items_,frequent.itemsets.list_){
        
        # Extract support from respective frequent itemsets
        temp.fre.df = frequent.itemsets.list_[[length(items_)]]
        
        # all the items should be present in the dataframe in order to grab correct support
        # If some of the items is present then mean(items_ %in% x) will not return 1 as c(1,2) %in% c(1,3) returns TRUE,FALSE. So mean is mean(0,1) = 0.5
        support = temp.fre.df[apply(temp.fre.df,1,function(x) mean(items_ %in% x) == 1),"support"]
        if(length(support)==1){
            return(support)    
        }else{
                return(NA)
        }
        
        
        
}
# 

# Functions to generate rules and confidence based pruning
generate.rules.combinations <- function(items.vec_,frequent.itemsets.list_,conf_,lift_,IsPruningAllowed_){
        
        #items.vec_ = as.character(as.vector(freq.itemsets.s01[[2]][1,1:(length(freq.itemsets.s01[[2]])-1)]))
        #frequent.itemsets.list_ = freq.itemsets.s01
        #conf_ = 0.8
        #lift_ = -1
        #IsPruningAllowed_ = T
        
        # This counter is used to calculate number of candidate rules generation
        counter = 0
        
        # Creates empty dataframe to hold rules for particular frequent itemset row
        rules.df = data.frame(rules = NA,support=0,confidence = 0,lift = 0,stringsAsFactors = F,row.names = F)
        
        # If confidence level pruning is allowed then we will use this list, so we won't generate child rules if their parent's confidence value is below threshold
        pruned.list = NULL
        
        # Rules from items A,B,C can be generated as A,B -> C, B,C -> A, A,C -> B, A -> B,C, B -> A,C, C -> A,B.
        # So, we will use this loop to generate all combinations
        for(i in length(items.vec_):2)
        {
                #i = 2
                # This function will generate all possible combinations of i-1 length as LHS.
                # Rest part will be used as RHS
                comb.list = setdiff(combn(c(1:length(items.vec_)),i-1,simplify = F),pruned.list)
                #print(comb.list)
                #print(comb.list)
                # Iterate all combinations and find confidence for each rule
                for(j in comb.list){
                        #j = comb.list[[1]]
                        rule = paste(paste(items.vec_[j],collapse=","),"=>",paste(items.vec_[-j],collapse=","))
                        #print(rule)
                        support = extract.support(items.vec_,frequent.itemsets.list_)
                        confidence = support/extract.support(items.vec_[j],frequent.itemsets.list_)
                        lift = confidence/(extract.support(items.vec_[-j],frequent.itemsets.list_))
                        
                        if(is.na(confidence)){
                                confidence = -2
                        }
                        if(is.na(lift)){
                                lift = -2
                        }
                        
                        if(IsPruningAllowed_){
                                if(confidence < conf_){
                                        #print(j)
                                        if(length(j) > 1)
                                                pruned.list[length(pruned.list)+1] = list(j[length(j)-1])
                                }
                                if(lift < lift_){
                                        pruned.list[length(pruned.list)+1] = list(j[length(j)-1])
                                }
                        }
                        counter = counter + 1
        
                        row = c(rule,support,confidence,lift)
                        names(row) = names(rules.df)
                        rules.df = rbind(rules.df,row)
                }
        }
        return(list(rules.df[complete.cases(rules.df),] %>% filter(confidence > conf_,lift > lift_),counter))
}

generate.rules.per.itemset <- function(itemset_,frequent.itemsets.list_,conf_,lift_,IsPruningAllowed_){
        rules = data.frame(rules = NA,support = 0,confidence = 0,lift=0,stringsAsFactors = F,row.names = F)
        counter = 0
        #print(itemset_)
        for(i in 1:nrow(itemset_)){
                items = as.character(as.vector(itemset_[i,1:(length(itemset_)-1)]))
                rules.and.counter =  (generate.rules.combinations(items,frequent.itemsets.list_,conf_,lift_,IsPruningAllowed_))
                if(nrow(rules) == 0){
                       rules = rules.and.counter[[1]]
                        
                }else{
                        rules = rbind(rules,rules.and.counter[[1]])
                }
                
                counter = counter + rules.and.counter[[2]]
        }
        

        return(list(rules[complete.cases(rules),],counter))
}

generate.all.rules <- function(frequent.itemsets.list_,conf_= -1,lift_= -1,IsPruningAllowed_ = F){
        
        if(IsPruningAllowed_){
                print("Generating association rules with confidence pruning method")
        }else{
                print("Generating association rules with brutefore method")
        }
        all.rules = data.frame(rules = NA,support = 0,confidence = 0,lift=0,stringsAsFactors = F)
        counter = 0
        for(i in 2:length(frequent.itemsets.list_)){
                all.rules.and.count = generate.rules.per.itemset(frequent.itemsets.list_[[i]],frequent.itemsets.list_,conf_,lift_,IsPruningAllowed_)
                all.rules = rbind(all.rules,all.rules.and.count[[1]])
                counter = counter + all.rules.and.count[[2]]
        }
        rownames(all.rules) = seq(length=nrow(all.rules))
        #print(all.rules)
        
        # If confidence and lift values are not provided and computer rules without pruning
        # Here we have minimum confidence and lift as negative 
        
        if(conf_ != -1 & lift_ != -1){
                all.rules = all.rules[complete.cases(all.rules),] %>% arrange(desc(support),desc(confidence),desc(lift))
        }else if(conf_ == -1 & lift_ != -1) {
                all.rules = all.rules[complete.cases(all.rules),] %>% arrange(desc(support),desc(lift)) %>% select(-confidence)
        }else if(lift_ == -1 & conf_ != -1){
                all.rules = all.rules[complete.cases(all.rules),] %>% arrange(desc(support),desc(confidence)) %>% select(-lift)
        }
        print(paste0("Number of candidates generated: ",counter))
        print(paste0("Number of rules generated: ",nrow(all.rules)))
        return(all.rules)
}



