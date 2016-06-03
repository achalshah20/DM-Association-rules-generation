find.itemsets.and.rules <- function(dataset,methodType,dataset.name,IsPruningAllowed,support,confidence,lift){
        # Conver to transactions data
        #dataset = cmcData
        #methodType = 1
        #dataset.name = "cmc"
        #IsPruningAllowed = F
        dataset.transactions = Convert.to.transactions(dataset)
        
        # Choose method 
        # 1. fk-1 x fk1
        # 2. fk-1 x fk-1
        
        if(methodType == 1){
                methodName = "fk_1xf1"
        }else{
                methodName = "fk_1xfk_1"
        }
        
        # Pruning method or bruteforce
        # False = Bruteforce
        # True = Confidence based pruning
        
        if(IsPruningAllowed){
                rulesGenerationMethod = "conf_pruning"
        }else{
                rulesGenerationMethod = "bruteforce"
        }
        
        # Generate all itemsets
        Sys.time()
        #all.itemsets = generate.itemsets(dataset.transactions,methodType_ = methodType)
        #write.list(all.itemsets,dataset.name,"all_itemsets",methodName,outputDir)
        Sys.time()

        # Generate all frequent itemsets for different support values
        for(support.value in support){
                
               
                assign(paste("cand.and.fre.itemsets","s",support.value,sep = "."),generate.itemsets(dataset.transactions,minsup_ = support.value,methodType_= methodType))
                assign(paste("candidate.itemsets","s",support.value,sep = "."),eval(parse(text = paste("cand.and.fre.itemsets","s",support.value,sep = ".")))[[1]])
                assign(paste("fre.itemsets","s",support.value,sep = "."),eval(parse(text = paste("cand.and.fre.itemsets","s",support.value,sep = ".")))[[2]])

        
        write.list(eval(parse(text = paste("candidate.itemsets","s",support.value,sep = "."))),dataset.name,paste("candidate.itemsets","s",support.value,sep = "."),methodName,outputDir)

        write.list(eval(parse(text = paste("fre.itemsets","s",support.value,sep = "."))),dataset.name,paste("fre.itemsets","s",support.value,sep = "."),methodName,outputDir)
        }
        
        for(support.value in support){
        
                print(paste("Finding closed and maximal itemsets of fre itemsets with support",support.value))              # Find closed frequent itemsets from all frequent itemsets
        assign(paste("closed.or.max.fre.itemsets","s",support.value,sep = "."),find.closed.or.maximal.fre.itemsets(eval(parse(text = paste("fre.itemsets","s",support.value,sep = ".")))))
        assign(paste("closed.fre.itemsets","s",support.value,sep = "."),eval(parse(text = paste("closed.or.max.fre.itemsets","s",support.value,sep = ".")))[[1]])
        assign(paste("maximal.fre.itemsets","s",support.value,sep = "."),eval(parse(text = paste("closed.or.max.fre.itemsets","s",support.value,sep = ".")))[[2]])
       
       
        
        write.list(eval(parse(text = paste("closed.fre.itemsets","s",support.value,sep = "."))),dataset.name,paste("closed.fre.itemsets","s",support.value,sep = "."),methodName,outputDir)
        write.list(eval(parse(text = paste("maximal.fre.itemsets","s",support.value,sep = "."))),dataset.name,paste("maximal.fre.itemsets","s",support.value,sep = "."),methodName,outputDir)
        }
        
        for(support.value in support){
                for(conf.value in confidence){
                        
                        print(paste("Generating rules for frequent itemsets: support",support.value,"confidence: ",conf.value))
                        assign(paste("assoc.rules","s",support.value,"c",conf.value,sep = "."),generate.all.rules(eval(parse(text = paste("fre.itemsets","s",support.value,sep = "."))),conf_ = conf.value,IsPruningAllowed_ = IsPruningAllowed))
                        write.custom.csv(eval(parse(text = paste("assoc.rules","s",support.value,"c",conf.value,sep = "."))),dataset.name,paste0(paste("assoc.rules","s",support.value,"c",conf.value,sep = "."),rulesGenerationMethod),methodName,outputDir)
                       
                }
        }
        
        for(support.value in support){
                for(lift.value in lift){
                        rulesGenerationMethod = "bruteforce"
                        
                        print(paste("Generating rules for frequent itemsets: support",support.value,"lift: ",lift.value))
                        assign(paste("assoc.rules","s",support.value,"l",lift.value,sep = "."),generate.all.rules(eval(parse(text = paste("fre.itemsets","s",support.value,sep = "."))),lift_ = lift.value,IsPruningAllowed_ = F))
                        write.custom.csv(eval(parse(text = paste("assoc.rules","s",support.value,"l",lift.value,sep = "."))),dataset.name,paste0(paste("assoc.rules","s",support.value,"l",lift.value,sep = "."),rulesGenerationMethod),methodName,outputDir)
                        
                }
        }
  
        
}
    