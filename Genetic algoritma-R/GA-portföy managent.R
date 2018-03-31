library(genalg) #In R I have used the package genalg to set-up the model...
datasets <- data.frame(item = c("BASCM" ,"KIPA","MGROS","PETKM","PINSU","TS","CCOLA"),
                       ort_get_risk = c(0.02,0.11,0.07,0.09,0.02,0.05,0.04),
                       beta = c(0.1762,1.0333,0.5985,1.0858,0.9363,0.5478,0.2147)) # that is my datasets some companies and them ort_get_risk and beta values...datasets
weightlimit <- 20
#Before creating the model we have to set-up an evaluation function.
#The evaluation function will evaluate the different individuals (chromosomes) of the population on the value of their gene configuration.

chromosome <- c(0,1,0,1,0,0,0)  #An individual can for example have the following gene configuration: 0,1,0,1,0,0,0
datasets[chromosome ==0 ,]
datasets[chromosome ==1 ,]

cat(chromosome %*% datasets$ort_get_risk)#Above we gave a value to the gene configuration of a given chromosome. This is exactly what the evaluation function does.
                                        #the genalg algorithm tries to optimize towards the minimum value

evalFunc <- function(x){                #We define the evaluation function as follows.
  current_solution_ort_get_risk <- x %*% datasets$ort_get_risk
  current_solution_beta <- x %*% datasets$beta
  if(current_solution_beta > weightlimit)
    return(0) 
  else(-current_solution_ort_get_risk)
  }
#Next, we choose the number of iterations, design and run the model.
iter =200
GAmodel.results <-rbga.bin(size = 7,popSize = 200,iters = iter,mutationChance = 0.01,elitism = T,
                   evalFunc = evalFunc)

summary(GAmodel.results,echo = TRUE) #The best solution is found to be 1111111. This leads us to take the following items with us on our aim.

solution <- c(1,1,1,1,1,0,1)
datasets[solution ==1 ,]

cat(paste(solution %*% datasets$ort_get_risk, "/", sum(datasets$ort_get_risk))) #This in turn gives us the total number of our portfolio
                                                                                #that means that result is we will get all of them







