optimizeAdStock <- function(grp,sales){
    
    rmax <- 0
    r22max <- -1
    rates <- seq(0.00, 1.00, by = 0.01)
    r2 <- vector()
    
    for(r in rates){
      adstock[1] <- data4$Investment_all[1]
      for(i in 2:length(data4$Investment_all)){
        adstock[i] = data4$Investment_all[i] + r*adstock[i-1]
      }
    
      r22 <- summary(lm(sales~adstock))$r.squared
      if(r22>r22max){
        r22max <- r22
        rmax <- r
      }
      r2 <- append(r2,r22)
    }
    plot(rates,r2)
    sprintf("忘却率が%fのとき相関係数は最大値%f",rmax,r22max)
  }
  
  
  
  optimizeAdStock <- function(grp,sales){
    
    rmax <- 0
    r22max <- -1
    rates <- seq(0.00, 1.00, by = 0.01)
    r2 <- vector()
    v <- 0.2
    
    for(r in rates){
      adstock[1] <- data4$Investment_all[1]
      for(i in 2:length(data4$Investment_all)){
        adstock[i] = 1/(1+exp(-v*data4$Investment_all[i])) + 
          r*adstocked_advertising[i-1]
      }
      
      r22 <- summary(lm(sales~adstock))$r.squared
      if(r22>r22max){
        r22max <- r22
        rmax <- r
      }
      r2 <- append(r2,r22)
    }
    plot(rates,r2)
    sprintf("忘却率が%fのとき相関係数は最大値%f",rmax,r22max)
  }
  
