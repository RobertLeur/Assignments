#Removes NA values from a vector
rem_na <- function(sample_vec) {
  init <- c()
  for (i in 1:length(sample_vec)) {
    if (!is.na(sample_vec[1])){
      init <- c(init, sample_vec[i])
    }
  }
  return (init)
}


#Computes the factorial given an integer argument
factorial = function(x) {
  if (x == 0 || x == 1) {
    return (1)
  } 
  else {
    return (x * factorial(x - 1)) 
  } 
}


##Sorts a given vector in decreasing order
arr=c(1,3,10,5,8,37,370,-2,-1)
ins_sort<-function(arr){
  for(j in 2:length(arr)){
    init=arr[j]
    
    i=j-1
    while(i>0 && arr[i]<init){
      arr[(i+1)]=arr[i]
      i=i-1
    }
    arr[(i+1)]=init
  }
  return (arr)
}


#Return the nth highest number
nth_largest<-function(arr, nth){
  return(ins_sort(arr)[nth])
}


#Function to compute for your net pay at work
net_pay = function(basic, tax_alw = 0, ntax_alw = 0, lwop_days = 0, months_pay = 13, working_days = 22){
  
  annual = (basic + tax_alw) * months_pay
  
  if (annual <=  250000){
    net = annual
  } else if (annual <= 400000) {
    net = annual - (annual - 250000) * 0.2
  } else if (annual <= 800000) {
    net = annual - (annual - 400000) * 0.25 - 30000
  } else if (annual <= 2000000) {
    net = annual - (annual - 800000) * 0.30 - 130000
  } else if (annual <= 8000000) {
    net = annual - (annual - 2000000) * 0.32 - 490000
  } else {
    net = annual - (annual - 8000000) * 0.35 - 2410000
  }
  
  mnet = net * 1./months_pay
  net_final = mnet + ntax_alw - (mnet * 1./working_days) * lwop_days
  return (net_final)
}

net_pay(50000, 1000, 1000, 2, 13, 20)


#Computes the compound interest of an investment

compound_interest = function(principal, interest_rate = 0.01, n_cpd_periods = 1){
  return (principal * ((1 + interest_rate)**n_cpd_periods - 1))
}

compound_interest(1000)
