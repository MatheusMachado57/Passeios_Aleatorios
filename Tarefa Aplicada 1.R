
# Q1
runif(1)

q1 = function(n){                                              
     Sn = 0                                        # objeto a ser usado na contagem
     a = c(NULL)                                   # vetor a ser usado para guardar
     for(i in 1:n){                                #  os diferentes valores de Sn
         m = runif(1)                              
         if(m<0.5){Sn = Sn + 1                     # se menor que 0.5, soma 1
         }else{
         Sn = Sn - 1}                              # senão, subtrai 1
         a[i] = Sn}                                # guardando os valores de Sn  
     cat(plot(a), Sn)                              # devolvendo Sn e o gráfico
}
q1(100)

# Q2
q2 = function(n,r){                             
     Sn = 0                           # objeto de contagem
     a = c(NULL)                      # vetor a ser usado para guardar os diferentes valores de sn  
     for(j in 1:r){                   # rodando  os r passeios aleatórios     
         for(i in 1:n){               # rodando cada passeio aleatório           
             m = runif(1)             
             if(m<0.5){Sn = Sn + 1    # se menor que 0.5, soma 1 
             }else{                    
             Sn = Sn - 1}}            # senão, subtrai 1
         a[j] = Sn                    # guardando os valores de Sn
         Sn = 0}                      # retornando Sn a 0 para contar o próximo passeio 
         cat(mean(a))                 # retornando a média dos Sn
         return(hist(a))              # retornando o histograma dos sn 
}  
q2(100,10)  

# Q3
# A
q3a = function(n,r){
      if((r+n)%%2 != 0){stop("N e R devem ter a mesma paridade")}           
      if(r>n){stop("R deve ser igual ou menor que N ")}
      resp = factorial(n)/((factorial((n+r)/2)*factorial(n - (n+r)/2))) # fórmula do numero de 
      return(resp)                                                      # caminhos da origem até 
}                                                                       # (n,r)
q3a(10,10)
q3a(4,0)
  
# B
q3b = function(n,r){
      if((r+n)%%2 != 0){stop("N e R devem ter a mesma paridade")}
      if(r>n){stop("R deve ser igual ou menor que N ")}
      cam = factorial(n)/((factorial((n+r)/2)*factorial(n - (n+r)/2))) # fórmula do numero de caminhos da origem até (n,r)
      resp = cam/(2^n)                                                 # dividindo pelo número de caminhos possíveis/       
      return(resp)                      # retornando a probbilidade
}
q3b(100,10)

# Q4
q4 = function(n,r){ 
  Sn = 0                                       # objeto que ajudará na contagem 
  prop = c(NULL)                               # vetor que guardará os valores da proporção                                # vetor que guardará os valores de Sn
  for(j in 1:n){                               # rodando os n passeios
    for(i in 1:r){                             # rodando cada passeio
      m = runif(1)
      if(m<0.5){Sn = Sn + 1                    # se menor que 0.5, soma 1
      }else{                                   # senão
      Sn = Sn - 1}}                            # subtrai 1
    if(Sn == 10){prop[j] = 1                   # contando os Sn = 10;
    }else{
    prop[j] = 0}
    Sn = 0}
  cat(" Proporção de passeios onde (Sn = 10):",mean(prop))  # retorno da probabilidade
}  
q4(1000,100)  

# Q5
# A
q5a = function(n){
      if(n%%2 != 0){stop("N deve ser um número par")}             # verificando se n é par
      U1 = factorial(2*n)/((factorial(n)*factorial(2*n - n)))     # primeira parte da f´´ormula de Un
      U2 = 2^(2*n)                                                # segunda parte da fórmua de Un      
      U  = U1/U2                                                  # divisão
      cat(U)                                                      # resultado
} 

q5a(10)
q5a(2)

# B
q5b = function(n,alt){                                    
      if(n%%2 != 0){stop("N deve ser um nÃºmero par")}       # verificando se n é ímpar
      if(alt == "Stirling"){                                 # se o usuário prefere Stirling
         resp = 1/sqrt(pi*n)                                 # fórmula 
         return(resp)                                        # retornando a resposta
      }
      U1 = factorial(2*n)/(factorial(n)^2)                   # mesmo código dado anteriormente
      U2 = 2^(2*n)
      U  = U1/U2
      cat(U)
}
q5b(2,"Stirling")
  
# C
a = data.frame(NULL)


# Q6
q6 = function(n,r){
  Sn = 0                            # mesmo código do exercício 4, porém com Sn = 0; 
  prop = c(NULL)
  for(j in 1:r){
    for(i in 1:n){
      m = runif(1)
      if(m<0.5){Sn = Sn + 1}else{Sn = Sn - 1}}
    if(Sn == 0){prop[j] = 1}else{prop[j] = 0}
    Sn = 0}
  cat("Proporção de passeios onde (Sn = 0):",mean(prop))
}  
q6(1000,100)  

# Q7
q7 = function(n){
     if(n%%2 != 0){stop("N deve ser um número par")}       # Verificando se n é par
     if( n <= 0 ){stop("N deve ser maior que 0")}          # Verificando se n é positivo 
     U1 = factorial(2*n)/(factorial(n)^2)                  
     U2 = 2^(2*n)
     U  = U1/U2                                            # fórmula de Un já vista antes
     resp = U/(2*n -1)                                     # fórmula de fn = Un/(2*n-1)
     cat(resp)                                             # retornando resposta
}
q7(0)
q7(4)

# Q8
q8 = function(n,k){
  if(n%%2 != 0){stop("N deve ser um número par")}    # Verificando se n e k são números pares.
  if(k%%2 != 0){stop("K deve ser um número par")}
  U = factorial(n - k)/((factorial(n - k)*factorial(n - k)))/2^(2*(n-k))  # fórmula de Un
  K = factorial(k)/((factorial(k)*factorial(k)))/2^(2*(k))                # fórmula de Un com k
  cat(U*K)                                                 # retornando a resposta: U(n)*U(n-k) 
} 

q8(20,10)

# Q9
q9 = function(n,r){
  a = c(NULL)
  for(j in 1:n){
    Sn = 0
    for(i in 1:r){
      m = runif(1)
      if(m<0.5){Sn = Sn + 1}else{Sn = Sn - 1}
      if(Sn == 0){a[i] = i}}}
  cat(a)
  plot(a)
}  
q9(1000,100)  





