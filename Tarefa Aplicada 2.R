
# Q1 - a probabilidade de haver r mudanças de sinal em 2n+1 unidades de tempo;
q1 = function(n,r){
     resp = choose(2*n+1 , n+r+1)/(2^(2*n))
     # sendo choose uma funcao matematica relaconada
     # as funcoes beta e gamma; 
     cat(resp)
}
q1(1,1)

# Q2 - prob do ponto máx de um PA de tamanho n ser r
q2 = function(n,r){
     if(n < r){stop("n deve ser maior ou igual a r")}
     if((n+r)%%2 == 0){
     resp = choose(n, (n+r)/2)/(2^n)
     }else{
     resp = choose(n, (n+r+1)/2)/(2^n)
     }
     cat(resp)
}
q2(2,0)

# Q3 - 10000 replicacoes de um PA de 199 passos;
#      guardar: A) n de troca de sinais
#               B) o max atingido por cada passeio
#               C) n de lados acima do eixo

q3 = function(n,r){
     f  = c(NULL)
     c  = c(NULL)
     b  = c(NULL)
     for(i in 1:r){     
     # rodando os 1000 passeios
         a  = 0         
         # zerando para a contagem do proximo Passeio
         d  = 0         
         # zerando para a contagem do proximo Passeio
         v  = c(NULL)
         Sn = 0 
     for(j in 1:n){    
     # rodando Passeio de 199 passos
         m = runif(1)          
         # criando o passeio
         if(m < 0.5){Sn = Sn + 1}else{Sn = Sn - 1}
            v[j] = Sn
         if(j > 2){                                                           
         # um Passeio so pode mudar de sinal depois do 2 momento;
            if(v[j - 2] > 0 & v[j] < 0 || v[j - 2] < 0 & v[j] > 0){a = a + 1} 
            # contando o numero de troca de sinal de cada passeio;  
         }
         if(Sn > 0){d = d + 1}     
         # contando o numero de lados acima do eixo x de cada passeio
     }                                
       f[i] = a                 
       # armazenando o numero de troca de sinal de cada passeio         
       b[i] = max(v)            
       # armazenando o maior valor de cada passeio 
       c[i] = d                 
       # armazenando o numero de lados acima do eixo x   
     }
     cat("\nNumero de troca de sinais de cada Passeio:",f)
     cat("\n")
     cat("\nO maior valor de cada Passeio:",b)
     cat("\n")
     cat("\nO numero de lados de acima do eixo x de cada passeio:",c)
     plot(v)
}

q3(199,3)

# A
q3a = function(n,r){
  f  = c(NULL)
  c  = c(NULL)
  b  = c(NULL)
  for(i in 1:r){    
    # rodando os 1000 passeios
    a = 0           
    # zerando para a contagem do proximo Passeio
    d = 0           
    # zerando para a contagem do proximo Passeio
    v = c(NULL)     
    # limpando o vetor v para armazenar o proximo Passeio
    Sn = 0          
    # zerando para a contagem do Sn do proximo Passeio
    for(j in 1:n){          
    # rodando Passeio de 199 passos
      m = runif(1)          
      # criando o passeio
      if(m < 0.5){Sn = Sn + 1}else{Sn = Sn - 1}
      v[j] = Sn
      if(j > 2){                                                           
      # um Passeio so pode mudar de sinal depois do 2 momento;
        if(v[j - 2] > 0 & v[j] < 0 || v[j - 2] < 0 & v[j] > 0){a = a + 1}  
        # contando o numero de troca de sinal de cada passeio;  
      }
      if(Sn > 0){d = d + 1}     
      # contando o numero de lados acima do eixo x de cada passeio
    }                                
    f[i] = a                    
    # armazenando o numero de troca de sinal de cada passeio         
    b[i] = max(v)               
    # armazenando o maior valor de cada passeio 
    c[i] = d                    
    # armazenando o n?mero de lados acima do eixo x   
  }
  cat("\nNumero de troca de sinais de cada Passeio:",f)
  cat("\n")
  cat("\nO maior valor de cada Passeio:",b)
  cat("\n")
  cat("\nO numero de lados de acima do eixo x de cada passeio:",c)
  hist(f)
}

q3a(199,10000)

# B

v1 = c(q1(199,1),q1(199,2),q1(199,3),q1(199,4),q1(199,5),
      q1(199,10),q1(199,20),q1(199,30),q1(199,40))
v2 = 
  
table(v1,v2)  
  
# C
q3c = function(n,r){
  f  = c(NULL)
  c  = c(NULL)
  b  = c(NULL)
  for(i in 1:r){    
  # rodando os 1000 passeios
    a = 0           
    # zerando para a contagem do proximo Passeio
    d = 0           
    # zerando para a contagem do proximo Passeio
    v = c(NULL)     
    # limpando o vetor v para armazenar o proximo Passeio
    Sn = 0          
    # zerando para a contagem do Sn do proximo Passeio
    for(j in 1:n){          
    # rodando Passeio de 199 passos
      m = runif(1)          
      # criando o passeio
      if(m < 0.5){Sn = Sn + 1}else{Sn = Sn - 1}
      v[j] = Sn
      if(j > 2){                                                           
      # um Passeio so pode mudar de sinal depois do 2 momento;
        if(v[j - 2] > 0 & v[j] < 0 || v[j - 2] < 0 & v[j] > 0){a = a + 1}  
        # contando o numero de troca de sinal de cada passeio;  
      }
      if(Sn > 0){d = d + 1}     
      # contando o numero de lados acima do eixo x de cada passeio
    }                                
    f[i] = a                    
    # armazenando o numero de troca de sinal de cada passeio         
    b[i] = max(v)               
    # armazenando o maior valor de cada passeio 
    c[i] = d                    
    # armazenando o n?mero de lados acima do eixo x   
  }
  cat("\nNumero de troca de sinais de cada Passeio:",f)
  cat("\n")
  cat("\nO maior valor de cada Passeio:",b)
  cat("\n")
  cat("\nO numero de lados de acima do eixo x de cada passeio:",c)
  hist(b)
}
q3c(199,1000)
  
# D 
w1 = c(q2(199,1),q2(199,2),q2(199,3),q2(199,4),q2(199,5),
       q2(199,10),q2(199,15),q2(199,20),q2(199,50),q2(199,100))

w2 = c()

# E 
q3e = function(n,r){
  f  = c(NULL)
  c  = c(NULL)
  b  = c(NULL)
  for(i in 1:r){    
  # rodando os 1000 passeios
    a = 0           
    # zerando para a contagem do proximo Passeio
    d = 0           
    # zerando para a contagem do proximo Passeio
    v = c(NULL)     
    # limpando o vetor v para armazenar o proximo Passeio
    Sn = 0          
    # zerando para a contagem do Sn do proximo Passeio
    for(j in 1:n){          
    # rodando Passeio de 199 passos
      m = runif(1)          
      # criando o passeio
      if(m < 0.5){Sn = Sn + 1}else{Sn = Sn - 1}
      v[j] = Sn
      if(j > 2){                                                           
      # um Passeio so pode mudar de sinal depois do 2 momento;
        if(v[j - 2] > 0 & v[j] < 0 || v[j - 2] < 0 & v[j] > 0){a = a + 1}  
        # contando o numero de troca de sinal de cada passeio;  
      }
      if(Sn > 0){d = d + 1}     
      # contando o numero de lados acima do eixo x de cada passeio
    }                                
    f[i] = a                    
    # armazenando o numero de troca de sinal de cada passeio         
    b[i] = max(v)               
    # armazenando o maior valor de cada passeio 
    c[i] = d                    
    # armazenando o numero de lados acima do eixo x   
  }
  cat("\nNumero de troca de sinais de cada Passeio:",f)
  cat("\n")
  cat("\nO maior valor de cada Passeio:",b)
  cat("\n")
  cat("\nO numero de lados de acima do eixo x de cada passeio:",c)
  hist(c)
}
q3e(199,1000)

# 4 - funcao  q constroi um P.A. em Z2, com n passos.
#     A fucao de ve retornar um grafico e o estado final; 

Q4 = function(n){
     x = 0
     y = 0
     v = c(NULL)
     z = c(NULL)
     v[1] = 0
     z[1] = 0
     for(i in 2:n){                       
     # começa com 2. pois o primeiro momento já esta em (0,0)
         m = runif(1)
         if(m<0.5){                       
         # se m < 0.5, se move na horizontal
            m=runif(1)                     
            if(m<0.5){x=x+1}else{x=x-1}   
            # se m < 0.5, se move para direita, senão, para a esquerda 
            v[i] = x
            if(i == 1){z[i] = 0}else      
            # a coordenada vertical se mantém
                      {z[i] = z[i-1]}
            }else{                        
            # se m > 0.5, se move na vertical 
            m=runif(1)  
            if(m<0.5){y=y+1}else{y=y-1}   
            # se m < 0.5, se move para cima, senão, para baixo
            z[i] = y
            if(i == 1){v[i] = 0}else      
            # a coordenada horizontal se mantém
                      {v[i] = v[i-1]}
     }}
     plot(v,z, type = "l")                
     # plotando o gráfico em linhas
}

Q4(1000)

# 5 - funcao que controi n replicacoes de um P.A. de n passos em Z2;
#     retorna um hisograma com a distancia de Sn a origem

Q5 = function(n,r){
     x  = 0
     y  = 0
     v  = c(NULL)
     z  = c(NULL)
     Sn = c(NULL)
     v[1]  = 0                      
     # similar ao passeio anterior
     z[1]  = 0
     for(j in 1:r){
     for(i in 2:n){
         m = runif(1)
         if(m<0.5){
         m=runif(1)
         if(m<0.5){x=x+1}else{x=x-1}
         v[i] = x
         if(i == 1){z[i] = 0}else
                   {z[i] = z[i-1]}
         }else{
         m=runif(1)  
         if(m<0.5){y=y+1}else{y=y-1} 
         z[i] = y
         if(i == 1){v[i] = 0}else
                   {v[i] = v[i-1]}
      }}
     Sn[j] = sqrt((v[n])^2) + sqrt((z[n])^2)   
     }
     hist(Sn)
}
Q5(1000,100)

# 6 - graf. estrela ate repetir um viz

q6 = function(r,n){
     m = c(NULL)     
     # vetor que contera os valores de runif;
     s = c(NULL)     
     # vetor que contera o maior valor de cada instervalo;
     c = c(NULL)
     for (i in 1:r){s[i] = i/r}
     for(j in 1:n){
     f = rep(0,r)     
     # vetor que contera os intervalos ao qual cada runif pertence;    
     for(i in 1:r){
         m[i] = runif(1)
         if(m[i]<0.5){f[i] = f[i] + 1}
         if(f[i] == 2)break}
     c[j] = sum(f)
     }                      
     # obs: os intervalos sao como os amigos e runif sao como as visitas;
      hist(c)
      cat("\nVetor com o maior valor de cada intervalo:",s)
      cat("\nVetor com os vizinhos que foram visitados:",f)
}

q6(3,1000)
q6(5,1000)
q6(10,1000)
q6(20,1000)
q6(100,1000)


# 7 - graf. estrela ata atingir tds os viz:

q7 = function(r,n){
     m = c(NULL)     
     # vetor que contera os valores de runif;
     s = c(NULL)     
     # vetor que contera o maior valor de cada instervalo;
     c = c(NULL)
     for (i in 1:r){
          s[i] = i/r
     }
     for(j in 1:n){
     f = rep(0,r)     
     # vetor que contera os intervalos ao qual cada runif pertence;    
     d = 1
     repeat{
         m[d] = runif(1)
         if(m[d] < .5){f[d] = f[d] + 1}
         if(f[d] == 2){f[d] = 1}
         d = d + 1
         if(sum(f) == r){break}}
     c[j] = sum(f)
     }                      
     # obs: os intervalos sao como os amigos e runif sao como as visitas;
      hist(c)
      cat("\nVetor com o maior valor de cada intervalo:",s)
#     cat("\nVetor com os vizinhos que foram visitados:",f)
}
q7(3,1000)
q7(5,1000)
q7(10,1000)
q7(20,1000)
q7(100,1000)

# 8 - P.A. circular:
q8 = function(r,n,p){
     c = 1                               
     # começando no primeiro vizinho (vizinho 1)
     f = rep(0,r)                        
     # vetor para guardar os vizinhos visitados
     for(i in 1:n){
         m = runif(1)
         if(m<p){c = c + 1}else{c = c - 1}    
         # pulando de vizinho em vizinho
         if(c > r){c = 1}                     
         # se maior que r, volta pro primeiro vizinho
         if(c == 0){c = r}                    
         # se menor que 1, vai para o último vizinho
         f[c] = f[c] + 1                      
         # avisando que tal vizinho foi vizitado
         if(f[c] == 2){f[c] = 1}              
         # anulando as segundas visitas
     }
     cat(sum(f))    
     # retornando o número de vizinhos visitados; (onde f[i] = 1)
}

q8(10,10,0.5)

# 9 - P.A. Ruína do Jogador
q9 = function(i,r,p){
     v = c(NULL)
     j = 1
     repeat{
           m = runif(1)
           if(m<p){i = i +1}else{i = i -1}   
           # se o jogador ganha ou perde
           v[j] = i                          
           # armazenando a informação
           j = j + 1                         
           # proseguindo com o passeio
           if(i == 0 || i == r)break         
           # se atingir 0 ou r, o jogador para;
           }
     cat("Sn:",i)
     plot(v)
}
q9(5,10,0.5)

# 10 - 1000 replicações da Ruína do Jogador

q10 = function(i,r,p,n){
     v = c(NULL)
     w = c(NULL)            
     j = 1
     q = 1
     for(g in 1:n){                  
     # similar ao código anterior, porém com n passeios aleatórios
     t = 0   
     repeat{                                  
           m = runif(1)
           if(m<p){i = i +1}else{i = i -1}
           v[j] = i
           j = j + 1
           if(i == 0){t = t + 1}
           if(i == 0 || i == r)break} 
     w[g] = t
     }
     resp1 = t/(j-1)
     resp2 = (j-1-t)/(j-1)
     table(resp1,resp2)
#    cat("Sn:",i)
#    plot(v)
}

q10(3,6,0.3,2)
q10(3,6,0.5,1000)
q10(3,6,0.7,1000)


