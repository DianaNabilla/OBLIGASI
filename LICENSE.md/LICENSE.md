setwd("C:\\Users\\toshiba\\Documents\\DIANA\\psk")
#f= pokok hutang
#r=bunga obilgasi
#m=banyaknya pemberian bunga dalam 1 tahun
#ttm=time to maternity
#t=tahun
#bm=harga obligasi untuk pemilik baru
#frk=harga obligasi untuk pemilik lama
valobligasi<- function(f, r, i, ttm, m){
  t= ceiling(ttm*m+1)
  b= ttm*m-floor(ttm*m)
  k= 1-b
  rbin= r/m
  ibin=i/m
  vbin=1/(1+ibin)
  an=(1-(vbin^t))/ibin
  Bt = (f*rbin*an)+(f*(vbin^t))
  #Theoritical method
  Bf1= Bt*(1+ibin)^k
  frk1= f*rbin*((1+ibin)^k-1)/ibin
  Bm1= Bf1-frk1
  print(Bm1)
  
  #Practical method
  Bf2= Bt*(1+k*ibin)
  frk2= k*f*rbin
  Bm2= Bf2-frk2
  print(Bm2)
  
  #Semi-Theoretical Method
  Bf3= Bt*(1+ibin)^k
  frk3= k*f*rbin
  Bm3= Bf3-frk3
  print(Bm3)
  
  
  
  
}
