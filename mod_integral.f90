!Módulo: Métodos de integración
!Alejandra Ichina
!Abril de 2019
!Input: intervalos
!Output: valor de las integrales con diferentes metodos

module integrales

 contains

!método de Simpson
 double precision function dsimp(f,N,dx)		!real*8 function dsimp()
  implicit none
  integer::N, i
  real*8::dx
  real*8::sumapar, sumaimpar 

  real*8, dimension(0:N)::f
 
   sumapar=0.d0
   do i = 2, N-2, 2					!donde empiezo, donde termino, salto

     sumapar = sumapar + f(i)

   enddo

   sumaimpar=0.d0
   do i = 1, N-1, 2					!donde empiezo, donde termino, salto

     sumaimpar = sumaimpar + f(i)

   enddo

   dsimp=(dx/3.d0)*(f(0)+f(N) + 2.d0*sumapar + 4.d0*sumaimpar)	!la suma par en realidad es la impar

 end function dsimp


!método del trapecio
 double precision function dtrap(f, N, dx)			!dtrap es por doble precision
  implicit none
  integer::N, i
  real*8::dx
  real*8::suma
  real*8, dimension(0:N)::f

   suma=0

   do i=1,N-1
							
   suma = suma+f(i)

   enddo

   dtrap=(dx)*((f(0)+f(N))*0.d5 + suma)	

 end function dtrap

end module

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


