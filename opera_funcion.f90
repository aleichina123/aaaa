!Este modulo contiene diferentes operaciones
!Alejandra Ichina
!25/04/2019

module operaciones_funciones

contains

subroutine operaciones(fun, N, suma, media, maximo)
implicit none

integer, intent(in)::N
integer, dimension(1:N), intent(in)::fun
integer, intent(out)::suma, maximo
real, intent(out)::media
integer::i

suma=0

do i=1,N

  suma = suma + fun(i)

enddo


media=suma/float(N)						!float: hace que el N sea un real


maximo=fun(1)
do i=1, N
  if (maximo.lt.fun(i)) then
   
    maximo=fun(i)
  
  endif
enddo

end subroutine operaciones

end module operaciones_funciones
 
