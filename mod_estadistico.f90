!Este módulo contiene variables estadísticas como: media, varianza
!Alejandra Ichina
!26/04/2019

module est_operaciones

contains

subroutine estadistica(temp, N, media, var, sigma, absd)
implicit none

integer, intent(in)::N
real, dimension(1:N), intent(in)::temp
real, intent(out)::media, var, sigma, absd
real::suma, suma2, suma3, xmed
integer::i
 
suma=0
suma2=0
suma3=0

do i=1,N

  suma=suma+temp(i)

enddo

  media=suma/N

  xmed=(media)*(media)

do i=1, N

  suma2=suma2+(temp(i)*temp(i))-(xmed)				!no olvidar inicializar la suma

enddo
 
 var=suma2/(N-1)
  
 sigma=sqrt(abs(var))

do i=1,N

  suma3=suma3+abs(temp(i)-media)

enddo

  absd=suma3/N



end subroutine estadistica

end module est_operaciones
