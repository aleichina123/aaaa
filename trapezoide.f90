program trapecio

 implicit none
 integer::N
 integer::i
 real::a, b, h
 real::pi
 real::f0, fN, xi, fi
 real::modul, suma, Iex

pi=acos(-1.)				!defino el pi

print*, 'Escriba el intevalo en el que desea integrar: '
read*, a, b				!ingreso los intervalos a integrar
print*, 'Escriba un valor entero N (numero de trapezoides): '
read*,N					!leo el numero de trapezoides

h=(b-a)/N

modul = 1./sqrt(2.*pi)

f0 = fun2(a)				!f(x0)=1/sqrt(2pi)*exp(-x0/2)
fN = fun2(b)				!f(xN)=1/sqrt(2pi)*exp(-xN/2)

suma = 0.5*(f0+fN)

do i=1,N-1
  xi = a+i*h
  fi = fun2(xi)				!f(xi)=1/sqrt(2pi)*exp(-xi/2)
  suma = suma+fi
enddo

print*, 'I= ', h*suma

!Iex=((b**5)/5-(b**3)/3-b)-((a**5)/5-(a**3)/3-a)

print*, 'Iex= ', prim(b)-prim(a)
print*, 'error= ', abs(prim(b)-prim(a)-(h*suma))

contains

  real function fun(x)			!defino una primera funcion
  implicit none
  real::x
  
    fun=modul*exp(-0.5*x*x)
 
  end function

  real function fun2(x)			!defino una segunda funcion
  implicit none				!se esta usando esta funcion
  real::x
  
    fun2=(x**4)-(x*x)-1.
 
  end function

  real function prim(x)			!defino la primitiva del polinomio
  implicit none				
  real::x
  
    prim=((x**5)/5-(x**3)/3-x)
 
  end function

end program
