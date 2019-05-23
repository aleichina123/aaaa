! x e w sono vettori a n dimensione che contengono le ascisse e i pesi corrispondenti
! gli altri valori della subroutine rappresentano valori caratteristici delle formule

! Gauss Legendre - W(x) = 1
! integrali del tipo INT[x1,x2]f(x) dx = sum{j=1,n}(w_j*f(x_j))
module cuad_int

contains


      SUBROUTINE gauleg(x1,x2,x,w,n)
      implicit none

      INTEGER n
      DOUBLE PRECISION x1,x2,x(n),w(n)
      DOUBLE PRECISION EPS
      PARAMETER (EPS=1.d-14)
      INTEGER i,j,m
      DOUBLE PRECISION p1,p2,p3,pp,xl,xm,z,z1
      m=(n+1)/2
      xm=0.5d0*(x2+x1)
      xl=0.5d0*(x2-x1)
      do 12 i=1,m
        z=cos(3.141592654d0*(i-.25d0)/(n+.5d0))
1       continue
          p1=1.d0
          p2=0.d0
          do 11 j=1,n
            p3=p2
            p2=p1
            p1=((2.d0*j-1.d0)*z*p2-(j-1.d0)*p3)/j
11        continue
          pp=n*(z*p1-p2)/(z*z-1.d0)
          z1=z
          z=z1-p1/pp
        if(abs(z-z1).gt.EPS)goto 1
        x(i)=xm-xl*z
        x(n+1-i)=xm+xl*z
        w(i)=2.d0*xl/((1.d0-z*z)*pp*pp)
        w(n+1-i)=w(i)
12    continue
      return
      END



! Gauss Laguerre - W(x) = (x^alf)*(e^(-x))
! integrali del tipo INT[x1,x2]{f(x)*W(x)} dx = sum{j=1,n}(w_j*f(x_j))
     SUBROUTINE gaulag(x,w,n,alf)
     implicit none

      INTEGER n,MAXIT
      DOUBLE PRECISION alf,w(n),x(n)
      DOUBLE PRECISION EPS
      PARAMETER (EPS=1.d-14,MAXIT=10)
!CU    USES gammln
      INTEGER i,its,j
      DOUBLE PRECISION ai
      DOUBLE PRECISION p1,p2,p3,pp,z,z1
      do 13 i=1,n
        if(i.eq.1)then
          z=(1.d0+alf)*(3.d0+.92d0*alf)/(1.d0+2.4d0*n+1.8d0*alf)
        else if(i.eq.2)then
          z=z+(15.d0+6.25d0*alf)/(1.d0+.9d0*alf+2.5d0*n)
        else
          ai=i-2
          z=z+((1.d0+2.55d0*ai)/(1.9d0*ai)+1.26d0*ai*alf/ &
		 (1.d0+3.5d0*ai))*(z-x(i-2))/(1.d0+.3d0*alf)
        endif
        do 12 its=1,MAXIT
          p1=1.d0
          p2=0.d0
          do 11 j=1,n
            p3=p2
            p2=p1
            p1=((2*j-1+alf-z)*p2-(j-1+alf)*p3)/j
11        continue
          pp=(n*p1-(n+alf)*p2)/z
          z1=z
          z=z1-p1/pp
          if(abs(z-z1).le.EPS)goto 1
12      continue
        pause 'too many iterations in gaulag'
1       x(i)=z
        w(i)=-exp(gammln(alf+n)-gammln( dble(n)))/(pp*n*p2)
13    continue
      return
      END


! Gauss Hermite - W(x) = e^(-x^2)
! integrali del tipo INT[x1,x2]{f(x)*W(x)} dx = sum{j=1,n}(w_j*f(x_j))
      SUBROUTINE gauher(x,w,n)
      implicit none

      INTEGER n,MAXIT
      DOUBLE PRECISION w(n),x(n)
      DOUBLE PRECISION EPS,PIM4
      PARAMETER (EPS=1.d-14,PIM4=.7511255444649425D0,MAXIT=10)
      INTEGER i,its,j,m
      DOUBLE PRECISION p1,p2,p3,pp,z,z1
      m=(n+1)/2
      do 13 i=1,m
        if(i.eq.1)then
          z=sqrt( dble(2*n+1))-1.85575d0*(2*n+1)**(-.16667d0)
        else if(i.eq.2)then
          z=z-1.14d0*n**.426d0/z
        else if (i.eq.3)then
          z=1.86d0*z-.86d0*x(1)
        else if (i.eq.4)then
          z=1.91d0*z-.91d0*x(2)
        else
          z=2.d0*z-x(i-2)
        endif
        do 12 its=1,MAXIT
          p1=PIM4
          p2=0.d0
          do 11 j=1,n
            p3=p2
            p2=p1
            p1=z*sqrt(2.d0/j)*p2-sqrt(dble(j-1)/dble(j))*p3
11        continue
          pp=sqrt(2.d0*n)*p2
          z1=z
          z=z1-p1/pp
          if(abs(z-z1).le.EPS)goto 1
12      continue
        pause 'too many iterations in gauher'
1       x(i)=z
        x(n+1-i)=-z
        w(i)=2.d0/(pp*pp)
        w(n+1-i)=w(i)
13    continue
      return
      END


! Gauss Jacobi - W(x) = ((1-x)^alf)*((1+x)^bet)  ****alf=-1/2  bet = 0 ----> Gauss Chebyshev
! integrali del tipo INT[x1,x2]{f(x)*W(x)} dx = sum{j=1,n}(w_j*f(x_j))
      SUBROUTINE gaujac(x,w,n,alf,bet)
      implicit none
      INTEGER n,MAXIT
      DOUBLE PRECISION alf,bet,w(n),x(n)
      DOUBLE PRECISION EPS
      PARAMETER (EPS=1.d-14,MAXIT=10)
!    USES gammln
      INTEGER i,its,j
      DOUBLE PRECISION alfbet,an,bn,r1,r2,r3
      DOUBLE PRECISION a,b,c,p1,p2,p3,pp,temp,z,z1
      do 13 i=1,n
        if(i.eq.1)then
          an=alf/n
          bn=bet/n
          r1=(1.d0+alf)*(2.78d0/(4.d0+n*n)+.768d0*an/n)
          r2=1.d0+1.48d0*an+.96d0*bn+.452d0*an*an+.83d0*an*bn
          z=1.d0-r1/r2
        else if(i.eq.2)then
          r1=(4.1d0+alf)/((1.d0+alf)*(1.d0+.156d0*alf))
          r2=1.d0+.06d0*(n-8.d0)*(1.d0+.12d0*alf)/n
          r3=1.d0+.012d0*bet*(1.d0+.25d0*abs(alf))/n
          z=z-(1.d0-z)*r1*r2*r3
        else if(i.eq.3)then
          r1=(1.67d0+.28d0*alf)/(1.d0+.37d0*alf)
          r2=1.d0+.22d0*(n-8.d0)/n
          r3=1.d0+8.d0*bet/((6.28d0+bet)*n*n)
          z=z-(x(1)-z)*r1*r2*r3
        else if(i.eq.n-1)then
          r1=(1.d0+.235d0*bet)/(.766d0+.119d0*bet)
          r2=1.d0/(1.d0+.639d0*(n-4.d0)/(1.d0+.71d0*(n-4.d0)))
          r3=1.d0/(1.d0+20.d0*alf/((7.5d0+alf)*n*n))
          z=z+(z-x(n-3))*r1*r2*r3
        else if(i.eq.n)then
          r1=(1.d0+.37d0*bet)/(1.67d0+.28d0*bet)
          r2=1.d0/(1.d0+.22d0*(n-8.d0)/n)
          r3=1.d0/(1.d0+8.d0*alf/((6.28d0+alf)*n*n))
          z=z+(z-x(n-2))*r1*r2*r3
        else
          z=3.d0*x(i-1)-3.d0*x(i-2)+x(i-3)
        endif
        alfbet=alf+bet
        do 12 its=1,MAXIT
          temp=2.d0+alfbet
          p1=(alf-bet+temp*z)/2.d0
          p2=1.d0
          do 11 j=2,n
            p3=p2
            p2=p1
            temp=2*j+alfbet
            a=2*j*(j+alfbet)*(temp-2.d0)
            b=(temp-1.d0)*(alf*alf-bet*bet+temp*(temp-2.d0)*z)
            c=2.d0*(j-1+alf)*(j-1+bet)*temp
            p1=(b*p2-c*p3)/a
11        continue
          pp=(n*(alf-bet-temp*z)*p1+2.d0*(n+alf)*(n+bet)*p2)/(temp*(1.d0-z*z))
          z1=z
          z=z1-p1/pp
          if(abs(z-z1).le.EPS)goto 1
12      continue
        pause 'too many iterations in gaujac'
1       x(i)=z
        w(i)=exp(gammln(alf+n)+gammln(bet+n)-gammln(n+1.d0)-gammln(n+alfbet+1.d0))* &
		  temp*2.d0**alfbet/(pp*p2)
13    continue
      return
      END

      real*8 FUNCTION gammln(xx)
      implicit none
      DOUBLE PRECISION xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,&
     24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,&
     -.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      end function gammln    

end module

