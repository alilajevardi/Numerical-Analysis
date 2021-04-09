C     THIS PROGRAM SOLVES THE P0ISSON's PARTIAL DIFFRENTIAL EQUATION            
C          d^2u(x,y)/dx^2 + d^2u(x,y)/dy^2 =(x^2 + y^2)EXP(xy)                   
C                      0 <x< 2    ,   0 <x< 1       
C                      u(0,y)=1   ,   u(2,y)=EXP(2y)
C                      u(x,0)=1   ,   u(x,1)=EXP(x)
C                 h=0.2 , k=0.1   ,   TOLERANCE=10^-4
C 
C                       BY: ALI REZA LAJEVARDI        
C
      Integer  I,J,n,m,l,Repeat
      Double Precision  FX,FY,XI,YJ,X(0:100),Y(0:100),W(0:100,0:100)
      Double Precision  X0,Xn,Y0,Ym,a,b,c,d,h,k,Z,hk,Miu,Norm,TOL     
      Character Name*11 
             
*     -------------------- First Functions and Values ------------------      
      f(FX,FY)=(FX*FX+FY*FY)*EXP(FX*FY)
      g1(X0,YJ)=1.0
      g2(Xn,YJ)=EXP(2.0*YJ)
      g3(XI,Y0)=1.0
      g4(XI,Ym)=EXP(XI)
      a=0.0
      b=2.0                                           
      c=0.0
      d=1.0
      n=10
      m=10
      TOL=10D-5
*     ------------------------------------------------------------------       

      Write (*,*) ' Enter Numbers Of Repeat in Gauss-Seidel iteration '
      Read  (*,*) Repeat
      If (Repeat.LT.1)  Stop
      Write (*,*) ' Enter Name for Output File '      
      Read  (*,1) Name
1     Format (A)
      Open  (10,File=Name)
      
      h=(b-a)/n
      k=(d-c)/m 
*     --------------------- Generation of net X and Y ------------------      
      Do I=0 , n
         X(I)=a+I*h
      EndDo
      DO J=0 , m
         Y(J)=c+J*k
      EndDo
*     ------------------------------------------------------------------      
      Do J=0 , m
         W(0,J)=g1(a,Y(J))
         W(n,J)=g2(b,Y(J))
      EndDo
      Do I=1 , n-1
         W(I,0)=g3(X(I),c)
         W(I,m)=g4(X(I),d)
      EndDo
*     ---------------------------- Zero Vector -------------------------      
      Do J=1 , m-1
        Do I=1 , n-1
           W(I,J)=0.0
        EndDo
      EndDo
*     ------------------------------------------------------------------      
      hk=(h/k)**2.0
      Miu=2.0*(1+hk)
      l=0
      
*     ******************** Gauss-Seidel iteration **********************
1000  Norm=0.0
      Do J=m-1 , 1 ,-1
        Do I=1 , n-1
           Z=(-h*h*f(X(I),Y(J))+W(I-1,J)+hk*W(I,J+1)+hk*W(I,J-1)+
     +        W(I+1,J))/Miu         
           If (ABS(W(I,J)-Z).GT.Norm) Then
              Norm=ABS(W(I,J)-Z)
              W(I,J)=Z
           EndIf
        EndDo
      EndDo
*     ------------ Comparison Tolerance with Norm for Output -----------
      If (TOL.GE.Norm) Then
          Do  I=0 , n
            Do  J=0 , m
              Write (10,101) X(I),Y(J),W(I,J)
            EndDo
          EndDo
          Stop
      EndIf
*     ------------------------------------------------------------------      
      l=l+1
      If (l.LT.Repeat) Goto 1000  
*     ******************************************************************      
101   Format (2X,F5.2,3X,F5.2,3X,F8.4)
      Write (*,*)'Tolerance or Numbers of Repeat are Small that is need'
      Close(10)
      END 
      
 