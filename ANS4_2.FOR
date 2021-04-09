C           THIS PROGRAM SOLVES THE D.E. WITH BANDERY CONDITION
C                          Y"=(32+2X^2-YY')/8
C                      1<X<3 , Y(1)=17 , Y(3)=43/3
C                       TOL=10^-4  ,  Nsteps=100
C                    WITH NON LINEAR SHOOTING METHOD
C
C                         BY: ALI REZA LAJEVARDI

****************************** MAIN PROGRAM ****************************
      Integer  I,Nsteps,Repeat,l
      Double Precision  X(0:100),W(2,0:100),K(2,4,2),U1,U2
      Double Precision  Xbegin,Xend,Alfa,Beta,TK,TOL
      Character Name*11 
             
*     ---------------------- BANDERY CONDITION -------------------------      
      Xbegin=1.0
      Xend=3.0
      Alfa=17.0                ! Y(1)=Alfa=17
      Beta=43.0/3.0            ! Y(3)=Beta=43/3   
      TOL=10D-5                ! Tolerance
      Nsteps=100
*     ------------------------------------------------------------------       

      Write (*,*) ' Enter Numbers Of Repeat in Runge-Kutta Method '
      Read  (*,*) Repeat
      If (Repeat.LT.1)  Stop
      Write (*,*) ' Enter Name for Output File '      
      Read  (*,1) Name
1     Format (A)
      Open  (10,File=Name)
      
      h=(Xend-Xbegin)/Nsteps           ! Length of Step
      l=1                              ! Counter
      TK=(Beta-Alfa)/(Xend-Xbegin)
      
*     ---------------------- Generation of net X -----------------------      
      Do I=0 , Nsteps
         X(I)=Xbegin+I*h
      EndDo
*     ------------------------------------------------------------------      
*     *************************** ITERATION ****************************
1000  W(1,0)=Alfa
      W(2,0)=TK
      U1=0.0
      U2=1.0
*     ----------------------- Runge-Kutta Method -----------------------      
      Do I=0 , Nsteps-1
*                   ---------- Calculation of W ----------
         K(1,1,1)=h*W(2,I)
         K(1,1,2)=h*f(X(I),W(1,I),W(2,I))
         
         K(1,2,1)=h*(W(2,I)+K(1,1,2)/2.0)
         K(1,2,2)=h*f(X(I)+h/2.0,W(1,I)+K(1,1,1)/2.0,W(2,I)+K(1,1,2)/2.)
     
         K(1,3,1)=h*(W(2,I)+K(1,2,2)/2.0)
         K(1,3,2)=h*f(X(I)+h/2.0,W(1,I)+K(1,2,1)/2.0,W(2,I)+K(1,2,2)/2.) 
     
         K(1,4,1)=h*(W(2,I)+K(1,3,2))
         K(1,4,2)=h*f(X(I)+h,W(1,I)+K(1,3,1),W(2,I)+K(1,3,2))
         
         W(1,I+1)=W(1,I)+(K(1,1,1)+2.0*(K(1,2,1)+K(1,3,1))+K(1,4,1))/6.0
         W(2,I+1)=W(2,I)+(K(1,1,2)+2.0*(K(1,2,2)+K(1,3,2))+K(1,4,2))/6.0
         
*                   ---------- Calculation of U ----------
         K(2,1,1)=h*U2
         K(2,1,2)=h*(fy(X(I),W(1,I),W(2,I))*U1+
     +               fyPrm(X(I),W(1,I),W(2,I))*U2)
         
         K(2,2,1)=h*(U2+K(2,1,2)/2.0)
         K(2,2,2)=h*(fy(X(I)+h/2.0,W(1,I),W(2,I))*(U1+K(2,1,1)/2.0)+
     +               fyPrm(X(I)+h/2.0,W(1,I),W(2,I))*(U2+K(2,1,2)/2.0))
     
         K(2,3,1)=h*(U2+K(2,2,2)/2.0)
         K(2,3,2)=h*(fy(X(I)+h/2.0,W(1,I),W(2,I))*(U1+K(2,2,1)/2.0)+
     +               fyPrm(X(I)+h/2.0,W(1,I),W(2,I))*(U2+K(2,2,2)/2.0)) 
     
         K(2,4,1)=h*(U2+K(2,3,2)/2.0)
         K(2,4,2)=h*(fy(X(I)+h,W(1,I),W(2,I))*(U1+K(2,3,1))+
     +               fyPrm(X(I)+h,W(1,I),W(2,I))*(U2+K(2,3,2)))
         
         U1=U1+(K(2,1,1)+2.0*(K(2,2,1)+K(2,3,1))+K(2,4,1))/6.0
         U2=U2+(K(2,1,2)+2.0*(K(2,2,2)+K(2,3,2))+K(2,4,2))/6.0
      EndDo
*     ------------------------------------------------------------------      
*     ------------ Comparison Tolerance with Norm for Output ----------- 
      If (ABS(W(1,Nsteps)-Beta).LE.TOL) Then
        Do I=0 , Nsteps
           Write (10,101) I,X(I),W(1,I),W(2,I)
        EndDo  
        Stop
      EndIf  
*     ------------------------------------------------------------------
      TK=TK-(W(1,Nsteps)-Beta)/U1
      l=l+1
      If (l.LE.Repeat) Goto 1000
*     ******************************************************************      
      Write (*,*)'Tolerance or Numbers of Repeat are Small that is need'
101   Format (I3,2X,F6.3,3X,F10.4,3X,F10.4)
      Close(10)
      END
         

******************************* FUNCTIONS ******************************
      Function f(X,Y,YPrime)
               Real*8 X,Y,YPrime
               f=(32.0+2.0*X*X*X-Y*YPrime)/8.0
      Return
      End
*     ------------------------------------------------------------------      
      Function fy(X,Y,YPrime)
               Real*8 X,Y,YPrime
               fy=-YPrime/8.0
      Return
      End
*     ------------------------------------------------------------------      
      Function fyPrm(X,Y,YPrime)
               Real*8 X,Y,YPrime
               fyPrm=-Y/8.0
      Return
      End   
