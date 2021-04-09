C           THIS PROGRAM SOLVES THE D.E. WITH BANDERY CONDITION
C                   Y"=(-2/X)Y'+(2/X^2)Y+Sin(LnX)/X^2
C                        1<X<2 , Y(1)=1 , Y(2)=2
C                      WITH LINEAR SHOOTING METHOD
C
C                         BY: ALI REZA LAJEVARDI

****************************** MAIN PROGRAM ****************************
      Integer  I,Nsteps
      Double Precision  X(0:100),U(2,0:100),V(2,0:100),K(2,4,2)
      Double Precision  Xbegin,Xend,Alfa,Beta,W(2,0:100)
      Character Name*11 
             
*     ---------------------- BANDERY CONDITION -------------------------      
      Xbegin=1.0
      Xend=2.0
      Alfa=1.0             ! Y(1)=Alfa=1
      Beta=2.0             ! Y(2)=Beta=2
*     ------------------------------------------------------------------       

      Write (*,*) ' Enter Numbers of Subinterval '
      Read  (*,*) Nsteps
      If (Nsteps.LT.1)  Stop
      Write (*,*) ' Enter Name for Output File '      
      Read  (*,1) Name
1     Format (A)
      Open  (10,File=Name)
      
      h=(Xend-Xbegin)/Nsteps          ! Length of Step
      U(1,0)=Alfa
      U(2,0)=0.0
      V(1,0)=0.0
      V(2,0)=1.0

*     ---------------------- Generation of net X -----------------------      
      Do I=0 , Nsteps
         X(I)=Xbegin+I*h
      EndDo
*     ------------------------------------------------------------------      
*     ----------------------- Runge-Kutta Method -----------------------      
      Do I=0 , Nsteps-1
*                   ---------- Calculation of U ----------
         K(1,1,1)=h*U(2,I)
         K(1,1,2)=h*(p(X(I))*U(2,I)+q(X(I))*U(1,I)+r(X(I)))
         
         K(1,2,1)=h*(U(2,I)+K(1,1,2)/2.0)
         K(1,2,2)=h*(p(X(I)+h/2.0)*(U(2,I)+K(1,1,2)/2.0)+
     +               q(X(I)+h/2.0)*(U(1,I)+K(1,1,1)/2.0)+r(X(I)+h/2.0))
     
         K(1,3,1)=h*(U(2,I)+K(1,2,2)/2.0)
         K(1,3,2)=h*(p(X(I)+h/2.0)*(U(2,I)+K(1,2,2)/2.0)+
     +               q(X(I)+h/2.0)*(U(1,I)+K(1,2,1)/2.0)+r(X(I)+h/2.0)) 
     
         K(1,4,1)=h*(U(2,I)+K(1,3,2))
         K(1,4,2)=h*(p(X(I)+h)*(U(2,I)+K(1,3,2))+
     +               q(X(I)+h)*(U(1,I)+K(1,3,1))+r(X(I)+h))
         
         U(1,I+1)=U(1,I)+(K(1,1,1)+2.0*(K(1,2,1)+K(1,3,1))+K(1,4,1))/6.0
         U(2,I+1)=U(2,I)+(K(1,1,2)+2.0*(K(1,2,2)+K(1,3,2))+K(1,4,2))/6.0
         
*                   ---------- Calculation of V ----------
         K(2,1,1)=h*V(2,I)
         K(2,1,2)=h*(p(X(I))*V(2,I)+q(X(I))*V(1,I))
         
         K(2,2,1)=h*(V(2,I)+K(2,1,2)/2.0)
         K(2,2,2)=h*(p(X(I)+h/2.0)*(V(2,I)+K(2,1,2)/2.0)+
     +               q(X(I)+h/2.0)*(V(1,I)+K(2,1,1)/2.0))
     
         K(2,3,1)=h*(V(2,I)+K(2,2,2)/2.0)
         K(2,3,2)=h*(p(X(I)+h/2.0)*(V(2,I)+K(2,2,2)/2.0)+
     +               q(X(I)+h/2.0)*(V(1,I)+K(2,2,1)/2.0)) 
     
         K(2,4,1)=h*(V(2,I)+K(2,3,2))
         K(2,4,2)=h*(p(X(I)+h)*(V(2,I)+K(2,3,2))+
     +               q(X(I)+h)*(V(1,I)+K(2,3,1)))
         
         V(1,I+1)=V(1,I)+(K(2,1,1)+2.0*(K(2,2,1)+K(2,3,1))+K(2,4,1))/6.0
         V(2,I+1)=V(2,I)+(K(2,1,2)+2.0*(K(2,2,2)+K(2,3,2))+K(2,4,2))/6.0
      EndDo
*     ------------------------------------------------------------------      
      C=(Beta-U(1,Nsteps))/V(1,Nsteps)      ! Y(X)=Y1(X)+C*Y2(X)
      Do I=0 , Nsteps                       
         W(1,I)=U(1,I)+C*V(1,I)             ! Y1(X)=U1  ,  Y2(X)=V1
         W(2,I)=U(2,I)+C*V(2,I)             ! Y1'(X)=U2 , Y2'(X)=V2
      EndDo
*------------------------------- Output --------------------------------      
      DO I=0 , Nsteps
         Write (10,101) I,X(I),W(1,I),W(2,I)
      EndDo
*-----------------------------------------------------------------------
101   Format (I3,2X,F6.3,3X,F9.4,3X,F9.4)
      Close(10)
      END
         

******************************* FUNCTIONS ******************************
      Function p(X)
               Real*8 X
               p=-2.0/X
      Return
      End
*     ------------------------------------------------------------------      
      Function q(X)
               Real*8 X
               q=2.0/(X*X)
      Return
      End
*     ------------------------------------------------------------------      
      Function r(X)
               Real*8 X
               r=SIN(LOG(X))/(X*X)
      Return
      End   
