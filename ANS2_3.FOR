C            THIS PROGRAM SOLVES THE SYSTEM OF DIFFRENTIAL EQUATION
C                               u1'=3u1+2u2 
C                               u2'=4u1+u2
C                                 0 <X< 1
C                            u1(0)=0  u2(0)=1 
C                         WITH RUNGE-KUTTA METHOD 
C
C                         BY: ALI REZA LAJEVARDI 
                     
****************************** Main Program ****************************
      
      Implicit Real*8 (X)
      Integer  I,J,Nsteps
      Double Precision  h,K1(2),K2(2),K3(2),K4(2),X(0:100),U(2,0:100)
      Character Name*11 
            
      Write (*,*) ' Enter Numbers Of Repeat in Runge-Kutta Method '
      Read  (*,*) Nsteps
      If (Nsteps.LT.1)  Stop
      Write (*,*) ' Enter Name for Output File '      
      Read  (*,1) Name
1     Format (A)
      Open  (10,File=Name)
      
*     ------------------------- First Values ---------------------------      
      Xbegin=0.0D0
      Xend=1.0D0
      U(1,0)=0.0
      U(2,0)=1.0      
*     ------------------------------------------------------------------      
      h=(Xend-Xbegin)/Nsteps

*     ---------------------- Generation of net X -----------------------
      Do I=0 , Nsteps
         X(I)=Xbegin+I*h
      EndDo
*     ------------------------------------------------------------------

*     ----------------------- Runge-Kutta Method -----------------------
      Do I=0 , Nsteps-1 
         Do J=1 , 2
           K1(J)=h*f(J,X(I),U(1,I),U(2,I))
         EndDo
         Do J=1 , 2
           K2(J)=h*f(J,X(I)+h/2.0,U(1,I)+K1(1)/2.,U(2,I)+K1(2)/2.)
         EndDo
         Do J=1 , 2
           K3(J)=h*f(J,X(I)+h/2.0,U(1,I)+K2(1)/2.,U(2,I)+K2(2)/2.)
         EndDo
         Do J=1 , 2
           K4(J)=h*f(J,X(I)+h,U(1,I)+K3(1),U(2,I)+K3(2))
         EndDo
         Do J=1 , 2
           U(J,I+1)=U(J,I)+(K1(J)+2.0*(K2(J)+K3(J))+K4(J))/6.0
         EndDo 
      EndDo  
*     ------------------------------------------------------------------      
*     ---------------------------- Output ------------------------------
      Do I=0 , Nsteps
         Write(10,101) I,X(I),(U(J,I),J=1,2)  
      EndDo
101   Format (2X,I3,2X,F5.3,2X,2(F9.4,2X))
*     ------------------------------------------------------------------
      Close(10)
      END
      

*     *************************** FUNCTION *****************************
      
      Function f(J,X,U1,U2)
               Integer J
               Double Precision  X,U1,U2
               If (J.EQ.1) f=3.0*U1+2.0*U2
               If (J.EQ.2) f=4.0*U1+U2
      Return
      End        
         