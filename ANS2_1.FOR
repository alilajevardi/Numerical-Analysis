C                THIS PROGRAM SOLVES THE DIFFRENTIAL EQUATION
C                           Y"'+2Y"-Y'-2Y=EXP(X) 
C                                 0 <X< 3
C                         Y(0)=1  Y'(0)=2  Y"(0)=0
C                         WITH RUNGE-KUTTA METHOD 
C
C                        BY: ALI REZA LAJEVARDI 

****************************** Main Program ****************************
      
      Implicit Real(X)
      Integer I,J,Nsteps
      Real h,K1(2),K2(2),K3(2),K4(2),X(0:100),U(2,0:100)
      Character Name*11
      
      Write (*,*)' Enter Numbers Of Repeat in Runge-Kutta Method '
      Read (*,*) Nsteps
      If (Nsteps.LT.1) Stop
      Write (*,*) ' Enter Name for Output File '      
      Read (*,1) Name
1     Format (A)
      Open (10,File=Name)
      
*     ------------------------- First Values ---------------------------      
      Xbegin=0.0
      Xend=3.0
      U(1,0)=1.0D0
      U(2,0)=2.0D0
      U(3,0)=0.0D0
*     ------------------------------------------------------------------      
      h=(Xend-Xbegin)/Nsteps

*     ---------------------- Generation of net X -----------------------
      Do I=0 , Nsteps
         X(I)=Xbegin+I*h
      EndDo
*     ------------------------------------------------------------------

*     ----------------------- Runge-Kutta Method -----------------------
      Do I=0 , Nsteps-1 
         Do J=1 , 3
           K1(J)=h*f(J,X(I),U(1,I),U(2,I),U(3,I))
         EndDo
         Do J=1 , 3
           K2(J)=h*f(J,X(I)+h/2.0,U(1,I)+1./2.*K1(1),U(2,I)+1./2.*K1(2),
     +               U(3,I)+1./2.*K1(3))
         EndDo
         Do J=1 , 3
           K3(J)=h*f(J,X(I)+h/2.0,U(1,I)+1./2.*K2(1),U(2,I)+1./2.*K2(2),
     +               U(3,I)+1./2.*K2(3))
         EndDo
         Do J=1 , 3
           K4(J)=h*f(J,X(I)+h,U(1,I)+K3(1),U(2,I)+K3(2),U(3,I)+K3(3))
         EndDo
         Do J=1 , 3
           U(J,I+1)=U(J,I)+1.0/6.0*(K1(J)+2.0*(K2(J)+K3(J))+K4(J))
         EndDo 
      EndDo  
*     ------------------------------------------------------------------      
*     ---------------------------- Output ------------------------------
      Do I=0 , Nsteps
         Write(10,101) I,X(I),(U(J,I),J=1,3)  
      EndDo
101   Format (2X,I3,2X,F5.3,3(F8.4,2X))
*     ------------------------------------------------------------------
      Close(10)
      END
      

*     *************************** FUNCTION *****************************
      
      Function f(J,X,U1,U2,U3)
              Integer J
              Real    X,U1,U2,U3
              If (J.EQ.1) f=U2
              If (J.EQ.2) f=U3
              If (J.EQ.3) f=-2.0*U3+U2+2.0*U1+Exp(X)
      Return
      End        
         