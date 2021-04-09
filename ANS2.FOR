C                THIS PROGRAM SOLVES THE DIFFRENTIAL EQUATION
C                            Y'=(1/X^2)-Y/X-Y^2
C                                 1<X<2
C                                Y(1)=-1
C                          WITH RUNGE-KUTTA METHOD 
C
C                        BY: ALI REZA LAJEVARDI POUR

*******************************MAIN PROGRAM**********************************
      Integer N,Nsteps
      Real    h,Deriv,Xbegin,Ybegin,Xend,XN,YN
      Common  h,N,Xbegin
      Character Name*11
*     ------------------------- First Values -------------------------------
      Xbegin=1.0
      Xend=2.0
      Ybegin=-1.0
*     -----------------------------------------------------------------------      
      Write (*,*)' Enter Numbers Of Repeat in Runge-Kutta Method '
      Read (*,*) Nsteps
      If (Nsteps.LT.1) Stop
      Write (*,*) ' Enter Name for Output File '      
      Read (*,1) Name
1     Format (A)
      Open (10,File=Name)
      
      h=(Xend-Xbegin)/Nsteps
      XN=Xbegin
      YN=Ybegin
      
      Do  N=0,Nsteps
         Call Runge(XN,YN,Deriv)
         Write (10,101) N,XN,YN,Deriv
      EndDo                          
      
101   Format (I3,3X,F5.3,3X,F9.4,3X,F9.4)      
      Close(10)
      END  

**************************FUNCTIONS AND SUBPROGRAM**********************
      Function FUN (A,B)
              FUN=1/(A*A)-B/A-B*B
       Return
       End

*----------------------------------------------------------------------*               

      Subroutine Runge(X,Y,Df)
               Real K1,K2,K3,K4
               Common h,N,Xbegin
               If (N.EQ.0) Goto 1000
*     ------------------------- Runge-Kutta Method ---------------------        
               K1=h*FUN(X,Y)
               K2=h*FUN(X+h/2.0,Y+K1/2.0)
               K3=h*FUN(X+h/2.0,Y+K2/2.0)
               K4=h*FUN(X+h,Y+K3)
1000           Y=Y+(1.0/6.0)*(K1+2.0*K2+2.0*K3+K4)
*     ------------------------------------------------------------------               
               X=Xbegin+N*h           
               Df=FUN(X,Y)
      Return
      End   
                           