IMPLICIT NONE
      INTEGER N,NMAX,EMAX,J
      REAL(16) TAA,TAB,EA,EB,TC,E2,EPS,DOS,PI
      COMPLEX(16) AO(100),BO(100),CO(100),DO(100),AM(100),BM(100),
     &CM(100),DM(100),TO(100),TM(100),ELO(100),ERO(100),ELM(100),
     &ERM(100),DENOO(100),DENOM(100),ALFA1(100),ALFA2(100),ALFA3(100),
     &ALFA4(100),ALFA5(100),ALFA6(100),BETA1(100),BETA2(100),
     &BETA3(100),BETA4(100),BETA5(100),BETA6(100),EE,GM11,GM12,GM22
!
      OPEN(1,FILE='dos5tab0p8.dat')
1     FORMAT(F9.5,1x,E27.18,1x,E27.18)
2     FORMAT(I3,I3,2F27.18)
3     FORMAT(I7,1x,2E22.14)
5     FORMAT(F18.9,1x,E27.18)

      PI=4.0q0*ATAN(1.0q0)
      DOS=0.0q0
      TAA=-1.0q0
      TAB=-0.8q0
      EA=0.5q0
      EB=-0.5q0
      EPS=1.0q-3
      NMAX=10  !Generacion minima
      EMAX=8000
!
      DO J=1,EMAX+1
      E2=-2.0q0+4.0q0*(J-1)/EMAX
      EE=QCMPLX(E2,EPS)
!
      ELO(2)=EA
      ERO(2)=EB
      TO(2)=TAB
!
      ELM(2)=EB
      ERM(2)=EA
      TM(2)=TAB
!
      ELO(3)=EB+TAB*TAB/(EE-EA)
      ERO(3)=EA+TAA*TAA/(EE-EA)
      TO(3)=TAB*TAA/(EE-EA)
!
      ELM(3)=EA+TAA*TAA/(EE-EA)
      ERM(3)=EB+TAB*TAB/(EE-EA)
      TM(3)=TAB*TAA/(EE-EA)
!
      AO(2)=1.0q0
      BO(2)=1.0q0
      CO(2)=0.0q0
      DO(2)=0.0q0
!
      AM(2)=1.0q0
      BM(2)=1.0q0
      CM(2)=0.0q0
      DM(2)=0.0q0
!
      AO(3)=1.0q0+TAB*TAB/((EE-EA)*(EE-EA))
      BO(3)=1.0+TAA*TAA/((EE-EA)*(EE-EA))
      CO(3)=2.0q0*TAA*TAB/((EE-EA)*(EE-EA))
      DO(3)=1.0q0/(EE-EA)
!
      AM(3)=1.0q0+TAA*TAA/((EE-EA)*(EE-EA))
      BM(3)=1.0q0+TAB*TAB/((EE-EA)*(EE-EA))
      CM(3)=2.0q0*TAA*TAB/((EE-EA)*(EE-EA))
      DM(3)=1.0q0/(EE-EA)
!
      DO N=4,NMAX
!
      DENOM(N)=(EE-ERM(N-2))*(EE-ELO(N-1))-TAB*TAB
      DENOO(N)=(EE-ELO(N-2))*(EE-ERM(N-1))-TAB*TAB
      ELM(N)=ELM(N-2)+TM(N-2)*TM(N-2)*(EE-ELO(N-1))/DENOM(N)
      ERM(N)=ERO(N-1)+TO(N-1)*TO(N-1)*(EE-ERM(N-2))/DENOM(N)
      TM(N)=TAB*TO(N-1)*TM(N-2)/DENOM(N)
      ELO(N)=ELM(N-1)+TM(N-1)*TM(N-1)*(EE-ELO(N-2))/DENOO(N)
      ERO(N)=ERO(N-2)+TO(N-2)*TO(N-2)*(EE-ERM(N-1))/DENOO(N)
      TO(N)=TAB*TO(N-2)*TM(N-1)/DENOO(N)
!
      ALFA1(N)=TM(N-1)*(EE-ELO(N-2))/DENOO(N)
      ALFA2(N)=TO(N-2)*(EE-ERM(N-1))/DENOO(N)
      ALFA3(N)=TM(N-1)*TAB/DENOO(N)
      ALFA4(N)=TO(N-2)*TAB/DENOO(N)
      ALFA5(N)=2.0q0*TO(N-2)*TM(N-1)*TAB*(EE-ELO(N-2))/
     &(DENOO(N)*DENOO(N))
      ALFA6(N)=2.0q0*TO(N-2)*TM(N-1)*TAB*(EE-ERM(N-1))/
     &(DENOO(N)*DENOO(N))
!
      BETA1(N)=TM(N-2)*(EE-ELO(N-1))/DENOM(N)
      BETA2(N)=TO(N-1)*(EE-ERM(N-2))/DENOM(N)
      BETA3(N)=TM(N-2)*TAB/DENOM(N)
      BETA4(N)=TO(N-1)*TAB/DENOM(N)
      BETA5(N)=2.0q0*TO(N-1)*TM(N-2)*TAB*(EE-ELO(N-1))/
     &(DENOM(N)*DENOM(N))
      BETA6(N)=2.0q0*TO(N-1)*TM(N-2)*TAB*(EE-ERM(N-2))/
     &(DENOM(N)*DENOM(N))
!
      AO(N)=AM(N-1)+ALFA1(N)*ALFA1(N)*BM(N-1)+ALFA1(N)*CM(N-1)+
     &ALFA3(N)*ALFA3(N)*AO(N-2)
      BO(N)=BO(N-2)+ALFA4(N)*ALFA4(N)*BM(N-1)+ALFA2(N)*ALFA2(N)*
     &AO(N-2)+ALFA2(N)*CO(N-2)
      CO(N)=ALFA5(N)*BM(N-1)+ALFA4(N)*CM(N-1)+ALFA6(N)*AO(N-2)+
     &ALFA3(N)*CO(N-2)
      DO(N)=DO(N-2)+DM(N-1)+ALFA2(N)/TO(N-2)*AO(N-2)+ALFA1(N)/TM(N-1)*
     &BM(N-1)
!
      AM(N)=AM(N-2)+BETA1(N)*BETA1(N)*BM(N-2)+BETA1(N)*CM(N-2)+
     &BETA3(N)*BETA3(N)*AO(N-1)
      BM(N)=BO(N-1)+BETA4(N)*BETA4(N)*BM(N-2)+BETA2(N)*BETA2(N)*
     &AO(N-1)+BETA2(N)*CO(N-1)
      CM(N)=BETA5(N)*BM(N-2)+BETA4(N)*CM(N-2)+BETA6(N)*AO(N-1)+
     &BETA3(N)*CO(N-1)
      DM(N)=DO(N-1)+DM(N-2)+BETA2(N)/TO(N-1)*AO(N-1)+BETA1(N)/TM(N-2)*
     &BM(N-2)
!
      END DO
!
      GM11=(EE-ERM(NMAX))/((EE-ERM(NMAX))*(EE-ELM(NMAX))-
     &TM(NMAX)*TM(NMAX))
!
      GM12=TM(NMAX)/((EE-ERM(NMAX))*(EE-ELM(NMAX))-TM(NMAX)*TM(NMAX))
!
      GM22=(EE-ELM(NMAX))/((EE-ERM(NMAX))*(EE-ELM(NMAX))-
     &TM(NMAX)*TM(NMAX))
!
      DOS=-AIMAG(AM(NMAX)*GM11+BM(NMAX)*GM22+CM(NMAX)*GM12+DM(NMAX))/PI
!
      WRITE(1,1)E2,DOS
!      WRITE(*,1)E2,DOS
      END DO
      END
