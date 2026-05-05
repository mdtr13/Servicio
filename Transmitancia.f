IMPLICIT NONE
      INTEGER N, N_final
      REAL(16) M_o(2,2,100), U_1(2,2), TMP_o(2,2)
      REAL(16) M_m(2,2,100), U_2(2,2), TMP_m(2,2)
      REAL(16) TMP_tau_o(2,2), TMP_tau_m(2,2)
      REAL(16) TAU_o(2,2), TAU_m(2,2)
      REAL(16) U_1_1(2,2), U_1_2(2,2)
      REAL(16) U_2_1(2,2), U_2_2(2,2)
      REAL(16) I_o(2,2), F_o(2,2), I_m(2,2), F_m(2,2)
      !! Parametros para el bucle!!!!!!!!!!!!!!!!!!
      INTEGER I, Pasos
      !REAL(16) E_min, E_max, Delta_E, E, Trans_o
      REAL(16) E_min, E_max, Delta_E
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      REAL(16) E, EA, EB, TAA, TAB, TBA, EPS, T
      REAL(16) E_I_o, T_I_o, E_F_o, T_F_o
      REAL(16) E_I_m, T_I_m, E_F_m, T_F_m
      REAL(16) suma_1, suma_2, suma_3, suma_4, suma_5
      REAL(16) suma_6, suma_7, suma_8
      REAL(16) denom_1, denom_2, denom_3, denom_4
      REAL(16) Trans_m, Trans_o
      REAL(16) denom_o, denom_m
      REAL(16) Aux1_o(2,2), Aux2_o(2,2), Aux3_o(2,2), M_temp_o(2,2)
      REAL(16) Aux1_m(2,2), Aux2_m(2,2), Aux3_m(2,2), M_temp_m(2,2)
      ! --- Parametros ---
      E = 0.0q0
      EPS = 1.0q-3
      EA = 0.0q0
      EB = -0.0q0
      TAA = -1.0q0
      TAB = -0.8q0
      TBA = -0.8q0
      N_final =10 ! Generacions totales
      T = -1.0q0
      ! --- Intervalo de energia ---
      E_min = -2.0q0
      E_max = 2.0q0
      Pasos = 40000
      Delta_E = (E_max - E_min) / REAL(Pasos, 16)

      ! Aqui se guardan los datos

1     FORMAT(F9.5,1x,E27.18,1x,E27.18)
      OPEN(1, FILE='trans_o_g11.dat')
      ! --- INICIO DEL DO ---
      DO I = 0, Pasos
          E = E_min + REAL(I, 16) * Delta_E
      !-----------------------------------------------
      !       Matrices Iniciales para ORIGINAL
      ! ----------------------------------------------
      !!! M(3)_o!!!
      M_o(1,1,3) = (E-EA)/TAA ; M_o(1,2,3) = -TBA/TAA
      M_o(2,1,3) = 1.0q0                  ; M_o(2,2,3) = 0.0q0
      !!! M(4)_o !!!
      !   --- Aux_1 ---
      Aux1_o(1,1) = (E-EA)/TAB ; Aux1_o(1,2) = -TBA/TAB
      Aux1_o(2,1) = 1.0q0                  ; Aux1_o(2,2) = 0.0q0
      !   --- Aux_2 ---
      Aux2_o(1,1) = (E-EB)/TBA ; Aux2_o(1,2) = -TAB/TBA
      Aux2_o(2,1) = 1.0q0                  ; Aux2_o(2,2) = 0.0q0
      !   --- Aux_3 ---
      Aux3_o(1,1) = (E-EA)/TAB ; Aux3_o(1,2) = -TAA/TAB
      Aux3_o(2,1) = 1.0q0                  ; Aux3_o(2,2) = 0.0q0
      CALL MAT_MUL(Aux1_o, Aux2_o, M_temp_o)
      CALL MAT_MUL(M_temp_o, Aux3_o, M_o(1,1,4))
      !-----------------------------------------------
      !       Matrices Iniciales para MIRROR
      ! ----------------------------------------------
      !!! M(3)_m !!!
      M_m(1,1,3) = (E-EA)/TAB ; M_m(1,2,3) = -TAA/TAB
      M_m(2,1,3) = 1.0q0                  ; M_m(2,2,3) = 0.0q0
      !!! M(4)_m !!!
      !   --- Aux_1 ---
      Aux1_m(1,1) = (E-EA)/TAA ; Aux1_m(1,2) = -TBA/TAA
      Aux1_m(2,1) = 1.0q0                  ; Aux1_m(2,2) = 0.0q0
      !   --- Aux_2 ---
      Aux2_m(1,1) = (E-EB)/TBA ; Aux2_m(1,2) = -TAB/TBA
      Aux2_m(2,1) = 1.0q0                  ; Aux2_m(2,2) = 0.0q0
      !   --- Aux_3 ---
      Aux3_m(1,1) = (E-EA)/TAB ; Aux3_m(1,2) = -TBA/TAB
      Aux3_m(2,1) = 1.0q0                  ; Aux3_m(2,2) = 0.0q0
      CALL MAT_MUL(Aux1_m, Aux2_m, M_temp_m)
      CALL MAT_MUL(M_temp_m, Aux3_m, M_m(1,1,4))
      !-----------------------------------------------
      !       Matrices U_1 y U_2
      ! ----------------------------------------------
      !!! Auxiliares de U_1 !!!
      U_1_1(1,1) = (E-EA)/TAA ; U_1_1(1,2) = -TBA/TAA
      U_1_1(2,1) = 1.0q0                  ; U_1_1(2,2) = 0.0q0
      U_1_2(1,1) = (E-EB)/TBA ; U_1_2(1,2) = -TAB/TBA
      U_1_2(2,1) = 1.0q0                  ; U_1_2(2,2) = 0.0q0
      !!! U_1 !!!
      CALL MAT_MUL(U_1_1, U_1_2, U_1)
      !!! Auxiliares de U_2 !!!
      U_2_1(1,1) = (E-EB)/TBA ; U_2_1(1,2) = -TAB/TBA
      U_2_1(2,1) = 1.0q0                  ; U_2_1(2,2) = 0.0q0
      U_2_2(1,1) = (E-EA)/TAB ; U_2_2(1,2) = -TAA/TAB
      U_2_2(2,1) = 1.0q0                  ; U_2_2(2,2) = 0.0q0
      !!! U_2 !!!
      CALL MAT_MUL(U_2_1, U_2_2, U_2)
      !-----------------------------------------------
      !       M(n) para ORIGINAL
      ! ----------------------------------------------
      DO N = 5, N_final   ! Va del rango 5 hasta N_final
         IF (MOD(N, 2) .NE. 0) THEN !!! Caso IMPAR
          CALL MAT_MUL(M_o(1,1,N-2), U_2, TMP_o)
          CALL MAT_MUL(TMP_o, M_m(1,1,N-1), M_o(1,1,N))
        ELSE
         CALL MAT_MUL(M_o(1,1,N-2), U_1, TMP_o)
         CALL MAT_MUL(TMP_o, M_m(1,1,N-1), M_o(1,1,N))
        END IF
      END DO
      !-----------------------------------------------
      !       M(n) para MIRROR
      ! ----------------------------------------------
      ! ---- GENERAR M(N) PARA LA CADENA MIRROR -----
      DO N = 5, N_final
         IF (MOD(N, 2) .NE. 0) THEN !!! Caso IMPAR
          CALL MAT_MUL(M_o(1,1,N-1), U_1, TMP_m)
          CALL MAT_MUL(TMP_m, M_m(1,1,N-2), M_m(1,1,N))
        ELSE
         CALL MAT_MUL(M_o(1,1,N-1), U_2, TMP_m)
         CALL MAT_MUL(TMP_m, M_m(1,1,N-2), M_m(1,1,N))
        END IF
      END DO
      !-----------------------------------------------
      !       Matrices F y I PARA ORIGINAL
      ! ----------------------------------------------
      CALL Matriz_F(N_final, E, EPS, EA, EB, TAA, TAB, TBA, F_o,T,F_m )
      CALL Matriz_I(N_final, E, EPS, EA, EB, TAA, TAB, TBA, I_o,T,I_m )
      !-----------------------------------------------
      !       Matriz Tau
      ! ----------------------------------------------
      ! ORIGINAL !
      CALL MAT_MUL(F_o,M_o(1,1,N_final),TMP_tau_o)
      CALL MAT_MUL(TMP_tau_o, I_o, TAU_o)
      ! MIRROR !
      CALL MAT_MUL(F_m,M_m(1,1,N_final),TMP_tau_m)
      CALL MAT_MUL(TMP_tau_m, I_m, TAU_m)
      !-----------------------------------------------
      !      Transmitancia
      ! ----------------------------------------------
        ! Original !
        suma_1 = (Tau_o(2,1)) - (Tau_o(1,2))
        suma_2 = ((Tau_o(2,2)) - (Tau_o(1,1))) * (E/(2.0q0*T))
        denom_1 = (suma_1 + suma_2)**2
        suma_3 = ( (Tau_o(2,2)) + (Tau_o(1,1)) )**2
        suma_4 = 1.0q0 - ((E*E)/ (4.0q0*(T**2)))
        denom_2 = suma_3 * suma_4
        denom_o = denom_1 + denom_2
        Trans_o = ((4.0q0 - ((E/T)**2) ) / denom_o)
        ! Mirror !
        suma_5 = (Tau_m(2,1)) - (Tau_m(1,2))
        suma_6 = ((Tau_m(2,2)) - (Tau_m(1,1))) * (E/(2.0q0*T))
        denom_3 = (suma_5 + suma_6)**2
        suma_7 = ( (Tau_m(2,2)) + (Tau_m(1,1)) )**2
        suma_8 = 1.0q0-((E*E) / (4.0q0*(T**2)))
        denom_4 = suma_7 * suma_8
        denom_m = denom_3 + denom_4
        Trans_m = ((4.0q0 - ((E/T)**2) ) / denom_m)
          ! ==========================================
          ! Escribe resultado de esta energÃ­en el archivo
          ! Columna 1: EnergÃ­          ! Columna 2: Transmitancia
          WRITE(1,1) E, Trans_o
      END DO
      ! --- FIN DEL DO ---
      CLOSE(10)
      WRITE(*,*) "Terminado. Datos en 'trans_o_g5.dat'"
      END PROGRAM
!*************************************************
!                SUBRUTINAS
!*************************************************
! -----------------------------------------
! Subrutina: MultiplicaciÃ³e Matrices 2x2
! -----------------------------------------
      SUBROUTINE MAT_MUL(A, B, C)
      IMPLICIT NONE
      REAL(16) A(2,2), B(2,2), C(2,2)
      C(1,1) = A(1,1)*B(1,1) + A(1,2)*B(2,1)
      C(1,2) = A(1,1)*B(1,2) + A(1,2)*B(2,2)
      C(2,1) = A(2,1)*B(1,1) + A(2,2)*B(2,1)
      C(2,2) = A(2,1)*B(1,2) + A(2,2)*B(2,2)
      RETURN
      END
! ------------------------------------------
! Subrutina: Imprimir Matrices 2x2
! ------------------------------------------
      SUBROUTINE IMPRIMIR_MATRIZ(MAT)
      REAL(16) MAT(2,2)
      WRITE(*,'(A, 2(E11.3, 1X, E11.3, 2X))') "  Fila 1: ",
     &      (MAT(1,1)),
     &      (MAT(1,2))
      WRITE(*,'(A, 2(E11.3, 1X, E11.3, 2X))') "  Fila 2: ",
     &      (MAT(2,1)),
     &      (MAT(2,2))
      WRITE(*,*) ""
      END SUBROUTINE
! ------------------------------------------
! Subrutina: Matriz F
! ------------------------------------------
      SUBROUTINE Matriz_F(N, E, EPS, EA, EB, TAA, TAB, TBA, F_o,T, F_m)
      INTEGER N
      REAL(16) E, EA, EB, TAA, TAB, TBA, EPS
      REAL(16) E_F_o, T_F_o, T, E_F_m, T_F_m
      REAL(16) F_o(2,2), F_m(2,2)
      ! PARA EL CASO DE LA CADENA ORIGINAL
      E_F_o = ((1.0q0 - ((-1.0q0)**N))/2.0q0)*EA +
     &          ((1.0q0 + ((-1.0q0)**N))/2.0q0)*EB
      T_F_o = ((1.0q0 - ((-1.0q0)**N))/2.0q0)*TAA +
     &          ((1.0q0 + ((-1.0q0)**N))/2.0q0)*TAB
      F_o(1,1) = (E-E_F_o)/T
      F_o(1,2) = -T_F_o/T
      F_o(2,1) = 1.0q0
      F_o(2,2) = 0.0q0
      ! PARA EL CASO DE LA CADENA MIRROR
      E_F_m = ((1.0q0 - ((-1.0q0)**N))/2.0q0)*EB +
     &          ((1.0q0 + ((-1.0q0)**N))/2.0q0)*EA
      T_F_m = ((1.0q0 - ((-1.0q0)**N))/2.0q0)*TAB +
     &          ((1.0q0 + ((-1.0q0)**N))/2.0q0)*TAA
      F_m(1,1) = (E-E_F_m)/T
      F_m(1,2) = -T_F_m/T
      F_m(2,1) = 1.0q0
      F_m(2,2) = 0.0q0
      END SUBROUTINE
! ------------------------------------------
! Subrutina: Matriz I
! ------------------------------------------
      SUBROUTINE Matriz_I(N, E, EPS, EA, EB, TAA, TAB, TBA, I_o,T,I_m)
      INTEGER N
      REAL(16) E, EA, EB, TAA, TAB, TBA, EPS
      REAL(16) E_I_o, T_I_o, T, E_I_m, T_I_m
      REAL(16) I_o(2,2), I_m(2,2)
      ! PARA EL CASO DE LA CADENA ORIGINAL
      E_I_o = ((1.0q0 - ((-1.0q0)**N))/2.0q0)*EB +
     &          ((1.0q0 + ((-1.0q0)**N))/2.0q0)*EA
      T_I_o = ((1.0q0 - ((-1.0q0)**N))/2.0q0)*TBA +
     &          ((1.0q0 + ((-1.0q0)**N))/2.0q0)*TAA
      I_o(1,1) = (E-E_I_o)/T_I_o
      I_o(1,2) = -T/T_I_o
      I_o(2,1) = 1.0q0
      I_o(2,2) = 0.0q0
      ! PARA EL CASO DE LA CADENA MIRROR
      E_I_m = ((1.0q0 - ((-1.0q0)**N))/2.0q0)*EA +
     &          ((1.0q0 + ((-1.0q0)**N))/2.0q0)*EB
      T_I_m = ((1.0q0 - ((-1.0q0)**N))/2.0q0)*TAA +
     &          ((1.0q0 + ((-1.0q0)**N))/2.0q0)*TBA
      I_m(1,1) = (E-E_I_m)/T_I_m
      I_m(1,2) = -T/T_I_m
      I_m(2,1) = 1.0q0
      I_m(2,2) = 0.0q0
      END SUBROUTINE