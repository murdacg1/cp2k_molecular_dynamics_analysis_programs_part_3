        MODULE dist_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE dist

        INTEGER :: jn, jf, ja1, ja2
        CHARACTER(len=80) :: filename12
        REAL(KIND=dp), DIMENSION(d) :: r_vec = zero
        REAL(KIND=dp) :: r = zero

        WRITE(6,'(a)') '# SUBROUTINE = dist'
        CALL FLUSH(6)

!       WRITE(3,'(a)') '# jf r'
        DO jn = 1, n_dist
          ja1 = atom1_array(jn) + 1
          ja2 = atom2_array(jn) + 1
          filename12 = filename12_array(jn)
          OPEN(3,file=TRIM(filename12),status='unknown')
          DO jf = 0, f
            r_vec = coor(jf,ja2,:) -  coor(jf,ja1,:)
            r_vec(:) = r_vec(:) - cell(:)*ANINT(r_vec(:)/cell(:)) ! do not forget to appy PBCs
            r = SQRT(DOT_PRODUCT(r_vec, r_vec))
            WRITE(3,100) jf, r
          END DO
          CLOSE(3)
        END DO

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(1(1x,i10),1(1x,f12.6))

        END SUBROUTINE dist

        END MODULE dist_mod
