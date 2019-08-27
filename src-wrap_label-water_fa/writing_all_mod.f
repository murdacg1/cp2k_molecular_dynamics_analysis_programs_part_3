        MODULE writing_all_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE writing_all(my_m, my_a, my_d, my_array_all, my_symbol_all, &
          my_file)

        INTEGER :: my_m, my_a, my_d
        REAL(KIND=dp), DIMENSION(0:f,my_m,my_a,my_d) :: my_array_all
        CHARACTER(len=2), DIMENSION(:) :: my_symbol_all
        CHARACTER(len=80) my_file

        INTEGER :: jf, jm, ja, jd

        WRITE(6,'(2a)') '# SUBROUTINE = writing; filename = ', &
          TRIM(my_file)
        CALL FLUSH(6)

        OPEN(3,file=TRIM(my_file),status='unknown')

        DO jf = 0, f
          WRITE(3,100) my_m*my_a
          WRITE(3,200) &
            ' i = ',jf,', time = ',tim(jf)/fs2ps,', E = ',pot(jf)
          DO jm = 1, my_m
            DO ja = 1, my_a
              WRITE(3,300) &
                my_symbol_all(ja), (my_array_all(jf,jm,ja,jd),jd=1,my_d)
            END DO
          END DO
        END DO

        CLOSE(3)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(i8)
200     FORMAT(a,i8,a,f12.3,a,f20.10)
300     FORMAT(1x,a,1x,3(f20.10))

        END SUBROUTINE writing_all

        END MODULE writing_all_mod
