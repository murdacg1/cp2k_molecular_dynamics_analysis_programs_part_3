        MODULE writing_dist_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE writing_dist(my_m, my_d, my_array, &
          my_file)

        INTEGER :: my_m, my_d
        REAL(KIND=dp), DIMENSION(0:f, my_m, my_d) :: my_array
        CHARACTER(len=80) my_file

        INTEGER :: jf

        WRITE(6,'(a,a)') '# SUBROUTINE = writing_dist, my_file = ', my_file
        CALL FLUSH(6)

        OPEN(3,file=TRIM(my_file),status='unknown')
!       WRITE(3,'(a)') '# jf r'
        DO jf = 0, f
          WRITE(3,100) jf, my_array(jf, my_m, my_d)
        END DO
        CLOSE(3)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(1(1x,i10),1(1x,f12.6))

        END SUBROUTINE writing_dist

        END MODULE writing_dist_mod
