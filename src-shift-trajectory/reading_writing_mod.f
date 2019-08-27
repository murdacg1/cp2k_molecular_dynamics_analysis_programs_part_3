        MODULE reading_writing_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading_writing

        INTEGER :: jf, ja
        INTEGER :: atot_temp
        CHARACTER(len=80) char80_temp
        REAL(KIND=dp), DIMENSION(d) :: coor_temp, coor
        CHARACTER(len=2) :: symbol

        WRITE(6,'(4a)') '# SUBROUTINE = reading_writing; filenames = ', &
          TRIM(file_old), '   ', TRIM(file_new)
        CALL FLUSH(6)

        OPEN(3,file=TRIM(file_old),status='unknown')
        OPEN(4,file=TRIM(file_new),status='unknown')

        DO jf = 0, f

          READ(3,*) atot_temp
          WRITE(4,100) atot_temp
          READ(3,*) char80_temp
          WRITE(4,200) 'frame ', jf

          DO ja = 1, a
            READ(3,*) symbol, coor_temp(:)
            coor_temp = coor_temp + center_update
            coor = coor_temp
            WRITE(4,300) symbol, coor(:)
          END DO

        END DO

        CLOSE(3)
        CLOSE(4)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(i2)
200     FORMAT(1x,a6,1x, i8)
300     FORMAT(1x,a2,3(2x,f14.6))

        END SUBROUTINE reading_writing

        END MODULE reading_writing_mod
