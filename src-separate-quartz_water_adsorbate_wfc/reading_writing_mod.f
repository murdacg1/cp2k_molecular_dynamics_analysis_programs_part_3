        MODULE reading_writing_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading_writing(in_file, out_file_temp, out_file)

        CHARACTER(len=80) in_file, out_file_temp, out_file

        INTEGER :: jf, ja, ja_temp
        INTEGER :: a_temp
        CHARACTER(len=80) char80_temp
        REAL(KIND=dp), DIMENSION(:,:), ALLOCATABLE :: coor_temp
        CHARACTER(len=2), DIMENSION(:), ALLOCATABLE :: symbol_temp

        WRITE(6,'(4a)') &
          '# SUBROUTINE = reading_writing; in_file, out_file = ', &
          TRIM(in_file), '   ', TRIM(out_file)
        CALL FLUSH(6)

        ALLOCATE( coor_temp(a_temp,d) )
        ALLOCATE( symbol_temp(a_temp) )
        coor_temp = zero
        symbol_temp = '  '
        a_w_tot = 0

        OPEN(3,file=TRIM(in_file),status='unknown')
        OPEN(4,file=TRIM(out_file_temp),status='unknown')
        DO jf = 0, f
          READ(3,*) a_temp
          READ(3,'(a)') char80_temp
          DO ja_temp = 1, a_temp
            READ(3,*) &
              symbol_temp(ja_temp), coor_temp(ja_temp, :)
            IF (TRIM(symbol_temp(ja_temp)=='X')) &
              a_w_tot = a_w_tot + 1
          END DO
          WRITE(4,100) m_w*a_w + m_s*a_s + a_c_tot
          CALL FLUSH(4)
          WRITE(4,'(a)') TRIM(char80_temp)
          CALL FLUSH(4)
          DO ja = 1, a_in
            WRITE(4,300) &
              symbol_temp(atom_array_in(ja)), coor_temp(atom_array_in(ja), :)
          END DO
          DO ja_temp = 1, a_temp
            IF (TRIM(symbol_temp(ja_temp)=='X')) WRITE(4,300) &
              symbol_temp(ja_temp), coor_temp(ja_temp, :)
          END DO
        END DO
        CLOSE(3)
        CLOSE(4)

        OPEN(7,file=TRIM(out_file),status='unknown')
        DO jf = 0, f
          READ(4,*) a_temp
          READ(4,'(a)') char80_temp
          WRITE(7,100) m_w*(a_w+c_w) + m_s*(a_s+c_s)
          CALL FLUSH(7)
          WRITE(7,'(a)') TRIM(char80_temp)
          CALL FLUSH(4)
          DO ja_temp = 1, a_temp
            READ(3,*) &
              symbol_temp(ja_temp), coor_temp(ja_temp, :)
          END DO
          DO ja = 1, a_in
            WRITE(4,300) &
              symbol_temp(atom_array_in(ja)), coor_temp(atom_array_in(ja), :)
          END DO
        END DO
        CLOSE(3)
        CLOSE(4)




        DEALLOCATE( coor_temp )
        DEALLOCATE( symbol_temp )

        CLOSE(3)
        CLOSE(4)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(i8)
300     FORMAT(2x,a,3(f20.10))

        END SUBROUTINE reading_writing

        END MODULE reading_writing_mod
