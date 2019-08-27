        MODULE read_and_count_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE read_and_count(&
          coor_o, coor_h, counts, &
          traj_file, counts_file)

        REAL(KIND=dp), DIMENSION(0:f,a_o,d) :: coor_o
        REAL(KIND=dp), DIMENSION(0:f,a_h,d) :: coor_h
        INTEGER, DIMENSION(0:f,a_o) :: counts
        CHARACTER(len=80) traj_file, counts_file

        INTEGER :: jf, ja, ja_o, ja_h
        INTEGER :: a_temp
        INTEGER :: jf_temp
        REAL(KIND=dp) :: tim_temp, pot_temp
        CHARACTER(len=3) char3_temp
        CHARACTER(len=8) char8_temp
        CHARACTER(len=5) char5_temp
        CHARACTER(len=2) symbol_temp
        REAL(KIND=dp), DIMENSION(d) :: pos_temp, r_vec
        REAL(KIND=dp) :: r 

        WRITE(6,'(5a)') '# SUBROUTINE = read_and_count; ', &
          'traj_file, count_file = ', &
           TRIM(traj_file), ' ', TRIM(counts_file) 
        CALL FLUSH(6)

        coor_o = zero
        coor_h = zero
        counts = 0

        OPEN(1,file=TRIM(traj_file),status='unknown')
        DO jf = 0, f
          ja_o = 0
          ja_h = 0
          READ(1,*) a_temp
          READ(1,200) &
            char3_temp, jf_temp, &
            char8_temp, tim_temp, &
            char5_temp, pot_temp
!         WRITE(6,*) a_temp
!         WRITE(6,200) &
!           char3_temp, jf_temp, &
!           char8_temp, tim_temp, &
!           char5_temp, pot_temp
!         CALL FLUSH(6)
          DO ja = 1, a
            READ(1,300)  symbol_temp, pos_temp(:)
!           WRITE(6,300)  symbol_temp, pos_temp(:)
!           CALL FLUSH(6)
            SELECT CASE (symbol_temp)
            CASE (' O')
              ja_o = ja_o + 1
              coor_o(jf,ja_o,:) = pos_temp(:)
            CASE (' H')
              ja_h = ja_h + 1
              coor_h(jf,ja_h,:) = pos_temp(:)
            END SELECT
          END DO
        END DO
        CLOSE(1)
        WRITE(6,*) 'a_o, ja_o = ', a_o, ja_o
        WRITE(6,*) 'a_h, ja_h = ', a_h, ja_h
        CALL FLUSH(6)

        DO jf = 0, f
          DO ja_o = 1, a_o
            DO ja_h = 1, a_h
              r_vec = coor_h(jf,ja_h,:) -  coor_o(jf,ja_o,:)
              r_vec(:) = r_vec(:) - cell(:)*ANINT(r_vec(:)/cell(:))
              r = SQRT(DOT_PRODUCT(r_vec, r_vec))
              IF (r <= r_limit) counts(jf,ja_o) = counts(jf,ja_o) + 1
            END DO
          END DO
        END DO

        OPEN(2,file=TRIM(counts_file),status='unknown')
        DO jf = 0, f
          WRITE(2,400) counts(jf,:)
        END DO
        CLOSE(2)

        CALL FLUSH(6)

200     FORMAT(1x, &
          a3,1x,i8, &
          a8,1x,f12.3, &
          a5,1x,f20.10 &
          )
300     FORMAT(1x,a,1x,3(f20.10))
400     FORMAT(999(i1,1x))

        END SUBROUTINE read_and_count

        END MODULE read_and_count_mod
