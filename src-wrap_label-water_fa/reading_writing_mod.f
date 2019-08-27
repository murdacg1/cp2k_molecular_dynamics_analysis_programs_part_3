        MODULE reading_mod

        USE common_data_mod

        IMPLICIT NONE

        CONTAINS

        SUBROUTINE reading_writing

        CHARACTER(len=80) :: first_line = ' '
        CHARACTER(len=80), DIMENSION(:), ALLOCATABLE :: second_line
        CHARACTER(len=2), DIMENSION(:), ALLOCATABLE :: symbol
        REAL(KIND=dp), DIMENSION(:,:,:), ALLOCATABLE :: coor

        REAL(KIND=dp), DIMENSION(d) :: r_vec = zero
        REAL(KIND=dp) :: r = zero

        INTEGER :: jf, ja1, ja2

        WRITE(6,'(a)') '# SUBROUTINE = reading_writing; filenames = ', &
          TRIM(my_file1), TRIM(my_file2)
        CALL FLUSH(6)

        ALLOCATE( second_line(0:f) )
        ALLOCATE( symbol(a))
        ALLOCATE( coor(0:f,a,d) )
	ALLOCATE( h_counter_wat(a), h_counter_sol(a) )

        second_line = ' '
        symbol = '  '
        coor = zero

!       WRITE(6,*)
!       WRITE(6,*)
        OPEN(3,file=TRIM(my_file1),status='unknown')
        DO jf = 0, f
          READ(3,200) first_line
          READ(3,200) second_line(jf)
!         WRITE(6,200) first_line
!         WRITE(6,200) second_line(jf)
!         CALL FLUSH(6)
          DO ja1 = 1, a
            READ(3,300)  symbol(jf,ja1), coor(jf,ja,:)
!           WRITE(6,300) symbol(jf,ja1), coor(jf,ja1,:)
!           CALL FLUSH(6)
          END DO
        END DO
        CLOSE(3)

        DO jf = 0, f

	  h_counter_wat = 0
          DO ja1 = 1, a_wat ! find all water oxygens
	    IF ( symbol(jf,ja1) /= crit_symbol_wat ) CYCLE	  
            DO ja2 = 1, a ! find all hydrogens
	      IF ( ( ja2 == ja1 ) .OR. ( symbol(ja2) /= ' H' ) ) CYCLE
              r_vec = coor(jf,ja2,:) -  coor(jf,ja1,:)
              r_vec(:) = r_vec(:) - cell(:)*ANINT(r_vec(:)/cell(:)) ! do not forget to appy PBCs
              r = SQRT(DOT_PRODUCT(r_vec, r_vec))
	      IF (r < crit_dist_wat) h_counter_wat(ja1) = h_counter_wat(ja1) + 1
	    END DO
	  END DO

	  h_counter_sol = 0
          DO ja1 = a_wat+1, a ! find all solute special atoms
	    IF ( symbol1(ja1) /= crit_symbol_sol ) CYCLE	  
            DO ja2 = 1, a ! find all hydrogens
	      IF ( ( ja2 == ja1 ) .OR. ( symbol1(ja2) /= ' H' ) ) CYCLE
              r_vec = coor1(jf,ja2,:) -  coor1(jf,ja1,:)
              r_vec(:) = r_vec(:) - cell(:)*ANINT(r_vec(:)/cell(:)) ! do not forget to appy PBCs
              r = SQRT(DOT_PRODUCT(r_vec, r_vec))
	      IF (r < crit_dist_wat) h_counter_wat(ja1) = h_counter_wat(ja1) + 1
	    END DO
	  END DO


	  
            READ(3,300)  symbol1(ja), (coor1(jf,ja,jd),jd=1,my_d)
            SELECT CASE (symbol_temp)
            CASE ('Si')
              mass_temp = 27.9769271_dp
            CASE (' O')
              mass_temp = 15.99491463_dp
            CASE (' H')
              mass_temp = 1.0078250321_dp
            CASE ('Cl')
              mass_temp = 34.968852721_dp
            CASE (' C')
              mass_temp = 12.0_dp
            END SELECT
!           WRITE(6,300) symbol(ja), (total(jf,ja,jd),jd=1,my_d)
!           CALL FLUSH(6)
          END DO






        END DO
        CLOSE(3)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

200     FORMAT(1x, &
          a3,1x,i8, &
          a8,1x,f12.3, &
          a5,1x,f20.10 &
          )
300     FORMAT(1x,a,1x,3(f20.10))
500     FORMAT(1(1x,i10),1(1x,f20.10),4(1x,f20.10))

        END SUBROUTINE reading

        END MODULE reading_mod
