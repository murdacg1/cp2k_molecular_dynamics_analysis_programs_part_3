        PROGRAM wrap_label

        USE common_data_mod
        USE reading_writing_mod

! Purpose: Analyze a CP2K simulation of [(H20)n) slab] + [single adsorbate = sol = solute = fa = trans formic acid here]
! Program is run as:
!       wrap_label.exe < wrap_label.in >& wrap_label.out &
!
! It is assumed that the file slab-pos-1.xyz has the atoms organized as O,H,H (WCs); O,H,H (WCs); ...; Cl,H (WCs)
!
! Program reads first the file analyze.in:
! 10000         f_avg   final f frames are used for averaging (e.g., avg calculated from last 5ps of the run)
! 20000         f       total number of frames, not including the zeroth frame (total run=0.5fs*20k=10ps)
! 0.5           dt      time step [fs]
! 72            m_wat   number of water molecules
! 3             a_wat   number of atoms per water molecule (increase if have Wannier Centers or off-atomic sites in classical potential simulations)
! 1             m_sol   number of solute molecules
! 5             a_sol   number of atoms per solute molecule (this is for formic acid, change for differnt molecule--for, HCl will be 2; also increase if have Wannier Centers or off-atomic sites in classical potential simulations)
! 13.4724 15.5566 40.0              cell(d)   ABC values [Angstrom]
! 2           crit_counter_wat      critical counter value in water molecules
! 0           crit_counter_sol      critical counter value in solute molecule
! O           crit_symbol_wat       critical symbol in water molecules
! O           crit_symbol_sol       critical symbol in solute molecule
! 1.4         crit_distance_wat     critical distance in water molecules [Angstrom]
! 1.4         crit_distance_sol     critical distance in solute molecule [Angstrom]
! slab-pos-1.xyz                    my_file1
! slab-pos-1_wrapped_labeled.xyz    my_file2   

        IMPLICIT NONE

        REAL(KIND=dp) :: pot_a, pot_b

        INTEGER :: &
          my_m, &
          my_a, &
          my_d, my_d1, my_d2, my_d3, &
          jn
        CHARACTER(len=80) my_file
        REAL(KIND=dp) :: &
          my_pot_a, my_pot_b, &
          my_factor

        WRITE(6,'(a)') '# PROGRAM = analyze'
        CALL FLUSH(6)

        READ(5,*) f_avg
        READ(5,*) f
        READ(5,*) m_wat
        READ(5,*) a_wat
        READ(5,*) m_sol
        READ(5,*) a_sol
        a = m_wat*a_wat + m_sol*a_sol ! total number atoms
        a_waters = m_wat*a_wat ! total number atoms in waters
        ALLOCATE( cell(d) )
        cell = zero
        READ(5,*) cell(:)
        READ(5,*) crit_counter_wat
        READ(5,*) crit_counter_sol
        READ(5,*) crit_symbol_wat
        READ(5,*) crit_symbol_sol
        READ(5,*) crit_distance_wat
        READ(5,*) crit_distance_sol
        READ(5,*) my_file1
        READ(5,*) my_file2

        WRITE(6,100) '# f_avg = ', f_avg
        WRITE(6,100) '# f = ', f
        WRITE(6,100) '# a = ', a
        WRITE(6,100) '# a_waters = ', a_waters
        WRITE(6,100) '# m_wat = ', m_wat
        WRITE(6,100) '# a_wat = ', a_wat
        WRITE(6,100) '# m_sol = ', m_sol
        WRITE(6,100) '# a_sol = ', a_sol
        WRITE(6,100) '# d = ', d
        WRITE(6,300) '# cell = ', cell(:)
        WRITE(6,100) '# crit_counter_wat = ', crit_counter_wat
        WRITE(6,100) '# crit_counter_sol = ', crit_counter_sol
        WRITE(6,100) '# crit_symbol_wat = ', crit_symbol_wat
        WRITE(6,100) '# crit_symbol_sol = ', crit_symbol_sol
        WRITE(6,100) '# crit_distance_wat = ', crit_distance_wat
        WRITE(6,100) '# crit_distance_sol = ', crit_distance_sol
        WRITE(6,'(a)') '# my_file1 = ', TRIM(my_file1)
        WRITE(6,'(a)') '# my_file2 = ', TRIM(my_file2)
        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        CALL reading_writing

        DEALLOCATE( cell )

100     FORMAT(1x,a40,1(1x,i10))
200     FORMAT(1x,a40,1(1x,e20.10))
210     FORMAT(1x,a40,2(1x,e20.10))
300     FORMAT(1x,a40,3(1x,e20.10))
410     FORMAT(1x,a40,4(1x,e20.10))

        END PROGRAM wrap_label
