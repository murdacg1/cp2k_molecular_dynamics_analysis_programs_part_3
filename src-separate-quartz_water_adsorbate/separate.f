        PROGRAM separate

        USE common_data_mod
        USE reading_writing_mod

! Purpose: Separate a CP2K simulation of [Hydroxylated surface + (H20)n)] + [single adsorbate]
! Program is run as:
!       separate.exe < separate.in >& separate.out &
!
! It is assumed that the file quartz-pos-1.xyz has the atoms organized as Hydroxylated surface (Wannier centers); O,H,H (WCs); O,H,H (WCs); ...; Cl,H (WCs)
!
! Program reads first the file separate.in:
! 6000          f       total number of frames, not including the zeroth frame (total run=0.4fs*25k=10ps)
! quartz-pos-1.xyz      in_file ( OR quartz-vel-1.xyz )
! quartz-pos-W1-1.xyz   out_file ( OR quartz-vel-W1-1.xyz )
! 12                    a                number of atoms of type WI = W1 = H-flat
! 96                    atom_array(jn)   atom numbering based on vmd convention: 0, 1, ...
! 97
! 98 
! 99
! 100
! 101
! 105
! 106
! 107
! 114
! 115
! 116
!          OR
! quartz-pos-W2-1.xyz   out_file ( OR quartz-vel-W2-1.xyz )
! 12                    a                number of atoms of type WII = W2 = H-down
! 102                   atom_array(jn)   atom numbering based on vmd convention: 0, 1, ...
! 103
! 104
! 108
! 109
! 110
! 111
! 112
! 113
! 117
! 118
! 119

        IMPLICIT NONE

        INTEGER :: ja
        CHARACTER(len=80) in_file, out_file

        WRITE(6,'(a)') '# PROGRAM = separate'
        CALL FLUSH(6)

        READ(5,*) f
        READ(5,*) in_file
        READ(5,*) out_file
        READ(5,*) a
        ALLOCATE( atom_array(a) )
        DO ja = 1, a
          READ(5,*) atom_array(ja)
          atom_array(ja) = atom_array(ja) + 1 ! convert from vmd 0-start numbering to normal 1-start numbering
        END DO

        WRITE(6,100) '# f = ', f
        WRITE(6,'(a)') '# in_file = ', in_file
        WRITE(6,'(a)') '# out_file = ', out_file
        WRITE(6,100) '# number of atoms in out_file = ', a
        WRITE(6,'(a)') &
          '# vmd, normal: atom_array(ja) = '
        DO ja = 1, a
          WRITE(6,200) &
             atom_array(ja) - 1, atom_array(ja)
        END DO
        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        CALL reading_writing(in_file, out_file)

        DEALLOCATE( atom_array )

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(1x,a40,1(1x,i10))
200     FORMAT(2(1x,i10))

        END PROGRAM separate
