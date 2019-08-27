        PROGRAM separate_wfc

        USE common_data_mod
        USE reading_writing_mod

! Purpose: Separate a CP2K simulation of [Hydroxylated surface + (H20)n)] + [single adsorbate or solute]
! and assign the wfc centers to each of the heavy atoms (O or Cl) of interest, final output of form O,H,H,X,X,X; ...; O,H,H,X,X,X; Cl,H,X,X,X.
! Program is run as:
!       separate_wfc.exe < separate_wfc.in >& separate_wfc.out &
!
! Program reads first the file separate_wfc.in:
! 6000                  f       total number of frames, not including the zeroth frame (total run=0.4fs*25k=10ps)
! quartz-1.wfc          in_file
!
! quartz-W1-wfc-temp.xyz   out_file_temp
! quartz-W1-wfc.xyz        out_file
! 12                    a_in             number of atoms in list: of type WI = W1 = H-flat
! 96                    atom_array_in(jn)   atom numbering based on vmd convention: 0, 1, ...
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
! quartz-W2-wfc-temp.xyz   out_file_temp
! quartz-W2-wfc.xyz        out_file
! 12                    a_in             number of atoms in list: of type WII = W2 = H-down
! 102                   atom_array_in(ja)   atom numbering based on vmd convention: 0, 1, ...
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
!
! 120                 Cl
! 121                 H
!
! 4 (OR 8)      m_w   number of water molecules per frame
! 0 (OR 1)      m_a   number of solute molecules per frame
! 3             a_w   number of atoms per water molecule
! 2             a_a   number of atoms per solute molecule
! 4             c_w   number of wfc centers per water molecule
! 4             c_a   number of wfc centers per solute molecule
!
        IMPLICIT NONE

        INTEGER :: ja
        CHARACTER(len=80) in_file, out_file_temp, out_file

        WRITE(6,'(a)') '# PROGRAM = separate_wfc'
        CALL FLUSH(6)

        READ(5,*) f
        READ(5,*) in_file
        READ(5,*) out_file_temp
        READ(5,*) out_file
        READ(5,*) a_in
        ALLOCATE( atom_array_in(a_in) )
        DO ja = 1, a_in
          READ(5,*) atom_array_in(ja)
          atom_array_in(ja) = atom_array_in(ja) + 1 ! convert from vmd 0-start numbering to normal 1-start numbering
        END DO
        READ(5,*) m_w
        READ(5,*) m_s
        READ(5,*) a_w
        READ(5,*) a_s
        READ(5,*) c_w
        READ(5,*) c_s

        WRITE(6,100) '# f = ', f
        WRITE(6,'(a)') '# in_file = ', in_file
        WRITE(6,'(a)') '# out_file_temp = ', out_file_temp
        WRITE(6,'(a)') '# out_file = ', out_file
        WRITE(6,100) '# number of atoms in list = ', a_in
        WRITE(6,'(a)') &
          '# vmd, normal: atom_array_in(ja) = '
        DO ja = 1, a_in
          WRITE(6,200) &
             atom_array_in(ja) - 1, atom_array_in(ja)
        END DO
        WRITE(6,100) '# number of water molecules per frame = ', m_w
        WRITE(6,100) '# number of solute molecules per frame = ', m_s
        WRITE(6,100) '# number of atoms per water molecule = ', a_w
        WRITE(6,100) '# number of atoms per solute molecule = ', a_s
        WRITE(6,100) '# number of wfc centers per water molecule = ', c_w
        WRITE(6,100) '# number of wfc centers per solute molecule = ', c_s
        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        CALL reading_writing(in_file, out_file_temp, out_file)

        DEALLOCATE( atom_array_in )

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

100     FORMAT(1x,a40,1(1x,i10))
200     FORMAT(2(1x,i10))

        END PROGRAM separate_wfc
