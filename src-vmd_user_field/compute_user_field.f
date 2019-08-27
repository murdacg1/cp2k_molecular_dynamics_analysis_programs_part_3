        PROGRAM compute_user_field

        USE common_data_mod
        USE read_and_count_mod

! Purpose: Compute the vmd user field for a trajectory
! Program is run as:
!       compute_user_field.exe < compute_user_field.in >& compute_user_field.out &
!
! Program reads first the file compute_user_field.in:
! 35071                  f          last frame number (not including the zeroth frame)
! 221                    a          total atoms
! 74                     a_o        total oxygen atoms
! 146                    a_h        total hydrogen atoms
! 13.4724 15.5566 40.0   cell(d)    ABC values [Angstrom]
! slab-pos-1.xyz         traj_file  trajectory filename
! 1.30                   r_limit    to count bonds

        IMPLICIT NONE

        CHARACTER(len=80) traj_file, counts_file

        WRITE(6,'(a)') '# PROGRAM = compute_user_field'
        CALL FLUSH(6)

        READ(5,*) f
        READ(5,*) a
        READ(5,*) a_o
        READ(5,*) a_h
        ALLOCATE( cell(d) )
        cell = zero
        READ(5,*) cell(:)
        READ(5,*) traj_file
        READ(5,*) r_limit

        WRITE(6,100) '# f = ', f
        WRITE(6,100) '# a = ', a
        WRITE(6,100) '# a_o = ', a_o
        WRITE(6,100) '# a_h = ', a_h
        WRITE(6,100) '# d = ', d
        WRITE(6,300) '# cell = ', cell(:)
        WRITE(6,'(2a)') '# traj_file = ', TRIM(traj_file)
        WRITE(6,110) '# r_limit = ', r_limit
        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        counts_file='user.dat'
        ALLOCATE( coor_oxygens(0:f,a_o,d) )
        ALLOCATE( coor_hydrogens(0:f,a_h,d) )
        ALLOCATE( counts_oxygen_neighbors(0:f,a_o) )
        CALL read_and_count(&
          coor_oxygens, coor_hydrogens, counts_oxygen_neighbors, &
          traj_file, counts_file)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        DEALLOCATE( cell )
        DEALLOCATE( coor_oxygens, coor_hydrogens )
        DEALLOCATE( counts_oxygen_neighbors )

100     FORMAT(1x,a40,1(1x,i10))
110     FORMAT(1x,a40,1(1x,e20.10))
300     FORMAT(1x,a40,3(1x,e20.10))

        END PROGRAM compute_user_field
