        PROGRAM shift

        USE common_data_mod
        USE reading_writing_mod

! Purpose: Shift a trajectory

! Program is run as:
!       ../shift.x < shift.in >& shift.out &

! Program reads first the file shift.in:

! 20000         f (last frame number)
! 90            a (number of atoms)
!  7.0  7.5 20.0 center_old
! 15.0 15.0 15.0 center
! 30_all.xyz         file_old
! 30_all_moved.xyz   file_new

        IMPLICIT NONE

        WRITE(6,'(a)') '# PROGRAM = shift'
        CALL FLUSH(6)

        READ(5,*) f
        READ(5,*) a
        WRITE(6,*) 'f = ', f
        WRITE(6,*) 'a = ', a
        READ(5,*) center_old(:)
        READ(5,*) center(:)
        WRITE(6,*) 'center_old, center [bohr] = ', &
                    center_old(:) , center(:)
        center_update = center - center_old
        WRITE(6,*) 'center_update [A] = ', center_update(:)
        READ(5,*) file_old
        READ(5,*) file_new
        WRITE(6,*) 'file_old = ', TRIM(file_old)
        WRITE(6,*) 'file_new = ', TRIM(file_new)

        WRITE(6,*)
        WRITE(6,*)
        CALL FLUSH(6)

        CALL reading_writing

        END PROGRAM shift
