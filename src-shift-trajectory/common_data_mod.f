        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
        REAL(KIND=dp), PARAMETER :: bohr2ang=0.529177249_dp

        INTEGER :: f, a
        REAL(KIND=dp), DIMENSION(d) :: &
          center_old = zero, center = zero, center_update = zero
        CHARACTER(len=80) file_old, file_new

        END MODULE common_data_mod
