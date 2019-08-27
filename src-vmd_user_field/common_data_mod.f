        MODULE common_data_mod

        INTEGER, PARAMETER :: dp=8
        INTEGER :: f, a, a_o, a_h
        INTEGER, PARAMETER :: d=3
        REAL(KIND=dp) :: r_limit
        REAL(KIND=dp), DIMENSION(:), ALLOCATABLE :: cell
        REAL(KIND=dp), DIMENSION(:,:,:), ALLOCATABLE :: &
          coor_oxygens, &
          coor_hydrogens
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: &
          counts_oxygen_neighbors

        REAL(KIND=dp), PARAMETER :: zero=0.0_dp
        REAL(KIND=dp), PARAMETER :: eps=1.0e-12_dp, eps2=1.0e-6_dp
        REAL(KIND=dp), PARAMETER :: bohr2ang=0.529177249_dp

        END MODULE common_data_mod
