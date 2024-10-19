module rollsum
    implicit none
    real(kind=8) :: LOAD_ACCUM(0:366)
    real(kind=4) :: ADDEND
    real(kind=4) :: LENGPM_DA
    save :: LOAD_ACCUM
    save :: ADDEND
    save :: LENGPM_DA
end module rollsum

module rollsum_l4
    implicit none
    logical(kind=4) :: DERATABLE
    save :: DERATABLE
end module rollsum_l4

module rollsum_i2
    implicit none
    integer(kind=2) :: IDA
    integer(kind=2) :: JMO
    integer(kind=2) :: JDA
    integer(kind=2) :: LENGYR_DA
    integer(kind=2) :: LENGYR_MO
    integer(kind=2) :: LAST_ACTU
    integer(kind=2) :: CUMUL_CM_DA(0:24)
    save :: IDA
    save :: JMO
    save :: JDA
    save :: LENGYR_DA
    save :: LENGYR_MO
    save :: LAST_ACTU
    save :: CUMUL_CM_DA
end module rollsum_i2

