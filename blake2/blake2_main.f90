PROGRAM main
USE blake2
USE blake2_impl
USE blake2_kat
USE blake2_ref
IMPLICIT NONE

INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_KEYBYTES - 1) :: key
INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_KEYBYTES - 1) :: hash
INTEGER(KIND = 1), DIMENSION(0: KAT_LENGTH - 1) :: buf
INTEGER(KIND = 2) :: i, j

DO i = 0, (BLAKE2B_KEYBYTES - 1)
    key(i) = i
    hash(i) = 0
END DO

DO i = 0, (KAT_LENGTH - 1)
    buf(i) = i
END DO

DO i = 0, (KAT_LENGTH - 1)
    CALL blake2b(hash, buf, key, BLAKE2B_OUTBYTES, INT(i,8), BLAKE2B_KEYBYTES)
    CALL print_hex(blake2b_keyed_kat(i, 0:BLAKE2B_KEYBYTES - 1), BLAKE2B_KEYBYTES - 1)
END DO

PRINT*, "ok"

CONTAINS
SUBROUTINE print_hex(key, leng)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:), INTENT(in) :: key
    INTEGER, INTENT(IN) :: leng
    INTEGER :: i
    DO i = 1, leng
        WRITE(*, "(1Z2.2)", advance="no") key(i)
    END DO
    PRINT *
END SUBROUTINE print_hex
END PROGRAM main
