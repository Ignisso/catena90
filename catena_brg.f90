MODULE catena_BRG
USE catena_blake2b_ref
IMPLICIT NONE

INTEGER(KIND = 1) :: lambda = 2
INTEGER(KIND = 1) :: garlic = 21
INTEGER(KIND = 1) :: min_garlic = 21
INTEGER(KIND = 1) :: HLLEN = 64
CONTAINS
SUBROUTINE bswap_i8 (BYTE8)
IMPLICIT NONE
    INTEGER(KIND = 8), INTENT(INOUT) :: BYTE8
    INTEGER(KIND = 1), DIMENSION(8) :: BYTE_ARR, BYTE_ARR_TMP
    INTEGER :: I

    BYTE_ARR = TRANSFER (BYTE8, BYTE_ARR)
    BYTE_ARR_TMP = BYTE_ARR

    DO I = 1, 8
        BYTE_ARR(I) = BYTE_ARR_TMP(9-I)
    END DO

    BYTE8 = TRANSFER (BYTE_ARR, BYTE8)
    RETURN
END SUBROUTINE bswap_i8

INTEGER FUNCTION reverse(x, n) RESULT(return_value)
IMPLICIT NONE
    IMPLICIT NONE
    INTEGER(KIND = 8) :: x
    INTEGER(KIND = 1) :: n
        INTEGER(KIND = 8) :: temp
        
    CALL bswap_i8(x)
        temp = X'0f0f0f0f0f0f0f0f' 
    x = (ISHFT((x .and. temp), 4)) .or. (ISHFT((x .and. (.not. temp)), -4));
        temp = X'3333333333333333'
    x = (ISHFT((x .and. temp), 2)) .or. (ISHFT((x .and. (.not. temp)), -2));
        temp = X'5555555555555555'
    x = (ISHFT((x .and. temp), 1)) .or. (ISHFT((x .and. (.not. temp)), -1));
    
    return_value = ISHFT(x,-(64-n))
END FUNCTION reverse

SUBROUTINE flap(x, lambda, garlic, salt, saltlen, h)
IMPLICIT NONE
    INTEGER(KIND = 1), DIMENSION(0: HLLEN - 1) :: x
    INTEGER(KIND = 1), INTENT(INOUT), DIMENSION(0: HLLEN - 1) :: h
    INTEGER(KIND = 1) :: lambda
    INTEGER(KIND = 1) :: garlic
    INTEGER(KIND = 1) :: salt
    INTEGER(KIND = 1) :: saltlen
    INTEGER(KIND = 8) :: c = 1
    INTEGER(KIND = 8) :: i
    INTEGER(KIND = 1) :: k
    INTEGER(KIND = 1), DIMENSION(0: HLLEN - 1) :: r, p, previousR

    c = ISHFT(c, min_garlic)
    DO WHILE(k < lambda)
        CALL Hash2(INT(r + (c-1)*HLLEN, 1), INT(HLLEN,8), INT(r,1), INT(HLLEN,8), r)
        DO i = 1, (c - 1)
            p = r + reverse(i, garlic) * HLLEN
            previousR = p
        END DO
        k = k + 1
        IF (k >= lambda) THEN
            EXIT
        END IF
        CALL Hash2( INT(r + (c-1)*HLLEN, 1), INT(HLLEN,8), INT(r,1), INT(HLLEN,8), r)
        p = r + HLLEN
        DO i = 1, (c-1)
            p = p + HLLEN
        END DO
        k = k + 1
    END DO
    DO i = 0, HLLEN - 1
        h(i) = r(i)
    END DO
END SUBROUTINE flap
END MODULE catena_BRG