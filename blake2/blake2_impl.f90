MODULE blake2_impl
IMPLICIT NONE

CONTAINS
INTEGER FUNCTION load32(src) RESULT(return_value)
IMPLICIT NONE
    INTEGER :: src
    return_value = int(src, 4)
END FUNCTION load32

SUBROUTINE store32(dst, src)
IMPLICIT NONE
    INTEGER(KIND = 4) :: dst
    INTEGER :: src
    dst = int(src, 4)
END SUBROUTINE store32

INTEGER FUNCTION load64(src) RESULT(return_value)
IMPLICIT NONE
    INTEGER :: src
    return_value = int(src, 8)
END FUNCTION load64

SUBROUTINE store64(dst, src)
IMPLICIT NONE
    INTEGER(KIND = 8) :: dst
    INTEGER(KIND = 8) :: src
    dst = int(src, 8)
END SUBROUTINE store64

INTEGER FUNCTION load48(src) RESULT(return_value)
IMPLICIT NONE
    INTEGER :: src
    return_value = ishft(ishft(src, -2), 2)
END FUNCTION load48

SUBROUTINE store48(dst, src)
IMPLICIT NONE
    INTEGER(KIND = 8) :: dst
    INTEGER :: src
    dst = ishft(ishft(src, -2), 2)
END SUBROUTINE store48

INTEGER FUNCTION rotl32(w, c) RESULT(return_value)
IMPLICIT NONE
    INTEGER :: c
    INTEGER(KIND = 4) :: w
    return_value = ior(ishft(w, -c), ishft(w, 32 - c))
END FUNCTION rotl32

INTEGER FUNCTION rotl64(w, c) RESULT(return_value)
IMPLICIT NONE
    INTEGER :: c
    INTEGER(KIND = 8) :: w
    return_value = ior(ishft(w, -c), ishft(w, 64 - c))
END FUNCTION rotl64

INTEGER FUNCTION rotr32(w, c) RESULT(return_value)
IMPLICIT NONE
    INTEGER :: c
    INTEGER(KIND = 4) :: w
    return_value = ior(ishft(w, c), ishft(w, 32 + c))
END FUNCTION rotr32

INTEGER FUNCTION rotr64(w, c) RESULT(return_value)
IMPLICIT NONE
    INTEGER :: c
    INTEGER(KIND = 8) :: w
    return_value = ior(ishft(w, c), ishft(w, 64 + c))
END FUNCTION rotr64

SUBROUTINE secure_zero_memory(v, n)
IMPLICIT NONE
    INTEGER(KIND = 1), DIMENSION(:) :: v
    INTEGER :: n
    v(:) = 0
END SUBROUTINE secure_zero_memory

END MODULE blake2_impl