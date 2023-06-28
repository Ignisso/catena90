MODULE catena_blake2b_ref
USE blake2
USE blake2_impl
USE blake2_ref
IMPLICIT NONE

INTEGER(KIND = 1) :: HLEN = 64

CONTAINS
SUBROUTINE Hash1(input, inputlen, hash)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:) :: input, hash
    INTEGER (KIND = 8):: inputlen
    INTEGER :: res
    TYPE(blake2b_state) :: ctx
    res = blake2b_init(ctx, HLEN)
    CALL blake2b_update(ctx, input, inputlen);
    CALL blake2b_final(ctx, hash, HLEN);
END SUBROUTINE Hash1

SUBROUTINE Hash2(i1, i1len, i2, i2len, hash)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:) :: i1, i2, hash
    INTEGER (KIND = 8) :: i1len, i2len
    INTEGER :: res
    TYPE(blake2b_state) :: ctx
    res = blake2b_init(ctx, HLEN);
    CALL blake2b_update(ctx, i1, i1len);
    CALL blake2b_update(ctx, i2, i2len);
    CALL blake2b_final(ctx, hash, HLEN);
END SUBROUTINE Hash2

SUBROUTINE Hash3(i1, i1len, i2, i2len, i3, i3len, hash)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:) :: i1, i2, i3, hash
    INTEGER (KIND = 8) :: i1len, i2len, i3len
    INTEGER :: res
    TYPE(blake2b_state) :: ctx
    res = blake2b_init(ctx, HLEN);
    CALL blake2b_update(ctx, i1, i1len);
    CALL blake2b_update(ctx, i2, i2len);
    CALL blake2b_update(ctx, i3, i3len);
    CALL blake2b_final(ctx, hash, HLEN);
END SUBROUTINE Hash3

SUBROUTINE Hash4(i1, i1len, i2, i2len, i3, i3len, i4, i4len, hash)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:) :: i1, i2, i3, i4, hash
    INTEGER (KIND = 8) :: i1len, i2len, i3len, i4len
    INTEGER :: res
    TYPE(blake2b_state) :: ctx
    res = blake2b_init(ctx, HLEN);
    CALL blake2b_update(ctx, i1, i1len);
    CALL blake2b_update(ctx, i2, i2len);
    CALL blake2b_update(ctx, i3, i3len);
    CALL blake2b_update(ctx, i4, i4len);
    CALL blake2b_final(ctx, hash, HLEN);
END SUBROUTINE Hash4

SUBROUTINE Hash5(i1, i1len, i2, i2len, i3, i3len, i4, i4len, i5, i5len, hash)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:) :: i1, i2, i3, i4, i5, hash
    INTEGER (KIND = 8) :: i1len, i2len, i3len, i4len, i5len
    INTEGER :: res
    TYPE(blake2b_state) :: ctx
    res = blake2b_init(ctx, HLEN);
    CALL blake2b_update(ctx, i1, i1len);
    CALL blake2b_update(ctx, i2, i2len);
    CALL blake2b_update(ctx, i3, i3len);
    CALL blake2b_update(ctx, i4, i4len);
    CALL blake2b_update(ctx, i5, i5len);
    CALL blake2b_final(ctx, hash, HLEN);
END SUBROUTINE Hash5
END MODULE catena_blake2b_ref
