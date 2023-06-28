MODULE blake2_ref
USE blake2
USE blake2_impl
! USE blake2_kat
IMPLICIT NONE

INTEGER(KIND = 8), DIMENSION(0:7), PARAMETER :: blake2b_IV = [ &
  INT(X'6a09e667f3bcc908',8), INT(X'bb67ae8584caa73b',8), &
  INT(X'3c6ef372fe94f82b',8), INT(X'a54ff53a5f1d36f1',8), &
  INT(X'510e527fade682d1',8), INT(X'9b05688c2b3e6c1f',8), &
  INT(X'1f83d9abfb41bd6b',8), INT(X'5be0cd19137e2179',8)  &
]

INTEGER(KIND = 1), DIMENSION(0:11, 0:15), PARAMETER :: blake2b_sigma = reshape([ &
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, &
   14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3, &
   11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4, &
    7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8, &
    9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13, &
    2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9, &
   12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11, &
   13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10, &
    6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5, &
   10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0, &
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, &
   14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3  &
], shape(blake2b_sigma))

CONTAINS
SUBROUTINE blake2b_set_lastnode(S)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
  
    S%f(1) = X'ffffffffffffffff'; !ffffffffffffffff
END SUBROUTINE blake2b_set_lastnode

SUBROUTINE blake2b_clear_lastnode(S)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
  
    S%f(1) = X'0000000000000000'
END SUBROUTINE blake2b_clear_lastnode

! Some helper functions, not necessarily useful
SUBROUTINE blake2b_set_lastblock(S)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
 
    IF (S%last_node /= 0) THEN
        CALL blake2b_set_lastnode(S)
    END IF
    S%f(0) = X'0fffffffffffffff'; !ffffffffffffffff
END SUBROUTINE blake2b_set_lastblock

SUBROUTINE blake2b_clear_lastblock(S)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S

    IF (S%last_node /= 0) THEN
        CALL blake2b_clear_lastnode(S)
    END IF
    S%f(0) = X'0000000000000000'
END SUBROUTINE blake2b_clear_lastblock

SUBROUTINE blake2b_increment_counter(S, inc)
IMPLICIT NONE
    INTEGER(KIND = 8), INTENT(IN) :: inc
    TYPE(blake2b_state), INTENT(INOUT) :: S
  
    S%t(0) = S%t(0) + inc
    IF ( S%t(0) .LT. inc ) THEN
        S%t(1) = S%t(1) + 1
    END IF
END SUBROUTINE blake2b_increment_counter

! Parameter-related functions
SUBROUTINE blake2b_param_set_digest_length(P, digest_length)
IMPLICIT NONE
    INTEGER(KIND = 1), INTENT(IN) :: digest_length
    TYPE(blake2b_param), INTENT(INOUT) :: P
  
    P%digest_length = digest_length
END SUBROUTINE blake2b_param_set_digest_length

SUBROUTINE blake2b_param_set_fanout(P, fanout)
IMPLICIT NONE
    INTEGER(KIND = 1), INTENT(IN) :: fanout
    TYPE(blake2b_param), INTENT(INOUT) :: P
  
    P%fanout = fanout
END SUBROUTINE blake2b_param_set_fanout

SUBROUTINE blake2b_param_set_max_depth(P, depth)
IMPLICIT NONE
    INTEGER(KIND = 1), INTENT(IN) :: depth
    TYPE(blake2b_param), INTENT(INOUT) :: P
  
    P%depth = depth
END SUBROUTINE blake2b_param_set_max_depth

SUBROUTINE blake2b_param_set_leaf_length(P, leaf_length)
IMPLICIT NONE
    INTEGER(KIND = 4), INTENT(IN) :: leaf_length
    TYPE(blake2b_param), INTENT(INOUT) :: P

    CALL store32(P%leaf_length, leaf_length) ! I hope this is going to work
END SUBROUTINE blake2b_param_set_leaf_length

SUBROUTINE blake2b_param_set_node_offset(P, node_offset)
IMPLICIT NONE
    INTEGER(KIND = 8), INTENT(IN) :: node_offset
    TYPE(blake2b_param), INTENT(INOUT) :: P

    CALL store64(P%node_offset, node_offset)
END SUBROUTINE blake2b_param_set_node_offset

SUBROUTINE blake2b_param_set_node_depth(P, node_depth)
IMPLICIT NONE
    INTEGER(KIND = 1), INTENT(IN) :: node_depth
    TYPE(blake2b_param), INTENT(INOUT) :: P

    P%node_depth = node_depth
END SUBROUTINE blake2b_param_set_node_depth

SUBROUTINE blake2b_param_set_inner_length(P, inner_length)
IMPLICIT NONE
    INTEGER(KIND = 1), INTENT(IN) :: inner_length
    TYPE(blake2b_param), INTENT(INOUT) :: P

    P%inner_length = inner_length
END SUBROUTINE blake2b_param_set_inner_length

SUBROUTINE blake2b_param_set_salt(P, salt)
IMPLICIT NONE
    INTEGER(KIND = 1), DIMENSION(0:BLAKE2B_SALTBYTES - 1), INTENT(IN) :: salt
    TYPE(blake2b_param), INTENT(INOUT) :: P

    P%salt(0:BLAKE2B_SALTBYTES - 1) = salt(0:BLAKE2B_SALTBYTES - 1)
END SUBROUTINE blake2b_param_set_salt

SUBROUTINE blake2b_param_set_personal(P, personal)
IMPLICIT NONE
    INTEGER(KIND = 1), DIMENSION(0:BLAKE2B_PERSONALBYTES - 1), INTENT(IN) :: personal
    TYPE(blake2b_param), INTENT(INOUT) :: P
  
    P%personal(0:BLAKE2B_SALTBYTES - 1) = personal(0:BLAKE2B_PERSONALBYTES - 1)
END SUBROUTINE blake2b_param_set_personal

SUBROUTINE blake2b_init0(S)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
    INTEGER(KIND = 1) :: i
    S%h = 0
    S%t = 0
    S%buf = 0
    S%buflen = 0
    S%last_node = 0

    DO i = 0,7
        S%h(i) = blake2b_IV(i)
    END DO
END SUBROUTINE blake2b_init0

! init .OR. IV with input parameter block
SUBROUTINE blake2b_init_param(S, P)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
    TYPE(blake2b_param), INTENT(INOUT) :: P

    CALL blake2b_init0(S)
    S%h(0) = XOR(INT(S%h(0),4), INT(load64(INT(P%digest_length,4)),4))
    S%h(1) = XOR(INT(S%h(1),4), INT(load64(INT(P%key_length,4)),4))
    S%h(2) = XOR(INT(S%h(2),4), INT(load64(INT(P%fanout,4)),4))
    S%h(3) = XOR(INT(S%h(3),4), INT(load64(INT(P%depth,4)),4))
    S%h(4) = XOR(INT(S%h(4),4), INT(load64(IBITS(P%leaf_length, 0, 8)),4))
    S%h(5) = XOR(INT(S%h(5),4), INT(load64(IBITS(P%leaf_length, 7, 8)),4))
    S%h(6) = XOR(INT(S%h(6),4), INT(load64(IBITS(P%leaf_length, 15, 8)),4))
    S%h(7) = XOR(INT(S%h(7),4), INT(load64(IBITS(P%leaf_length, 23, 8)),4))
END SUBROUTINE blake2b_init_param

INTEGER FUNCTION blake2b_init(S, outlen) RESULT(exitcode)
IMPLICIT NONE
    INTEGER(KIND = 1), INTENT(IN) :: outlen
    TYPE(blake2b_state), INTENT(INOUT) :: S
    TYPE(blake2b_param) :: P

    IF ((outlen .EQ. 0) .OR. (outlen .GT. BLAKE2B_OUTBYTES)) THEN
        exitcode = -1 
        RETURN
    END IF 

    P%digest_length = outlen
    P%key_length    = 0
    P%fanout        = 1
    P%depth         = 1
    CALL store32(P%leaf_length, 0)
    CALL store64(P%node_offset, 0_8)
    P%node_depth    = 0
    P%inner_length  = 0
    P%reserved      = 0
    P%salt          = 0
    P%personal      = 0
    CALL blake2b_init_param(S, P)
    exitcode = 0
END FUNCTION blake2b_init


INTEGER FUNCTION  blake2b_init_key(S, outlen, key, keylen) RESULT(exitcode)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
    INTEGER(KIND = 1), INTENT(IN) :: outlen
    INTEGER(KIND = 1), INTENT(IN) :: keylen
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_KEYBYTES - 1) :: key
    TYPE(blake2b_param) :: P
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_BLOCKBYTES - 1) :: bblock

    IF ((outlen .EQ. 0) .OR. (outlen .GT. BLAKE2B_OUTBYTES)) THEN
        exitcode = -1
        RETURN
    END IF
  
    IF (keylen .EQ. 0 .OR. keylen .GT. BLAKE2B_KEYBYTES) THEN ! .OR. key .EQ. 0
        exitcode = -1
        RETURN
    END IF

    P%digest_length = outlen
    P%key_length    = keylen
    P%fanout        = 1
    P%depth         = 1
    CALL store32(P%leaf_length, 0)
    CALL store64(P%node_offset, 0_8)
    P%node_depth    = 0
    P%inner_length  = 0
    P%reserved = 0
    P%salt = 0
    P%personal = 0

    CALL blake2b_init_param(S, P)

    bblock(0: BLAKE2B_BLOCKBYTES - 1) = 0
    bblock(0: keylen - 1) = key(0: keylen - 1) ! TODOOOO
    CALL blake2b_update(S, bblock, INT(BLAKE2B_BLOCKBYTES,8))
    ! If doesn't compile remove line below
    CALL secure_zero_memory(bblock, INT(BLAKE2B_BLOCKBYTES, 4)); ! Burn the key from stack 
    exitcode = 0
END FUNCTION blake2b_init_key

SUBROUTINE G(r,i,a,b,c,d,m)
IMPLICIT NONE
    INTEGER(KIND = 4) :: r, i
    INTEGER(KIND = 8) :: a, b, c, d, temp
    INTEGER(KIND = 8), DIMENSION(0: 15) :: m
    temp = blake2b_sigma(r, 2*i+0)
    a = a + b + m(temp)
    d = rotr64(XOR(d, a), 32)
    c = c + d
    b = rotr64(XOR(b, c), 24)
    temp = blake2b_sigma(r, 2*i+1)
    a = a + b + m(temp)
    d = rotr64(XOR(d, a), 16)
    c = c + d
    b = rotr64(XOR(b, c), 63)
END SUBROUTINE G

SUBROUTINE ROUND(r, v, m)
IMPLICIT NONE
    INTEGER(KIND = 8), DIMENSION(0: 15) :: v
    INTEGER(KIND = 8), DIMENSION(0: 15) :: m
    INTEGER(KIND = 4) :: r, i
    INTEGER(KIND = 8) :: a, b, c, d
    CALL G(r, 0, v(0), v(4), v( 8), v(12), m)
    CALL G(r, 1, v(1), v(5), v( 9), v(13), m)
    CALL G(r, 2, v(2), v(6), v(10), v(14), m)
    CALL G(r, 3, v(3), v(7), v(11), v(15), m)
    CALL G(r, 4, v(0), v(5), v(10), v(15), m)
    CALL G(r, 5, v(1), v(6), v(11), v(12), m)
    CALL G(r, 6, v(2), v(7), v( 8), v(13), m)
    CALL G(r, 7, v(3), v(4), v( 9), v(14), m)
END SUBROUTINE ROUND

SUBROUTINE blake2b_compress(S, bblock)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_BLOCKBYTES - 1) :: bblock
    INTEGER(KIND = 8), DIMENSION(0: 15) :: m
    INTEGER(KIND = 8), DIMENSION(0: 15) :: v
    INTEGER(KIND = 1) :: i

    DO i = 0, 15
        m(i) = load64(INT(bblock(i),4))
    END DO

    DO i = 0, 7
        v(i) = S%h(i)
    END DO

    v(8) = blake2b_IV(0)
    v(9) = blake2b_IV(1)
    v(10) = blake2b_IV(2)
    v(11) = blake2b_IV(3)
    v(12) = XOR(S%t(0), blake2b_IV(4))
    v(13) = XOR(S%t(1), blake2b_IV(5))
    v(14) = XOR(S%f(0), blake2b_IV(6))
    v(15) = XOR(S%f(1), blake2b_IV(7))
  
    DO i = 0, 11
        CALL ROUND(INT(i,4), v, m)
    END DO

    DO i = 0, 7
        S%h(i) = XOR(XOR(S%h(i), v(i)), v(i + 8))
    END DO
END SUBROUTINE blake2b_compress

! inlen now in bytes
SUBROUTINE blake2b_update(S, iin, inlen)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_BLOCKBYTES - 1) :: iin
    INTEGER(KIND = 8) :: inlen
    INTEGER(KIND = 2) :: iin_index

    INTEGER(KIND = 4) :: left, fill = 0

    iin_index = 0
    DO WHILE(inlen .GT. 0)
        IF (inlen .EQ. 0) THEN
        RETURN
    END IF 
    left = S%buflen
    fill = 2 * BLAKE2B_BLOCKBYTES - left

    IF (inlen .GT. fill) THEN
        S%buf(left:left + fill) = iin(iin_index:iin_index + fill) ! Fill buffer
        S%buflen = S%buflen + fill
        CALL blake2b_increment_counter(S, INT(BLAKE2B_BLOCKBYTES,8))
        CALL blake2b_compress(S, S%buf) ! Compress
        S%buf(0:BLAKE2B_BLOCKBYTES) = S%buf(BLAKE2B_BLOCKBYTES: 2*BLAKE2B_BLOCKBYTES) ! Shift buffer left
        S%buflen = S%buflen - BLAKE2B_BLOCKBYTES
        iin_index = iin_index + fill
        inlen = inlen - fill
    ELSE
        S%buf(left: left + inlen) = iin(iin_index:iin_index + inlen) 
        S%buflen = S%buflen + inlen; ! Be lazy, do not compress
        iin_index = iin_index + inlen
        RETURN
    END IF
  END DO
END SUBROUTINE blake2b_update

! Is this correct?
SUBROUTINE blake2b_final(S, oout, outlen)
IMPLICIT NONE
    TYPE(blake2b_state), INTENT(INOUT) :: S
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_OUTBYTES - 1) :: oout
    INTEGER(KIND = 1), INTENT(IN) :: outlen
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_OUTBYTES - 1) :: buffer
    INTEGER(KIND = 1) :: i

    IF (S%buflen .GT. BLAKE2B_BLOCKBYTES) THEN
        CALL blake2b_increment_counter(S, INT(BLAKE2B_BLOCKBYTES, 8))
        CALL blake2b_compress(S, S%buf)
        S%buflen = S%buflen - BLAKE2B_BLOCKBYTES
        S%buf(0 : S%buflen) = S%buf(BLAKE2B_BLOCKBYTES: BLAKE2B_BLOCKBYTES + S%buflen)
    END IF 

    CALL blake2b_increment_counter(S, INT(S%buflen, 8))  
    CALL blake2b_set_lastblock(S)
    S%buf(S%buflen : 2 * BLAKE2B_BLOCKBYTES) = 0 ! Padding 
    CALL blake2b_compress(S, S%buf)

    DO i = 0, 7 ! Output full hash to temp buffer
        buffer(8*i+0) = IBITS(S%h(i), 8*i+0, 8)
        buffer(8*i+1) = IBITS(S%h(i), 8*i+1, 8)
        buffer(8*i+2) = IBITS(S%h(i), 8*i+2, 8)
        buffer(8*i+3) = IBITS(S%h(i), 8*i+3, 8)
        buffer(8*i+4) = IBITS(S%h(i), 8*i+4, 8)
        buffer(8*i+5) = IBITS(S%h(i), 8*i+5, 8)
        buffer(8*i+6) = IBITS(S%h(i), 8*i+6, 8)
        buffer(8*i+7) = IBITS(S%h(i), 8*i+7, 8)
    END DO
    oout(0: outlen) = buffer(0: outlen) ! TODOOOOO 
END SUBROUTINE blake2b_final

! inlen, at least, should be uint64_t. Others can be size_t.
SUBROUTINE blake2b(oout, iin, key, outlen, inlen, keylen)
IMPLICIT NONE
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_OUTBYTES - 1) :: oout
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_BLOCKBYTES - 1)  :: iin
    INTEGER(KIND = 1), DIMENSION(0: BLAKE2B_KEYBYTES - 1) :: key
    INTEGER(KIND = 1), INTENT(IN) :: outlen, keylen
    INTEGER(KIND = 8) :: inlen
    TYPE(blake2b_state) :: S

    ! If breaks, thats because Fortran doesn't have null-checks :(
    IF (keylen .GT. 0) THEN
        IF(blake2b_init_key(S, outlen, key, keylen) .LT. 0) THEN
            RETURN
        END IF
    ELSE
        IF(blake2b_init(S, outlen) .LT. 0) THEN
            RETURN
        END IF
    END IF

    CALL blake2b_update(S, iin, inlen)
    CALL blake2b_final(S, oout, outlen)
END SUBROUTINE blake2b
END MODULE blake2_ref
