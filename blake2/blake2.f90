MODULE blake2
IMPLICIT NONE

! blake2s_constant
INTEGER(KIND = 1), PARAMETER :: BLAKE2S_BLOCKBYTES    = 64
INTEGER(KIND = 1), PARAMETER :: BLAKE2S_OUTBYTES      = 32
INTEGER(KIND = 1), PARAMETER :: BLAKE2S_KEYBYTES      = 32
INTEGER(KIND = 1), PARAMETER :: BLAKE2S_SALTBYTES     = 8
INTEGER(KIND = 1), PARAMETER :: BLAKE2S_PERSONALBYTES = 8

! blake2b_constant
INTEGER(KIND = 2), PARAMETER :: BLAKE2B_BLOCKBYTES    = 128
INTEGER(KIND = 1), PARAMETER :: BLAKE2B_OUTBYTES      = 64
INTEGER(KIND = 1), PARAMETER :: BLAKE2B_KEYBYTES      = 64
INTEGER(KIND = 1), PARAMETER :: BLAKE2B_SALTBYTES     = 16
INTEGER(KIND = 1), PARAMETER :: BLAKE2B_PERSONALBYTES = 16

TYPE blake2s_param
    INTEGER(KIND = 1)                                           ::  digest_length;  ! 1
    INTEGER(KIND = 1)                                           ::  key_length;     ! 2
    INTEGER(KIND = 1)                                           ::  fanout;         ! 3
    INTEGER(KIND = 1)                                           ::  depth;          ! 4
    INTEGER(KIND = 4)                                           ::  leaf_length;    ! 8
    INTEGER(KIND = 1), DIMENSION(0:5)                           ::  node_offset;    ! 14
    INTEGER(KIND = 1)                                           ::  node_depth;     ! 15
    INTEGER(KIND = 1)                                           ::  inner_length;   ! 16
    INTEGER(KIND = 1), DIMENSION(0:BLAKE2S_SALTBYTES - 1)       ::  salt;           ! 24
    INTEGER(KIND = 1), DIMENSION(0:BLAKE2S_PERSONALBYTES - 1)   ::  personal;       ! 32
END TYPE blake2s_param

TYPE blake2s_state
    INTEGER(KIND = 4), DIMENSION(0:7)                      ::  h
    INTEGER(KIND = 4), DIMENSION(0:1)                      ::  t
    INTEGER(KIND = 4), DIMENSION(0:1)                      ::  f
    INTEGER(KIND = 1), DIMENSION(0:2 * BLAKE2S_BLOCKBYTES) ::  buf
    INTEGER(KIND = 4)                                      ::  buflen
    INTEGER(KIND = 1)                                      ::  last_node
END TYPE blake2s_state 

TYPE blake2b_param
    INTEGER(KIND = 1)                                         ::  digest_length; ! 1
    INTEGER(KIND = 1)                                         ::  key_length;    ! 2
    INTEGER(KIND = 1)                                         ::  fanout;        ! 3
    INTEGER(KIND = 1)                                         ::  depth;         ! 4
    INTEGER(KIND = 4)                                         ::  leaf_length;   ! 8
    INTEGER(KIND = 8)                                         ::  node_offset;   ! 16
    INTEGER(KIND = 1)                                         ::  node_depth;    ! 17
    INTEGER(KIND = 1)                                         ::  inner_length;  ! 18
    INTEGER(KIND = 1), DIMENSION(0:13)                        ::  reserved;      ! 32
    INTEGER(KIND = 1), DIMENSION(0:BLAKE2B_SALTBYTES - 1)     ::  salt;          ! 48
    INTEGER(KIND = 1), DIMENSION(0:BLAKE2B_PERSONALBYTES - 1) ::  personal;      ! 64
END TYPE blake2b_param

TYPE blake2b_state
    INTEGER(KIND = 8), DIMENSION(0:7)                      ::  h
    INTEGER(KIND = 8), DIMENSION(0:1)                      ::  t
    INTEGER(KIND = 8), DIMENSION(0:1)                      ::  f
    INTEGER(KIND = 1), DIMENSION(0:2 * BLAKE2B_BLOCKBYTES) ::  buf
    INTEGER(KIND = 4)                                      ::  buflen
    INTEGER(KIND = 1)                                      ::  last_node
END TYPE blake2b_state

TYPE blake2sp_state
    TYPE(blake2s_state), DIMENSION(0:7)                        :: S
    TYPE(blake2s_state)                                        :: R
    INTEGER(KIND = 1), DIMENSION(0:8 * BLAKE2S_BLOCKBYTES - 1) :: buf
    INTEGER(KIND = 4)                                          :: buflen
END TYPE blake2sp_state

TYPE blake2bp_state
    TYPE(blake2b_state), DIMENSION(0:3)                        :: S
    TYPE(blake2b_state)                                        :: R
    INTEGER(KIND = 1), DIMENSION(0:4 * BLAKE2B_BLOCKBYTES - 1) :: buf
    INTEGER(KIND = 4)                                          :: buflen
END TYPE blake2bp_state

END MODULE blake2