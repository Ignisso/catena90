MODULE catena
USE catena_blake2b_ref
USE catena_brg
IMPLICIT NONE

INTEGER (KIND = 1), PARAMETER :: H_LEN = 64
INTEGER (KIND = 1), PARAMETER :: KEY_LEN = 16
INTEGER (KIND = 1), PARAMETER :: CONSTLAMBDA = 2
INTEGER (KIND = 1), PARAMETER :: CONSTGARLIC = 21
INTEGER (KIND = 1), PARAMETER :: CONSTMIN_GARLIC = 21

CHARACTER (LEN = *), PARAMETER :: VERSION_ID = "Dragonfly-Full"
INTEGER (KIND = 1), PARAMETER :: PASSWORD_HASHING_MODE = 0
INTEGER (KIND = 1), PARAMETER :: KEY_DERIVATION_MODE = 1
INTEGER (KIND = 1), PARAMETER :: REGULAR = 0
INTEGER (KIND = 1), PARAMETER :: CONSTCLIENT = 1


CONTAINS
INTEGER FUNCTION pCatena(pwd, pwdlen, salt, saltlen, datas, &
    datalen, lambda, min_garlic, garlic, hashlen, &
    client, tweak_id, hash) RESULT(exitcode)
IMPLICIT NONE    
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: pwd, salt
    INTEGER (KIND = 4), INTENT(IN) :: pwdlen, datalen
    INTEGER (KIND = 1), INTENT(IN) :: saltlen, lambda, min_garlic, garlic, hashlen, client, tweak_id
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: datas
    INTEGER (KIND = 1), DIMENSION(:) :: hash
    
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: x
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: hv
    INTEGER (KIND = 1), DIMENSION(4) :: t
    INTEGER (KIND = 4) :: c
    
    IF (hashlen > H_LEN .OR. garlic > 63 .OR. min_garlic > garlic .OR. &
        lambda == 0 .OR. min_garlic == 0) THEN
        exitcode = -1
        RETURN
    END IF
    

    t(1) = tweak_id;
    t(2) = lambda;
    t(3) = hashlen;
    t(4) = saltlen;

    CALL Hash1(datas, INT(datalen,8), INT(x,1));

    CALL Hash5(hv, INT(H_LEN,8), t, INT(4,8), x, INT(H_LEN,8), pwd,  INT(pwdlen,8), salt, INT(saltlen,8), INT(x,1));

    DO c = min_garlic,garlic

        IF (c == garlic .AND. client == CONSTCLIENT) THEN
            hash(:H_LEN) = x(:H_LEN)
            exitcode = 0
            RETURN
        END IF

        x(hashlen:H_LEN - hashlen - 1) = 0
    END DO
    hash(:hashlen) = x(:hashlen)
    exitcode = 0
END FUNCTION pCatena

INTEGER FUNCTION Catena(pwd, pwdlen, salt, saltlen, datas, &
    datalen, lambda, min_garlic, garlic, hashlen, &
    hash) RESULT(exitcode)
IMPLICIT NONE    
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: pwd, salt
    INTEGER (KIND = 4), INTENT(IN) :: pwdlen, datalen
    INTEGER (KIND = 1), INTENT(IN) :: saltlen
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: datas
    INTEGER (KIND = 1), INTENT(IN) :: lambda, min_garlic, garlic, hashlen
    INTEGER (KIND = 1), DIMENSION(:) :: hash
    
    exitcode = pCatena(pwd, pwdlen, salt, saltlen, datas, datalen, lambda, &
        min_garlic, garlic, hashlen, REGULAR, PASSWORD_HASHING_MODE, hash)
END FUNCTION Catena

INTEGER FUNCTION NaiveCatena(pwd, salt, datas, hash) RESULT(exitcode)
IMPLICIT NONE
    CHARACTER , DIMENSION(:), INTENT(IN) :: pwd, salt, datas
    INTEGER (KIND = 1), DIMENSION(LEN(pwd)) :: cpwd
    INTEGER (KIND = 1), DIMENSION(LEN(salt)) :: csalt
    INTEGER (KIND = 1), DIMENSION(LEN(datas)) :: cdatas
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: hash
    INTEGER :: i
    
    DO i = 1,LEN(pwd)
        cpwd(i) = ichar(pwd(i))
    END DO
    DO i = 1,LEN(salt)
        csalt(i) = ichar(salt(i))
    END DO
    DO i = 1,LEN(datas)
        cdatas(i) = ichar(datas(i))
    END DO
    exitcode = pCatena(cpwd, LEN(pwd), csalt, int(LEN(salt), 1), cdatas, LEN(datas), &
        CONSTLAMBDA, CONSTMIN_GARLIC, CONSTGARLIC, H_LEN, REGULAR, PASSWORD_HASHING_MODE, hash)
END FUNCTION NaiveCatena

INTEGER FUNCTION SimpleCatena(pwd, pwdlen, salt, saltlen, datas, &
    datalen, hash) RESULT(exitcode)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: pwd, salt
    INTEGER (KIND = 4), INTENT(IN) :: pwdlen, datalen
    INTEGER (KIND = 1), INTENT(IN) :: saltlen
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: datas
    INTEGER (KIND = 1), DIMENSION(:) :: hash
    
    exitcode = pCatena(pwd, pwdlen, salt, saltlen, datas, datalen, CONSTLAMBDA, &
        CONSTMIN_GARLIC, CONSTGARLIC, H_LEN, REGULAR, PASSWORD_HASHING_MODE, hash)
END FUNCTION SimpleCatena

INTEGER FUNCTION CatenaClient(pwd, pwdlen, salt, saltlen, datas, &
    datalen, lambda, min_garlic, garlic, hashlen, x) RESULT(exitcode)
IMPLICIT NONE    
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: pwd, salt
    INTEGER (KIND = 4), INTENT(IN) :: pwdlen, datalen
    INTEGER (KIND = 1), INTENT(IN) :: saltlen, lambda, min_garlic, garlic, hashlen
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: datas
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: x
    
    exitcode = pCatena(pwd, pwdlen, salt, saltlen, datas, datalen, lambda, &
        min_garlic, garlic, hashlen, CONSTCLIENT, PASSWORD_HASHING_MODE, x)
END FUNCTION CatenaClient

INTEGER FUNCTION CatenaServer(garlic, x, hashlen, hash) RESULT(exitcode)
IMPLICIT NONE
    INTEGER (KIND = 1), INTENT(IN) :: garlic, hashlen
    INTEGER (KIND = 1), DIMENSION(H_LEN), INTENT(IN) :: x
    INTEGER (KIND = 1), DIMENSION(:) :: hash
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: z
    INTEGER :: i
    IF (hashlen > H_LEN) THEN
        exitcode = -1
        RETURN
    END IF

    DO i = 1,hashlen
        hash(i) = z(i)
    END DO
    exitcode = 0
END FUNCTION CatenaServer

INTEGER FUNCTION CatenaKeyedServer(garlic, x, key, uuid, hashlen, chash) RESULT(exitcode)
IMPLICIT NONE
    INTEGER (KIND = 1), INTENT(IN) :: garlic
    INTEGER (KIND = 1), DIMENSION(H_LEN), INTENT(IN) :: x
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: key
    INTEGER (KIND = 8), INTENT(IN) :: uuid, hashlen
    INTEGER (KIND = 1), DIMENSION(:) :: chash
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: z
    INTEGER :: i
    
    IF (hashlen > H_LEN) THEN
        exitcode = -1
        RETURN
    END IF
    DO i = 1,hashlen
        chash(i) = z(i)
    END DO

    DO i = 1,hashlen
        chash(i) = XOR(chash(i), z(i))
    END DO
    exitcode = 0
END FUNCTION CatenaKeyedServer

SUBROUTINE CIUpdate(old_hash, lambda, salt, saltlen, old_garlic, new_garlic, &
    hashlen, new_hash)
IMPLICIT NONE    
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: old_hash, salt
    INTEGER (KIND = 1), INTENT(IN) :: lambda, saltlen, old_garlic, new_garlic, hashlen
    INTEGER (KIND = 1), DIMENSION(:) :: new_hash
    INTEGER (KIND = 1) :: c
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: x
    
    x(:hashlen) = old_hash(:hashlen)
    x(hashlen + 1:) = 0
    
    DO c = old_garlic + 1,new_garlic
        x(hashlen + 1:) = 0
    END DO
    
    DO c = 1,hashlen
        new_hash(c) = x(c)
    END DO
END SUBROUTINE CIUpdate

SUBROUTINE CIKeyedUpdate(old_hash, lambda, salt, saltlen, old_garlic, new_garlic, &
    hashlen, key, uuid, new_hash)
IMPLICIT NONE    
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: old_hash, salt
    INTEGER (KIND = 1), INTENT(IN) :: lambda, saltlen, old_garlic, new_garlic, hashlen
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: key
    INTEGER (KIND = 8), INTENT(IN) :: uuid
    INTEGER (KIND = 1), DIMENSION(:) :: new_hash
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: keystream
    INTEGER (KIND = 1) :: c
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: x
    
    x(:hashlen) = old_hash(:hashlen)
    x(hashlen + 1:) = 0
    
    DO c = 1,hashlen
        x(c) = XOR(x(c), keystream(c))
    END DO
    
    DO c = old_garlic + 1,new_garlic
        x(hashlen + 1:) = 0
    END DO
    
    DO c = 1,hashlen
        x(c) = XOR(x(c), keystream(c))
    END DO
    
    DO c = 1,hashlen
        new_hash(c) = x(c)
    END DO
END SUBROUTINE CIKeyedUpdate

SUBROUTINE CatenaKG(pwd, pwdlen, salt, saltlen, datas, datalen, lambda, &
    min_garlic, garlic, keylen, keyid, key)
IMPLICIT NONE    
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: pwd, salt
    INTEGER (KIND = 4), INTENT(IN) :: pwdlen, datalen, keylen
    INTEGER (KIND = 1), INTENT(IN) :: saltlen, lambda, min_garlic, garlic, keyid
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: datas
    INTEGER (KIND = 1), DIMENSION(:) :: key
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: hash
    INTEGER (KIND = 1), PARAMETER :: zero = 0
    INTEGER (KIND = 4) :: leng, rest
    INTEGER (KIND = 8) :: i
    leng = keylen / H_LEN
    rest = mod(keylen, H_LEN)
    
    i = pCatena(pwd, pwdlen, salt, saltlen, datas, datalen, lambda, &
        min_garlic, garlic, H_LEN, REGULAR, KEY_DERIVATION_MODE, hash)
    
    IF (rest > 0) THEN
        DO i = 1,leng
            key(leng * H_LEN + i) = hash(i)
        END DO
    END IF
END SUBROUTINE CatenaKG

SUBROUTINE CatenaKeyedHashing(pwd, pwdlen, salt, saltlen, datas, &
    datalen, lambda, min_garlic, garlic, hashlen, key, uuid, chash)
IMPLICIT NONE    
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: pwd, salt, key
    INTEGER (KIND = 4), INTENT(IN) :: pwdlen, datalen
    INTEGER (KIND = 1), INTENT(IN) :: saltlen, lambda, min_garlic, garlic, hashlen
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: datas
    INTEGER (KIND = 8), INTENT(IN) :: uuid
    INTEGER (KIND = 1), DIMENSION(:) :: chash
    INTEGER (KIND = 1), DIMENSION(H_LEN) :: keystream
    INTEGER :: i
    
    i = pCatena(pwd, pwdlen, salt, saltlen, datas, datalen, lambda, &
        min_garlic, garlic, hashlen, REGULAR, PASSWORD_HASHING_MODE, chash)
    DO i = 1,hashlen
        chash(i) = XOR(chash(i), keystream(i))
    END DO
END SUBROUTINE CatenaKeyedHashing

INTEGER FUNCTION PHS(outs, outlen, ins, inlen, salt, saltlen, tcost, mcost) &
    RESULT(exitcode)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: outs, ins, salt
    INTEGER (KIND = 4), INTENT(IN) :: outlen, inlen, saltlen
    INTEGER, INTENT(IN) :: tcost, mcost
    INTEGER (KIND = 1), DIMENSION(0) :: tmp
    
    INTEGER (KIND = 1) :: int8
    INTEGER (KIND = 4) :: int32
    INTEGER, PARAMETER :: MAXINT8 = (2 * huge(int8) + 1)
    INTEGER (KIND = 8), PARAMETER :: MAXINT32 = (2 * int(huge(int32), 8) + 1)
    
    IF (outlen > MAXINT8 .OR. int(inlen, 8) > MAXINT32 .OR. saltlen > MAXINT32 &
        .OR. tcost > MAXINT8 .OR. mcost > MAXINT8) THEN
        exitcode = 1
        RETURN
    END IF
    
    exitcode = pCatena(ins, inlen, salt, int(saltlen, 1), tmp, 0, int(tcost, 1), &
        int(mcost, 1), int(mcost, 1), int(outlen, 1), REGULAR, PASSWORD_HASHING_MODE, &
        int(outs, 1))
END FUNCTION PHS
END MODULE catena