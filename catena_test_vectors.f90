PROGRAM catena_test_vectors
USE catena
IMPLICIT NONE

CALL simpletest("password", "salt", "", INT(1,1));
CALL simpletest("password", "salt", "", INT(10,1));
CALL simpletest("password", "salt", "data", INT(10,1));
CALL simpletest("passwordPASSWORDpassword", "saltSALTsaltSALTsaltSALTsaltSALTsalt", "", INT(10,1));
CALL PHC_test();

CONTAINS    
SUBROUTINE print_hex(message, x, leng)
IMPLICIT NONE
    CHARACTER (LEN = *) :: message
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: x
    INTEGER, INTENT(IN) :: leng
    INTEGER :: i
    PRINT *, message
    DO i = 1,leng
        WRITE(*, "(1Z2.2)", ADVANCE="no") x(i)
        IF (MODULO(i, 8) .EQ. 0) THEN
            PRINT *
        ENDIF
    END DO
    PRINT *
END SUBROUTINE print_hex

SUBROUTINE test_output(pwd, pwdlen, salt, saltlen, datas, datalen, garlic, hashlen)
IMPLICIT NONE
    INTEGER (KIND = 1), DIMENSION(:), INTENT(IN) :: pwd, salt, datas
    INTEGER, INTENT(IN) :: pwdlen, datalen, garlic, hashlen
    INTEGER (KIND = 1), INTENT(IN) :: saltlen
    INTEGER (KIND = 1), DIMENSION(hashlen) :: hash
    INTEGER (KIND = 1), DIMENSION(pwdlen) :: pwdcpy
    INTEGER :: i
    
    DO i = 1,pwdlen
        pwdcpy(i) = pwd(i)
    END DO
    i = Catena(pwdcpy, pwdlen, salt, saltlen, datas, datalen, CONSTLAMBDA,&
        INT(garlic, 1), INT(garlic, 1), INT(hashlen, 1), hash)
    CALL print_hex("Password: ", pwd, pwdlen)
    CALL print_hex("Salt: ", salt, INT(saltlen, 4));
    CALL print_hex("Associated data:", datas, datalen);
    PRINT *, "Lambda: ", CONSTLAMBDA
    PRINT *, "(Min-)Garlic: ", garlic
    CALL print_hex("Output: ", hash, INT(hashlen, 4));
    PRINT *
END SUBROUTINE test_output

SUBROUTINE simpletest(password, salt, header, garlic)
IMPLICIT NONE
    CHARACTER (LEN = *) :: password, salt, header
    INTEGER (KIND = 1), INTENT(IN) :: garlic
    INTEGER (KIND = 1), DIMENSION(LEN(password)) :: ipassword
    INTEGER (KIND = 1), DIMENSION(LEN(salt)) :: isalt
    INTEGER (KIND = 1), DIMENSION(LEN(header)) :: iheader
    INTEGER :: i
    
    DO i = 1,LEN(password)
        ipassword(i) = IACHAR(password(i:i))
    END DO
    DO i = 1,LEN(salt)
        isalt(i) = IACHAR(salt(i:i))
    END DO
    DO i = 1,LEN(header)
        iheader(i) = IACHAR(header(i:i))
    END DO
    CALL test_output(ipassword, LEN(password), isalt, INT(LEN(salt), 1), iheader, LEN(header), INT(garlic, 4), INT(H_LEN, 4));
END SUBROUTINE simpletest

SUBROUTINE PHC_test()
IMPLICIT NONE
    INTEGER :: i
    INTEGER (KIND = 1), DIMENSION(1) :: j
    DO i = 1,256
        CALL test_output([INT(i,1)], 1, [j], INT(1,1), [INT(0,1)], 0, 10, 32)
    END DO
    DO i = 1,256
        CALL test_output([j], 1, [INT(i,1)], INT(1,1), [INT(0,1)], 0, 10, 32)
    END DO
END SUBROUTINE PHC_test
END PROGRAM catena_test_vectors