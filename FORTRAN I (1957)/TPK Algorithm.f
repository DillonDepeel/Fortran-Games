  C      THE TPK ALGORITHM
      C      FORTRAN I STYLE
             FUNF(T)=SQRTF(ABSF(T))+5.0*T**3
             DIMENSION A(11)
        1    FORMAT(6F12.4)
             READ 1,A
             DO 10 J=1,11
             I=11-J
             Y=FUNF(A(I+1))
             IF(400.0-Y)4,8,8
        4    PRINT 5,I
        5    FORMAT(I10,10H TOO LARGE)
             GOTO 10
        8    PRINT 9,I,Y
        9    FORMAT(I10,F12.7)
       10    CONTINUE
             STOP 52525
