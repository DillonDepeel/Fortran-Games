          DIMENSION A(11)
             READ A
      2      DO 3,8,11 J=1,11
      3      I=11-J
             Y=SQRT(ABS(A(I+1)))+5*A(I+1)**3
             IF (400>=Y) 8,4
      4      PRINT I,999.
             GOTO 2
      8      PRINT I,Y
      11     STOP
