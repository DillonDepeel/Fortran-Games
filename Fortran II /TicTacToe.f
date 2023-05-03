C      3D TICTACTOE, BOB LOUDEN, 2/15/62.?                              TTT 0001
DIMENSION NUM(64), LANE(76), MOVE(76,4), NTEST(9), NMOVE(16)?           TTT 0002
300 FORMAT (I3)?                                                        TTT 0003
301 FORMAT (I4)?                                                        TTT 0004
311 FORMAT (20H                    )?                                   TTT 0005
312 FORMAT (I4, 11H           )?                                        TTT 0006
313 FORMAT (32H                                )?                       TTT 0007
314 FORMAT (32H                                )?                       TTT 0008
315 FORMAT (24H                        )?                               TTT 0009
316 FORMAT (29H                             )?                          TTT 0010
317 FORMAT (25H                         )?                              TTT 0011
318 FORMAT (32H                                )?                       TTT 0012
319 FORMAT (32H                                )?                       TTT 0013
DO 1 I=1,9?                                                             TTT 0014
1 READ 300, NTEST(I)?                                                   TTT 0015
DO 3 I=1,16?                                                            TTT 0016
3 READ 300, NMOVE(I)?                                                   TTT 0017
DO 2 I=1,76?                                                            TTT 0018
DO 2 J=1,4?                                                             TTT 0019
2 READ 300, MOVE(I,J)?                                                  TTT 0020
K1=1?                                                                   TTT 0021
DO 400 I=1,9?                                                           TTT 0022
READ 313,?                                                              TTT 0023
400 TYPE 313,?                                                          TTT 0024
PAUSE?                                                                  TTT 0025
DO 100 I=1,25?                                                          TTT 0026
IF (SENSE SWITCH 1) 101,100?                                            TTT 0027
101 TYPE 313,?                                                          TTT 0028
100 READ 313,?                                                          TTT 0029
4 DO 401 I=1,5?                                                         TTT 0030
READ 313,?                                                              TTT 0031
401 TYPE 313,?                                                          TTT 0032
ACCEPT 311,?                                                            TTT 0033
READ 312, I?                                                            TTT 0034
READ 313,?                                                              TTT 0035
READ 314,?                                                              TTT 0036
READ 315,?                                                              TTT 0037
READ 316,?                                                              TTT 0038
READ 317,?                                                              TTT 0039
READ 318,?                                                              TTT 0040
READ 319,?                                                              TTT 0041
DO 5 I=1,64?                                                            TTT 0042
5 NUM(I)=0?                                                             TTT 0043
IF (SENSE SWITCH 2) 6,7?                                                TTT 0044
6 TYPE 313,?                                                            TTT 0045
ACCEPT 300, J1?                                                         TTT 0046
IF (SENSE SWITCH 3) 4,61?                                               TTT 0047
61 K1=J1/100?                                                           TTT 0048
J2=J1-100*K1?                                                           TTT 0049
J3=J2/10?                                                               TTT 0050
M=16*(K1-1)+4*(J3-1)+J2-10*J3?                                          TTT 0051
IF (M-64) 11,11,91?                                                     TTT 0052
11 IF (NUM(M)) 91,10,91?                                                TTT 0053
91 TYPE 314,?                                                           TTT 0054
GO TO 6?                                                                TTT 0055
10 NUM(M)=1?                                                            TTT 0056
7 DO 12 I=1,76?                                                         TTT 0057
J1=MOVE(I,1)?                                                           TTT 0058
J2=MOVE(I,2)?                                                           TTT 0059
J3=MOVE(I,3)?                                                           TTT 0060
J4=MOVE(I,4)?                                                           TTT 0061
12 LANE(I)=NUM(J1)+NUM(J2)+NUM(J3)+NUM(J4)?                             TTT 0062
J3=0?                                                                   TTT 0063
DO 13 J=1,9?                                                            TTT 0064
IF (J-7) 130,131,132?                                                   TTT 0065
132 IF (J-9) 130,131,130?                                               TTT 0066
131 DO 133 I=1,64?                                                      TTT 0067
J2=LANE(I)-100?                                                         TTT 0068
IF (J2) 133,134,134?                                                    TTT 0069
134 LANE(I)=J2?                                                         TTT 0070
133 CONTINUE?                                                           TTT 0071
130 DO 13 I=1,76?                                                       TTT 0072
IF (LANE(I)-NTEST(J)) 13,14,13?                                         TTT 0073
13 CONTINUE?                                                            TTT 0074
DO 15 I=K1,16?                                                          TTT 0075
M=NMOVE(I)?                                                             TTT 0076
IF (NUM(M)) 16,16,15?                                                   TTT 0077
15 CONTINUE?                                                            TTT 0078
14 GO TO (71,72,73,74,75,75,74,75,74),J?                                TTT 0079
71 TYPE 311,?                                                           TTT 0080
TYPE 319,?                                                              TTT 0081
18 DO 17 K=1,4?                                                         TTT 0082
M=MOVE(I,K)?                                                            TTT 0083
GO TO 80?                                                               TTT 0084
17 TYPE 301, M?                                                         TTT 0085
GO TO 4?                                                                TTT 0086
72 TYPE 311,?                                                           TTT 0087
TYPE 318,?                                                              TTT 0088
GO TO 18?                                                               TTT 0089
73 DO 19 K=1,4?                                                         TTT 0090
M=MOVE(I,K)?                                                            TTT 0091
IF (NUM(M)) 164,164,19?                                                 TTT 0092
19 CONTINUE?                                                            TTT 0093
74 DO 20 K=1,4?                                                         TTT 0094
M=MOVE(I,K)?                                                            TTT 0095
IF (NUM(M)) 21,21,20?                                                   TTT 0096
21 IF (LANE(M)-100) 22,166,166?                                         TTT 0097
166 IF (J-7) 167,168,16?                                                TTT 0098
167 TYPE 316,?                                                          TTT 0099
GO TO 165?                                                              TTT 0100
168 TYPE 317,?                                                          TTT 0101
GO TO 165?                                                              TTT 0102
22 LANE(M)=LANE(M)+100?                                                 TTT 0103
20 CONTINUE?                                                            TTT 0104
GO TO 13?                                                               TTT 0105
75 J2=0?                                                                TTT 0106
DO 23 K=1,4?                                                            TTT 0107
M=MOVE(I,K)?                                                            TTT 0108
IF (LANE(M)-100) 23,25,25?                                              TTT 0109
25 IF (J2) 26,26,27?                                                    TTT 0110
26 J2=1?                                                                TTT 0111
23 CONTINUE?                                                            TTT 0112
GO TO 13?                                                               TTT 0113
27 IF (J-6) 16,271,16?                                                  TTT 0114
271 DO 28 K=1,4?                                                        TTT 0115
M=MOVE(I,K)?                                                            TTT 0116
IF (NUM(M)) 29,29,28?                                                   TTT 0117
29 IF (LANE(M)-100) 30,28,28?                                           TTT 0118
28 CONTINUE?                                                            TTT 0119
30 J3=M?                                                                TTT 0120
GO TO 13?                                                               TTT 0121
164 TYPE 315,?                                                          TTT 0122
165 TYPE 311,?                                                          TTT 0123
16 IF (J-9) 161,163,163?                                                TTT 0124
163 IF (J3) 161,161,162?                                                TTT 0125
162 M=J3?                                                               TTT 0126
J=6?                                                                    TTT 0127
161 NUM(M)=5?                                                           TTT 0128
80 J2=(M-1)/16?                                                         TTT 0129
J3=M-16*J2?                                                             TTT 0130
J4=(J3-1)/4?                                                            TTT 0131
M=100*(J2+1)+10*(J4+1)+J3-4*J4?                                         TTT 0132
IF (J-2) 17,17,62?                                                      TTT 0133
62 TYPE 312, M?                                                         TTT 0134
GO TO 6?                                                                TTT 0135
END?                                                                    TTT 0136

 04?                                                                    TTT 0137
 15?                                                                    TTT 0138
 03?                                                                    TTT 0139
 10?                                                                    TTT 0140
 05?                                                                    TTT 0141
 00?                                                                    TTT 0142
 02?                                                                    TTT 0143
 01?                                                                    TTT 0144
 05?                                                                    TTT 0145
 01?                                                                    TTT 0146
 64?                                                                    TTT 0147
 13?                                                                    TTT 0148
 52?                                                                    TTT 0149
 04?                                                                    TTT 0150
 61?                                                                    TTT 0151
 16?                                                                    TTT 0152
 49?                                                                    TTT 0153
 22?                                                                    TTT 0154
 43?                                                                    TTT 0155
 23?                                                                    TTT 0156
 42?                                                                    TTT 0157
 26?                                                                    TTT 0158
 39?                                                                    TTT 0159
 27?                                                                    TTT 0160
 38?                                                                    TTT 0161
 22?                                                                    TTT 0162
 43?                                                                    TTT 0163
 64?                                                                    TTT 0164
 01?                                                                    TTT 0165
 23?                                                                    TTT 0166
 42?                                                                    TTT 0167
 61?                                                                    TTT 0168
 04?                                                                    TTT 0169
 26?                                                                    TTT 0170
 39?                                                                    TTT 0171
 52?                                                                    TTT 0172
 13?                                                                    TTT 0173
 27?                                                                    TTT 0174
 38?                                                                    TTT 0175
 49?                                                                    TTT 0176
 16?                                                                    TTT 0177
 21?                                                                    TTT 0178
 41?                                                                    TTT 0179
 61?                                                                    TTT 0180
 01?                                                                    TTT 0181
 22?                                                                    TTT 0182
 42?                                                                    TTT 0183
 62?                                                                    TTT 0184
 02?                                                                    TTT 0185
 23?                                                                    TTT 0186
 43?                                                                    TTT 0187
 63?                                                                    TTT 0188
 03?                                                                    TTT 0189
 24?                                                                    TTT 0190
 44?                                                                    TTT 0191
 64?                                                                    TTT 0192
 04?                                                                    TTT 0193
 19?                                                                    TTT 0194
 34?                                                                    TTT 0195
 49?                                                                    TTT 0196
 04?                                                                    TTT 0197
 23?                                                                    TTT 0198
 38?                                                                    TTT 0199
 53?                                                                    TTT 0200
 08?                                                                    TTT 0201
 27?                                                                    TTT 0202
 42?                                                                    TTT 0203
 57?                                                                    TTT 0204
 12?                                                                    TTT 0205
 31?                                                                    TTT 0206
 46?                                                                    TTT 0207
 61?                                                                    TTT 0208
 16?                                                                    TTT 0209
 25?                                                                    TTT 0210
 37?                                                                    TTT 0211
 49?                                                                    TTT 0212
 13?                                                                    TTT 0213
 26?                                                                    TTT 0214
 38?                                                                    TTT 0215
 50?                                                                    TTT 0216
 14?                                                                    TTT 0217
 27?                                                                    TTT 0218
 39?                                                                    TTT 0219
 51?                                                                    TTT 0220
 15?                                                                    TTT 0221
 28?                                                                    TTT 0222
 40?                                                                    TTT 0223
 52?                                                                    TTT 0224
 16?                                                                    TTT 0225
 18?                                                                    TTT 0226
 35?                                                                    TTT 0227
 52?                                                                    TTT 0228
 01?                                                                    TTT 0229
 22?                                                                    TTT 0230
 39?                                                                    TTT 0231
 56?                                                                    TTT 0232
 05?                                                                    TTT 0233
 26?                                                                    TTT 0234
 43?                                                                    TTT 0235
 60?                                                                    TTT 0236
 09?                                                                    TTT 0237
 30?                                                                    TTT 0238
 47?                                                                    TTT 0239
 64?                                                                    TTT 0240
 13?                                                                    TTT 0241
 17?                                                                    TTT 0242
 33?                                                                    TTT 0243
 49?                                                                    TTT 0244
 01?                                                                    TTT 0245
 18?                                                                    TTT 0246
 34?                                                                    TTT 0247
 50?                                                                    TTT 0248
 02?                                                                    TTT 0249
 19?                                                                    TTT 0250
 35?                                                                    TTT 0251
 51?                                                                    TTT 0252
 03?                                                                    TTT 0253
 20?                                                                    TTT 0254
 36?                                                                    TTT 0255
 52?                                                                    TTT 0256
 04?                                                                    TTT 0257
 21?                                                                    TTT 0258
 37?                                                                    TTT 0259
 53?                                                                    TTT 0260
 05?                                                                    TTT 0261
 22?                                                                    TTT 0262
 38?                                                                    TTT 0263
 54?                                                                    TTT 0264
 06?                                                                    TTT 0265
 23?                                                                    TTT 0266
 39?                                                                    TTT 0267
 55?                                                                    TTT 0268
 07?                                                                    TTT 0269
 24?                                                                    TTT 0270
 40?                                                                    TTT 0271
 56?                                                                    TTT 0272
 08?                                                                    TTT 0273
 25?                                                                    TTT 0274
 41?                                                                    TTT 0275
 57?                                                                    TTT 0276
 09?                                                                    TTT 0277
 26?                                                                    TTT 0278
 42?                                                                    TTT 0279
 58?                                                                    TTT 0280
 10?                                                                    TTT 0281
 27?                                                                    TTT 0282
 43?                                                                    TTT 0283
 59?                                                                    TTT 0284
 11?                                                                    TTT 0285
 28?                                                                    TTT 0286
 44?                                                                    TTT 0287
 60?                                                                    TTT 0288
 12?                                                                    TTT 0289
 29?                                                                    TTT 0290
 45?                                                                    TTT 0291
 61?                                                                    TTT 0292
 13?                                                                    TTT 0293
 30?                                                                    TTT 0294
 46?                                                                    TTT 0295
 62?                                                                    TTT 0296
 14?                                                                    TTT 0297
 31?                                                                    TTT 0298
 47?                                                                    TTT 0299
 63?                                                                    TTT 0300
 15?                                                                    TTT 0301
 32?                                                                    TTT 0302
 48?                                                                    TTT 0303
 64?                                                                    TTT 0304
 16?                                                                    TTT 0305
 02?                                                                    TTT 0306
 03?                                                                    TTT 0307
 04?                                                                    TTT 0308
 01?                                                                    TTT 0309
 06?                                                                    TTT 0310
 07?                                                                    TTT 0311
 08?                                                                    TTT 0312
 05?                                                                    TTT 0313
 10?                                                                    TTT 0314
 11?                                                                    TTT 0315
 12?                                                                    TTT 0316
 09?                                                                    TTT 0317
 14?                                                                    TTT 0318
 15?                                                                    TTT 0319
 16?                                                                    TTT 0320
 13?                                                                    TTT 0321
 05?                                                                    TTT 0322
 09?                                                                    TTT 0323
 13?                                                                    TTT 0324
 01?                                                                    TTT 0325
 06?                                                                    TTT 0326
 10?                                                                    TTT 0327
 14?                                                                    TTT 0328
 02?                                                                    TTT 0329
 07?                                                                    TTT 0330
 11?                                                                    TTT 0331
 15?                                                                    TTT 0332
 03?                                                                    TTT 0333
 08?                                                                    TTT 0334
 12?                                                                    TTT 0335
 16?                                                                    TTT 0336
 04?                                                                    TTT 0337
 06?                                                                    TTT 0338
 11?                                                                    TTT 0339
 16?                                                                    TTT 0340
 01?                                                                    TTT 0341
 07?                                                                    TTT 0342
 10?                                                                    TTT 0343
 13?                                                                    TTT 0344
 04?                                                                    TTT 0345
 18?                                                                    TTT 0346
 19?                                                                    TTT 0347
 20?                                                                    TTT 0348
 17?                                                                    TTT 0349
 22?                                                                    TTT 0350
 23?                                                                    TTT 0351
 24?                                                                    TTT 0352
 21?                                                                    TTT 0353
 26?                                                                    TTT 0354
 27?                                                                    TTT 0355
 28?                                                                    TTT 0356
 25?                                                                    TTT 0357
 30?                                                                    TTT 0358
 31?                                                                    TTT 0359
 32?                                                                    TTT 0360
 29?                                                                    TTT 0361
 21?                                                                    TTT 0362
 25?                                                                    TTT 0363
 29?                                                                    TTT 0364
 17?                                                                    TTT 0365
 22?                                                                    TTT 0366
 26?                                                                    TTT 0367
 30?                                                                    TTT 0368
 18?                                                                    TTT 0369
 23?                                                                    TTT 0370
 27?                                                                    TTT 0371
 31?                                                                    TTT 0372
 19?                                                                    TTT 0373
 24?                                                                    TTT 0374
 28?                                                                    TTT 0375
 32?                                                                    TTT 0376
 20?                                                                    TTT 0377
 22?                                                                    TTT 0378
 27?                                                                    TTT 0379
 32?                                                                    TTT 0380
 17?                                                                    TTT 0381
 23?                                                                    TTT 0382
 26?                                                                    TTT 0383
 29?                                                                    TTT 0384
 20?                                                                    TTT 0385
 34?                                                                    TTT 0386
 35?                                                                    TTT 0387
 36?                                                                    TTT 0388
 33?                                                                    TTT 0389
 38?                                                                    TTT 0390
 39?                                                                    TTT 0391
 40?                                                                    TTT 0392
 37?                                                                    TTT 0393
 42?                                                                    TTT 0394
 43?                                                                    TTT 0395
 44?                                                                    TTT 0396
 41?                                                                    TTT 0397
 46?                                                                    TTT 0398
 47?                                                                    TTT 0399
 48?                                                                    TTT 0400
 45?                                                                    TTT 0401
 37?                                                                    TTT 0402
 41?                                                                    TTT 0403
 45?                                                                    TTT 0404
 33?                                                                    TTT 0405
 38?                                                                    TTT 0406
 42?                                                                    TTT 0407
 46?                                                                    TTT 0408
 34?                                                                    TTT 0409
 39?                                                                    TTT 0410
 43?                                                                    TTT 0411
 47?                                                                    TTT 0412
 35?                                                                    TTT 0413
 40?                                                                    TTT 0414
 44?                                                                    TTT 0415
 48?                                                                    TTT 0416
 36?                                                                    TTT 0417
 38?                                                                    TTT 0418
 43?                                                                    TTT 0419
 48?                                                                    TTT 0420
 33?                                                                    TTT 0421
 39?                                                                    TTT 0422
 42?                                                                    TTT 0423
 45?                                                                    TTT 0424
 36?                                                                    TTT 0425
 50?                                                                    TTT 0426
 51?                                                                    TTT 0427
 52?                                                                    TTT 0428
 49?                                                                    TTT 0429
 54?                                                                    TTT 0430
 55?                                                                    TTT 0431
 56?                                                                    TTT 0432
 53?                                                                    TTT 0433
 58?                                                                    TTT 0434
 59?                                                                    TTT 0435
 60?                                                                    TTT 0436
 57?                                                                    TTT 0437
 62?                                                                    TTT 0438
 63?                                                                    TTT 0439
 64?                                                                    TTT 0440
 61?                                                                    TTT 0441
 53?                                                                    TTT 0442
 57?                                                                    TTT 0443
 61?                                                                    TTT 0444
 49?                                                                    TTT 0445
 54?                                                                    TTT 0446
 58?                                                                    TTT 0447
 62?                                                                    TTT 0448
 50?                                                                    TTT 0449
 55?                                                                    TTT 0450
 59?                                                                    TTT 0451
 63?                                                                    TTT 0452
 51?                                                                    TTT 0453
 56?                                                                    TTT 0454
 60?                                                                    TTT 0455
 64?                                                                    TTT 0456
 52?                                                                    TTT 0457
 54?                                                                    TTT 0458
 59?                                                                    TTT 0459
 64?                                                                    TTT 0460
 49?                                                                    TTT 0461
 55?                                                                    TTT 0462
 58?                                                                    TTT 0463
 61?                                                                    TTT 0464
 52?                                                                    TTT 0465
 ?                                                                      TTT 0466
GOOD MORNING, SIR, MADAM OR MISS?                                       TTT 0467
(I CAN HARDLY TELL FROM IN HERE)?                                       TTT 0468
WE ARE GOING TO PLAY TIC-TAC-TOE?                                       TTT 0469
IF YOU KNOW HOW WE CAN BEGIN.?                                          TTT 0470
IF YOU WANT ME TO EXPLAIN FIRST,?                                       TTT 0471
RAISE PROGRAM SWITCH 1.?                                                TTT 0472
NOW PUSH START.?                                                        TTT 0473
 ?                                                                      TTT 0474
THIS GAME IS TYPICAL OF WHAT YOU?                                       TTT 0475
CAN ACCOMPLISH USING THE 1620?                                          TTT 0476
IF YOU HAVE NOTHING BETTER TO DO?                                       TTT 0477
THE GAME IS PLAYED ON 4 LEVELS?                                         TTT 0478
EACH LEVEL IS A 4X4 GRID?                                               TTT 0479
THE OBJECT IS TO GET 4 IN A ROW?                                        TTT 0480
THE LEVELS ARE ABOVE EACH OTHER?                                        TTT 0481
AND WINNING ROWS MAY BE SET UP?                                         TTT 0482
IN ANY DIRECTION.?                                                      TTT 0483
YOU AND I WILL TAKE TURNS MOVING?                                       TTT 0484
 ?                                                                      TTT 0485
TO TELL ME WHERE YOU HAVE MOVED,?                                       TTT 0486
JUST TYPE A 3-DIGIT NUMBER.?                                            TTT 0487
FOR EXAMPLE, 243 WOULD MEAN?                                            TTT 0488
2ND LEVEL, 4TH ROW, 3RD COLUMN.?                                        TTT 0489
 ?                                                                      TTT 0490
TO RESTART IN CASE OF TIES,?                                            TTT 0491
RAISE SWITCH 3, RELEASE, START.?                                        TTT 0492
TO CORRECT A TYPING ERROR,?                                             TTT 0493
RAISE SWITCH 4, RELEASE, START.?                                        TTT 0494
 ?                                                                      TTT 0495
YOU MAY DRAW THE GAME ON PAPER.?                                        TTT 0496
I KEEP TRACK IN MY HEAD.....?                                           TTT 0497
BOB LOUDEN, IBM, FEBRUARY, 1962.?                                       TTT 0498
 ?                                                                      TTT 0499
 ?                                                                      TTT 0500
RAISE SWITCH 2 TO MOVE FIRST.?                                          TTT 0501
OTHERWISE I WILL. TYPE YOUR NAME?                                       TTT 0502
RELEASE AND START.?                                                     TTT 0503
 ?                                                                      TTT 0504
4321 IS MY MOVE?                                                        TTT 0505
TYPE YOUR MOVE, RELEASE, START.?                                        TTT 0506
COME ON NOW, THAT SQUARE IS FULL?                                       TTT 0507
GOOD TRY THERE,?                                                        TTT 0508
LETS SEE YOU GET OUT OF THIS,?                                          TTT 0509
VERY TRICKY OF YOU,?                                                    TTT 0510
I WIN ON THE FOLLOWING SEQUENCE?                                        TTT 0511
YOU WIN (DARN IT) AS FOLLOWS?                                           TTT 0512
 ?                                                                      TTT 0513
RAISE SWITCH 2 TO MOVE FIRST.?                                          TTT 0514
PLEASE TYPE YOUR NAME.?                                                 TTT 0515
THEN RELEASE AND START.?                                                TTT 0516
 ?                                                                      TTT 0517
4321 IS MY PLAY?                                                        TTT 0518
MAKE YOUR PLAY NOW.?                                                    TTT 0519
COME ON NOW, THAT PLACE IS TAKEN?                                       TTT 0520
YOURE IN THERE PITCHING,?                                               TTT 0521
AFRAID YOUVE HAD IT,?                                                   TTT 0522
THOUGHT YOU HAD ME THERE,?                                              TTT 0523
I WIN AS FOLLOWS?                                                       TTT 0524
CONGRATULATIONS?                                                        TTT 0525
 ?                                                                      TTT 0526
NOW DECIDE WHO GOES FIRST.?                                             TTT 0527
GIVE ME YOUR NAME, PLEASE?                                              TTT 0528
THEN RELEASE AND START.?                                                TTT 0529
 ?                                                                      TTT 0530
4321 IS MY MOVE?                                                        TTT 0531
WHATS YOUR MOVE?                                                        TTT 0532
COME ON NOW, THAT SQUARE IS GONE?                                       TTT 0533
NOW, REALLY,?                                                           TTT 0534
THIS WILL BE A SHORT GAME,?                                             TTT 0535
AN EXCELLENT PLAY,?                                                     TTT 0536
THANK YOU VERY MUCH FOR THE GAME?                                       TTT 0537
PLEASE TELL ME HOW YOU DID IT?                                          TTT 0538
 ?                                                                      TTT 0539
DECIDE WHO WILL START.?                                                 TTT 0540
SIGN ON THE NEXT LINE, PLEASE.?                                         TTT 0541
THEN RELEASE AND START.?                                                TTT 0542
 ?                                                                      TTT 0543
4321 IS MY PLAY?                                                        TTT 0544
CARRY ON.?                                                              TTT 0545
REALLY, THAT SQUARE IS OCCUPIED.?                                       TTT 0546
RATHER OBVIOUS,?                                                        TTT 0547
AFRAID YOU ARE TRAPPED,?                                                TTT 0548
VERY GOOD SHOW.?                                                        TTT 0549
THAT WAS A TOP-NOTCH EFFORT.?                                           TTT 0550
BET YOU CANT DO THAT AGAIN.?                                            TTT 0551
 ?                                                                      TTT 0552
WHO GOES FIRST, YOU OR I.?                                              TTT 0553
PLEASE SIGN THE REGISTER.?                                              TTT 0554
THEN RELEASE AND START.?                                                TTT 0555
 ?                                                                      TTT 0556
4321 IS MY MOVE?                                                        TTT 0557
WHAT DO YOU SAY NOW.?                                                   TTT 0558
THAT SQUARE IS OCCUPIED.?                                               TTT 0559
SORRY TO DISAPPOINT YOU,?                                               TTT 0560
ONLY TWO MOVES TO GO,?                                                  TTT 0561
CLEVER OF YOU, ?                                                        TTT 0562
YOU CANT WIN THEM ALL.?                                                 TTT 0563
I CAN SEE YOUVE DONE THIS BEFORE?                                       TTT 0564
 ?                                                                      TTT 0565
LETS STOP AND GET BACK TO WORK..?                                       TTT 0566
 ?                                                                      TTT 0567
 ?                                                                      TTT 0568
