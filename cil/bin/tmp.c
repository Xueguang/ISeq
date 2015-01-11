#line 50 "dfh_simple.c"
void B_IemsTask(void) 
{ 
  unint32 i1 ;
  unint32 tmsLeftLen2 ;
  unint32 fifoEmptyLen3 ;
  unint32 sendLen4 ;
  unint32 tmp5 ;
  unint32 i6 ;
  unint32 tmpPingPang7 ;

  {
  {
  {
  {
#line 85
  tmsLeftLen2 = 256U - G_cntTmsSend;
#line 86
  fifoEmptyLen3 = (unint32 )(128 - (int )(*((int volatile   *)285214568) & (int volatile   )255));
  }
#line 88
  if (tmsLeftLen2 > 0U) {
    {
#line 90
    sendLen4 = tmsLeftLen2 < fifoEmptyLen3 ? tmsLeftLen2 : fifoEmptyLen3;
#line 92
    i1 = (unint32 )0;
    }
#line 92
    while (i1 < sendLen4) {
      {
#line 94
      tmp5 = G_cntTmsSend;
#line 94
      G_cntTmsSend ++;
#line 94
      *((int volatile   *)285214572) = (int volatile   )G_tmsDataBuf[G_tmsPingPong][tmp5];
#line 92
      i1 ++;
      }
    }
  }

#line 74
  goto Lret_B_SendTmsData;
  }
  Lret_B_SendTmsData: /* CIL Label */ ;
  }
#line 54
  if (G_cntTmsInterrupt == 0U) {
    {
#line 56
    G_bTmsPack = (unint32 )449838109;
#line 57
    G_cntTmsInterrupt ++;
    }
  } else
#line 59
  if (G_cntTmsInterrupt < 7U) {
#line 61
    if (G_cntTmsInterrupt == 6U) {
      {
#line 63
      G_bTmsPack = (unint32 )0;
      {
      {
      {
#line 112
      tmpPingPang7 = (G_tmsPingPong ^ 1U) & 1U;
#line 114
      i6 = (unint32 )4;
      }
#line 114
      while (i6 < 256U) {
        {
#line 116
        G_tmsDataBuf[tmpPingPang7][i6] = G_tmsTestBuf[i6];
#line 114
        i6 ++;
        }
      }

#line 102
      goto Lret_B_TmPack;
      }
      Lret_B_TmPack: /* CIL Label */ ;
      }
      }
    }
#line 67
    G_cntTmsInterrupt ++;
  }

#line 47
  return;
}
}
#line 78 "dfh_simple.c"
