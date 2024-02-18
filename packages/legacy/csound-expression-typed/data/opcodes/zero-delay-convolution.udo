; Here’s a UDO for zero-latency partitioned convolution

;/**************************************************
;asig ZConv ain,ipart,irat,inp,ifn
;ain - input signal
;ipart - first partition size in samples
;irat - partition growth ratio
;inp - total number of partition sizes
;ifn - function table number containing the IR
;**************************************************/
opcode ZConv,a,aiiiio
 asig,iprt,irat,inp,ifn,icnt xin
 if icnt < inp-1 then
  acn ZConv asig,iprt,irat,inp,ifn,icnt+1
 endif
 if icnt == 0 then
   a1 dconv asig,iprt,ifn
 elseif icnt < inp-1 then
   ipt = iprt*irat^(icnt-1)
   isiz = ipt*(irat-1)
   print ipt
   print isiz
   a1 ftconv asig,ifn,ipt,ipt,isiz
 else
   ipt = iprt*irat^(icnt-1)
   a1 ftconv asig,ifn,ipt,ipt
 endif
 xout a1 + acn
endop

; a1 ZConv asig,64,4,6,1