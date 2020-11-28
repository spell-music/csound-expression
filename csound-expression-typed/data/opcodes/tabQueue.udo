opcode TabQueue_Append, 0, ii
itab, ival xin
ind = 0
ik tab_i ind, itab
tabw_i ival, (ik + 1), itab
tabw_i (ik + 1), ind, itab
endop

opcode TabQueue_Delete, 0,ii
itab, ival xin
ind = 0
iLast tab_i ind, itab
iCount = 1
iFound = 0
until iCount > iLast do
    iCur tab_i iCount, itab
    if (iCur == ival) then
        iFound = 1
        tabw_i (iLast - 1), ind, itab
    endif

    if (iFound == 1) then
        iNext tab_i (iCount + 1), itab
        tabw_i iNext, iCount, itab
    endif

    iCount = iCount + 1
od
endop

opcode TabQueue_HasElement, k, i
itab xin
knd = 0
kk tab knd, itab
kres = (kk == 0) ? 0 : 1
xout kres
endop

opcode TabQueue_ReadLastElement, k, i
itab xin
knd = 0
kk tab knd, itab
kres tab kk, itab
xout kres
endop
