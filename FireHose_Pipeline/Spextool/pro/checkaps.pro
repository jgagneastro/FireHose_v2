function checkaps,edgecoeffs,xranges,slith_arc,appos,apradii,CANCEL=cancel

cancel = 0

s       = size(edgecoeffs)
norders = (s[0] eq 2) ? 1:s[3]

s       = size(appos)
naps    = (s[0] eq 1) ? 1:s[1]


for i = 0, norders-1 do begin

    x       = findgen(xranges[1,i]-xranges[0,i]+1)+xranges[0,i]
    botedge = poly(x,edgecoeffs[*,0,i])
    topedge = poly(x,edgecoeffs[*,1,i])

    dif       = floor(topedge-botedge)
    slith_pix = min(dif)

    psscale = slith_arc/slith_pix

    for j = 0,naps-2 do begin
        
        del_arc = abs((appos[j*2+1,i]-apradii[j])-(appos[j*2,i]+apradii[j]))
        
        if del_arc/psscale le 1.0 then begin

            cancel = 1
            return, cancel
          
        endif
        
    endfor

endfor



return, cancel






end
