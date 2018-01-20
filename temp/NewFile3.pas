{$PROCESSOR PIC12F629}

begin
  while TXSTA_TRMT = 0 do end;
  
end.
