@echo off
del /S /F /Q *.exe *.dll *.dcu *.ppu *.local *.identcache *.stat *.tds *.o *.compiled *.obj *.res *.*~ *.lps *.stat *.dbg
rmdir /S /Q win32 lib backup __recovery __history
cd myrivi
rmdir /S /Q win32 lib backup __recovery __history
cd ..
@echo on
pause
