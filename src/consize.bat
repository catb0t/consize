@echo off
rem Remove 1st line to see setting of env vars when calling batch file
rem Ensure that java is on %PATH%
rem Set env var CLOJURE to dir of clojure installation
java -cp %CLOJURE%\*;*; clojure.main consize.clj %2 %3 %4 %5 %6 %7 %8 
