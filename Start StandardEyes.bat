@echo off
REM ============================================================
REM  StandardEyes
REM  By Noyman DBE, Mimouni M, Safir M
REM
REM  Self-contained: the Python runtime and every R package the
REM  analysis needs are bundled in this folder. Nothing is
REM  installed and nothing is downloaded.
REM
REM  The launcher picks a FREE port automatically, so this app
REM  never clashes with another local app or a second copy.
REM
REM  The only external requirement is R (4.5.x). If it is not
REM  found, the app opens a Setup screen where you can point it
REM  at your R installation.
REM
REM  First start: Windows may show "Windows protected your PC"
REM  for a file that came from a downloaded ZIP. Click More info,
REM  then Run anyway. To avoid it, right-click the ZIP before
REM  extracting, choose Properties, tick Unblock, then extract.
REM ============================================================
setlocal
title StandardEyes - close this window to stop the app
cd /d "%~dp0"

set "PY=%~dp0runtime\python\python.exe"

REM ---------------------------------------------------------------
REM  Deliberately NO parenthesised if-blocks below this line.
REM  cmd.exe expands %VAR% while it PARSES a whole "if (...)" block,
REM  so a folder whose name contains ")" - e.g.
REM  "App - StandardEyes (v1)" - closes the block early and
REM  the launcher dies with "... was unexpected at this time.",
REM  even when the branch would never have run. Plain GOTO labels
REM  are immune, so this file works from ANY folder name.
REM ---------------------------------------------------------------
if not exist "%PY%" goto :nopython

REM Keep any other Python on this PC out of our bundled runtime:
REM -I (isolated mode) ignores every PYTHON* variable and the per-user
REM site-packages, so a "pip install --user" elsewhere cannot shadow
REM the bundled packages. launch.py puts its own folder on the path.
set "PYTHONPATH="
set "PYTHONHOME="

"%PY%" -I "%~dp0webapp\server\launch.py"
goto :stopped

:nopython
echo.
echo ERROR: the bundled Python runtime is missing.
echo Expected: "%PY%"
echo.
echo This usually means the folder was copied incompletely, or
echo extracted from the ZIP without keeping subfolders.
echo Please copy the whole folder again.
echo.
pause
exit /b 1

:stopped
echo.
echo The app has stopped.
pause
endlocal
