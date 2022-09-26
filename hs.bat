@echo off

docker run -it --rm -v %HASKELLPATH%:/hs haskell:9 bash