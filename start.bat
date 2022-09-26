SET CURRENTDIR="%cd%"

docker run -it -v %CURRENTDIR%:/haskell haskell:9 bash