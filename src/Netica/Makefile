## The purpose of this makefile is to download and install the Netica
## API from the Norsys website, thus making its installation
## transparent to the user.

APIurlLinux = https://norsys.com/downloads/NeticaAPI_Linux_607.zip
APIurlWin = https://norsys.com/downloads/NeticaAPI_Win_510.zip
APIurlMac = https://norsys.com/downloads/NeticaAPI_Mac.zip
NeticaDir = Netica_API_504

## Assume we have either MacOS or Linux:  win will use Makefile.win
ifeq ($(shell uname -s),Darwin)
  APIurl = $(APIurlMac)
else
  APIurl = $(APIurlLinux)
  NeticaDir = Netica_API_607
endif


NeticaAPI:
	echo $(findstring darwin, $(OSTYPE))
	curl $(APIurl) --out NeticaAPI.zip
	unzip NeticaAPI.zip
	cp $(NeticaDir)/src/Netica.h .
	cp $(NeticaDir)/lib/libnetica.* .

