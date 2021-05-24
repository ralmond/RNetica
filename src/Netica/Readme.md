## Local Netica Installation Directory

The purpose of this directory is to provide code to download and
install the latest version of the Netica C API from the Norsys web
site.  The appropriate shared library (libnetica.so or netica.dll)
will be moved to ${R_HOME}/lib.  


Alternatively, one can download and install Netica API.  Because of
current Unix security restrictions, libnetica.so must copied to one of
/usr/lib, /usr/share/lib, and ${R_HOME}/lib.

## License Information

The Netica software is owned by Norsys, LLC (https://norsys.com).  By
downloading and installing this software, you agree to be bound by the
terms on the Norsys license, which can be found in the doc folder as
LicAgree.txt.  The Netica License Agreement does not apply to the
RNetica software, especially including both the contents of the `R`
directory and the C code in the `src` directory which is the parent of
this one.  Upon installation, this directory is populated with the
API library package from Norsys.  In particular, the contents of the
directory `Netica_API_nnn` is Norsys proprietary code.

The Netica API is available in two versions a free version and a paid
version.  The difference is a license code, which can be purchased
from Norsys.  The free version has limits on the size of the network
which can be constructed.  

The free version can be used for the following purposes:

* Installing and testing the RNetica software, including running the
  R check script.
  
* Educational purposes including learning the RNetica and Netica software.

* Exploring to see if Netica/RNetica is suitable for a particular
  application.
  
Persons or organizations interested in other applications should
contact Norsys for licenses.  
  
Note also, that there are two Netica licenses, one for the GUI and one
for the API.  RNetica requires the API license; a GUI-only license
will not work.  Networks generated with the GUI can be used in
RNetica, and visa verse, so many analysts will want both licenses.  

