# FireHose
A modified version of the Firehose IDL pipeline to reduce data obtained with the Folded-port InfraRed Echellette (FIRE) near-infrared spectrograph described here : http://web.mit.edu/~rsimcoe/www/FIRE/

Please note that I am not the original author of this reduction package, which was created by Robert Simcoe. I have simply made several changes and fixed some glitches to make the pipeline easier to use.

Erini Lambridges and the BDNYC group have helped me in this enterprise.
https://github.com/elambrid

The latest modifications to this pipeline were made with IDL 8.4. I tried to avoid using IDL 8-specific syntax, however it is possible that I forgot about some of them, which would cause error messages if used with IDL 7. Please contact me if this happens.

Please refer to /FireHose/1-Firehose/Documentation/Firehose_Installation_Instructions.txt for instructions to properly set up Firehose on your maching.

Then refer to /FireHose/1-Firehose/Documentation/ECHELLETTE_README.txt to reduce Echelle data with firehose.pro
or to /FireHose/1-Firehose/Documentation/PRISM_README.txt to reduce Prism data with firehose_ld.pro

As a first step to correctly setting up FIREHOSE, include the following lines in your bash profile :

XIDL_DIR="/Users/gagne/Documents/IDL/IDL_library/FireHose/XIDL/xidl/"
IDLSPEC2D_DIR="/Users/gagne/Documents/IDL/IDL_library/FireHose/XIDL/idlspec2d/"
IDLUTILS_DIR="/Users/gagne/Documents/IDL/IDL_library/FireHose/XIDL/idlutils/"
FIRE_DIR="/Users/gagne/Documents/IDL/IDL_library/FireHose/FireHose_Pipeline/"

You'll need to change these paths to your local paths pointing to these directories in the Firehose package. For Windows users, you can go into Firehose.pro around line ~1590, and un-comment the four calls to "setenv" in IDL. You will also need to update their paths accordingly. A similar operation is needed around line 522 of firehose_ld.pro
