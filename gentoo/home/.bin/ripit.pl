#!/usr/bin/env perl
########################################################################
#
# LICENSE
#
# Copyright (C) 2005 Felix Suwald
#
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
#
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307,
# USA.
#
#
########################################################################
#
# CODE
#
# ripit.pl - Rips audio CD and encodes files, following steps can be
#            performed (unsorted list):
#            1) Query CDDB data for album/artist/track info
#            2) Rip the audio files from the CD
#            3) Encode the wav files
#            4) ID3 tag the encoded files
#            5) Extract possible hidden tracks
#            6) Optional: Create a playlist (M3U) file.
#            7) Optional: Prepares and sends a CDDB submission.
#            8) Optional: saves the CDDB file
#            9) Optional: creates a toc (cue) file to burn a CD in DAO
#               with text
#           10) Optional: analyze the wavs for gaps and splits them into
#               chunks and/or trim lead-in/out (experimental)
#           11) Optional: merges wavs for gapless encoding
#           12) Optional: normalizes the wavs before encoding.
#           13) Optional: adds coverart to tags of sound files and
#               copies albumcover to encoding directories.
#           14) Optional: calculates album gain for sound files.
#           15) Optional: creates a md5sum for each type of sound files.
#
#
# Version 3.9.0 - July 14th 2010 - Felix Suwald, thanks for input:
#                                  F. Sundermeyer
#                                  S. Noé
# Version 3.8.1 - November 18th 2009 - Felix Suwald
#                                      D. Mader
# Version 3.8.0 - September 28th 2009 - Felix Suwald
#
# Version 3.7.0 - May 6th 2009 - Felix Suwald, thanks for input:
#                                C. Blank
#                                A. Gillis
#                                and to all the bug-reporters!
# Version 3.6.0 - June 16th 2007 - Felix Suwald, thanks for input:
#                                  C. Blank
#                                  G. Edwards
#                                  G. Ross
# Version 3.5.0 - June 6th 2006 - Felix Suwald, credits to
#                                 E. Riesebieter (deb-package)
#                                 C. Walter (normalize option)
#                                 S. Warten (general support & loop)
#                                 S. Warten (signal handling)
# Version 3.4 - December 3rd 2005 - Felix Suwald, credits to
#                                   M. Bright (infolog file)
#                                   M. Kaesbauer (lcdproc) and
#                                   C. Walter (config file).
# Version 2.5 - November 13th 2004 - Felix Suwald
# Version 2.2 - October 18th 2003 - Mads Martin Joergensen
# Version 1.0 - February 16th 1999 - Simon Quinn
#
#
########################################################################
#
# User configurable variables:
# Keep these values and save your own settings in a config file with
# option --save!
#
my $cddev     = "/dev/cdrom";# Path of CD device.
my $scsi_cddev = "";         # Path of CD device for non audio commands.
my $outputdir = "";         # Where the sound should go to.
my $ripopt    = "";         # Options for audio CD ripper.
my $span      = "";         # Options for track spans.
my $ripper    = 1;          # 0 - dagrab, 1 - cdparanoia,
                            # 2 - cdda2wav, 3 - tosha, 4 - cdd.
my @coder     = (0);        # 0 - Lame, 1 - Oggenc, 2 - Flac,
                            # 3 - Faac, 4 - mp4als, 5 - Musepack,
                            # 6 - Wavpack, 7 - ffmpeg,
                            # comma separated list.
my $coverart  = 0;          # Add cover metadata, (1 yes, 0 no),
                            # comma separated list in same order as
                            # list of encoders.
my $coverpath = "";         # Path to cover to be added to sound files.
my $copycover = "";         # Path to album cover source.
my $bitrate   = 128;        # Bitrate for lame, if --vbrmode used,
                            # bitrate is equal to the -b option.
my $maxrate   = 0;          # Bitrate for lame using --vbrmode,
                            # maxrate is equal to the -B option.
my @quality   = (5,3,5,100,0,5);# Quality for lame in vbr mode (0-9), best
                            # quality = 0, quality for oggenc (1-10),
                            # best = 10; or compression level for Flac
                            # (0-8), lowest = 0, quality for Faac
                            # (10-500), best = 500, no values for
                            # Wavpack and ffmpeg.
my $qualame   = 5;          # Same as above, more intuitive. Use quotes
my $qualoggenc= 3;          # if values shall be comma separated lists.
my $quaflac   = 5;          #
my $quafaac   = 100;        #
my $quamp4als = 0;          #
my $quamuse   = 5;          #
my $lameopt   = "";         #
my $oggencopt = "";         #
my $flacopt   = "";         #
my $faacopt   = "";         #
my $mp4alsopt = "";         #
my $museopt   = "";         #
my $wavpacopt = "-y";       #
my $ffmpegopt = "";         #
my $ffmpegsuffix = "";      # The suffix of the encoder used
my $musenc    = "mpcenc";   # The default Musepack encoder.
my $mp3gain   = "";         # The mp3 album gain command with options.
my $vorbgain  = "";         # The vorbis album gain command with options.
my $flacgain  = "";         # The flac album gain command with options.
my $aacgain   = "";         # The aac album gain command with options.
my $mpcgain   = "";         # The mpc album gain command with options.
my $wvgain    = "";         # The wv album gain command with options.
my $lcd       = 0;          # Use lcdproc (1 yes, 0 no).
my $chars     = "XX";       # Exclude special chars in file names.
my $verbose   = 3;          # Normal output: 3, no output: 0, minimal
                            # output: 1, normal without encoder msgs: 2,
                            # normal: 3, verbose: 4, extremely verbose: 5
my $commentag = "";         # Comment ID3 tag.
my $genre     = "";         # Genre of Audio CD for ID3 tag.
my $year      = "";         # Year of Audio CD for ID3 tag.
my @mp3tags   = ();         # Add special mp3 tag.
my $utftag    = 1;          # Keep Lame-tags in utf or decode them to
                            # ISO8895-1 (1 yes, 0 no).
my $vatag     = 0;          # Detect VA style for tagging (1 yes, 0 no,
                            # 2 backward style (trackname / artist)).
my $vastring  = "\\bVA\\b|Variou*s|Various Artists";
my $eject     = 0;          # Eject the CD when done (1 yes, 0 no).
my $ejectcmd  = "eject";    # Command to use for eject
my $ejectopt  = "{cddev}";  # Options to above
my $quitnodb  = 0;          # Quit if no CDDB entry found (1 yes, 0 no).
my $overwrite = "n";        # Overwrite existing directory / rip
                            # (n no, y yes, e eject if directory exists)
my $halt      = 0;          # Shutdown machine when finished
                            # (1 yes, 0 no).
my $nice      = 0;          # Set nice for encoding process.
my $nicerip   = 0;          # Set nice for ripping process.
my $savenew   = 0;          # Saved passed parameters to new config
                            # file (1 yes, 0 no).
my $savepara  = 0;          # Save parameters passed in config file
                            # (1 yes, 0 no).
my $config    = 1;          # Use config file to read parameters
                            # (1 yes, 0 no).
my $confdir   = "";         # Full path to the users config file.
my $confname  = "config";   # File name of config file.
my $submission= 1;          # Send CDDB submission
                            # (1 yes, 0 no).
my $parano    = 1;          # Use paranoia mode in cdparanoia
                            # (1 yes, 0 no).
my $playlist  = 1;          # Do create the m3u playlist file
                            # (1 yes, 0 no, 2 no full path in filename).
my $book      = 0;          # Merge all tracks into a single file and
                            # write a chapter file (1 yes, 0 no).
my $resume    = 0;          # Resume a previously started session
                            # (1 yes, 0 no).
my $infolog   = "";         # InfoLog
                            # (filename)
my $interaction = 1;        # If 0 do not ask anything, take the 1st
                            # CDDB entry found or use default names!
                            # (1 yes, 0 no).
my $underscore = 0;         # Use _ instead of spaces in filenames
                            # (1 yes, 0 no).
my $lowercase = 0;          # Lowercase filenames
                            # (1 yes, 0 no).
my $uppercasefirst = 0;     # Uppercase first filenames
                            # (1 yes, 0 no).
my $archive   = 0;          # Save CDDB files in ~/.cddb dir
                            # (1 yes, 0 no).
my $mb        = 0;          # Use the musicbrainz DB instead of freedb
                            # (1 yes, 0 no).
my $mirror    = "freedb";   # The host (a freedb mirror) that
                            # shall be used instead of freedb.
my $transfer  = "cddb";     # Transfer mode, cddb or http, will set
                            # default port to 8880 or 80 (for http).
my $vbrmode   = "";         # Variable bitrate, only used with lame,
                            # (new or old), see lame-manpage.
my $proto     = 6;          # CDDB protocol level for CDDB query.
my $proxy     = "";         # Set proxy.
my $CDDB_HOST = "freedb.org"; # Set cddb host
my $mailad    = "";         # Users return e-mail address.
my @threads   = 1;          # Number of encoding processes for each box
my @sshlist   = ();         # List of remote machines.
my $scp       = 0;          # Use scp to access files (1 yes, 0 no).
my $local     = 1;          # Encode on locale machine (1 yes, 0 no).
my $wav       = 0;          # Keep the wav files (1 yes, 0 no).
my $encode    = 1;          # Encode the wav files (1 yes, 0 no).
my $rip       = 1;          # Rip the CD files (1 yes, 0 no).
my $cdcue     = 0;          # Create a cue-file for what? (1 yes, 0 no).
my $cdtoc     = 0;          # Create a cd.toc for CDRDAO (1 yes, 0 no).
my $inf       = 0;          # Create inf files for wodim (1 yes, 0 no).
my $loop      = 0;          # Restart ripit as soon as the previous CD
                            # is done. This option will force ejection!
                            # (1 yes, 0 no, 2 immediate restart after
                            # ripping, experimental, use with caution!).
my $ghost     = 0;          # Check and extract ghost songs from all
                            # tracks (1 yes, 0 no).
my $prepend   = 2.0;        # Extend ghost songs by 2 seconds at the
                            # beginning.
my $extend    = 2.0;        # Extend ghost songs by 2 seconds at the
                            # end.
my $dpermission = "0755";   # Directory permissions.
my $fpermission = "";       # Audio and text file permissions.
my $md5sum    = 0;          # Generate MD5 sums for every sound file
                            # not deleted (1 yes, 0 no).
my @suffix    = ();         # Array of suffixes according to coders.
my $execmd    = "";         # Execute a command when done.
my $precmd    = "";         # Execute a command before ripping.
my $multi     = 0;          # Not yet official. Do not use!
my $mbname    = "";         # Musicbrainz login name.
my $mbpass    = "";         # Musicbrainz password for ISRC submission.
my $isrc      = 0;          # Detect ISRC with icedax (1 yes, 0 no).
#
# New options step 1: Add global variables here above or below in case
# they shouldn't be user configurable.
#
#
#
# Directory and track template variables:
# Contains the format the track names will be written as.
# The '" and "' are important and must surround the template.
# Example variables to use are: $tracknum, $album, $artist, $genre,
# $trackname or $year.
# E.g. example setting of $tracktemplate produces a trackname of
# "07 The Game - Swandive" .
# $tracktemplate  = '"$tracknum $trackname - $artist"';
#
my @dirtemplate = '"$artist - $album"';
my $tracktemplate  = '"$tracknum $trackname"';
my $trackoffset = 0;
#
#
# LCDproc settings:
#
my $lcdhost   = "localhost";
my $lcdport   = "13666";
my $lcdline1  = "   [initializing]   ";
my $lcdline2  = " ripit-lcd-module     ";
my $lcdline3  = "          2005 ";
my $lcdoline1 = "";
my $lcdoline2 = "";
my $lcdoline3 = "";
my $lcdproc;
my $lcdtrackno       = 0;
#
#
#
# Normalize settings:
#
my $normalize = 0;           # normalize CD, needs 'normalize' in $path.
my $normcmd   = "normalize"; # This might differ for other distros.
my $normopt   = "-b";        # Additional options for normalize.
my $subdir    = "Unsorted";
#
#
########################################################################
#
# System variables, no user configurable variables below.
#
use Encode;                 # Needed for decode_utf8 calls.
use Fcntl;                  # Needed for sysopen calls.
use File::Copy;
use Getopt::Long qw(:config no_ignore_case);
use IO::Socket;
use strict;
#use warnings;
#
# Initialize paths.
#
my $homedir = "$ENV{HOME}";
my $workdir = "$ENV{PWD}";
my $usename = "$ENV{USER}";
# The hostname is not so important and not available on Ubuntu(s) (?).
my $hostnam = "";
if($ENV{HOSTNAME}) {
   $hostnam = "$ENV{HOSTNAME}";
}
elsif($ENV{HOST}) {
   $hostnam = "$ENV{HOST}";
}
my $charset = "";
if($ENV{G_FILENAME_ENCODING}) {
   $charset = "$ENV{G_FILENAME_ENCODING}";
}
else {
   $charset = "$ENV{LANG}";
}
if($charset =~ /UTF-8/) {
   $charset = "UTF-8";
}
elsif($charset  =~ /ISO-8859-15/) {
   $charset = "ISO-8859-15";
}
else {
   $charset = "ISO-8859-1";
}
#print ($_,$ENV{$_},"\n") foreach (keys %ENV);

#
# Initialize global variables.
#
my $version          = "3.9.0";
my $album_utf8       = "";
my $artist_utf8      = "";
my $distro           = "";  # Linux distribution
my $categ            = "";  # CDDB category
my $cddbid           = 0;   # Needed in several subroutines
my $lameflag         = 0;   # Flag to check if lame is used, some users
                            # are not aware that lame is needed for mp3!
my $oggflag          = 0;   # Flag to check if oggenc is used. Needed to
                            # load modules if coverart for ogg is used.
my $wvpflag          = 0;   # Flag to check wavpack version and its
                            # coverart support.
my $trackselection   = "";  # Passed from command line
my @tracksel         = ();  # Array of all track numbers, including
                            # those not explicitly stated.
my @seltrack         = ();  # Array of all track numbers, including
                            # those not explicitly stated and ghost
                            # songs found by ripper needed by encoder.
my @framelist        = ();  # Needed in several subroutines
my @secondlist       = ();  # Needed in several subroutines
my @tracklist        = ();  # Needed in several subroutines
my @tracktags        = ();  # Needed in several subroutines
my %cd               = ();  # HoH of all CD-data.
my $cddbsubmission   = 2;   # If zero then data for CDDB submission is
                            # incorrect, if 1: submission OK, if 2: CDDB
                            # entry not changed (edited)
my $wpreset          = "";  # Preset written into config file.
my $wcoder           = "";  # Use a comma separated string to write the
                            # coder-array into the config file.
my $wthreads         = "";  # Use a comma separated string to write the
                            # threads-array into the config file.
my $wsshlist         = "";  # As above for the list of remote machines.
my $sshflag          = 0;   # Ssh encoding OK if sshflag == 1.
my %sshlist          = ();  # Hash of remote machines.
my $hiddenflag       = 0;
my $logfile          = "";  # Used with not *to-use* option --multi.
my $help             = 0;   # Print help and exit if 1.
my $printver         = 0;   # Print version and exit if 1.
my @delname          = ();  # List of tracknames being processed, i.e.
                            # ready for deletion.
my @skip             = ();  # List of merged tracks.
my @globopt          = ();  # All encoder options sorted according to
                            # encoder.
my @sepdir           = ();  # Array of sound directories sorted
                            # according to encoder.
my $wavdir           = "";  # (Default) directory for sound.
my $limit_flag       = 0;   # Directory and file length flag.
my $va_flag          = 0;   # VA style flag.
my $va_delim         = "/"; # VA style delimiter.
my @isrcs            = ();  # ISRC array.
my @idata            = ();  # Array for the MB track IDs.
#
# New options step 2: Add global variables here in case they shouldn't
# be user configurable; additional modules can be added right below.
#
#
# Initialize subroutines without ().
#
sub ask_subm;
sub check_bitrate;
sub check_cddev;
sub check_chars;
sub check_cover;
sub check_vbrmode;
sub choose_genre;
sub copy_cover;
sub disp_info;
sub extract_comm;
sub get_rev;
sub get_isrcs;
sub init_mod;
sub init_var;
sub lame_preset;
sub main_sub;
sub skip_tracks;
sub write_cddb;
sub merge_wav;
sub write_wavhead;
#
# New options step 3: Do not forget to initialize new subroutines.
#
#
# Define the variables which catch the command line input.
# The p stands for passed (from command line).
my (
   $parchive,       $pbitrate,       $pmaxrate,         $PCDDB_HOST,
   $pcddev,         $pcdtoc,         @pcoder,           $pcommentag,
   $pconfig,        @pdirtemplate,   $ptracktemplate,   $peject,
   $pencode,        $pfaacopt,       $pflacopt,         $plameopt,
   $poggencopt,     $pgenre,         $phalt,            $pinfolog,
   $pinteraction,   $plcdhost,       $plcdport,         $plcd,
   $plocal,         $ploop,          $plowercase,       $pmirror,
   $pmailad,        $pmulti,         $pnice,            $pnormalize,
   $pnormopt,       $poutputdir,     $pparano,          $pplaylist,
   $ppreset,        $pproto,         $pproxy,           @pquality,
   $pripopt,        $prip,           $pripper,          $psavenew,
   $psavepara,      $pscp,           @psshlist,         $psubdir,
   $psubmission,    $ptransfer,      $punderscore,      $putftag,
   $pvbrmode,       $pverbose,       $pwav,             $pyear,
   $presume,        $pmerge,         $pghost,           $pprepend,
   $pextend,        $pejectopt,      $pejectcmd,        $pdpermission,
   $pfpermission,   $pmd5sum,        $pnicerip,         @pthreads,
   $pnormcmd,       $pmb,            $puppercasefirst,  $pexecmd,
   $pspan,          $poverwrite,     $pquitnodb,        $pbook,
   $pmusenc,        $pmp4alsopt,     $pmuseopt,         $pinf,
   $pscsi_cddev,    $pwavpacopt,     $pffmpegopt,       $pffmpegsuffix,
   $pprecmd,        $pcoverart,      $pcoverpath,       $pcdcue,
   $pvatag,         $pvastring,      $pmp3gain,         $pvorbgain,
   $pflacgain,      $paacgain,       $pmpcgain,         $pwvgain,
   @pmp3tags,       $pcopycover,     $ptrackoffset,     $pmbname,
   $pmbpass,        $pisrc,
);
#
# New options step 4: For distinction of variables passed on the command
# line and other from the configuration file, introduce for each new
# option the variable name prefixed with 'p'; 'p' stands for passed.
#
#
########################################################################
#
# Get the parameters from the command line.
#
# available:             E F     jJkK           Q               Y
# already used: aAbBcCdDe f gGhiI    lLmMnNoOpPq rRsStTuUvVwWxXy zZ
#
GetOptions(
   "archive|a!"             => \$parchive,
   "bitrate|b=s"            => \$pbitrate,
   "book|A=i"               => \$pbook,
   "maxrate|B=i"            => \$pmaxrate,
   "chars|W:s"              => \$chars,
   "cddbserver|C=s"         => \$PCDDB_HOST,
   "cdcue=i"                => \$pcdcue,
   "cdtoc=i"                => \$pcdtoc,
   "config!"                => \$pconfig,
   "coder|c=s"              => \@pcoder,
   "coverart=s"             => \$pcoverart,
   "coverpath=s"            => \$pcoverpath,
   "copycover=s"            => \$pcopycover,
   "comment=s"              => \$pcommentag,
   "threads=s"              => \@pthreads,
   "device|d=s"             => \$pcddev,
   "dirtemplate|D=s"        => \@pdirtemplate,
   "eject|e!"               => \$peject,
   "ejectcmd=s"             => \$pejectcmd,
   "ejectopt=s"             => \$pejectopt,
   "encode!"                => \$pencode,
   "execmd|X=s"             => \$pexecmd,
   "extend=f"               => \$pextend,
   "faacopt=s"              => \$pfaacopt,
   "flacopt=s"              => \$pflacopt,
   "lameopt=s"              => \$plameopt,
   "oggencopt=s"            => \$poggencopt,
   "mp4alsopt=s"            => \$pmp4alsopt,
   "musenc=s"               => \$pmusenc,
   "museopt=s"              => \$pmuseopt,
   "wavpacopt=s"            => \$pwavpacopt,
   "ffmpegopt=s"            => \$pffmpegopt,
   "ffmpegsuffix=s"         => \$pffmpegsuffix,
   "mp3gain=s"              => \$pmp3gain,
   "vorbgain=s"             => \$pvorbgain,
   "flacgain=s"             => \$pflacgain,
   "aacgain=s"              => \$paacgain,
   "mpcgain=s"              => \$pmpcgain,
   "wvgain=s"               => \$pwvgain,
   "genre|g=s"              => \$pgenre,
   "ghost|G!"               => \$pghost,
   "halt"                   => \$phalt,
   "help|h"                 => \$help,
   "inf=i"                  => \$pinf,
   "infolog=s"              => \$pinfolog,
   "interaction|i!"         => \$pinteraction,
   "isrc=i"                 => \$pisrc,
   "lcd!"                   => \$plcd,
   "lcdhost=s"              => \$plcdhost,
   "lcdport=s"              => \$plcdport,
   "lowercase|l!"           => \$plowercase,
   "uppercasefirst!"        => \$puppercasefirst,
   "local!"                 => \$plocal,
   "loop=i"                 => \$ploop,
   "mb!"                    => \$pmb,
   "md5sum!"                => \$pmd5sum,
   "merge=s"                => \$pmerge,
   "mirror|m=s"             => \$pmirror,
   "mail|M=s"               => \$pmailad,
   "mbname=s"               => \$pmbname,
   "mbpass=s"               => \$pmbpass,
   "mp3tags=s"              => \@pmp3tags,
   "multi"                  => \$pmulti,
   "nice|n=s"               => \$pnice,
   "nicerip=s"              => \$pnicerip,
   "normalize|N!"           => \$pnormalize,
   "normcmd=s"              => \$pnormcmd,
   "normopt|z=s"            => \$pnormopt,
   "subdir=s"               => \$psubdir,
   "outputdir|o=s"          => \$poutputdir,
   "overwrite|O=s"          => \$poverwrite,
   "dpermission=s"          => \$pdpermission,
   "fpermission=s"          => \$pfpermission,
   "playlist|p:s"           => \$pplaylist,
   "precmd=s"               => \$pprecmd,
   "prepend=f"              => \$pprepend,
   "preset|S=s"             => \$ppreset,
   "proxy|P=s"              => \$pproxy,
   "protocol|L=i"           => \$pproto,
   "quality|q=s"            => \@pquality,
   "quitnodb=i"             => \$pquitnodb,
   "resume|R"               => \$presume,
   "rip!"                   => \$prip,
   "ripper|r=s"             => \$pripper,
   "ripopt=s"               => \$pripopt,
   "savenew"                => \$psavenew,
   "save"                   => \$psavepara,
   "scp"                    => \$pscp,
   "scsidevice=s"           => \$pscsi_cddev,
   "sshlist=s"              => \@psshlist,
   "span|I=s"               => \$pspan,
   "submission|s!"          => \$psubmission,
   "tracktemplate|T=s"      => \$ptracktemplate,
   "trackoffset=i"          => \$ptrackoffset,
   "transfer|t=s"           => \$ptransfer,
   "underscore|u!"          => \$punderscore,
   "utftag|U!"              => \$putftag,
   "vatag=i"                => \$pvatag,
   "vastring=s"             => \$pvastring,
   "vbrmode|v=s"            => \$pvbrmode,
   "verbose|x=i"            => \$pverbose,
   "version|V"              => \$printver,
   "year|y=i"               => \$pyear,
   "wav|w!"                 => \$pwav,
   "disable-paranoia|Z:i"   => \$pparano,
)
or exit print_usage();
#
# New options step 5: Add the command line option here.
#
#
########################################################################
#
# Evaluate the command line parameters if passed. We need to do it this
# way, because passed options have to be saved (in case user wants to
# save them in the config file) before config file is read to prevent
# overriding passed options with options from config file. The passed
# options shall be stronger than the config file options!
# Problems arise with options that can be zero. Because a usual if-test
# can not distinguish between zero or undef, use the defined-test!
#
# New options step 6: force use of command line options if passed.
#
#
# First for the normal options, e. g. options which are never zero.
#
# The check of array @coder will be done in the subroutine!
$faacopt = $pfaacopt if($pfaacopt);
$flacopt = $pflacopt if($pflacopt);
$lameopt = $plameopt if($plameopt);
$oggencopt = $poggencopt if($poggencopt);
$mp4alsopt = $pmp4alsopt if($pmp4alsopt);
$musenc = $pmusenc if($pmusenc);
$museopt = $pmuseopt if($pmuseopt);
$wavpacopt = $pwavpacopt if($pwavpacopt);
$ffmpegopt = $pffmpegopt if($pffmpegopt);
$ffmpegsuffix = $pffmpegsuffix if($pffmpegsuffix);
$oggencopt = " " unless($oggencopt); # Oops, only to prevent warnings...
$mp3gain = $pmp3gain if($pmp3gain);
$vorbgain = $pvorbgain if($pvorbgain);
$flacgain = $pflacgain if($pflacgain);
$aacgain = $paacgain if($paacgain);
$mpcgain = $pmpcgain if($pmpcgain);
$wvgain = $pwvgain if($pwvgain);
$CDDB_HOST = $PCDDB_HOST if($PCDDB_HOST);
$cddev = $pcddev if($pcddev);
$overwrite = $poverwrite if($poverwrite);
#
# Get the default "no-argument" values.
# Note, that subroutine clean_all already purges ;><" and \015.
$chars = "NTFS" if($chars eq "");
$chars = "" if($chars eq "off");
$commentag = $pcommentag if($pcommentag);
$copycover = $pcopycover if($pcopycover);
@dirtemplate = @pdirtemplate if($pdirtemplate[0]);
$tracktemplate = $ptracktemplate if($ptracktemplate);
$execmd = $pexecmd if($pexecmd);
$precmd = $pprecmd if($pprecmd);
$halt = $phalt if($phalt);
$infolog = $pinfolog if($pinfolog);
$lcdhost = $plcdhost if($plcdhost);
$lcdport = $plcdport if($plcdport);
$mailad = $pmailad if($pmailad);
$mbname = $pmbname if($pmbname);
$mbpass = $pmbpass if($pmbpass);
@mp3tags = @pmp3tags if($pmp3tags[0]);
$mirror = $pmirror if($pmirror);
$normcmd = $pnormcmd if($pnormcmd);
$normopt = $pnormopt if($pnormopt);
$outputdir = $poutputdir if($poutputdir);
my $preset = $ppreset if($ppreset);
$ripopt = $pripopt if($pripopt);
$dpermission = $pdpermission if($pdpermission);
$fpermission = $pfpermission if($pfpermission);
$proto = $pproto if($pproto);
$proxy = $pproxy if($pproxy);
# Check for variable $psshlist will be done in the subroutine!
# Check for variable $pthreads will be done in the subroutine!
$transfer = $ptransfer if($ptransfer);
$vbrmode = $pvbrmode if($pvbrmode);
$year = $pyear if($pyear);
#
# Options which might be zero.
$bitrate = $pbitrate if($pbitrate);
$book = $pbook if($pbook);
$cdcue = $pcdcue if defined $pcdcue;
$cdtoc = $pcdtoc if defined $pcdtoc;
$coverart = $pcoverart if defined $pcoverart;
$extend = $pextend if defined $pextend;
$genre = $pgenre if defined $pgenre;
$inf = $pinf if defined $pinf;
$isrc = $pisrc if defined $pisrc;
$loop = $ploop if defined $ploop;
$md5sum = $pmd5sum if defined $pmd5sum;
$maxrate = $pmaxrate if defined $pmaxrate;
$nice = $pnice if defined $pnice;
$nicerip = $pnicerip if defined $pnicerip;
$parano = $pparano if defined $pparano;
$playlist = $pplaylist if defined $pplaylist;
$playlist = 1 if($playlist eq "");
$prepend = $pprepend if defined $pprepend;
$quitnodb = $pquitnodb if defined $pquitnodb;
$resume = $presume if defined $presume;
$ripper = $pripper if defined $pripper;
$savepara = $psavepara if defined $psavepara;
$savenew = $psavenew if defined $psavenew;
$scp = $pscp if defined $pscp;
if(defined $pscsi_cddev) {
   $scsi_cddev = $pscsi_cddev;
}
else {
   $scsi_cddev = $pcddev if($pcddev);
}
$span = $pspan if defined $pspan;
$trackoffset = $ptrackoffset if defined $ptrackoffset;
$verbose = $pverbose if defined $pverbose;
$vatag = $pvatag if defined $pvatag;
#
# And for the negatable options.
$archive = $parchive if defined $parchive;
$config = $pconfig if defined $pconfig;
$encode = $pencode if defined $pencode;
$eject = $peject if defined $peject;
$ejectcmd = $pejectcmd if defined $pejectcmd;
$ejectopt = $pejectopt if defined $pejectopt;
$ghost = $pghost if defined $pghost;
$interaction = $pinteraction if defined $pinteraction;
$lcd = $plcd if defined $plcd;
$local = $plocal if defined $plocal;
$lowercase = $plowercase if defined $plowercase;
$mb = $pmb if defined $pmb;
$uppercasefirst = $puppercasefirst if defined $puppercasefirst;
$multi = $pmulti if defined $pmulti;
$normalize = $pnormalize if defined $pnormalize;
$rip = $prip if defined $prip;
$submission = $psubmission if defined $psubmission;
$underscore = $punderscore if defined $punderscore;
$utftag = $putftag if defined $putftag;
$wav = $pwav if defined $pwav;
#
########################################################################
#
# Preliminary start: print version, read (and write) config file.
#
# To have the version printed first of all other (warning-) messages,
# find out if verbosity is set off or not, either by command line or
# by config file.
#
my $ripdir = $confdir . "/" . $confname if($confdir ne "");
# Fallback:
$ripdir = $homedir . "/.ripit/config" unless(-r "$ripdir");
$ripdir = "/etc/ripit/config" unless(-r "$ripdir");
if(-r "$ripdir" && $config == 1) {
   open(CONF, "$ripdir") || print "Can not read config file!\n";
   my @conflines = <CONF>;
   close(CONF);
   chomp($verbose = join('', grep(s/^verbose=//, @conflines)))
      unless($pverbose);
   chomp($infolog = join('', grep(s/^infolog=//, @conflines)))
      unless($pinfolog);
}
#
print "\n\n\nRipIT version $version.\n" if($verbose >= 1);
# Preliminary creation of the infolog path.
# No log_system call here because this would try to write to the infolog
# file not yet created.
if($infolog) {
   my($log_path, $filename) = $infolog =~ m/(.*\/)(.*)$/;
   system("mkdir -m 0755 -p \"$log_path\"") and
   print "Can not create directory \"$log_path\": $!\n";
}
log_info("RipIT version $version infolog-file.\n");
#
#
# Do some checks before writing a new config file (if wanted):
#
# First check if arguments of option merge are OK.
my @dummy = skip_tracks if($pmerge);
#
# Then the options that will be written to the config file.
if($help ne 1 && $printver ne 1) {
   check_coder();           # Check encoder array.
   check_quality();         # Check if quality is valid.
   check_proto();           # Check if protocol level is valid.
   check_sshlist();         # Check list of remote machines.
   check_preset() if($preset);     # Check preset settings.
#
# To be able to save a new config file we have to write it before
# reading the parameters not yet passed from the config file.
#
   $chars = "" if($chars eq "XX" && ($savenew == 1 || $savepara == 1));
   if($savenew == 1) {
      $verbose = 3; # Set back to default value.
      $infolog = ""; # Set back to default value.
      save_config();
      print "Saved a new config file!\n\n" if($verbose >= 3);
   }
#
# Read the config file.
#
   read_config() if($config == 1);
   check_enc("lame", "mp3");
#   check_enc("faac", "m4a");
#
# Check if the necessary modules are installed properly.
#
   init_mod;
#
#
# Security check for new options: give them default value if empty.
# This can happen, if the config file is not yet up-to date.
# This will go away again in version 4.0. This is also done to prevent
# warnings. And to avoid problems when updating from versions like 3.3.
#
# New options step 7: not mandatory, might be useful.
#
   $copycover = "" unless($copycover);
   $uppercasefirst = 0 unless($uppercasefirst);
   $mb = 0 unless($mb);
   $execmd = "" unless($execmd);
   $precmd = "" unless($precmd);
   $overwrite = "n" unless($overwrite);
   $quitnodb = 0 unless($quitnodb);
   $book = 0 unless($book);
   $cdcue = 0 unless($cdcue);
   $inf = 0 unless($inf);
   $isrc = 0 unless($isrc);
   $resume = 0 unless($resume);
   $musenc = "mpcenc" unless($musenc);
   $quamp4als = 0 unless($quamp4als);
   $quamuse = 5 unless($quamuse);
   $trackoffset = 0 unless($trackoffset);
   $vatag = 0 unless($vatag);
#
# Save the config file.
#
   save_config() if($savepara == 1);
   print "Updated the config file!\n\n"
      if($verbose >= 3 && $savepara == 1);
#
# It might be a good to x-check settings from config file because they
# can be edited manually.
#
   check_coder();           # Check it again for lame cbr vs vbr.
   check_quality();         # Check it again if quality is valid.
   check_sshlist();         # Check it again to create the hash.
   check_options();         # Sort the options according to the encoder.
   check_distro();          # Check for the distribution used.
}
#
########################################################################
#
# MAIN ROUTINE
#
########################################################################
#
if($printver) {
   print "\n";
   exit 2;
}

if($verbose >= 2) {
   print "Process summary:\n", "-" x 16, "\n" unless($help == 1);
}

if($help == 1) {
   print "\nThis is a shorten man page. Refer to the full manpage ",
         "for more details.\n";
   print_help();
   exit 3;
}

if(!$pcddev) {                 # Check CD dev if none defined.
   check_cddev;
}
else {                         # Close the tray.
   my $closeopt = $cddev if($ejectopt eq '{cddev}');
   $closeopt = "-t " . $closeopt if($ejectcmd =~ /^eject$/);
   $closeopt = $closeopt . " close" if($ejectcmd =~ /cdcontrol/);
   log_system("$ejectcmd $closeopt > /dev/null 2>&1");
}

if($scsi_cddev eq "") {
   $scsi_cddev = $cddev;
}

if($chars) {
   check_chars;
}

if($lcd == 1) {
   init_lcd();
}

if($outputdir eq "") {
   $outputdir = $homedir;
   chomp $outputdir;
}

if($outputdir =~ /^\.\//) {
   $outputdir =~ s/^\./$workdir/;
}
elsif($outputdir =~ /^\.\s*$/) {
   $outputdir =~ s/^\./$workdir/;
}

if($outputdir =~ /^~\//) {
   $outputdir =~ s/^~/$homedir/;
}

if($outputdir =~ /^\$HOME/) {
   $outputdir =~ s/^\$HOME/$homedir/;
}

# New options step 8: Add a message about selected options if needed.

if(length($year) > 0 && length($year) != 4 ) {
   print STDERR "Warning: year should be in 4 digits - $year.\n"
      if($verbose >= 1);
}

if($pdpermission && $verbose >= 2) {
   # Print this message only, if a dpermission has been passed on CL.
   $dpermission = sprintf("%04d", $dpermission);
   print "Directory permission will be set to $dpermission.\n";
}

if($fpermission && $verbose >= 2) {
   $fpermission = sprintf("%04d", $fpermission);
   print "File permission will be set to $fpermission.\n";
}

if($resume == 1 && $verbose >= 2) {
   print "Resuming previous session.\n";
}

if($span && $verbose >= 2) {
   print "Partial wav files will be ripped.\n" unless($span =~ /\d-$/);
}

if($wav == 1 && $verbose >= 2) {
   print "Wav files will not be deleted.\n";
}

if($normalize == 1 && $verbose >= 2) {
   print "Normalizeing the CD-tracks.\n";
}

if($book >= 1 && $verbose >= 2) {
   print "All tracks will be merged into one file and a chapter file written.\n";
   $pmerge = "1-";
   $ghost = 0;
}

if($cdcue > 0 && $verbose >= 2) {
   print "All tracks will be merged into one file and a cue-file written.\n";
   $pmerge = "1-" if($cdcue == 2);
   $ghost = 0;
   $ghost = 1 if($cdcue == 1);
}

if($cdtoc >= 1 && $verbose >= 2) {
   print "A toc file will be written.\n";
}

if($inf >= 1 && $verbose >= 2) {
   print "Inf files will be written for each track.\n";
}

if($ghost == 1) {
   print "Tracks will be analyzed for ghost songs.\n" if($verbose >= 2);
}

if($utftag == 0 && $verbose >= 2 && "@coder" =~ /0/) {
   print "Lame-tags will be encoded to ISO8859-1.\n";
}

if($mp3tags[0] && $verbose >= 2 ) {
   print "Special track tags will be added to mp3 files.\n";
}

if($vatag > 0 && $verbose >= 2 ) {
   print "Track tags will be analyzed for VA style.\n";
}

if($playlist >= 1 && $verbose >= 2) {
   print "Playlist (m3u) file will be written.\n";
}

if($md5sum == 1 && $verbose >= 2) {
   print "MD5SUMs of sound files will be calculated.\n";
}

if($copycover && $verbose >= 2) {
   print "Copying the cover to encoder directories.\n";
}

if($coverart =~ /1/ && $verbose >= 2) {
   print "Adding coverart to sound files.\n";
}

if(($mp3gain || $vorbgain || $flacgain || $aacgain || $mpcgain || $wvgain)
   && $encode == 1 && $verbose >= 2) {
   print "Adding album gain tags to sound files.\n";
}

if($parano >= 3 && $verbose >= 2 ) {
   print "Warning: paranoia argument unknown, will use paranoia.\n";
   $parano = 1;
}

if($halt == 1 && $verbose >= 2) {
   print "Halting machine when finished.\n";
}

if($eject == 1 || $loop >= 1) {
   print "CD will be ejected when finished.\n" if($verbose >= 2);
   $ejectcmd = "eject -v" if($ejectcmd =~ /eject/ && $verbose >= 4);
}

if($loop >= 1) {
   print "Endless looping and ejection of each CD.\n" if($verbose >= 2);
   print "\n" if($verbose >= 2);
   while($loop >= 1) {
      main_sub;
      last if($loop == 0);
      init_var;
      print "Please insert a new CD!\n" if($verbose >= 1);
      while( not cd_present() ) {
         sleep(6);
      }
   }
}
else {
   print "\n" if($verbose >= 2);
   main_sub;
}
exit;
#
########################################################################
#
# Main subroutine.
#
########################################################################
#
sub main_sub {
   if(@ARGV) {
      $trackselection = $ARGV[0];
   }

   if($bitrate ne "off" && $lameflag == 1) {
      check_bitrate;
   }

   if($vbrmode ne "" && $lameflag == 1) {
      check_vbrmode;
   }

   if($preset) {
      lame_preset;
   }

   unless( cd_present() ) {
      print "\nPlease insert an audio CD!\n" if($verbose > 0);
      while( not cd_present() ) {
         check_cddev;
         sleep(12);
      }
   }
   if($archive == 1 && $multi == 0) {
      get_arch()
   }
   else {
      get_cdinfo() if($mb == 0);
      get_mb() if($mb == 1);
   }
   disp_info;
   create_seltrack($trackselection);
   ask_subm();
   my $answer = create_dirs();

   if($answer eq "go") {
      if($precmd) {
         $precmd =~ s/\$/\\\$/g;
         print "Will execute command \"$precmd\".\n" if($verbose >= 3);
         log_system("$precmd");
      }
      if(-f "$copycover" && -s "$copycover") {
         copy_cover;
      }
      elsif($copycover ne "") {
         print "\nAlbum cover with path $copycover not found.\n"
         if($verbose > 2);
         if($interaction == 1) {
            check_cover;
            copy_cover if(-f "$copycover" && -s "$copycover");
         }
      }
      if($normalize == 1 or $cdcue > 0) {
         rip_cd();
         norm_cd() if($normalize == 1);
         enc_cd();
      }
      else {
         rip_cd();
      }
   }
   elsif($answer eq "next") {
      print "\nRelease $cd{artist} - $cd{title} already done, ",
            "giving up.\n" if($verbose > 0);
      log_info("Directory already present, giving up.");
      log_info("*" x 72, "\n");
   }
   elsif($answer eq "unknown") {
      print "\nRelease unknown, giving up.\n" if($verbose > 0);
      log_info("Release unknown, giving up.");
      log_info("*" x 72, "\n");
   }

   if($eject == 1 or $loop >= 1
                  or $overwrite eq "e" and $answer eq "next") {
      my $ejectopt = $cddev if($ejectopt eq '{cddev}');
      $ejectopt = $ejectopt . " eject" if($ejectcmd =~ /cdcontrol/);
      log_system("$ejectcmd $ejectopt");
   }

   return if($answer eq "next" or $answer eq "unknown");

   if($loop == 2) {

      my $pid = fork();
      if (not defined $pid) {
         print "\nResources not avilable, will quit.\n";
      }
      elsif($pid != 0) {
         finish_process($pid);
      }
      else {
         # Child: restart process.
         return;
         # Problem: being recursive, we won't come back! Hello zombie.
         exit(0);
      }
   }
   else {
      finish_process();
   }
   $loop = 0 if($loop == 2);
   return;
}
#
########################################################################
#
# SUBROUTINES
#
########################################################################
#
# New options step 9: Add new code as a subroutine somewhere below,
# the very end might be appropriate.
#
########################################################################
#
# Check local .cddb directory for cddb files with album, artist, discID
# and track titles.
#
sub get_arch {
   # Get cddbid and number of tracks of CD.
   my $trackno;
   ($cddbid, $trackno) = get_cddbid();

   my ($artist, $album);
   my @comment = ();

   if($pgenre) {
      $genre = $pgenre;
   }
   else {
      $genre = "";
   }
   if($pyear) {
      $year = $pyear;
   }
   else {
      $year = "";
   }

   my $usearch = "x";
   my @categs = ();


   print "\nChecking for a local DB entry, please wait...\n\n"
      if($verbose > 1);
   log_system("mkdir -m 0755 -p $homedir/.cddb")
      or print "Can not create directory $homedir/.cddb: $!\n";
   opendir(CDDB, "$homedir/.cddb/")
      or print "Can not read in $homedir/.cddb: $!\n";
   @categs = grep(/\w/i, readdir(CDDB));
   close(CDDB);
   my @cddbid = ();
   foreach (@categs) {
      if(-d "$homedir/.cddb/$_") {
         opendir(CATEG, "$homedir/.cddb/$_")
            or print "Can not read in $homedir/.cddb: $!\n";
         my @entries = grep(/$cddbid$/, readdir(CATEG));
         close(CATEG);
         push @cddbid, $_ if($entries[0]);
      }
      elsif(-f "$homedir/.cddb/$_" && -s "$homedir/.cddb/$_") {
         push @cddbid, $_ if($_ =~ /$cddbid/);
      }
   }
   my $count = 1;
   my @dirflag = ();
   if($cddbid[0]) {
      print "Found local entry $cddbid in $homedir/.cddb !\n"
         if($interaction == 1);
      print "This CD could be:\n\n" if($interaction == 1);
      foreach (@cddbid) {
         my $openflag = "no";
         if(-s "$homedir/.cddb/$_/$cddbid") {
            open(LOG, "$homedir/.cddb/$_/$cddbid");
            $openflag = "ok";
            $dirflag[$count-1] = 1;
         }
         elsif(-s "$homedir/.cddb/$cddbid") {
            open(LOG, "$homedir/.cddb/$cddbid");
            $_ = "no category found!";
            $openflag = "ok";
            $dirflag[$count-1] = 0;
         }
         if($openflag eq "ok") {
            my @loglines = <LOG>;
            close(LOG);
            # Here we should test if @loglines is a good entry!
            # If there are empty files, we get warnings!
            chomp(my $artist = join(' ', grep(s/^DTITLE=//, @loglines)));
            $artist = clean_all($artist);
            chomp(my $agenre = join(' ', grep(s/^DGENRE=//, @loglines)));
            $agenre =~ s/[\015]//g;
            $agenre = "none" unless($agenre);
            print "$count: $artist (genre: $agenre, category: $_)\n"
               if($interaction == 1);
            $count++;
            $agenre = "";
         }
      }
      print "\n0: Search online DB instead.\n"
         if($interaction == 1);
      if($interaction == 0) {
         $usearch = 1;
      }
      else {
         while($usearch !~ /\d/ || $usearch >= $count) {
            print "\nChoose: (1) ";
            $usearch = <STDIN>;
            chomp $usearch;
            $usearch = 1 if($usearch eq "");
            print "\n";
         }
      }
   }
   else {
      get_cdinfo() if($mb == 0);
      get_mb() if($mb == 1);
      return;
   }

   if($usearch != 0) {
      # We use "musicbrainz" as key when reading entry if coming from
      # MB section (below). So if we have a ~/.cddb/musicbrainz
      # directory this will give problems, use word "archive" for the
      # category name instead.
      my $ctg = $cddbid[$usearch-1];
      $ctg = "archive" if($ctg =~ /musicbrainz/);
      if($dirflag[$usearch-1] == 1) {
         read_entry("$homedir/.cddb/$cddbid[$usearch-1]/$cddbid",
            $ctg, $trackno);
      }
      elsif($dirflag[$usearch-1] == 0) {
         read_entry("$homedir/.cddb/$cddbid", $ctg, $trackno);
      }
      $categ = $cd{cat} = $cddbid[$usearch-1];
   }
   else {
      get_cdinfo() if($mb == 0);
      get_mb() if($mb == 1);
      return;
   }

   if($mb == 1) {
      open(DISCID, "discid $scsi_cddev|");
      my @response = <DISCID>;
      close(DISCID);
      chomp($cd{discid} = join("", grep(s/^DiscID\s*:\s//, @response)));
      # Fill up the track->id array in case we want to submit ISRCs, but
      # only if ripit found one single match.
      if($isrc == 1) {
         my $ws = WebService::MusicBrainz::Release->new();
         my $result = $ws->search({ DISCID => $cd{discid} });
         my @releases = @{$result->release_list->releases};
         if(scalar(@releases) == 1) {
            my $release = $releases[0];
            my @tracks = @{$release->track_list->tracks};
            for(my $i = 0; $i < scalar(@tracks); $i++) {
               my $track = $tracks[$i];
               push(@idata, $track->id);
            }
         }
      }
   }
}
########################################################################
#
# Read the album, artist, discID and track titles from the get_CDDB()
# generated TOC file.
#
sub get_cdinfo {
   my $writecddb = shift; # Passed when calling this sub from get_mb.
   $writecddb = 0 unless($writecddb);

   # Get cddbid and number of tracks of CD.
   my $trackno;
   ($cddbid, $trackno) = get_cddbid();

   my ($artist, $album, %config, $revision);
   my ($CDDB_INPUT, $CDDB_MODE, $CDDB_PORT);
   my @comment = ();

   if($pgenre) {
      $genre = $pgenre;
   }
   else {
      $genre = "";
   }
   if($pyear) {
      $year = $pyear;
   }
   else {
      $year = "";
   }


   #Configure CDDB_get parameters
   if($CDDB_HOST eq "freedb2.org") {
      $config{CDDB_HOST} = $CDDB_HOST;
   }
   elsif($CDDB_HOST eq "musicbrainz.org") {
      $config{CDDB_HOST} = "freedb." . $CDDB_HOST;
   }
   else {
      $config{CDDB_HOST} = $mirror . "." . $CDDB_HOST;
   }
   while($transfer !~ /^cddb$|^http$/) {
      print "Transfer mode not valid!\n";
      print "Enter cddb or http : ";
      $transfer = <STDIN>;
      chomp $transfer;
   }
   if($transfer eq "cddb") {
      $CDDB_PORT = 8880;
      $CDDB_MODE = "cddb";
   }
   elsif($transfer eq "http") {
      $CDDB_PORT = 80;
      $CDDB_MODE = "http";
   }
   $config{CDDB_MODE} = $CDDB_MODE;
   $config{CDDB_PORT} = $CDDB_PORT;
   $config{CD_DEVICE} = $scsi_cddev;
   $config{HTTP_PROXY}= $proxy if($proxy);
   if($interaction == 0) {
      $CDDB_INPUT = 0;
   }
   else {
      $CDDB_INPUT = 1;
   }
   $config{input} = $CDDB_INPUT;
   $config{PROTO_VERSION} = $proto;

   # Change to whatever, but be aware to enter exactly 4 words!
   # E.g. username hostname clientname version
   my $hid = "RipIT www.suwald.com/ripit/ripit.html RipIT $version";
   my @hid = split(/ /, $hid);
   if($hid[4]) {
      print "There are more than 4 words in the \"HELLO_ID\"!\n",
            "The handshake with the freeDB-server will fail!\n\n";
   }
   $config{HELLO_ID} = $hid;

   print "\nChecking for a DB entry \@ $config{CDDB_HOST}...\n"
      if($verbose >= 1 && $writecddb != 0);
   eval {%cd = get_cddb(\%config);};
   if($@) {
      $@ =~ s/db:\s/db:\n/;
      $@ =~ s/at\s\//at\n\//;
      print "No connection to internet? The error message is:\n",
            "$@\n" if($verbose >= 1);
      $submission = 0;
   }
   #
   # Thanks to Frank Sundermeyer
   # If wanting to write the CDDB data (archive=1) but having set
   # interaction=0, the CDDB data will not get modified, so it
   # is safe to write it now.
   # But this routine is also called from get_mb (for getting a genre)
   # with $interaction temporarily set to 0. We call it with a parameter
   # set to zero (0) when calling it from get_mb. This parameter is
   # saved to $writecddb. Therefore do not write the entry if
   # $writecddb == 0.
   #
   if($interaction == 0 && $archive == 1 && defined $cd{title}) {
       write_cddb() unless $writecddb == 0;
   }

   if($multi == 1) {
      my $cddevno = $cddev;
      $cddevno =~ s/\/dev\///;
      $cddevno =~ s/(\d)/0$1/ unless($cddevno =~ /\d\d/);
      $logfile = $outputdir . "/" . $cddevno;
      read_entry($logfile, "multi");
   }
}
########################################################################
#
# Read the album, artist, discID and track titles from MusicBrainz.
#
sub get_mb {

   print "Querying MusicBrainz DB." if($verbose > 2);
   my ($cddbid, $trackno, $totaltime) = get_cddbid();


   # Using the perl module to retrieve the MB discid.
   my $discid = 0;
   my $submit_url = 0;
   my $disc;
   eval{ $disc = new MusicBrainz::DiscID($scsi_cddev);};
   if($@) {
      # Use the libdiscid command discid to retrieve the MB discid.
      open(DISCID, "discid $scsi_cddev|");
      my @response = <DISCID>;
      close(DISCID);
      chomp($discid = join("", grep(s/^DiscID\s*:\s//, @response)));
      chomp($submit_url = join("", grep(s/^Submit\svia\s*:\s//, @response)));
   }
   else {
      if($disc->read() == 0) {
         print "Error: %s\n", $disc->error_msg();
         get_cdinfo();
         return;
      }
      $discid = $disc->id();
      $submit_url = $disc->submission_url();
   }

   # Get the xml contents of that file, parse the MB-ID and do another
   # lookup at http://musicbrainz.org/ws/1/release/MBID, parse this data
   # again and you're done.

   print "\nChecking for a DB entry \@ MusicBrainz.org...\n"
      if($verbose >= 1);
   my $service;
   eval {$service = WebService::MusicBrainz::Release->new();};
   my $discid_response;
   eval {$discid_response = $service->search({ DISCID => $discid });};
   if($@){
      print "\nMusicBrainz lookup failed... 2nd try in 3s.",
            "\nError message is: $@.\n" if($verbose > 3);
      sleep 3;
      eval {$discid_response = $service->search({ DISCID => $discid });};
      if($@){
         print "\nMusicBrainz lookup failed! Using freedb instead.",
               "\nError message is: $@.\n" if($verbose > 3);
         get_cdinfo();
         return;
      }
   }
   else {
      print "DiscID retrieved.\n" if($verbose > 4);
      print "Discid is: $discid.\n" if($verbose > 3);
   }
   # Print the data for further queries.
#   use Data::Dumper;
#   print Dumper($discid_response);
#   print "*" x 72, "\n\n";

   my $mbid;
   eval {$mbid = $discid_response->release()->id();};
   if($@){
      print "\nMusicBrainz does not know this discid! Use\n",
            "$submit_url for submission!\n" if($verbose > 1);
      get_cdinfo();
      return;
   }
   # This should not happen.
   elsif($mbid eq "") {
      print "\nNo discid $discid found at MusicBrainz! Use\n",
            "$submit_url for submission!\n" if($verbose > 1);
   }

   my $mbid_response;
   eval {$mbid_response = $service->search({ MBID => $mbid, INC => 'artist' });};
   if($@){
      print "\nMusicBrainz artist lookup failed... 2nd try in 3s.",
            "\nError message is: $@.\n" if($verbose > 3);
      sleep 3;
      eval {$mbid_response = $service->search({ MBID => $mbid, INC => 'artist' });};
      if($@){
         print "\nMusicBrainz lookup artist failed!",
               "\nUsing freedb instead.",
               "\nError message is: $@.\n" if($verbose > 3);
         get_cdinfo();
         return;
      }
   }
   else {
      print "MB artist retrieved.\n" if($verbose > 4);
   }

   my $release = $mbid_response->release();
   my $artist = $release->artist()->name();
   my $album = $release->title();
   my $asin = $release->asin();
   my $language = $release->text_rep_language();
   $artist =~ s/^The\s+// unless($artist =~ /^The\sThe/);
   $language = "English" unless($language);
   $language = "English" if($language eq "ENG");
   $language = "French" if($language eq "FRA");
   $language = "German" if($language eq "DEU");

   # Retrieve the year and barcode:
   eval {$mbid_response = $service->search({ MBID => $mbid, INC => 'release-events' });};
   if($@){
      print "\nMusicBrainz lookup failed... 2nd try in 3s:\n" if($verbose > 3);
      sleep 3;
      eval {$mbid_response = $service->search({ MBID => $mbid, INC => 'release-events' });};
      if($@){
         print "\nMusicBrainz lookup failed! Using freedb instead.\n"
            if($verbose > 3);
         get_cdinfo();
         return;
      }
   }
   else {
      print "MB release eventrs retrieved.\n" if($verbose > 4);
   }
   $release = $mbid_response->release();
   my $content = $release->release_event_list();
   my $reldate = ${$content->events()}[0]->date() if($content);
   my $barcode = ${$content->events()}[0]->barcode() if($content);
   $year = $reldate;
   $year =~ s/-.*$// if($year);

   # Some people insist in getting a genre, but MB does not supply one.
   unless($genre) {
      my $save_inter = $interaction;
      $interaction = 0;
      print "Retrieving a genre from freedb.org.\n" if($verbose > 2);
      get_cdinfo(0);
      $interaction = $save_inter;
      $genre = $cd{genre};
      $year = $cd{year} unless($year);
   }

   # Why this? Actually, I don't know. Thought that in the 21st
   # century there should be no UTF-8 problem anymore... But getting
   # e.g. tracknames with pure ascii in one track, latin chars in an
   # other track and even true wide chars in a third one will totally
   # mess up encoder tags and even the file names used by the ripper.
   my $temp_file = "/tmp/ripit-MB-$$\.txt";
   open(TMP, '>:utf8', "$temp_file") or print "$temp_file $!";
   print TMP "artist: $artist\n";
   print TMP "album: $album\n";
   print TMP "genre: $genre\n" if($genre);
   print TMP "category: musicbrainz\n";
   print TMP "cddbid: $cddbid\n";
   print TMP "discid: $discid\n";
   print TMP "asin: $asin\n" if($asin);
   print TMP "year: $year\n" if($year);
   print TMP "trackno: $trackno\n";
   print TMP "language: $language\n";
   print TMP "reldate: $reldate\n" if($reldate);
   print TMP "barcode: $barcode\n" if($barcode);

   # Retrieve the track list.
   eval {$mbid_response = $service->search({ MBID => $mbid, INC => 'tracks' });};
   if($@){
      print "\nMusicBrainz lookup failed... 2nd try in 3s:\n" if($verbose > 3);
      sleep 3;
      eval {$mbid_response = $service->search({ MBID => $mbid, INC => 'tracks' });};
      if($@){
         print "\nMusicBrainz lookup failed! Using freedb instead.\n"
            if($verbose > 3);
         close(TMP);
         unlink("$temp_file");
         get_cdinfo();
         return;
      }
   }
   else {
      print "MB track list retrieved.\n" if($verbose > 4);
   }
   $release = $mbid_response->release();
   my $track_list = $release->track_list();
   my $mb_trackno = $#{$track_list->tracks()} + 1;

   my $i=1;
   my $j=0;
   foreach my $track (@{$track_list->tracks()}) {
      $_ = $track->title();
      # Various artist style
      if($track->artist) {
         print TMP "track $i: ", $track->artist->name(), " / $_\n";
         $va_flag = 2;
         $va_delim = "/";
      }
      # Normal tracklist style.
      else {
         print TMP "track $i: $_\n";
      }
      # For ISRC detection/submission use the track IDs.
      push(@idata, $track->id);
      $i++;
      $j++;
   }

   close(TOC);
   # MusicBrainz does not state data tracks. Let's continue to fill up
   # the tracklist in the %cd-hash.
   while($i <= $trackno) {
   print TMP "track $i: data\n";
      $i++;
      $j++;
   }
   close(TMP);
   read_entry("$temp_file", "musicbrainz", $trackno);
   unlink("$temp_file");
}
########################################################################
#
# Display CDDB info.
#
sub disp_info {
   my $latinflag = 0;
   my $wideflag = 0;
   my $utf_latinflag = 0;
   my $utf_wideflag = 0;
   my ($artist, $album, %config, $revision);
   my @comment = ();

   CDDB_get->import( qw( get_cddb get_discids ) );
   my $cd = get_discids($scsi_cddev);
   my ($id, $trackno, $toc) = ($cd->[0], $cd->[1], $cd->[2]);
   my $cddbid = sprintf("%08x", $id);
   my $totaltime = sprintf("%02d:%02d",$toc->[$trackno]->{min},$toc->[$trackno]->{sec});

   if(defined $cd{title}) {
      $album = clean_all($cd{title});
      $artist = clean_all($cd{artist});
      # Remember: use of lowercase was supposed for file names only,
      # tags should not be lowercase (what for?). But option
      # ucfirst is useful if DB entry is in uppercase only, and tags
      # in uppercase are rather ugly.
      $album = change_case($cd{title}) if($uppercasefirst == 1);
      $artist = change_case($cd{artist}) if($uppercasefirst == 1);
      $categ = $cd{cat};

      # Set the year if it wasn't passed on command line.
      unless($year) {
         $year = $cd{year} if($cd{year});
         $year =~ s/[\015]//g if($year);
      }

      # Set the genre if it wasn't passed on command line.
      if(!defined $pgenre && defined $cd{genre}) {
         $genre = $cd{genre};
         $genre =~ s/[\015]//g if($genre);
      }

      @comment = extract_comm;
      $revision = get_rev() unless($cd{discid});
      # In case of corrupted (local) DB files.
      $revision = "unknown" unless($revision);
   }
   else {
      if($submission == 0) {
         print "\nNo CDDB info chosen or found for this CD\n"
            if($verbose >= 1);
      }
      # Set submission OK, will be set to 0 if default names are used.
      $cddbsubmission = 1;
      # Don't ask for default settings, use them ...
      if($interaction == 0) {
         create_deftrack(1);
      }
      # ... or ask whether 1) default or 2) manual entries shall be used
      # or entered.
      else {
         create_deftrack(2);
      }
      $album = $cd{title};
      $artist = $cd{artist};
      $revision = $cd{revision};
   }

   if($cd{discid}) {
      # We do nothing anymore because we read the data from a file.
   }
   else {
      # The strings from archive files should be OK, because the files
      # should be written in the corresponding encoding. Only strings
      # from CDDB_get must be treated.
      # But still, this gives the error:
      # Cannot decode string with wide characters at
      # /usr/lib/perl5/5.8.8/i586-linux-threads-multi/Encode.pm line 186.
      # So do it here to be sure to analyze manually entered data!
      #
      # Create a string with the DB data to be analyzed for true UTF-8
      # (wide) characters.
      my $char_string =  $cd{title} . $cd{artist};
      $char_string .= $_ foreach (@{$cd{track}});

      ($latinflag, $wideflag, $utf_latinflag, $utf_wideflag) =
         check_encoding($char_string);

      if($utf_latinflag >= $latinflag * 3 && $utf_wideflag == 0 && $wideflag == 0) {
         print "\nRare case: Decoding from iso 8859-1 to utf-8?\n"
            if($verbose >= 5);
         Encode::from_to($artist, 'iso-8859-1', 'UTF-8');
         Encode::from_to($album, 'iso-8859-1', 'UTF-8');
      }
      elsif($wideflag == 0 && $latinflag == 0) {
         print "No wide char found, artist is <$artist>.\n"
            if($verbose >= 5);
         print "No latin char found, artist is <$artist>.\n"
            if($verbose >= 5 && $latinflag == 0);
         $album = UTF8_encoding($album);
         $artist = UTF8_encoding($artist);
      }
      elsif($utf_wideflag == 1 && $wideflag == 0) {
         print "\nForcing UTF8 case 1:\n"
            if($verbose >= 5);
         $album =  Encode::decode('UTF-8', $album);
         $artist =  Encode::decode('UTF-8', $artist);
      }
      elsif($utf_latinflag % 2 == 0 && $latinflag % 2 == 0 && $utf_wideflag == 0 && $wideflag == 0) {
         print "\nTrying to force UTF8 case 2: source might be utf8!\n"
            if($verbose >= 5);
         # Keep it commented for the archive and online Röyksopp case.
#         $album = Encode::decode('UTF-8', $album);
#         $artist = Encode::decode('UTF-8', $artist);
      }
      elsif($utf_wideflag > 0 && $wideflag >= 0) {
         print "\nTrying to force UTF8 case 3: source might be utf8!\n"
            if($verbose >= 5);
         # Keep it commented for the clean archive Enya case.
#         $album =  Encode::decode('UTF-8', $album);
#         $artist =  Encode::decode('UTF-8', $artist);
      }
      else {
         print "\nDon't know what to do. Is it cp-1252 or iso 8859-1?\n"
            if($verbose >= 5);
      }
   }


# Resetting the album and artist in the %cd-hash will screw up all the
# track titles (e.g. Bang Bang). Can you believe it? Change album and
# artist and tracknames will blow up. That's life, that's Perl.
# Again: we need a save copy of the string as it is now! What is wrong
# changing an entry of a hash? Why are all other entries of that
# hash screwed up?

   $album_utf8 = $album;
   $artist_utf8 = $artist;

   my $genreno = "";
   if($genre eq "" && $interaction == 1) {
      print "\nPlease enter a valid CDDB genre (or none): ";
      $genre = <STDIN>;
      chomp $genre;
      $cd{genre} = $genre;
   }
   if($genre) {
      $genre =~ s/[\015]//g;
      ($genre,$genreno) = check_genre($genre);
   }

   if($verbose >= 1) {
      print "\n", "-" x 17, "\nCDDB and tag Info", "\n", "-" x 17, "\n";
      print "Artist: $artist_utf8\n";
      print "Album: $album_utf8\n";
      print "Category: $categ\n" if($verbose >= 2);
      if($genre) {
         print "ID3-Genre: $genre ($genreno)\n" if($lameflag >= 0);
         print "Genre-tag: $genre\n" if($lameflag == -1);
         if(lc($genre) ne lc($cd{'genre'})) {
            print "CDDB-Genre: $cd{genre}\n";
         }
      }
      else{
         print "ID3-Genre: none\n";
      }
      print "ASIN: $cd{asin}\n" if($cd{asin});
      print "Barcode: $cd{barcode}\n" if($cd{barcode});
      print "Language: $cd{language}\n" if($cd{language});
      print "Release date: $cd{reldate}\n" if($cd{reldate});
      print "Year: $year\n" if($year);
      print "Revision: $revision\n" if($verbose >= 2);
      # It happens, that the ID from CDDB is NOT identical to the ID
      # calculated from the frames of the inserted CD...
      if($cddbid ne $cd{id} && defined $cd{id} ) {
         print "CDDB id: $cd{id}\n";
      }
      print "CD id: $cddbid\n";
      print "Discid: $cd{discid}\n" if($cd{discid});
      if(@comment && $verbose >= 2) {
         foreach (@comment) {
            print "Comment: $_\n" if($_);
         }
      }
      print "CD length: $totaltime\n";
      print "\n";
   }
   log_info("\nArtist: $artist");
   log_info("Album: $album");
   log_info("ID3-Genre: $genre ($genreno)") if($genre);
   log_info("ID3-Genre: none") unless($genre);
   log_info("Category: $categ");
   log_info("ASIN: $cd{asin}") if($cd{asin});
   log_info("CD id: $cddbid");
   log_info("MB id: $cd{discid}") if($cd{discid} && $cd{discid} ne $cddbid);
   log_info("CD length: $totaltime\n");

   # Read out pregap before calculating track lengths.
   my $frames = $toc->[0]->{'frames'};
   push @framelist, "$frames";
   if($frames > 400) {
      my $second = int($frames/75);
      my $frame = $frames - $second * 75;
      my $minute = int($second/60);
      $second -= $minute * 60;
      printf("%s %02d:%02d %s %d %s\n",
         "There might be a hidden track", $minute, $second,
         "long,\nbecause offset of track 01 has", $frames,
         "frames\nintstead of typically 150 (equals 2 seconds).\n")
         if($verbose >= 1);
      my $riptrackname = "Hidden Track";
      $riptrackname = change_case($riptrackname);
      $riptrackname =~ s/ /_/g if($underscore == 1);
      printf("%s: [%02d:%02d.%02d] %s\n",
         "00", $minute, $second, $frame, $riptrackname)
         if($verbose >= 1);
      $second = int($frames/75);
      $hiddenflag = 1 if($trackselection eq ""
         || $trackselection =~ /^0/
         || $trackselection =~ /\D0/);
      # We can't add this track to seltrack and framelist, because this
      # would break (re-) submission of CDDB.
      # Note: seltrack is not yet defined... But we start to fill the
      # @secondlist array (yet empty) with track lengths in seconds.
      # TODO: push the pregap seconds to the list in any case, then we
      # don't need to differentiate between the case hiddenflag == 1 or
      # hiddenflag == 0 while choosing tracknames.
      push @secondlist, "$second" if($hiddenflag == 1);
   }
   my $n = 1;
   # Print track information.
   foreach (@{$cd{track}}) {
      $_ = clean_all($_);
      $_ = change_case($_) if($uppercasefirst == 1);

      if($cd{discid}) {
         # We do nothing anymore because we read the data from a file.
      }
      else {
         if($utf_latinflag >= $latinflag * 3 && $utf_wideflag == 0 && $wideflag == 0) {
            Encode::from_to($_, 'iso-8859-1', 'UTF-8');
         }
         elsif($latinflag == 0 && $wideflag == 0) {
            $_ = UTF8_encoding($_);
         }
         elsif($utf_wideflag == 1 && $wideflag == 0) {
            $_ =  Encode::decode('UTF-8', $_);
         }
      }
      push @tracktags, $_;

      # Get frames and total time.
      my $frames = $toc->[$n]->{'frames'};
      push @framelist, "$frames";
      $frames = $frames - $framelist[$n - 1];
      my $second = int($frames / 75);
      push @secondlist, "$second";
      my $frame = $frames - $second * 75;
      my $minute = int($second / 60);
      $second -= $minute * 60;
      $_ = clean_chars($_) if($chars);
      printf("%02d: [%02d:%02d.%02d] %s\n",
             $n + $trackoffset, $minute, $second, $frame, $_)
         if($verbose >= 2);
      $_ = clean_name($_);
      $_ = change_case($_);
      $_ =~ s/ /_/g if($underscore == 1);
      push @tracklist, $_;
      $n++;
   }
   print "\n\n" if($verbose >= 1);

   # Some more error checking.
   if($artist eq "") {
      die "Error: No artist found!\n";
   }
   unless($tracklist[0]) {
      die "Error: No tracks found!\n";
   }

   get_isrcs if($isrc == 1 && $mb == 1);

   # LCDproc
   if($lcd == 1) {
      $lcdline1 = $artist . "-" . $album;
      $lcdline2 = "R00|00.0%|----------";
      $lcdline3 = "E00|00.0%|----------";
      ulcd();
   }
}
########################################################################
#
# Create the track selection from the parameters passed on the command-
# line, i. e. create an array with all track numbers including those not
# explicitly stated at the command line.
#
sub create_seltrack {
   my($tempstr,$intrack);
   ($tempstr) = @_;
   if($_[0] eq "-") {
         die "Invalid track selection \"-\"!\n\n";
   }

   if(($tempstr =~ /,/) || ($tempstr =~ /\-/)) {
      my @intrack = split(/,/ , $tempstr);
      # If last character is a , add an other item with a -
      if($tempstr =~ /,$/) {
         push @intrack, ($intrack[$#intrack]+1) . "-";
      }
      foreach $intrack (@intrack) {
         if($intrack =~ /\-/) {
            my @outrack = split(/-/ , $intrack);
            # If last character is a -, add last track to $outrack
            if($#outrack == 0) {
               $outrack[1] = $#tracklist + 1;
               if($outrack[0] > ($#tracklist + 1)) {
                  die "Track selection higher than number of tracks ",
                      "on CD.\n\n";
               }
            }
            for(my $i = $outrack[0]; $i <= $outrack[1]; $i++) {
               push @seltrack, $i;
            }
         }
         else {
            push @seltrack, $intrack;
         }
      }
   }
   elsif($tempstr eq '') {
      for(my $i = 1; $i <= ($#tracklist + 1); $i++) {
         $seltrack[$i - 1] = $i;
      }
   }
   elsif($tempstr =~ /^[0-9]*[0-9]$/) {
      $seltrack[0] = $tempstr;
   }
   else {
      die "Track selection invalid!\n";
   }

   @seltrack = sort {$a <=> $b} @seltrack;

   # Check the validity of the track selection.
   foreach (@seltrack) {
      if($_ > ($#tracklist + 1)) {
         die "Track selection higher than number of tracks on CD.\n\n";
      }
      elsif($_ == 0) {
         shift @seltrack;
      }
   }
}
########################################################################
#
# Ask if CDDB submission shall be done. Either because one might change
# some settings a last time before writing to directories and files (if
# there was not DB entry and operator entered all by hand) or because
# DB entry has some typos! Also x-check for VA-style and let operator
# change settings according to metadata retrieved (in case interaction
# is on) and finally submit ISRCs to MusicBrainz if login info available
# and the ISRCs are OK.
#
sub ask_subm {
   my $index = 2;
   unless($cddbsubmission == 0 || $interaction == 0) {
      while($index !~ /^[0-1]$/) {
         print "\nDo you want to edit or submit the CDDB entry?";
         print "\nTo confirm each question type Enter.\n\n";
         print "1: Yes, and I know about the naming-rules of ";
         print "freedb.org!\n\n";
         print "0: No\n\nChoose [0-1]: (0) ";
         $index = <STDIN>;
         chomp $index;
         if($index eq "") {
            $index = 0;
         }
         print "\n";
      }
      if($index == 1) {
         my $revision = get_rev() unless($cd{discid});
         if($revision) {
            print "\nPlease change some settings.";
            print "\nYou may confirm CDDB settings with \"enter\".\n";
            create_deftrack(0);
         }
         else {
            print "\nPlease change some settings.";
            print "\nYou may confirm given settings with \"enter\".\n";
            create_deftrack(0);
         }
      }
      elsif($index == 0) {
         #
         # CDDB data does not get modified, write the existing data to
         # the local CDDB if wanted.
         if($archive == 1 && defined $cd{title}) {
             write_cddb();
         }
      }
      else {
         print "Choose 0 or 1!\n";
      }
   }
   if($index == 1) {
      pre_subm();
   }

   # Once the metadata has been altered (optionally), check for
   # VA style.
   # Delimeters to be checked for various artists style.
   my $delim_colon = 0;
   my $delim_hyphen = 0;
   my $delim_slash = 0;
   my $delim_parenthesis = 0;
   my $n = 0;
   if($vatag > 0) {
      # We call check_va only to print detected results if verbosity is
      # switched on.
      my $delim = check_va(1);
      if($interaction == 1) {
         $index = 9;
         while($index !~ /^[0-8]$/) {
            print "\nDo you want to change option --vatag to alter";
            print "\ndetection of compilation style?";
            print "\n\nChoose [0-8]: ($vatag) ";
            $index = <STDIN>;
            chomp $index;
            if($index eq "") {
               $index = $vatag;
            }
            print "\n";
            $vatag = $index;
         }
      }
   }
   return;
}
########################################################################
#
# Create the directory where the sound files shall go.
# Directory created will be: /outputdir/$dirtemplate[$c] .
# We first check the wavdir and set the counter $c for the encoder
# depending arrays @sepdir, @suffix, @globopt to -1. In this way,
# directory names will not be suffixed with a counter if they shall be
# the same for wavs and encoded files (condition $soundir ne $wavdir and
# the exception handling below).
#
sub create_dirs {
   my $c = -1;

   # Get cddbid and number of tracks of CD.
   my $trackno;
   ($cddbid, $trackno) = get_cddbid();

   foreach("wav", @coder) {
      my $suffix = $suffix[$c] if(defined $suffix[$c]);
      $suffix = "wav" if($_ eq "wav");
      my $quality = $globopt[$c] if(defined $globopt[$c]);
      $quality = "" if($_ eq "wav");

      # Why this? Remember, we have worked a lot with encoding of artist
      # and album names!
      my $album = clean_all($album_utf8);
      my $artist = clean_all($artist_utf8);
      $album = clean_name($album);
      $artist = clean_name($artist);
      $album = clean_chars($album) if($chars);
      $artist = clean_chars($artist) if($chars);
      $artist = change_case($artist);
      $album = change_case($album);
      $album =~ s/ /_/g if($underscore == 1);
      $artist =~ s/ /_/g if($underscore == 1);

      # Define variable for initial letter of artist.
      my $iletter = $artist;
      $iletter =~ s/\s*(.).*/$1/;
      if($iletter =~ /\d/) {
         my @words = split(/ /, $artist);
         shift(@words);
         foreach (@words) {
            $iletter = $_;
            $iletter =~ s/\s*(.).*/$1/;
            last if($iletter =~ /\w{1}/);
         }
      }
      $iletter = "A" unless($iletter);
      $iletter = "\u$iletter" unless($lowercase == 1);

      # Take the last dirtemplate for missing ones and for wav.
      my $dirindex = $c;
      if($suffix eq "wav") {
         $dirindex = $#dirtemplate;
      }
      elsif($c > $#dirtemplate) {
         $dirindex = $#dirtemplate;
      }

      # Check and create the full path where the files will go.
      # Check the dirtemplate and use the actual year as default if
      # $year is in the template and none is given!
      if(($dirtemplate[$dirindex] =~ /\$year/ or
          $tracktemplate =~ /\$year/) && !$year) {
         $year = sprintf("%04d", sub {$_[5]+1900}->(localtime));
      }
      # Do the same for the genre.
      if(($dirtemplate[$dirindex] =~ /\$genre/ or
          $tracktemplate =~ /\$genre/)) {
         $genre = "Other" if($genre eq "");
         chomp $genre;
      }

      my $dir;
      if(!eval("\$dir = $dirtemplate[$dirindex]")) {
         die "Directory template incorrect, caused eval to fail: $!\n";
      }

      $dir =~ s,\s-\s/,/,g; # Do this in any case, even if all chars are
      $dir =~ s,\s-\s$,,g;  # allowed.
      $dir =~ s,\s+/,/,g; # Do this in any case, even if all chars are
      $dir =~ s,\s+, ,g;  # allowed.
      $dir =~ s,\s-\s-\s*, ,g;
      # Change case again only if lowercase wanted! Else we will get
      # lower case of special dirtemplates like: $iletter/$artist: here
      # artist would be converted to lowercase, since we check for words!
      $dir = change_case($dir) if($lowercase == 1);
      $dir = clean_chars($dir) if($chars);
      $dir =~ s/ /_/g if($underscore == 1);

      $dir =~ s/\.+$// if($chars =~ /NTFS/);
      $dir =~ s/^\///;
      my $soundir = $outputdir . "/" . $dir if($outputdir !~ /\/$/);
      $soundir = $outputdir . $dir if($outputdir =~ /\/$/);
      # Check if the soundir already exists, if it does, try "soundir i"
      # with i an integer until it works, unless option resume is given.
      #
      # TODO: What if two identical named discs shall be done, but with
      # different number of tracks (different track names will be too
      # difficult to distinguish!)? Maybe we should test here the number
      # of tracks in an existing directory with same name...
      # E.g. Nouvelle Vague: Bande à part, EU version, US version,
      # LTD. Ed, initial release version... all have the same name but
      # different track names/numbers.
      #
      my $cdexistflag = 0;
      my $i = 1;
      my $nsoundir = $soundir;
      my $sfx = "";
      while(defined(opendir(TESTDIR, $nsoundir)) &&
            $rip == 1 && $resume == 0 && $soundir ne $wavdir) {
         $sfx = " " . $i if($underscore == 0);
         $sfx = "_" . $i if($underscore == 1);
         $sfx = clean_chars($sfx) if($chars);
         $nsoundir = $soundir . $sfx;
         $i++;
         $cdexistflag = 1;
      }
      return "next" if($cdexistflag == 1 && $overwrite =~ /^e|q$/);
      return "unknown" if($artist =~ /unknown.artist/i && $album =~ /unknown.album/i && $quitnodb == 1);

      $nsoundir = $soundir if($overwrite eq "y");
      # Exception handling: if the $wavdir is identical to the
      # $nsoundir apart from a suffixed counter, use the $wavdir as
      # $soundir instead of the incremented $nsoundir!
      esc_char($soundir);
      $nsoundir = $wavdir if($wavdir =~ /$soundir.\d+/);

      if($multi == 1 && $_ eq "wav") {
         if($overwrite =~ /^y$/) {
            $cdexistflag = 0;
            $sfx = "";
         }
         my $aadir = $dir . $sfx;
         if($cdexistflag == 1) {
            $i--;
            open(SRXY,"$logfile") or
               print "Can not open \"$logfile\"!\n";
            my @srxylines = <SRXY>;
            close(SRXY);
            chomp(my $orig_album = join(' ', grep(/^album:\s/, @srxylines)));
            grep(s/^album:\s(.*)$/album: $1 $i/, @srxylines)
               if($underscore == 0);
            grep(s/^album:\s(.*)$/album: $1_$i/, @srxylines)
               if($underscore == 1);
            open(SRXY,">$logfile")
               or print "Can not write to file \"$logfile\"!\n";
            print SRXY @srxylines;
            print SRXY "Original-$orig_album\n";
            close(SRXY);
         }
         open(SRXY,">>$logfile")
            or print "Can not append to file \"$logfile\"!\n";
         print SRXY "\n\nArtist - Album:$aadir";
         close(SRXY);
      }
      $soundir = $nsoundir;
      $soundir =~ s;/$;;g;

      # Problem: multi level directory creation should set permission to
      # each directory level. I thought the easiest way would be to
      # alter permissions using umask and then set it back. I did not
      # succeed.
      #
      # Save machines umask for reset.
      my $umask = umask();

      # Get the default permission mode.
      my $dperm = sprintf("%04o", 0777 & ~umask());

      if(!opendir(TESTDIR, $soundir)) {
         # Explicitly log soundir creation.
         log_info("new-mediadir: $soundir");

         # The so called Holzhacker-Method: create dir level by level.
         # TODO: Let me know the good way to do it, thanks.
         my $growing_dir = "";
         foreach (split(/\//, $soundir)) {
            next if($_ eq " ");
            # Should we allow relative paths?
            if($_ =~ /^\.{1,2}$/ && $growing_dir eq "") {
               $growing_dir .= "$_";
            }
            else {
               $growing_dir .= "/$_";
            }
            $growing_dir =~ s;//;/;g;
            if(!opendir(TESTDIR, $growing_dir)) {
               log_system("mkdir -m $dpermission -p \"$growing_dir\"");
               if (! -d "$growing_dir") {
                  print "\nWill try to trim length of directory.\n" if($verbose > 4);
                  while(length($_) > 250) {
                     chop;
                     chop($growing_dir);
                  }
                  # Again problems with umask... not comprehensive.
                  # eval { mkpath($growing_dir, {mode => $dpermission,}) };
                  # Actually, why should I use mkpath if it is not
                  # recommended to be used with eval...
                  #use File::Path;
                  #eval { mkpath($growing_dir) };
                  #if($@) {
                  #   die "\nRelease directory $growing_dir creation failed : $!\n\n";
                  #}
                  log_system("mkdir -m $dpermission -p \"$growing_dir\"")
                     or die "Can not create directory $growing_dir: $!\n";
                  $limit_flag = 255;
               }
            }
         }
         # In case $growing_dir needed to be shorten.
         $soundir = $growing_dir;
         # Do it again for security reasons.
         log_system("mkdir -m $dpermission -p \"$soundir\"")
            or die "Can not create directory $soundir: $!\n";
      }
      else {
         closedir(TESTDIR);
      }

      # Reset umask
      #umask($umask) if defined $umask;

      $sepdir[$c] = $soundir unless($_ eq "wav");
      $wavdir = $soundir if($_ eq "wav");
      $c++;

      # This might not be the best place to set up the pre- and exe-
      # command and the coverpath but this is where most variables are
      # available. The same goes for the copycover variable.
      if($execmd && $suffix eq "wav") {
         my $exec;
         if(!eval("\$exec = $execmd")) {
            print "execmd incorrect, caused eval to fail: $!\n";
         }
         $execmd = $exec;
      }
      if($precmd && $suffix eq "wav") {
         my $prec;
         if(!eval("\$prec = $precmd")) {
            print "precmd incorrect, caused eval to fail: $!\n";
         }
         $precmd = $prec;
      }
      if($coverpath && $suffix eq "wav") {
         my $covp;
         if(!eval("\$covp = $coverpath")) {
            print "coverpath incorrect, caused eval to fail: $!\n";
         }
         $coverpath = $covp;
      }
      if($copycover && $suffix eq "wav") {
         my $copy;
         if(!eval("\$copy = $copycover")) {
            print "copycover path incorrect, caused eval to fail: $!\n";
         }
         $copycover = $copy;
      }
   }
   return("go");
}
########################################################################
#
# Create the full-path track file name from the tracktemplate variable.
#
sub get_trackname {
   my($trnum, $trname, $riptrname, $shortflag);

   ($trnum, $trname, $shortflag) = @_;
   $shortflag = 0 unless($shortflag);

   my $album = clean_all($album_utf8);
   my $artist = clean_all($artist_utf8);
   $album = clean_name($album);
   $artist = clean_name($artist);
   $album = clean_chars($album) if($chars);
   $artist = clean_chars($artist) if($chars);
   $album =~ s/ /_/g if($underscore == 1);
   $artist =~ s/ /_/g if($underscore == 1);
   # Create the full file name from the track template, unless
   # the disk is unknown.
   if($trname =~ /short/ && $shortflag =~ /short/) {
      $riptrname = $trname;
   }
   elsif(defined $cd{title}) {
      # We do not need to lowercase the tracktemplate because all
      # variables are already lowercase!
      $tracktemplate =~ s/ /\\_/g if($underscore == 1);
      # We have to update tracknum and trackname because they're
      # evaluated by the tracktemplate!
      my $tracknum = sprintf("%02d", $trnum + $trackoffset);
      my $trackname = $trname;
      if(!eval("\$riptrname = $tracktemplate")) {
         die "\nTrack Template incorrect, caused eval to fail: $!.\n";
      }
   }
   else {
      $trname  = change_case($trname);
      $trname =~ s/ /_/g if($underscore == 1);
      $riptrname = $trname;
   }

   if($limit_flag == 255) {
      $riptrname = substr($riptrname, 0, 250);
   }

   # No counters if option book or cdcue is used:
   if($book == 1 or $cdcue > 0) {
      $riptrname =~ s/^\d+[\s|_]// if($tracktemplate =~ /^\$tracknum/);
   }
   return $riptrname;
}
########################################################################
#
# Rip the CD.
#
sub rip_cd {
   my($ripcom, $riptrackname, $riptracktag);
   my $startenc = 0;
   my $failflag = 0;
   my $resumerip = $resume;
   my $trackstart = 0;
   my $cue_point = 0;
   # Cleaning.
   my $albumtag = clean_all($album_utf8);
   my $artistag = clean_all($artist_utf8);
   my $album = $albumtag;
   $album = clean_name($album);
   my $artist = $artistag;
   $artist = clean_name($artist);
   $album = clean_chars($album) if($chars);
   $artist = clean_chars($artist) if($chars);
   $album =~ s/ /_/g if($underscore == 1);
   $artist =~ s/ /_/g if($underscore == 1);

   # Delete existing md5 files in case of resuming.
   if($md5sum == 1 && $resume == 1) {
      if($wav == 1) {
         my @paths = split(/\//, $wavdir);
         my $md5file =  $paths[$#paths] . " - wav" . ".md5";
         $md5file =~ s/ /_/g if($underscore == 1);
         unlink("$wavdir/$md5file");
      }
      for(my $c = 0; $c <= $#coder; $c++) {
         my @paths = split(/\//, $sepdir[$c]);
         my $md5file =  $paths[$#paths] . " - " . $suffix[$c] . ".md5";
         $md5file =~ s/ /_/g if($underscore == 1);
         unlink("$sepdir[$c]/$md5file");
      }
   }

   # Delete machine.lock files.
   if($resume == 1) {
      opendir (DIR, "$wavdir") or print "Can't open $wavdir $!\n";
      my @lockfiles = grep(/\.lock_\d+$/, readdir(DIR));
      @lockfiles = grep(/\.lock$/, readdir(DIR)) unless($lockfiles[0]);
      closedir(DIR);
      unlink("$wavdir/$_") foreach (@lockfiles);
   }

   # Define an array with intervals and the tracks to be skipped.
   my @merge = ();
   my @skip = ();
   if($pmerge) {
      # If hidden track supposed, try to merge it too in case operator
      # wants a book or a cdcue.
      $pmerge = "0-" if($hiddenflag == 1 && ($book == 1 || $cdcue == 2));
      @skip = skip_tracks;
      @merge = split(/,/, $pmerge);
      # If merge is used, we need to calculate the true track length for
      # the playlist file. And it would be nice, if the filename
      # reflects "missing" tracks. Define a string to concatenate the
      # track names.
      my $concat = " + ";
      $concat =~ s/ /_/g if($underscore == 1);
      $concat = clean_chars($concat) if($chars);
      foreach(@merge) {
         my @bea = split(/-|\+/, $_);
         my $beg = $bea[0] - 1;
         $beg = 0 if($beg < 0); # If nerds want to merge hidden tracks.
         while($bea[0] < $bea[1]) {
            $secondlist[$beg] += $secondlist[$bea[0]];
            # Don't merge all track names if option book or cdcue is used.
            $tracklist[$beg] = $tracklist[$beg] . $concat .
                               $tracklist[$bea[0]] unless($book == 1 or $cdcue == 2);
            $tracktags[$beg] = $tracktags[$beg] . " + " .
                               $tracktags[$bea[0]] unless($book == 1 or $cdcue == 2);
            $bea[0]++;
         }
      }
   }

   # Display info which tracks are going to be ripped. Because of option
   # merge we have to work hard to make it look nice:
   @tracksel = @seltrack; # Use a copy of @seltrack to work with.
   my @printracks;        # A new array in nice print format.
   my $trackcn;
   my $prevtcn = -1;
   # Add the hidden track to @tracksel if a nerd wants it to merge, i.e.
   # operator entered it in the merge argument.
   unshift(@tracksel, 0) if($pmerge && $pmerge =~ /^0/ && $hiddenflag == 1);
   foreach $trackcn (@tracksel) {
      next if($trackcn <= $prevtcn);
      my $trackno;
      # Check if next tracknumber is in the skip array of tracks being
      # merged. If so, add a hyphen.
      my $increment = 1;
      if($skip[0] && ($trackcn + $increment) =~ /^$skip[0]$/) {
         $trackno = $trackcn . "-";
         shift(@skip);
         $trackcn++;
         # Is the next tracknumber the last of the interval of merged
         # tracks? If not, continue to increase the tracknumber.
         while($skip[0] && ($trackcn + $increment) =~ /^$skip[0]$/) {
           $trackcn++;
           shift(@skip);
         }
         $trackno = $trackno . $trackcn;
         $prevtcn = $trackcn;
      }
      else {
         $trackno = $trackcn;
      }
      push(@printracks, $trackno);
   }

   if($#seltrack == 0 && $hiddenflag == 0) {
      print "Track @printracks will be ripped.\n\n" if($verbose > 0);
   }
   elsif(!@seltrack && $hiddenflag == 1) {
      print "Track 0 will be ripped.\n\n" if($verbose > 0);
   }
   elsif($pmerge && $pmerge =~ /^0/ && $hiddenflag == 1) {
      print "Tracks @printracks will be ripped.\n\n" if($verbose > 0);
   }
   elsif($hiddenflag == 1) {
      print "Tracks 0 @printracks will be ripped.\n\n" if($verbose > 0);
   }
   else {
      print "Tracks @printracks will be ripped.\n\n" if($verbose > 0);
   }

   # Prevent failure if hald occupies drive.
   sleep 6 if($loop == 2);
   # Get the time when ripping started, and save it in the error.log.
   my $ripstart = sprintf("%02d:%02d", sub {$_[2], $_[1]}->(localtime));
   my $date = sprintf("%04d-%02d-%02d",
      sub {$_[5]+1900, $_[4]+1, $_[3]}->(localtime));
   open(ERO,">$wavdir/error.log")
      or print "Can not append to file \"$wavdir/error.log\"!\n";
      print ERO "Ripping started: $ripstart\n";
   close(ERO);
   if($multi == 1) {
      open(SRXY,">>$logfile")
         or print "Can not append to file \"$logfile\"!\n";
      print SRXY "\nRipping started: $ripstart";
      close(SRXY);
   }

   # Write a toc-file.
   if($cdtoc == 1) {
      my $cdtocartis = $artistag;
      oct_char($cdtocartis);
      my $cdtocalbum = $albumtag;
      oct_char($cdtocalbum);
      open(CDTOC ,">$wavdir/cd.toc")
         or print "Can not write to file \"$wavdir/cd.toc\"!\n";
      print CDTOC "CD_DA\n//Ripit $version cd.toc file generated ",
                  "$date at $ripstart.",
                  "\n//Use command >cdrdao scanbus< to detect device.",
                  "\n//Assume the device found is:  1,0,0 : _NEC  ",
                  " then use e. g. command",
                  "\n//>cdrdao write --device 1,0,0 ",
                  "--speed 4 cd.toc< to burn the CD.",
                  "\n//Note: Not all CD (DVD) burners are able to burn",
                  " CD-text!\n//Test your device!";
      print CDTOC "\n\n//CD Text:\nCD_TEXT{LANGUAGE_MAP {0 : EN}\n\t";
      print CDTOC "LANGUAGE 0 {\n\t\tTITLE \"$cdtocalbum\"\n\t\t";
      print CDTOC "PERFORMER \"$cdtocartis\"\n";
#      print CDTOC "\t\tGENRE \"$genreno\"\n" if($genreno);
      print CDTOC "\t\tDISC_ID \"$cddbid\"\n\t}\n}\n";
      close(CDTOC);
   }

   # Start to rip the hidden track if there's one: First check if
   # cdparanoia is available.
   if($ripper != 1) {
      unless(log_system("cdparanoia -V")) {
         print "Cdparanoia not installed? Can't rip the hidden track ";
         print "without cdparanoia!\n"
            if($hiddenflag == 1);
         $hiddenflag = 0;
      }
   }

   # Check if the hidden track has been done in a previous session.
   my $checknextflag = 0;
   if($resumerip) {
      $riptrackname = "Hidden Track";
      $riptrackname = change_case($riptrackname);
      $riptrackname =~ s/ /_/g if($underscore == 1);
      $riptrackname = get_trackname(0, $riptrackname);
      if(-r "$wavdir/$riptrackname.rip") {
         unlink("$wavdir/$riptrackname.rip");
         print "Found $riptrackname.rip.\n" if($verbose >= 1);
      }
      elsif(-r "$wavdir/$riptrackname.wav") {
         $checknextflag = 1;
         print "Found $riptrackname.wav.\n" if($verbose >= 1);
         md5_sum("$wavdir", "$riptrackname.wav", 0)
            if($md5sum == 1 && $wav == 1);
      }
      else{
         for(my $c=0; $c<=$#coder; $c++) {
            if(-r "$sepdir[$c]/$riptrackname.$suffix[$c]") {
               $checknextflag = 1;
               print "Found file $riptrackname.$suffix[$c].\n";
            }
         }
      }
      if($checknextflag == 1) {
         $riptrackname = "Hidden Track";
         unshift(@tracktags, $riptrackname);
         unshift(@seltrack, 0);
         unshift(@tracklist, $riptrackname);
      }
   }

   # Define some counters:
   # Because cdtoc is written in different subroutines, define a counter
   # for each track written into the toc file. This way, ghost songs are
   # sorted in the toc file, while they aren't in the @seltrack array.
   my $cdtocn = 0 + $trackoffset;

   # Write header of cue-file.
   if($cdcue > 0) {
      open(CDCUE ,">$wavdir/cd.cue")
         or print "Can not write to file \"$wavdir/cd.cue\"!\n";
      print CDCUE "TITLE \"$albumtag\"\nPERFORMER \"$artistag\"\n",
                  "FILE \"$wavdir/$album.wav\" WAVE\n";
      close(CDCUE);
   }
   # Process a possible hidden (first) track.
   if($hiddenflag == 1 && $checknextflag == 0) {
      $riptrackname = "Hidden Track";
      unshift @tracktags, $riptrackname;
      my $cdtocname = $riptrackname;
      $riptrackname = change_case($riptrackname);
      $riptrackname =~ s/ /_/g if($underscore == 1);
      unshift @seltrack, 0;
      unshift @tracklist, $riptrackname;
      $riptrackname = get_trackname(0, $tracklist[0]);
      # If a cuefile shall be created, use album for the track name.
      $riptrackname = $album if($book == 1 or $cdcue == 2);
      my $start_point = "[00:00]";
      # What if the operator wants to merge a hidden track with the 1st
      # and so on tracks? Calculate the number of the last track to be
      # merged with the hidden track.
      my $endtrackno = 0;
      if($pmerge) {
         my @bea = split(/-|\+/, $merge[0]);
         # Hm, always confused. Should we use defined here to enter the
         # condition in case $bea[0] is defined, but zero?
         #if($bea[0] && $bea[0] == 0) {
         if(defined $bea[0] && $bea[0] == 0) {
            $endtrackno = $bea[1];
            $endtrackno =~ s/^0.//;
            $endtrackno++ unless($endtrackno == $seltrack[$#seltrack]);
            $start_point .= "-$endtrackno";
         }
      }
      # Assemble the command for cdparanoia to rip the hidden track.
      my $saveripopt = $ripopt;
      $ripopt .= " -Z" if($parano == 0);
      $ripopt .= " -q" if($verbose <= 1 && $ripopt !~ /\s-q/);
      $ripcom = "cdparanoia $ripopt -d $cddev $start_point \\
                \"$wavdir/$riptrackname.rip\"";
      printf "\n%02d:%02d:%02d: ", sub {$_[2], $_[1], $_[0]}->(localtime)
         if($verbose >= 1 && $rip == 1);
      print "Ripping \"$riptrackname\"...\n"
         if($verbose >= 1 && $rip == 1);

      unless(log_system("$ripcom")) {
         if($parano == 2) {
            $ripopt = $ripopt . " -Z" if($parano == 2);
            $ripcom = "cdparanoia $ripopt -d $cddev $start_point \\
                      \"$wavdir/$riptrackname.rip\"";
            print "\n\nTrying again without paranoia.\n"
               if($verbose > 1);
            unless(log_system("$ripcom")) {
               # If no success, shift the hidden track stuff out of
               # arrays.
               $hiddenflag = 0;
               shift(@secondlist);
               shift(@seltrack);
               shift(@tracklist);
               shift(@tracktags);
            }
         }
         else {
            # If no success, shift the hidden track stuff out of arrays.
            $hiddenflag = 0;
            shift(@secondlist);
            shift(@seltrack);
            shift(@tracklist);
            shift(@tracktags);
         }
      }

      # Write to the toc file.
      if($cdtoc == 1 && $hiddenflag == 1) {
         open(CDTOC ,">>$wavdir/cd.toc")
            or print "Can not append to file \"$wavdir/cd.toc\"!\n";
         print CDTOC "\n//Track 0:\nTRACK AUDIO\nTWO_CHANNEL_AUDIO\n";
         print CDTOC "CD_TEXT {LANGUAGE 0 {\n\t\tTITLE \"$cdtocname\"";
         print CDTOC "\n\t\tPERFORMER \"$artistag\"\n\t}\n}\n";
         print CDTOC "FILE \"$riptrackname.wav\" 0\n";
         close(CDTOC);
      }
      # Check the hidden track for gaps. We do not care about option
      # merge... should we? Yes, we should. If option merge has been
      # chosen for this track, splitting is not allowed, while
      # extracting one chunk of sound may be desired.
      my @times = (0);
      if($ghost == 1 && $hiddenflag == 1) {
            @times = get_chunks(0, $riptrackname);
            unless($times[0] eq "blank") {
               (my $shorten, @times) =
                  split_chunks(0, "$riptrackname", 0, @times);
               ($cdtocn, $cue_point) =
                  rename_chunks(0, "$riptrackname", 0, $cue_point,
                     $shorten, $artistag, $riptrackname, @times);
               }
      }
      if($hiddenflag == 1) {
         rename("$wavdir/$riptrackname.rip",
                "$wavdir/$riptrackname.wav");
      }
      $ripopt = $saveripopt;
   }

   # If ripping did not fail (on whatever track of the whole disc),
   # write the hidden track info to the cue file.
   if($cdcue ==  2 && $hiddenflag == 1) {
      my $points = chapter_length($framelist[1] - $framelist[0]);
      $points =~ /\.\d+$/;
      open(CDCUE ,">>$wavdir/cd.cue")
         or print "Can not append to file \"$wavdir/cd.cue\"!\n";
      print CDCUE "TRACK 01 AUDIO\n",
                  "   TITLE \"Hidden Track\"\n",
                  "   PERFORMER \"$artistag\"\n",
                  "   INDEX 01 $points\n";
      close(CDCUE);
   }
   # End preparation of ripping process.
   #
   #
   # Start ripping each track. Note that we have to skip a possible
   # hidden track. To prevent reripping ghost songs pushed into the
   # @seltrack array, make a copy which will not be altered.
   @tracksel = @seltrack;

   # Encoder messages are printed into a file which will be read by the
   # ripper to prevent splitting ripper-messages. Lines already printed
   # will not be printed again, use counter $encline.
   my $encline = 0;
   $trackcn = 0;

   foreach (@tracksel) {
      next if($_ == 0); # Skip hidden track.
      $trackcn++;
      $riptrackname = get_trackname($_, $tracklist[$_ - 1]);
      $riptrackname = get_trackname($_, $tracklist[$_])
         if($hiddenflag == 1);
      $riptrackname = $album if($book == 1 or $cdcue == 2);
      $riptracktag = $tracktags[$_ - 1];
      $riptracktag = $tracktags[$_] if($hiddenflag == 1);


      # Split the tracktag into its artist part and track part if
      # VA style is used, no messages to be printed.
      my $delim = check_va(0);
      $delim = quotemeta($delim);
      if($va_flag > 0 && $riptracktag =~ /$delim/) {
         $artistag = "";
         if($vatag % 2 == 1) {
            ($artistag, $riptracktag) = split(/$delim/, $riptracktag);
            $riptracktag =~ s/\)// if($delim =~ /\(/);
            $riptracktag =~ s/^\s*//;
            $artistag =~ s/\s*$//;
            # If artistag got all info, rather use it as tracktag...
            if($riptracktag eq "") {
               $riptracktag = $artistag;
               $artistag = "";
            }
         }
         else {
            ($riptracktag, $artistag) = split(/$delim/, $riptracktag);
            $artistag =~ s/\)// if($delim =~ /\(/);
            $artistag =~ s/^\s*//;
            $riptracktag =~ s/\s*$//;
         }
      }

      my $riptrackno = $_;
      # If we use option merge, skip a previously merged track:
      my $skipflag = 0;
      if($pmerge) {
         @skip = skip_tracks;
         foreach my $skip (@skip) {
            $skipflag = 1 if($_ == $skip);
         }
      }
      if(($cdtoc == 1 || $cdcue > 0) && $failflag == 0) {
         $cdtocn++;
      }
      # Don't write the cue entry again in case ripper failed with
      # paranoia and retries ripping without.
      if($cdcue == 2 && $failflag == 0) {
         my $points = chapter_length($framelist[$_ - 1] - $framelist[0]);
         $points =~ /\.\d+$/;
         my $cuetrackno = sprintf("%02d", $cdtocn);
         open(CDCUE ,">>$wavdir/cd.cue")
            or print "Can not append to file \"$wavdir/cd.cue\"!\n";
         print CDCUE "TRACK $cuetrackno AUDIO\n",
                     "   TITLE \"$riptracktag\"\n",
                     "   PERFORMER \"$artistag\"\n",
                     "   INDEX 01 $points\n";
         close(CDCUE);
      }

      print "\nSkip track $_, it has been merged into previous one.\n"
         if($verbose >=1 && $skipflag == 1);
      next if($skipflag == 1);
      # Write the toc entry only if wav present, don't write it again in
      # case ripper failed with paranoia and retries ripping without.
      # In case we check for ghost songs, these might be deleted, so
      # don't write the toc file here.
      if($cdtoc == 1 && $failflag == 0 && $ghost == 0) {
         my $cdtoctitle = $riptracktag;
         oct_char($cdtoctitle);
         my $cdtocartis = $artistag;
         oct_char($cdtocartis);
         open(CDTOC, ">>$wavdir/cd.toc")
            or print "Can not append to file \"$wavdir/cd.toc\"!\n";
         print CDTOC "\n//Track $cdtocn:\nTRACK AUDIO\n";
         print CDTOC "TWO_CHANNEL_AUDIO\nCD_TEXT {LANGUAGE 0 {\n\t\t";
         print CDTOC "TITLE \"$cdtoctitle\"\n\t\t";
         print CDTOC "PERFORMER \"$cdtocartis\"\n\t}\n}\n";
         print CDTOC "FILE \"$riptrackname.wav\" 0\n";
         close(CDTOC);
      }
      # Remember: $riptrackno is the track number passed to the encoder.
      # If we want to merge, we substitute it with the interval, with a
      # hyphen for cdparanoia and a plus sign for cdda2wav.
      my $saveriptrackno = $riptrackno;
      if($pmerge && $merge[0]) {
         my @bea = split(/-|\+/, $merge[0]);
         if($bea[0] && $riptrackno == $bea[0]) {
            $riptrackno = shift(@merge);
            $riptrackno =~ s/-/\+/ if($ripper == 2);
            $riptrackno =~ s/\+/-/ if($ripper == 1);
            # TODO: check for dagrab and sox...
         }
      }
      # LCDproc
      if($lcd == 1) {
         my $_lcdtracks = scalar @tracksel;
         $lcdtrackno++;
         my $lcdperc;
         if($_lcdtracks eq $lcdtrackno) {
            $lcdperc = "*100";
         }
         else {
            $lcdperc = sprintf("%04.1f", $lcdtrackno/$_lcdtracks*100);
         }
         $lcdline2 =~ s/\|\d\d.\d/\|$lcdperc/;
         my $lcdtracknoF = sprintf("%02d", $lcdtrackno);
         $lcdline2 =~ s/\r\d\d/\r$lcdtracknoF/;
         substr($lcdline2,10,10) = substr($riptrackname,3,13);
         ulcd();
      }

      # There is a problem with too long file names, encountered e. g.
      # with some classical CDs. Cdparanoia cuts the length of the file
      # name, cdda2wav too...  but how should RipIT know? Therefore use
      # a shorter track name if total length (including the full path)
      # > 200 characters.
      my $rip_wavdir = $wavdir;
      if(length($riptrackname) + length($wavdir) > 200) {
         print "Warning: output trackname is longer than 200 chars,\n",
               "RipIT will use a temporary output name to for the ",
               "WAV-file.\n"
            if($verbose > 2);
         $riptrackname = get_trackname($_, $_ . "short", "short");
         # We still have problems in case total path is too long:
         $rip_wavdir = "/tmp" if(length($riptrackname) + length($wavdir) > 250);
      }

      # Check for tracks already done if option --resume is on.
      $checknextflag = 0;
      if($resumerip) {
         if($normalize == 0 and $cdcue == 0) {
            # Start the encoder in the background, but only once.
            # We do it already here, because:
            # i)  if all wavs are done, the encoding process at the end
            #     of this subroutine will not be started at all!
            # ii) why should we wait for the first missing wav, if
            #     other wavs are already here and encoding could start
            #     (continue) right away?
            if($startenc == 0 && $encode == 1) {
               $startenc = 1;
               open(ENCLOG,">$wavdir/enc.log");
               close(ENCLOG);
               unless(fork) {
                  enc_cd();
               }
            }
         }

         if(-r "$wavdir/$riptrackname.rip") {
            unlink("$wavdir/$riptrackname.rip");
            print "Found $riptrackname.rip.\n" if($verbose >= 1);
         }
         elsif(-r "$wavdir/$riptrackname\_rip.wav" && $ripper == 2) {
            unlink("$wavdir/$riptrackname\_rip.wav");
            print "Found $riptrackname\_rip.wav.\n" if($verbose >= 1);
         }
         elsif(-r "$wavdir/$riptrackname.wav") {
            $checknextflag = 1;
            print "Found $riptrackname.wav.\n" if($verbose >= 1);
            if($md5sum == 1 && $wav == 1) {
               md5_sum("$wavdir", "$riptrackname.wav", 0);
            }
         }
         elsif($wav == 0) {
            for(my $c = 0; $c <= $#coder; $c++) {
               if(-r "$sepdir[$c]/$riptrackname.$suffix[$c]") {
                  $checknextflag = 1;
                  print "Found file $riptrackname.$suffix[$c].\n"
                     if($verbose >= 1);
               }
               else {
                  $checknextflag = 2;
               }
               last if($checknextflag == 2);
            }
         }
         # Cdda2wav is somehow unpleasant. It dies not quick enough with
         # ^+c. I. e. even if a track has not been ripped to the end,
         # the *.rip file will become a *.wav. So we have to check for
         # completely encoded files and assume, that for not encoded
         # files, there is no fully ripped file. OK, perhaps it would be
         # better to check for the last *.wav file and rerip only that
         # one. But on a modern machine, the encoder won't be far from
         # catching up the ripper, so deleting all *.wavs for missing
         # encoded files won't hurt, because cdda2wav is quite fast,
         # ripping those tracks again doesn't cost a lot of time.
         if($ripper == 2 && $checknextflag == 1) {
            for(my $c = 0; $c <= $#coder; $c++) {
               if(-r "$sepdir[$c]/$riptrackname.$suffix[$c]") {
                  $checknextflag = 1;
               }
               else {
                  $checknextflag = 2;
               }
               last if($checknextflag == 2);
            }
         }
      }
      # Skip that track, i.e. restart the foreach-loop of tracks if a
      # wav file or other (mp3, ogg, flac, m4a) was found.
      next if($checknextflag == 1);
      # Don't resume anymore if we came until here.
      $resumerip = 0;

      # Now do the job of ripping:
      printf "\n%02d:%02d:%02d: ", sub {$_[2], $_[1], $_[0]}->(localtime)
         if($verbose >= 1 && $rip == 1);
      print "Ripping \"$riptrackname\"...\n"
         if($verbose >= 1 && $rip == 1);
      # Choose the cdaudio ripper to use.
      #
      # TODO: Check behaviour of all rippers on data tracks.
      # Choose to use print instead of die if ripper stops itself!
      # Dagrab fails @ data-track, so don't die and create an error.log,
      # cdparanoia fails @ data-track, so don't die and create an
      # error.log.
      # cdda2wav prints errors @ data-track, therefore die!
      if($ripper == 0 && $rip == 1) {
         if($trackcn == 1) {
            $ripopt .= " -r 3" if($parano == 0 && $ripopt !~ /\s-r\s3/);
            $ripopt .= " -v" if($verbose >= 2 && $ripopt !~ /\s-v/);
         }
         $ripcom = "(dagrab $ripopt -d $cddev \\
                    -f \"$rip_wavdir/$riptrackname.rip\" \\
                    $riptrackno 3>&1 1>&2 2>&3 \\
                    | tee -a \"$wavdir/error.log\") 3>&1 1>&2 2>&3 ";
         $ripcom =~ s/\$/\\\$/g;
         $ripcom = "nice -n $nicerip " . $ripcom if($nicerip != 0);
         unless(log_system("$ripcom")) {
            print "Dagrab detected some read errors on ",
                  "$tracklist[$_ - 1]\n\n";
            # Create error message in CD-directory for encoder: don't
            # wait.
            open(ERO,">>$wavdir/error.log")
               or print "Can not append to file ",
                        "\"$wavdir/error.log\"!\n";
            print ERO "Dagrab detected some read errors at $riptrackno";
            print ERO " on CD $artist - $album, do not worry!\n";
            close(ERO);
         }
         print "\n";
      }
      elsif($ripper == 1 && $rip == 1) {
         if($trackcn == 1) {
            $ripopt .= " -Z" if($parano == 0 && $ripopt !~ /\s-Z/);
            $ripopt .= " -q" if($verbose < 2 && $ripopt !~ /\s-q/);
         }
         # Introduce the span argument into the tracknumber, adjust the
         # tracknumber suffix according to cdparanoia and recalculate
         # the track length (used in the playlist file).
         if($span) {
            my @bea = split(/-/, $span);
            my $offset = 0;
            my $chunk = 0;
            $offset = span_length($bea[0]) if($bea[0]);
            $chunk = span_length($bea[1]) if($bea[1]);
            $bea[0] = "0.0" unless($bea[0]);
            $bea[1] = " " unless($bea[1]);
            $bea[0] = "[" . $bea[0] . "]" if($bea[0] =~ /\d+/);
            $bea[1] = "[" . $bea[1] . "]" if($bea[1] =~ /\d+/);
            if($riptrackno =~ /-/) {
               my($i, $j) = split(/-/, $riptrackno);
               # Special case: if the chunk of sound is larger than the
               # (last) track, use the true track length instead of chunk
               # size.
               if($hiddenflag == 0 && $secondlist[$j - 1] < $chunk) {
                  $chunk = 0;
                  $bea[1] = " ";
               }
               elsif($hiddenflag == 1 && $secondlist[$j] < $chunk) {
                  $chunk = 0;
                  $bea[1] = " ";
               }
               if($chunk <= 0) {
                  $chunk = $secondlist[$j - 1] if($hiddenflag == 0);
                  $chunk = $secondlist[$j] if($hiddenflag == 1);
               }
               $secondlist[$_ - 1] = $secondlist[$_ - 1] - $secondlist[$j - 1] + $chunk - $offset if($hiddenflag == 0);
               $secondlist[$_] = $secondlist[$_] - $secondlist[$j] + $chunk - $offset if($hiddenflag == 1);
               $riptrackno = $i . $bea[0] . "-" . $j . $bea[1];
            }
            else {
               # Special case: if the chunk of sound is larger than the
               # (last) track, use the true track length instead of chunk
               # size.
               if($hiddenflag == 0 && $secondlist[$_ - 1] < $chunk) {
                  $chunk = 0;
                  $bea[1] = " ";
               }
               elsif($hiddenflag == 1 && $secondlist[$_] < $chunk) {
                  $chunk = 0;
                  $bea[1] = " ";
               }
               $riptrackno = $riptrackno . $bea[0] . "-" . $riptrackno . $bea[1];
               # Variable $chunk is zero if span reaches the end of the
               # track.
               if($chunk <= 0) {
                  $chunk = $secondlist[$_ - 1] if($hiddenflag == 0);
                  $chunk = $secondlist[$_] if($hiddenflag == 1);
               }
               $chunk -= $offset;
               $secondlist[$_ - 1] = $chunk if($hiddenflag == 0);
               $secondlist[$_] = $chunk if($hiddenflag == 1);
            }
         }
         if($multi == 0) {
            # Handle special paranoia mode for single failed tracks.
            my $save_ripopt = $ripopt;
            my $save_failflag = $failflag;
            if($parano == 2 && $failflag == 1) {
               $ripopt = $ripopt . " -Z" if($parano == 2);
               print "\n\nTrying again without paranoia.\n"
                  if($verbose > 1);
            }
            # Make sure $failflag is set to 0 if success.
            $failflag = 0;
            $ripcom = "cdparanoia -d $cddev $riptrackno $ripopt \\
               \"$rip_wavdir/$riptrackname.rip\"";
            $ripcom =~ s/\$/\\\$/g;
            $ripcom = "nice -n $nicerip " . $ripcom if($nicerip != 0);
            unless(log_system("$ripcom")) {
               print "cdparanoia failed on track ", $_,
                     " $tracklist[$_ - 1]\n\n" if($hiddenflag == 0);
               print "cdparanoia failed on track ", $_,
                     " $tracklist[$_]\n\n" if($hiddenflag == 1);
               # Create error message in CD-directory for encoder:
               # don't wait.
               if($parano == 2 && $save_failflag == 1 || $parano < 2 ) {
                  open(ERO,">>$wavdir/error.log")
                     or print "Can not append to file ",
                              "\"$wavdir/error.log\"!\n";
                  print ERO "Track $saveriptrackno on CD $artist - $album ";
                  print ERO "failed!\n";
                  close(ERO);
               }
               $failflag = $save_failflag + 1;
            }
            $ripopt = $save_ripopt;
         }
         elsif($multi == 1) {
            my $save_ripopt = $ripopt;
            my $save_failflag = $failflag;
            if($parano == 2 && $failflag == 1) {
               $ripopt .= " -Z" if($parano == 2);
               print "\n\nTrying again without paranoia.\n"
                  if($verbose > 1);
            }
            $ripcom = "cdparanoia -d $cddev $riptrackno $ripopt \\
               \"$rip_wavdir/$riptrackname.rip\"";
            # Log the ripping output only when using paranoia!
            $ripcom .= " 2>> \"$logfile.$saveriptrackno.txt\""
               if($parano == 2 && $failflag == 1 || $parano < 2 );
            $ripcom =~ s/\$/\\\$/g;
            $ripcom = "nice -n $nicerip " . $ripcom if($nicerip != 0);
            $failflag = 0;
            unless(log_system("$ripcom")) {
               if($parano == 2 && $save_failflag == 1 || $parano < 2 ) {
                  # Append error message to file srXY for rip2m to start
                  # checktrack.
                  open(SRXY,">>$logfile")
                     or print "Can not append to file \"$logfile\"!\n";
                  print SRXY "\ncdparanoia failed on $tracklist[$_ - 1] "
                     if($hiddenflag == 0);
                  print SRXY "\ncdparanoia failed on $tracklist[$_] "
                     if($hiddenflag == 1);
                  print SRXY "in device $logfile";
                  close(SRXY);
                  # Create error message in CD-directory for encoder:
                  # don't wait.
                  open(ERO,">>$wavdir/error.log")
                     or print "Can not append to file ",
                              "\"$wavdir/error.log\"!\n";
                  print ERO "Track $saveriptrackno on CD $artist - $album ";
                  print ERO "failed!\n";
                  close(ERO);
                  # Kill failed CD only if it is not the last track. Last
                  # track may be data/video track.
                  # I.e. print error message to file srXY.Z.txt, checktrack
                  # will grep for string
                  # "cdparanoia failed" and kill the CD immediately!
                  if($riptrackno != $tracksel[$#tracksel]) {
                     open(SRTF,">>$logfile.$saveriptrackno.txt")
                        or print "Can not append to file ",
                                 "\"$logfile.$saveriptrackno.txt\"!\n";
                     print SRTF "cdparanoia failed on $tracklist[$_ - 1]"
                        if($hiddenflag == 0);
                     print SRTF "cdparanoia failed on $tracklist[$_ - 1]"
                        if($hiddenflag == 1);
                     print SRTF "\nin device $logfile, error !";
                     close(SRTF);
                     # Create on the fly error message in log-directory.
                     my $devnam = $cddev;
                     $devnam =~ s/.*dev.//;
                     open(ERO,">>$outputdir/failed.log")
                        or print "Can not append to file ",
                                 "\"$outputdir/failed.log\"!\n";
                     print ERO "$artist;$album;$genre;$categ;$cddbid;";
                     print ERO "$devnam;$hostnam; Cdparanoia failure!\n";
                     close(ERO);
                     # Now wait to be terminated by checktrack.
                     sleep 360;
                     exit;
                  }
               }
               $failflag = $save_failflag + 1;
            }
            $ripopt = $save_ripopt;
         }
         # This is an awkward workaround introduced because of the
         # enhanced --paranoia option. Failures on data tracks are not
         # captured anymore. Force update of error.log for encoder.
         # Remember, because of option --span $riptrackno can be a
         # string. Use $saveriptrackno instead.
         if(! -f "$rip_wavdir/$riptrackname.rip") {
            if($saveriptrackno == $tracksel[$#tracksel] &&
               $riptrackname =~ /data|video/i) {
               open(ERO,">>$wavdir/error.log")
                  or print "Can not append to file ",
                           "\"$wavdir/error.log\"!\n";
               print ERO "Track $saveriptrackno on CD $artist - $album ";
               print ERO "failed!\n";
               close(ERO);
               if($multi == 1) {
                  # Append error message to file srXY for rip2m to start
                  # checktrack.
                  open(SRXY,">>$logfile")
                     or print "Can not append to file \"$logfile\"!\n";
                  print SRXY "\ncdparanoia failed on $tracklist[$_ - 1] "
                     if($hiddenflag == 0);
                  print SRXY "\ncdparanoia failed on $tracklist[$_] "
                     if($hiddenflag == 1);
                  print SRXY "in device $logfile";
                  close(SRXY);
               }
               # Misuse of variable failflag, we don't care, it's the
               # last track!
               $failflag = 3;
            }
            else {
               print "\nRip file $riptrackname.rip not found...\n"
               if($verbose > 2);
            }
         }
      }
      elsif($ripper == 2 && $rip == 1) {
         if($trackcn == 1) {
            $ripopt .= " -q" if($verbose <= 1 && $ripopt !~ /\s-q/);
         }
         $ripcom = "cdda2wav -D $cddev -H $ripopt ";
         # Introduce the span argument into the tracknumber and recalculate the track
         # length (used in the playlist file). We use $duration instead of $chunk in the cdparanoia part above.
         if($span) {
            my @bea = split(/-/, $span);
            my $offset = 0;
            my $duration = 0;
            $offset = span_length($bea[0]) if($bea[0]);
            $duration = span_length($bea[1]) if($bea[1]);
            if($riptrackno =~ /\+/) {
               my($i, $j) = split(/\+/, $riptrackno);
               if($hiddenflag == 0) {
                  if($secondlist[$j - 1] < $duration) {
                     $duration = 0;
                  }
                  else {
                     $duration = $secondlist[$_ - 1] = $secondlist[$_ - 1] - $secondlist[$j - 1] + $duration - $offset;
                  }
               }
               elsif($hiddenflag == 1) {
                  # TODO: Oops, why is the counter reduced?
                  if($secondlist[$j - 1] < $duration) {
                     $duration = 0;
                  }
                  else {
                     $duration = $secondlist[$_] = $secondlist[$_] - $secondlist[$j] + $duration - $offset;
                  }
               }
            }
            else {
               if($hiddenflag == 0 && $secondlist[$_ - 1] < $duration) {
                  $duration = 0;
               }
               elsif($hiddenflag == 1 && $secondlist[$_] < $duration) {
                  $duration = 0;
               }
               else {
                  $duration -= int($offset);
                  $secondlist[$_ - 1] = $duration if($hiddenflag == 0);
                  $secondlist[$_] = $duration if($hiddenflag == 1);
               }
            }
            $duration = 0 if($duration < 0);
            $offset *= 75;
            $ripcom .= "-o $offset ";
            $ripcom .= "-d $duration " if($duration > 0);
         }
         if($multi == 0) {
            $ripcom .= "-t $riptrackno \"$rip_wavdir/$riptrackname\_rip\"";
            $ripcom = "nice -n $nicerip " . $ripcom if($nicerip != 0);
            $ripcom =~ s/\$/\\\$/g;
            unless(log_system("$ripcom")) {
               print "cdda2wav failed on <$tracklist[$_ - 1]>.\n"
                     if($hiddenflag == 0);
               print "cdda2wav failed on <$tracklist[$_]>.\n"
                     if($hiddenflag == 1);
               open(ERO,">>$wavdir/error.log")
                  or print "Can not append to file ",
                           "\"$wavdir/error.log\"!\n";
               print ERO "Track $saveriptrackno on CD $artist - $album ";
               print ERO "failed!\n";
               close(ERO);
               $failflag++;
            }
         }
         elsif($multi == 1) {
            $ripcom = "-t $riptrackno \"$rip_wavdir/$riptrackname\_rip\" \\
               2>> \"$logfile.$saveriptrackno.txt\"";
            $ripcom = "nice -n $nicerip " . $ripcom if($nicerip != 0);
            $ripcom =~ s/\$/\\\$/g;
            unless(log_system("$ripcom")) {
               # Append error message to file srXY for rip2m to start
               # checktrack.
               open(SRXY,">>$logfile")
                  or print "Can not append to file \"$logfile\"!\n";
               print SRXY "\ncdda2wav failed on $tracklist[$_ - 1] in "
                  if($hiddenflag == 0);
               print SRXY "\ncdda2wav failed on $tracklist[$_] in "
                  if($hiddenflag == 1);
               print SRXY "device $logfile";
               close(SRXY);
               # Create error message in CD-directory for encoder:
               # don't wait.
               open(ERO,">>$wavdir/error.log")
                  or print "Can not append to file ",
                           "\"$wavdir/error.log\"!\n";
               print ERO "Track $saveriptrackno on CD $artist - $album ";
               print ERO "failed!\n";
               close(ERO);
               # Kill failed CD only if it is not the last track.
               # Last track may be data/video track.
               # I.e. print error message to file srXY.Z.txt, checktrack
               # will grep for string
               # "cdparanoia failed" and kill the CD immediately!
               if($riptrackno != $tracksel[$#tracksel]) {
                  open(SRTF,">>$logfile.$saveriptrackno.txt")
                     or print "Can not append to file ",
                              "\"$logfile.$saveriptrackno.txt\"!\n";
                  print SRTF "cdda2wav failed on $tracklist[$_ - 1]\n"
                     if($hiddenflag == 0);
                  print SRTF "cdda2wav failed on $tracklist[$_]\n"
                     if($hiddenflag == 1);
                  print SRTF "in device $logfile, error !";
                  close(SRTF);
                  # Create on the fly error message in log-directory.
                  my $devnam = $cddev;
                  $devnam =~ s/.*dev.//;
                  open(ERO,">>$outputdir/failed.log")
                     or print "Can not append to file ",
                              "\"$outputdir/failed.log\"!\n";
                  print ERO "$artist;$album;$genre;$categ;$cddbid;";
                  print ERO "$devnam;$hostnam; Cdda2wav failure!\n";
                  close(ERO);
                  # Now wait to be terminated by checktrack.
                  sleep 360;
                  exit;
               }
            }
         }
         print "\n" if($verbose > 1);
      }
      elsif($ripper == 3 && $rip == 1) {
         $ripcom = "tosha -d $cddev -f wav -t $riptrackno \\
            -o \"$rip_wavdir/$riptrackname.rip\"";
         $ripcom = "nice -n $nicerip " . $ripcom if($nicerip != 0);
         unless(log_system("$ripcom")) {
            die "tosha failed on $tracklist[$_ - 1]";
         }
      }
      elsif($ripper == 4 && $rip == 1) {
         my $cdd_dev = $cddev;
         $cdd_dev =~ s/^\/dev\/r//;
         $cdd_dev =~ s/c$//;
         $ripcom = "cdd -t $riptrackno -q -f $cdd_dev - 2> /dev/null \\
                   | sox -t cdr -x - \"$rip_wavdir/$riptrackname.wav\"";
         $ripcom = "nice -n $nicerip " . $ripcom if($nicerip != 0);
         unless(log_system("$ripcom")) {
            die "cdd failed on $tracklist[$_ - 1]";
         }
      }
      elsif($rip == 1) {
         print "No CD Ripper defined.\n";
      }

      redo if($ripper == 1 && $failflag == 1 && $parano == 2);

      # If we had problems in case total path is too long (note: the
      # riptrackname is still the short one).
      if(length($riptrackname) + length($wavdir) > 250) {
         log_system("cd \"$wavdir\" && mv \"/tmp/$riptrackname.rip\" \"$riptrackname.rip\"");
      }

      # Cdda2wav output is not easy to handle. Everything beyond a last
      # period . has been erased. Example: riptrackname is something
      # like "never ending...", then we assign cdda2wav in the above
      # section to rip a file called: "never ending..._rip", but
      # cdda2wav misbehaves and the file is called "never ending...".
      # Therefore we rename the ripped file to the standard name
      # riptrackname.rip first (if cdda2wav was used).
      if($ripper == 2) {
         if($riptrackname =~ /\./) {
            # But split is too clever! If a trackname ends with "bla..."
            # all points get lost, so we've to add a word at the end!
            my $cddatrackname = $riptrackname . "end";
            my @riptrackname = split(/\./, $cddatrackname);
            delete($riptrackname[$#riptrackname]);
            $cddatrackname = join('.',@riptrackname);
            rename("$wavdir/$cddatrackname.wav",
                   "$wavdir/$riptrackname.rip");
         }
         else {
            rename("$wavdir/$riptrackname\_rip.wav",
                   "$wavdir/$riptrackname.rip");
         }
      }
      # Check for gaps and silence in tracks.
      my @times = (0);
      my $save_cdtocn = $cdtocn;
      if(-r "$wavdir/$riptrackname.rip") {
         # Remember: $saveriptrackno is the single track number, whereas
         # $riptrackno may hold an interval if option merge is used.
         if($ghost == 1 && $failflag == 0) {
            @times = get_chunks($saveriptrackno, $riptrackname);
            unless($times[0] eq "blank") {
               (my $shorten, @times) =
                  split_chunks($saveriptrackno, "$riptrackname",
                               $cdtocn, @times);
               ($cdtocn, $cue_point) =
                  rename_chunks($saveriptrackno, "$riptrackname",
                                $cdtocn, $cue_point, $shorten,
                                $artistag, $riptracktag, @times);
               }
         }
      }
      # A blank track has been deleted.
      $cdtocn-- if(($cdtoc == 1 || $cdcue > 0) && $times[0] eq "blank");
      next if($times[0] eq "blank");
      #
      # Final stuff.
      # Rename rip file to a wav for encoder so that it will be picked
      # up by the encoder background process.
      # If the track has been splited into chunks, check if the filename
      # holds information about the ghost song. If so, don't use it in
      # the file name!
      if($save_cdtocn < $cdtocn) {
         if($riptracktag =~ /\//) {
            my ($wavname, $dummy) = split(/\//, $riptracktag);
            $wavname =~ s/^\s+|\s+$//;
            # The new riptracktag is needed for inf files.
            $riptracktag = $wavname;
            $wavname = clean_all($wavname);
            $wavname = clean_name($wavname);
            $wavname = clean_chars($wavname) if($chars);
            $wavname = change_case($wavname);
            $wavname =~ s/ /_/g if($underscore == 1);
            $wavname = get_trackname($saveriptrackno, $wavname);
            rename("$wavdir/$riptrackname.rip", "$wavdir/$wavname.wav");
            $riptrackname = $wavname;
         }
         else {
            rename("$wavdir/$riptrackname.rip", "$wavdir/$riptrackname.wav");
         }
      }
      else {
         rename("$wavdir/$riptrackname.rip", "$wavdir/$riptrackname.wav");
      }
      # Delete the "single-track" wav if cdcue is used. The track is
      # already merged, no need to keep it.
      unlink("$wavdir/$riptrackname.wav")
         if($sshflag == 0 && $cdcue > 0);
      md5_sum("$wavdir", "$riptrackname.wav", 0)
         if($md5sum == 1 && $normalize == 0 &&
            $wav == 1 && $failflag == 0);
      # Writing inf files for cdburning.
      # We use the $save_cdtocn counter as track counter instead of the
      # $riptrackno because $riptrackno might hold a span argument and
      # does not reflect the exact number of tracks created.
      # Use failflag == 3 to prevent writing inf file for failed data
      # track.
      if($inf >= 1 && $failflag < 3) {
         $trackstart = write_inf($wavdir, $riptrackname, $artistag,
           $albumtag, $riptracktag, $save_cdtocn, $cdtocn, $trackstart);
      }
      chmod oct($fpermission), "$wavdir/$riptrackname.wav"
         if($fpermission);
      unlink("$logfile.$riptrackno.txt") if($multi == 1);
      $failflag = 0;

      if($normalize == 0 and $cdcue == 0) {
         # Start the encoder in the background, but only once.
         if($startenc == 0 && $encode == 1) {
            my $encstart = sprintf("%02d:%02d",
               sub {$_[2], $_[1]}->(localtime));
            chomp $encstart;
            if($multi == 1) {
               open(SRXY,">>$logfile")
                  or print "Can not append to file \"$logfile\"!\n";
               print SRXY "\nEncoding started: $encstart";
               close(SRXY);
            }
            $startenc = 1;
            open(ENCLOG,">$wavdir/enc.log");
            close(ENCLOG);
            unless(fork) {
               enc_cd();
            }
         }
      }
      # Print encoder messages saved in enc.log not to spoil the
      # ripper output. Maybe it would be better to test existence of the
      # file instead of testing all these conditions.
      if($encode == 1 && $normalize == 0 && $cdcue == 0) {
         open(ENCLOG, "< $wavdir/enc.log");
         my @loglines = <ENCLOG>;
         close(ENCLOG);
         my $lincn = 0;
         my @outlines = ();
         foreach (@loglines) {
            if($verbose >= 3) {
               push(@outlines, $_)
                  if($lincn >= $encline && $_ !~ /^\n/);
            }
            elsif($verbose == 1 || $verbose == 2) {
               print $_ if($lincn >= $encline && $_ =~ /complete\./);
            }
            $lincn++;
         }
         # Compact output.
         $encline = $lincn;
         if($outlines[0] && $verbose >= 1) {
            if($trackcn <= $#tracksel) {
               push(@outlines, "*" x 47, "\n") if($verbose >= 3);
               unshift(@outlines, "*" x 15, " Encoder reports ", "*" x 15, "\n") if($verbose >= 3);
            }
            else {
               print "\n", "*" x 47, "\nWaiting for encoder to finish...\n\n";
            }
            print @outlines if($verbose >= 2);
         }
      }
   }
   unlink("$wavdir/enc.log") if(-r "$wavdir/enc.log");

   # Hack to tell the child process that we are waiting for it to
   # finish.
   my $ripend = sprintf("%02d:%02d", sub {$_[2], $_[1]}->(localtime));
   open(ERR, ">>$wavdir/error.log")
      or print "Can not append to file error.log!\n";
   print ERR "The audio CD ripper reports: all done!\n";
   print ERR "Ripping ended: $ripend\n";
   close(ERR);
   if($multi == 1) {
      open(SRXY,">>$logfile")
         or print "Can not append to file \"$logfile\"!\n";
      print SRXY "\nRipping complete: $ripend";
      close(SRXY);
   }
}
########################################################################
#
# Normalize the wav.
# Using normalize will disable parallel ripping & encoding.
#
sub norm_cd {

   print "Normalizing the wav-files...\n" if($verbose >= 1);
   my($escdir, $norm, $normtrackname);
   $escdir = $wavdir;
   $escdir = esc_char($escdir);

   # Generate filelist to be processed:
   foreach (@seltrack) {
      my $riptrackname = get_trackname($_, $tracklist[$_ - 1]);
      $riptrackname = get_trackname($_, $tracklist[$_])
         if($hiddenflag == 1);
      # If the file name was too long for ripper, look for special name.
      my $wavname = $riptrackname;
      if(length($riptrackname) + length($wavdir) > 200) {
         $wavname = get_trackname($_, $_."short", "short");
      }
      # Normalize is picky about certain characters - get them escaped!
      $wavname = esc_char($wavname);
      $normtrackname .= "$escdir/$wavname.wav" . " \\\n          ";
   }
   $normtrackname =~ s/\s*$//;
   $normtrackname =~ s/\$/\\\$/g;

   # Add verbosity:
   $normopt .= "q" if($verbose == 0);
   $normopt .= "v" if($verbose >= 2 && $normopt !~ /q/);
   $normopt .= "vv" if($verbose >= 4 && $normopt !~ /q/);

   $norm = "$normcmd $normopt -- $normtrackname";

   if(log_system("$norm")) {
      log_info("\nNormalizing complete.\n");
      print "\nNormalizing complete.\n" if($verbose >= 1);
   }
   else {
      print "\nWarning: normalizing failed.\n";
   }
}
########################################################################
#
# Encode the wav.
# This runs as a separate process from the main program which
# allows it to continuously encode as the ripping is being done.
# The encoder will also wait for the ripped wav in-case the encoder
# is faster than the CDROM. In fact it will be waited 3 times the length
# of the track to be encoded.
#
sub enc_cd {

   my ($enc, $riptrackno, $riptrackname, $suffix, $tagtrackno);
   my ($albumlametag, $artislametag, $commentlametag, $tracklametag);
   my ($ripcomplete, $trackcn, $totalencs) = (0, 0, 0);
   my $lastskip = $tracksel[0];
   my $resumenc = $resume;
   my $encodername = "";
   my @md5tracks = ();  # List of tracks to be re-taged (coverart etc.).

   # Cleaning.
   my $albumtag = clean_all($album_utf8);
   my $artistag = clean_all($artist_utf8);
   my $album = $albumtag;
   my $artist = $artistag;
   $album = clean_name($album);
   $artist = clean_name($artist);
   $album = clean_chars($album) if($chars);
   $artist = clean_chars($artist) if($chars);
   $album =~ s/ /_/g if($underscore == 1);
   $artist =~ s/ /_/g if($underscore == 1);

   # Create special variables for Lame-tags because of UTF8 problem.
   if($utftag == 0) {
      $albumlametag = back_encoding($albumtag);
      $commentlametag = back_encoding($commentag);
   }
   else{
      $albumlametag = $albumtag;
      $commentlametag = $commentag;
   }

   # Write header of playlist file.
   my $playfile;
   if($playlist >= 1) {
      $playfile = "$artist" . " - " . "$album" . ".m3u";
      $playfile =~ s/ /_/g if($underscore == 1);
      if($limit_flag == 255) {
         $playfile = substr($playfile, 0, 250);
      }
      open(PLST, ">$wavdir/$playfile") or
         print "Can't open $wavdir/$playfile! $!\n";
      print PLST "#EXTM3U\n";
   }

   # Read the cdcue file (once) to be copied to the encoder directories.
   my @cuelines = ();
   if($cdcue > 0) {
      open(CUE, "<$wavdir/cd.cue")
         or print "Can not read file cue sheet!\n";
      @cuelines = <CUE>;
      close(CUE);
   }

   # If using book-option define a chapter file.
   my $chapterfile;
   if($book >= 1) {
      $chapterfile = "$artist" . " - " . "$album" . ".chapters.txt";
      $chapterfile =~ s/ /_/g if($underscore == 1);
      if($limit_flag == 255) {
         $chapterfile = substr($chapterfile, 0, 235);
         $chapterfile .= ".chapters.txt";
      }
   }

   my $ghostflag = 0;
   my $ghostcn = 0;

   if($commentag =~ /^discid|cddbid$/) {
      if($commentag =~ /^discid$/) {
         $commentag = $cd{discid}
      }
      elsif($commentag =~ /^cddbid$/) {
         $commentag = $cd{id};
      }
      $commentag = "" unless($commentag);
      $commentlametag = $commentag;
   }

   # Prevent using genre "other" if genre is not lame compliant but
   # other encoders than Lame are used:
   my $genre_tag = $genre;
   $genre_tag = $cd{genre} if($genre =~ /Other/ && $cd{genre} !~ /Other/);

   # Create a coverart array supposing its exactly in the same order as
   # encoder array.
   my @coverart = ();
   if($coverart) {
      @coverart = split(/,/, $coverart);
   }

   # Loop all tracknames for a last VA style detection needed for
   # tagging the files. Note that nothing is known about ghost songs.
   my $delim = check_va(0);

   # Start encoding each track.
   foreach (@tracksel) {
      # A lot of hacking for ghost songs. Remember, array @tracksel is
      # the original one, without ghost songs as long as we did not get
      # to the end. Once all tracks are done, this array will be
      # updated if ghost songs were found by the ripper.
      # Now: if only one track in the middle of the album has been
      # selected, problems occur if this track has ghost songs. Why?
      # Because the updated array @tracksel will be e.g. 4 4 4 4 if the
      # track 4 has 3 ghost songs. But the track-list and tag-list
      # arrays have all tracknames of the whole CD, so after track
      # number 4 will come track number 5! Therefor no track
      # "04 name of track 5" will be found and the encoder fails!
      # To prevent this: Once all (selected) tracks are done, we have to
      # set the $ghostcn to the total number of tracks of the CD to
      # access names of ghost songs added to the list by the ripper.
      $ghostflag = 2 if($ghostflag == 1 && $riptrackno >= $_);
      $ghostcn = $#{$cd{track}} + 1 if($ghostflag == 0);
      $riptrackno = $_;
      $tagtrackno = $_ + $trackoffset;
      $trackcn++;
      # A new problem araises if the track names of ghost (and original)
      # songs are changed (if the track name with ghost song has a
      # slash in). In this case, the resume option and the part that
      # waits for the ripped files to appear will fail. In this case we
      # need to check ghost.log. But the ghost.log is not yet present if
      # ripper is still ripping that file (the resume function in the
      # ripper process failed for the same reason). So we don't care
      # here and want the resume function fail again. An additional test
      # will be done in the waiting part below.

      $riptrackname = get_trackname($_, $tracklist[$_ - 1]);
      $riptrackname = get_trackname($_, $tracklist[$_])
         if($hiddenflag == 1);

      if($ghostflag >= 1) {
         $ghostcn++;
         $riptrackname = get_trackname($_, $tracklist[$ghostcn - 1]);
         $riptrackname = get_trackname($_, $tracklist[$ghostcn])
            if($hiddenflag == 1);
      }

      # Once the file is ripped and merged, it is called $album, no
      # matter if $cdcue == 1 or 2.
      $riptrackname = $album if($book == 1 or $cdcue > 0);
      # If we want to merge, skip a previously merged track:
      my $skipflag = 0;
      if($pmerge) {
         @skip = skip_tracks;
         foreach my $skip (@skip) {
            $skipflag = 1 if($_ == $skip);
         }
         if($book == 1) {
            # Search the index number of encoder faac.
            my $index = 0;
            for(my $c = 0; $c <= $#coder; $c++) {
               $index = $c if($coder[$c] == 3);
            }
            # Write the *.chapter.txt file.
            open(CHAP, ">>$sepdir[$index]/$chapterfile") or
               print "Can't open $sepdir[$index]/$chapterfile! $!\n";
            # Use timestamps, not the true track lengths. Where are the
            # specifications, please?
            my $points = chapter_length($framelist[$_ - 1] - $framelist[0]);
            my $chapname = $tracktags[$_ - 1];
            # Remember: merge writes all merged tracknames into the
            # first track of an interval.
            $chapname =~ s/\s\+\s.*$// if($_ == 1);
            print CHAP "$points $chapname\n";
            close(CHAP);
         }
      }
      next if($skipflag == 1);
      $lastskip = $_;

      # LCDproc
      if($lcd == 1) {
         my $_lcdtracks = scalar @tracksel;
         my $_lcdenctrack = $trackcn;
         my $lcdperc;
         if($_lcdtracks eq $_lcdenctrack) {
            $lcdperc = "*100";
         }
         else {
            $lcdperc = sprintf("%04.1f", $_lcdenctrack / $_lcdtracks * 100);
         }
         $lcdline3 =~ s/\|\d\d.\d/\|$lcdperc/;
         my $_lcdenctrackF = sprintf("%02d", $_lcdenctrack);
         $lcdline3 =~ s/\E\d\d/\E$_lcdenctrackF/;
         substr($lcdline3, 10, 10) = substr($riptrackname, 3, 13);
         ulcd();
      }

      # Adjust encoding of tracktag for Lame.
      my $tracktag = $tracktags[$_ - 1];
      $tracktag = $tracktags[$_] if($hiddenflag == 1);
      if($ghostflag >= 1) {
         $tracktag = $tracktags[$ghostcn - 1];
         $tracktag = $tracktags[$ghostcn] if($hiddenflag == 1);
      }

      # Split the tracktag into its artist part and track part if
      # VA style is used.
      if($va_flag > 0 && $tracktag =~ /$delim/) {
         $artistag = "";
         if($vatag % 2 == 1) {
            ($artistag, $tracktag) = split(/$delim/, $tracktag);
            $tracktag =~ s/\)// if($delim =~ /\(/);
            $tracktag =~ s/^\s*//;
            $artistag =~ s/\s*$//;
            # If artistag got all info, rather use it as tracktag...
            if($tracktag eq "") {
               $tracktag = $artistag;
               $artistag = "";
            }
         }
         else {
            ($tracktag, $artistag) = split(/$delim/, $tracktag);
            $artistag =~ s/\)// if($delim =~ /\(/);
            $artistag =~ s/^\s*//;
            $tracktag =~ s/\s*$//;
         }
      }

      if($utftag == 0) {
         $tracklametag = back_encoding($tracktag);
         $artislametag = back_encoding($artistag);
      }
      else{
         $tracklametag = $tracktag;
         $artislametag = $artistag;
      }
      $artistag = clean_all($artist_utf8) if($artistag eq "");

      $tracktag = $album if($cdcue > 0);
      # If the file name was too long for ripper, look for special name.
      my $wavname = $riptrackname;
      if(length($riptrackname) + length($wavdir) > 200) {
         $wavname = get_trackname($_, $_."short", "short");
      }

      # Check for tracks already done.
      my $checknextflag = 1;
      if($resumenc) {
         for(my $c=0; $c<=$#coder; $c++) {
            if(! -r "$sepdir[$c]/$riptrackname.$suffix[$c]") {
               $checknextflag = 0;
            }
            else{
               print "Found $riptrackname.$suffix[$c]:\n"
                  if($verbose >= 1);
               print "Will calculate and write md5sum for:\n"
                  if($verbose >= 4 && $md5sum == 1);
               print "$sepdir[$c], $riptrackname.$suffix[$c]\n"
                  if($verbose >= 4 && $md5sum == 1);
            }
            last if($checknextflag == 0);
         }
         if($checknextflag == 1 && $playlist >= 1) {
            print PLST "#EXTINF:$secondlist[$_ - 1],$tracktag\n"
               if($hiddenflag == 0);
            print PLST "#EXTINF:$secondlist[$_],$tracktag\n"
               if($hiddenflag == 1);
            print PLST "Sepdir/$riptrackname.suffix\n"
               if($playlist == 1);
            print PLST "$riptrackname.suffix\n" if($playlist == 2);
            print PLST "Add Ghost Song $_ Here.\n" if($ghost == 1);
         }
         unlink("$wavdir/$riptrackname.wav")
            if($wav == 0 && $sshflag == 0 && $checknextflag == 1);
      }
      # Skip that track, i. e. restart the foreach-loop of tracks if a
      # compressed file (mp3, ogg, ma4, flac) was found.
      next if($resumenc && $checknextflag == 1);
      # Don't resume anymore, if we came until here.
      $resumenc = 0;

      # Keep looping until the wav file appears, i.e. wait for
      # ripper timeout. Timeout is 3 times the length of track
      # to rip/encode. Then leave that one and finish the job!
      my $slength = $secondlist[$_ - 1];
      my $mlength = (int($slength / 60) + 1) * 3;
      my $tlength = (int($slength / 10) + 6) * 3;

      # We don't need this for ghost songs, as they are done only when
      # the (original) last track was successfully ripped.
      my $dataflag = 0;
      my $xtime = 0;
      my $ripsize = 0;
      while(! -r "$wavdir/$wavname.wav" && $ghostflag == 0) {
         $xtime++;
         last if($xtime > $tlength);
         # There might be a ghost song with an other name. If ripping
         # is done, ghost.log would help, but the ghost.log file
         # might not be present yet!
         if($ghost == 1) {
            my ($ghost_rtn, $dummy) = split(/\//, $tracktag);
            if($ghost_rtn) {
               $ghost_rtn =~ s/^\s+|\s+$//;
               my $ghost_trt = $ghost_rtn;
               $ghost_rtn = clean_all($ghost_rtn);
               $ghost_rtn = clean_name($ghost_rtn);
               $ghost_rtn = clean_chars($ghost_rtn) if($chars);
               $ghost_rtn = change_case($ghost_rtn);
               $ghost_rtn =~ s/ /_/g if($underscore == 1);
               $ghost_rtn = get_trackname($riptrackno, $ghost_rtn);
               # Rename the riptrackname to wavname to exit the
               # while-loop. Do it only when the wav appeared and the
               # rip file disappeared in case it's the last track and
               # the following check of ghost.log is mandatory. Else
               # we would leave this loop and possibly read an old
               # ghost.log not yet updated, because ripper is still
               # ripping.
               if(!-r "$wavdir/$wavname.rip" and
                  !-r "$wavdir/$ghost_rtn.rip" and
                  -r "$wavdir/$ghost_rtn.wav") {
                     $wavname = $riptrackname = $ghost_rtn;
                     $tracktag = $ghost_trt;
                     if($utftag == 0) {
                        $tracklametag = back_encoding($tracktag);
                     }
                     else{
                        $tracklametag = $tracktag;
                     }
               }
           }
         }
         # Condition 1: Too long waiting for the track!
         if($xtime >= $tlength) {
            # If the rip file has been found, give a chance to
            # continue if the rip-file increases in size.
            my $old_ripsize = $ripsize;
            #
            if(-r "$wavdir/$wavname.rip") {
               $ripsize = -s "$wavdir/$wavname.rip";
            }
            if($multi != 1) {
               if($ripsize > $old_ripsize * 1.2) {
                  $tlength = $tlength * 1.5;
               }
               else {
                  print "Encoder waited $mlength minutes for file\n";
                  print "$riptrackname.wav to appear, now giving up!\n";
                  print "with $artist - $album in device $cddev\n";
                  log_info("Encoder waited $mlength minutes for file");
                  log_info("$riptrackname.wav to appear, now giving up!");
                  log_info("with $artist - $album in device $cddev");
               }
            }
            else {
               print "Encoder waited $mlength minutes for file\n";
               print "$riptrackname.wav to appear\n";
               print "with $artist - $album in device $cddev.\n";
               # If the rip file has been found, give a chance to
               # continue if the rip-file increases in size.
               if(-r "$wavdir/$wavname.rip") {
                  if($ripsize > $old_ripsize * 1.2) {
                     $tlength = $tlength * 1.5;
                  }
                  else {
                     $xtime = 0 unless($riptrackname =~ /00 Hidden Track/);
                     open(ERR, ">>$wavdir/error.log");
                     print ERR "Ripping ended: 00:00!\n";
                     close(ERR);
                  }
               }
               else {
                  $xtime = 0 unless($riptrackname =~ /00 Hidden Track/);
                  open(ERR, ">>$wavdir/error.log");
                  print ERR "Ripping ended: 00:00!\n";
                  close(ERR);
               }
            }
         }
         sleep 10;
         # Condition 2: Check the error log!
         # If at this moment the ripper did not start with
         # the riptrackname.rip, assume it was a data track!
         # If cdparanoia failed on a data track, there will
         # be an entry in the error.log.
         # If dagrab gave error messages, but the wav file
         # was created, we won't get to this point, so don't
         # worry.
         if(-r "$wavdir/error.log") {
             open(ERR, "$wavdir/error.log")
               or print "Encoder can't read $wavdir/error.log!\n";
            my @errlines = <ERR>;
            close(ERR);
            # Note that the ripper wrote the $savetrackno into the
            # errorlog, we check for $riptrackno not $tagtrackno.
            chomp(my $errtrack = join(' ', grep(/^Track $riptrackno /, @errlines)));
            if($errtrack) {
               $xtime = $tlength + 1;
               $dataflag = 1;
               if($verbose >= 2) {
                  if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
                     open(ENCLOG, ">>$wavdir/enc.log");
                     print ENCLOG "\nDid not detect track $errtrack ",
                                  "($riptrackname.rip),\n assume ",
                                  "ripper failure!\n";
                     close(ENCLOG);
                  }
                  else {
                     print "\nDid not detect track $errtrack ",
                           "($riptrackname.rip), assume ripper ",
                           "failure!\n";
                  }
               }
               if($verbose >= 2 && $sshflag == 0) {
                  if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
                     open(ENCLOG, ">>$wavdir/enc.log");
                     print ENCLOG "\nRipIT will finish the job! ",
                                  "Check the error.log!\n";
                     close(ENCLOG);
                  }
                  else {
                     print "RipIT will finish the job! ",
                           "Check the error.log!\n";
                  }
               }
            }
            chomp(my $rip_ended = join(' ', grep(/^Ripping\sended:\s\d\d:\d\d/, @errlines)));
            if($rip_ended and $xtime == 0 and $multi == 1) {
               print "Ripper reported having ripped all wavs.\n";
               print "There is a problem with $riptrackname.wav.\n";
               print "with $artist - $album in device $cddev.\n";
               open(SRTF,">>$logfile.$riptrackno.txt")
                  or print "Can not append to file ",
                           "\"$logfile.$riptrackno.txt\"!\n";
               print SRTF "cdparanoia failed on $tracklist[$_ - 1]"
                  if($hiddenflag == 0);
               print SRTF "cdparanoia failed on $tracklist[$_ - 1]"
                  if($hiddenflag == 1);
               print SRTF "\nin device $logfile, error !";
               close(SRTF);
               # Create on the fly error message in log-directory.
               my $devnam = $cddev;
               $devnam =~ s/.*dev.//;
               open(ERO,">>$outputdir/failed.log")
                  or print "Can not append to file ",
                           "\"$outputdir/failed.log\"!\n";
               print ERO "$artist;$album;$genre;$categ;$cddbid;";
               print ERO "$devnam;$hostnam; Cdparanoia failure!\n";
               close(ERO);
               # Now wait to be terminated by checktrack.
               sleep 360;
               exit;
            }
         }
      }
      # This is an other hack to update the track-arrays modified by the
      # ripper if ghost songs were found. Is there another way to
      # communicate with the parent process?
      # This loop was supposed to be at the end of this sub-routine,
      # but we need it here in case of data tracks. The encoder would
      # stop here after a data track and fail to encode previously found
      # ghost songs because @tracksel has not yet been updated.
      # This loop was supposed to come right after the next part
      # checking for presence of wav-files. But we need it here in case
      # of ghost songs where the original track gets a new name.
      if($ghost == 1 && $_ == $tracksel[$#tracksel]
                     && -r "$wavdir/ghost.log") {
         open(GHOST, "<$wavdir/ghost.log")
            or print "Can not read file ghost.log!\n";
         my @errlines = <GHOST>;
         close(GHOST);
         my @selines = grep(s/^Array seltrack: //, @errlines);
         @tracksel = split(/ /, $selines[$#selines]);
         chomp($_) foreach(@tracksel);
         my @seclines = grep(s/^Array secondlist: //, @errlines);
         @secondlist = split(/ /, $seclines[$#seclines]);
         chomp($_) foreach(@secondlist);
         @tracklist = grep(s/^Array tracklist: //, @errlines);
         chomp($_) foreach(@tracklist);
         @tracktags = grep(s/^Array tracktags: //, @errlines);
         chomp($_) foreach(@tracktags);
         unlink("$wavdir/ghost.log");
         $ghost = 0;
         $ghostflag = 1;
         $resumenc = $resume; # Continue to resume ghost songs.
      }

      # Jump to the next track if wav wasn't found. Note that the
      # $tlength does not exist for additional ghost songs, so don't
      # test this condition when encoding ghost songs, furthermore we
      # assume that ghost songs are present as soon as one was found.
      next if($ghostflag == 0 && $xtime >= $tlength || $dataflag == 1);

      # It seems that we need to rename long filenames in a subshell,
      # because the rename function does not work if the full path is
      # even longer. NOTE: There is a problem with UTF8, when special
      # characters are true wide characters... Too many of them, and
      # it will fail again. Maybe one should check the length with the
      # unpack function.
      if(length($riptrackname) + length($wavdir) > 200) {
         $riptrackname = substr($riptrackname, 0, 200);
         $riptrackname =~ s/\s*$//;
#         rename("\"$wavdir/$wavname.wav\"","\"$wavdir/$riptrackname.wav\"");
         log_system("cd \"$wavdir\" && mv \"$wavname.wav\" \"$riptrackname.wav\"");
      }

      my $delwav = 0;
      my $starts = sprintf("%3d", sub {$_[1]*60+$_[0]}->(localtime));

      if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
         open(ENCLOG, ">>$wavdir/enc.log");
         print ENCLOG "\nEncoding \"$riptrackname\"...\n"
            if($verbose >= 3);
         close(ENCLOG);
      }
      else {
         print "\nEncoding \"$riptrackname\"...\n" if($verbose >= 3);
      }

      my $covertag;
      my $failflag = 0;
      # Set the encoder(s) we are going to use.
      for(my $c = 0; $c <= $#coder; $c++) {
         # Initialization of coverart variables.
         $covertag = " ";
         $coverart[$c] = 0 unless($coverart[$c]);
         # Get the command for the encoder to use!
         $genre = "" unless($genre);
         if($coder[$c] == 0) {
            $encodername = "Lame";
            $lameopt = $globopt[$c];

            # Coverart tagging will be done below because an additional
            # module will be used. Don't handle the whole picture-data
            # in this command.

            $enc = "lame $lameopt -S --tt \"$tracklametag\" \\
             --ta \"$artislametag\" --tl \"$albumlametag\" \\
             --ty \"$year\" --tg \"$genre\" --tn $tagtrackno \\
             --tc \"$commentlametag\" --add-id3v2 \\
             \"$wavdir/$riptrackname.wav\" \\
             \"$sepdir[$c]/$riptrackname.$suffix[$c]_enc\"";
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               printf ENCLOG "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print ENCLOG "Lame $lameopt encoding track $trackcn" .
                     " of " . ($#tracksel + 1) . "\n" if($verbose >= 3);
               close(ENCLOG);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print "Lame $lameopt encoding track $trackcn of " .
                     ($#tracksel + 1) if($verbose >= 3);
               print " merged tracks." if($verbose >= 3 && $cdcue > 0);
               print "\n" if($verbose >= 3);
            }
            log_info("new-mediafile: $sepdir[$c]/${riptrackname}.$suffix[$c]");
         }
         elsif($coder[$c] == 1) {
            $encodername = "Oggenc";
            $oggencopt = $globopt[$c];

            # Some info about coverart tagging.
            # This will happen below, after encoding, because we use
            # vorbiscomment. Don't handle the whole picture-data
            # in this command.

            # http://www.hydrogenaudio.org/forums/lofiversion/index.php/t48386.html

            # CLI solutions:
            # first: base64 encoding of the image:
            #
            # perl -MMIME::Base64 -0777 -ne 'print encode_base64($_, "")' < thumb.png > temp
            #
            # note the double quotes to prevent the newlines.
            # Redirect this output to a file.
            #
            # second: use vorbiscomment to tag the file: (http://darcs.tonywhitmore.co.uk/repos/podcoder/podcoder)
            #
            # vorbiscomment -a 01.ogg -t "COVERARTMIME=image/png" -t "COVERART=`cat temp`"
            #
            # and you're done.

            # Use of METADATA_BLOCK_PICTURE
            # http://wiki.xiph.org/index.php/VorbisComment
            # http://lists.xiph.org/pipermail/vorbis-dev/2009-April/019853.html

            # Proposals for extending Ogg Vorbis comments
            # http://reallylongword.org/vorbiscomment/

            $enc = "oggenc $oggencopt -Q -t \"$tracktag\" \\
               -a \"$artistag\" -l \"$albumtag\" \\
               -d \"$year\" -G \"$genre_tag\" \\
               -N $tagtrackno -c \"DESCRIPTION=$commentag\" \\
               -o \"$sepdir[$c]/$riptrackname.$suffix[$c]_enc\" \\
               \"$wavdir/$riptrackname.wav\"";
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               printf ENCLOG "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print ENCLOG "Oggenc $oggencopt encoding track" .
                     " $trackcn of " . ($#tracksel + 1) . "\n"
                     if($verbose >= 3);
               close(ENCLOG);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print "Oggenc $oggencopt encoding track $trackcn of " .
                      ($#tracksel + 1) if($verbose >= 3);
               print " merged tracks" if($verbose >= 3 && $cdcue > 0);
               print ".\n" if($verbose >= 3);
            }
            log_info("new-mediafile: $sepdir[$c]/${riptrackname}.$suffix[$c]");
         }
         elsif($coder[$c] == 2) {
            $encodername = "Flac";
            $flacopt = $globopt[$c];
            my $save_flacopt = $flacopt;
            $flacopt .= " -f" if($resume);
            # Don't know if the COMPILATION-tag is supported but it
            # should not harm at all.
            $flacopt .= " --tag=COMPILATION=1" if($va_flag > 0);
            if($coverart[$c] == 1 && -f "$coverpath" && -s "$coverpath") {
               $covertag = "--picture=\"$coverpath\"";
            }
            $enc = "flac $flacopt -s --tag=TITLE=\"$tracktag\" \\
             --tag=ARTIST=\"$artistag\" --tag=ALBUM=\"$albumtag\" \\
             --tag=DATE=\"$year\" --tag=TRACKNUMBER=\"$tagtrackno\" \\
             --tag=GENRE=\"$genre_tag\" --tag=CATEGORY=\"$categ\" \\
             --tag=DESCRIPTION=\"$commentag\" --tag=CDID=\"$cddbid\" \\
             $covertag \\
             -o \"$sepdir[$c]/$riptrackname.$suffix[$c]_enc\" \\
             \"$wavdir/$riptrackname.wav\"";
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               printf ENCLOG "%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print ENCLOG "Flac $flacopt encoding track $trackcn" .
                     " of " . ($#tracksel + 1) . "\n" if($verbose >= 3);
               close(ENCLOG);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print "Flac $flacopt encoding track $trackcn of " .
                     ($#tracksel + 1) if($verbose >= 3);
               print " merged tracks." if($verbose >= 3 && $cdcue > 0);
               print "\n" if($verbose >= 3);
            }
            log_info("new-mediafile: $sepdir[$c]/${riptrackname}.$suffix[$c]");
            my $flacopt = $save_flacopt if($resume);
         }
         elsif($coder[$c] == 3) {
            $encodername = "Faac";
            $faacopt = $globopt[$c];
            if($coverart[$c] == 1 && -f "$coverpath" && -s "$coverpath") {
               $covertag = "--cover-art \"$coverpath\"";
            }
            $enc = "faac $faacopt -w --title \"$tracktag\" \\
             --artist \"$artistag\" --album \"$albumtag\" \\
             --year \"$year\" --genre \"$genre_tag\" --track $tagtrackno \\
             --comment \"$commentag\" \\
             $covertag \\
             -o \"$sepdir[$c]/$riptrackname.$suffix[$c]_enc\" \\
             \"$wavdir/$riptrackname.wav\" \\
             > /dev/null 2>&1";
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               printf ENCLOG "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print ENCLOG "Faac $faacopt encoding track $trackcn" .
                     " of " . ($#tracksel + 1) . "\n" if($verbose >= 3);
               close(ENCLOG);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print "Faac $faacopt encoding track $trackcn of " .
                     ($#tracksel + 1) if($verbose >= 3);
               print " merged tracks." if($verbose >= 3 && $cdcue > 0);
               print "\n" if($verbose >= 3);
            }
            log_info("new-mediafile: $sepdir[$c]/${riptrackname}.$suffix[$c]");
         }
         elsif($coder[$c] == 4) {
            $encodername = "mp4als";
            $mp4alsopt = $globopt[$c];
            $enc = "mp4als $mp4alsopt \\
             \"$wavdir/$riptrackname.wav\" \\
             \"$sepdir[$c]/$riptrackname.$suffix[$c]_enc\" \\
             > /dev/null 2>&1 \\
             ";
            # Only add tags if MP4 container is set up, use artwork for
            # coverart.
            my $mp4suffix = $suffix[$c];
            if($mp4alsopt =~ /MP4/) {
               $mp4suffix = "mp4";
               if($coverart[$c] == 1 && -f "$coverpath" && -s "$coverpath") {
                  $covertag = "-P \"$coverpath\"";
               }
               $enc .= " && mp4tags -s \"$tracktag\" -a \"$artistag\" \\
                -A \"$albumtag\" -y \"$year\" -g \"$genre_tag\" \\
                -t $tagtrackno -c \"$commentag\" -e RipIT -E mp4als \\
                $covertag \\
                \"$sepdir[$c]/$riptrackname.$suffix[$c]_enc\"";
            }
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               printf ENCLOG "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print ENCLOG "Mp4als $mp4alsopt encoding track $trackcn" .
                     " of " . ($#tracksel + 1) . "\n" if($verbose >= 3);
               close(ENCLOG);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print "Mp4als $mp4alsopt encoding track $trackcn of " .
                     ($#tracksel + 1) if($verbose >= 3);
               print " merged tracks." if($verbose >= 3 && $cdcue > 0);
               print "\n" if($verbose >= 3);
            }
            log_info("new-mediafile: $sepdir[$c]/${riptrackname}.$suffix[$c]");
         }
         elsif($coder[$c] == 5) {
            $encodername = "Musepack";
            $museopt = $globopt[$c];
            # Musepack seems not to support coverart, the developper
            # probably assumes that coverart has nothing to do with a
            # track...
            $enc = "$musenc --silent $museopt --title \"$tracktag\" \\
             --artist \"$artistag\" --album \"$albumtag\" \\
             --year \"$year\" --genre \"$genre_tag\" --track $tagtrackno --comment \"$commentag\" \\
             \"$wavdir/$riptrackname.wav\" \\
             \"$sepdir[$c]/$riptrackname\_enc.$suffix[$c]\"";
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               printf ENCLOG "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print ENCLOG "Mppenc $museopt encoding track $trackcn" .
                     " of " . ($#tracksel + 1) . "\n" if($verbose >= 3);
               close(ENCLOG);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print "Mppenc $museopt encoding track $trackcn of " .
                     ($#tracksel + 1) if($verbose >= 3);
               print " merged tracks." if($verbose >= 3 && $cdcue > 0);
               print "\n" if($verbose >= 3);
            }
            log_info("new-mediafile: $sepdir[$c]/${riptrackname}.$suffix[$c]");
         }
         elsif($coder[$c] == 6) {
            $encodername = "wavpack";
            $wavpacopt = $globopt[$c];
            # Use command wvunpack -ss filename.wv to check if the cover
            # art is present or not. See:
            # www.hydrogenaudio.org/forums/index.php?showtopic=74828
            if($coverart[$c] == 1 && -f "$coverpath" && -s "$coverpath") {
               $covertag = "--write-binary-tag \"Cover Art (Front)=\@$coverpath\"";
            }
            $enc = "wavpack $wavpacopt -q \\
             -w \"Title=$tracktag\" \\
             -w \'Artist=$artistag\' -w \"Album=$albumtag\" \\
             -w \"Year=$year\" -w \"Genre=$genre_tag\" \\
             -w \"Track=$tagtrackno\" -w \"Comment=$commentag\" \\
             $covertag \\
             \"$wavdir/$riptrackname.wav\" \\
             -o \"$sepdir[$c]/$riptrackname\_enc\"";
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               printf ENCLOG "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print ENCLOG "Wavpack $wavpacopt encoding track $trackcn" .
                     " of " . ($#tracksel + 1) . "\n" if($verbose >= 3);
               close(ENCLOG);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print "Wavpack $wavpacopt encoding track $trackcn of " .
                     ($#tracksel + 1) if($verbose >= 3);
               print " merged tracks." if($verbose >= 3 && $cdcue > 0);
               print "\n" if($verbose >= 3);
            }
            log_info("new-mediafile: $sepdir[$c]/${riptrackname}.$suffix[$c]");
         }
         elsif($coder[$c] == 7) {
            $encodername = "ffmpeg";
            # Trying to solve the tag problem of tagging with ffmpeg in
            # general and within alac files in special:
            # First, I tried to use ffmpeg and the -map_meta_tag option:
            # ffmpeg -i 05\ I\ Beg\ For\ You.flac -acodec alac \\
            # 05\ I\ Beg\ For\ You.m4a -map_meta_data outfile:infile
            # Note: do not replace outfile:infile by the file names, use
            # the command as stated!
            #
            # OK, this works and we see, that the four character code
            # used in the m4a tags are "ART" and "wrt". So, what we need
            # is author to access these tags!
            #
            # http://archives.free.net.ph/message/20090925.222527.f3078d30.en.html
            # http://atomicparsley.sourceforge.net/mpeg-4files.html
            # http://code.google.com/p/mp4v2/wiki/iTunesMetadata
            #
            $ffmpegopt = $globopt[$c];
            $ffmpegopt .= " -y" if($overwrite eq "y");
            $ffmpegopt .= " -metadata compilation=1 " if($va_flag > 0 and $ffmpegopt =~ /alac/i);
#            Not yet supported... at least I don't know how to use the
#            -atag fourcc/tag option.
#            if($coverart[$c] == 1 && -f "$coverpath" && -s "$coverpath") {
#               $covertag = "-metadata artwork=\'$coverpath\'";
#            }
            $enc = "ffmpeg -i \"$wavdir/$riptrackname.wav\" \\
             $ffmpegopt \\
             -metadata author=\"$artistag\" -metadata album=\"$albumtag\" \\
             -metadata title=\"$tracktag\" -metadata genre=\"$genre_tag\" \\
             -metadata day=\"$year\" -metadata comment=\"$commentag\" \\
             -metadata track=\"$tagtrackno\" \\
             $covertag \\
             \"$sepdir[$c]/$riptrackname.$suffix[$c]\" > /dev/null 2>&1";
            # Only add artwork for coverart if alac is present.
            if($coverart[$c] == 1 && -f "$coverpath" && -s "$coverpath") {
               if($ffmpegopt =~ /alac/) {
                  $enc .= " && mp4art -q --add \"$coverpath\" \\
                  \"$sepdir[$c]/$riptrackname.$suffix[$c]\"";
               }
            }
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               printf ENCLOG "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print ENCLOG "ffmpeg $ffmpegopt encoding track $trackcn" .
                     " of " . ($#tracksel + 1) . "\n" if($verbose >= 3);
               close(ENCLOG);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime)
                  if($verbose >= 3);
               print "ffmpeg $ffmpegopt encoding track $trackcn of " .
                     ($#tracksel + 1) if($verbose >= 3);
               print " merged tracks." if($verbose >= 3 && $cdcue > 0);
               print "\n" if($verbose >= 3);
            }
            log_info("new-mediafile: $sepdir[$c]/${riptrackname}.$suffix[$c]");
         }
         # Set "last encoding of track" - flag.
         $delwav = 1 if($wav == 0 && $c == $#coder);
         # Set nice if wished.
         $enc = "nice -n $nice " . $enc if($nice != 0);
         # Make the output look nice, don't mess the messages!
         my $ripmsg = "The audio CD ripper reports: all done!";
         if($ripcomplete == 0 ) {
            if(-r "$wavdir/error.log") {
               open(ERR, "$wavdir/error.log")
                  or print "Can not open file error.log!\n";
               my @errlines = <ERR>;
               close(ERR);
               my @ripcomplete = grep(/^$ripmsg/, @errlines);
               $ripcomplete = 1 if(@ripcomplete);
            }
         }

         $enc =~ s/\$/\\\$/g;
         # Finally, do the job of encoding.
         if($sshflag == 1) {
            enc_ssh($delwav,$enc,$riptrackname,$sepdir[$c],$suffix[$c]);
            # Calculation of md5sum has been moved to the end, we still
            # use the process to check the files already done to add
            # coverart. Files not yet encoded will need to be post-
            # processed in del_erlog subroutine.
            push(@md5tracks,
                 "$sepdir[$c];#;$riptrackname.$suffix[$c]");
            my @waitracks;
            foreach my $md5tr (@md5tracks) {
               my ($sepdir, $donetrack) = split(/;#;/, $md5tr);
               # Proceede only if file appeared.
               if(-f "$sepdir/$donetrack") {
                  # Add special mp3 tags.
                  if(@mp3tags && $donetrack =~ /mp3$/) {
                     mp3_tags("$sepdir/$donetrack") if($mp3tags[0] ne "");
                  }
                  # Add coverart if it is a mp3 or ogg.
                  if($donetrack =~ /mp3$/ && -f "$coverpath" && -s "$coverpath") {
                     mp3_cover("$sepdir/$donetrack", "$coverpath");
                  }
                  elsif($donetrack =~ /ogg$/ && -f "$coverpath" && -s "$coverpath") {
                     ogg_cover("$sepdir/$donetrack", "$coverpath");
                  }
               }
               # Only add files to array @md5tracks if coverart shall be
               # added.
               else {
                  push(@waitracks, "$sepdir;#;$donetrack") if($coder[$c] <= 1 && $coverart[$c] == 1);
               }
            }
            @md5tracks = @waitracks;
         }
         else {
            if(log_system("$enc")) {
               if($ripcomplete == 0) {
                  if(-r "$wavdir/error.log") {
                     open(ERR, "$wavdir/error.log")
                        or print "Can open file error.log!\n";
                     my @errlines = <ERR>;
                     close(ERR);
                     my @ripcomplete = grep(/^$ripmsg/, @errlines);
                     $ripcomplete = 1 if(@ripcomplete);
                  }
               }
               if($coder[$c] == 4 && $mp4alsopt =~ /MP4/) {
                  rename("$sepdir[$c]/$riptrackname.$suffix[$c]_enc",
                         "$sepdir[$c]/$riptrackname.mp4");
               }
               elsif($coder[$c] == 5) {
                  rename("$sepdir[$c]/$riptrackname\_enc.$suffix[$c]",
                         "$sepdir[$c]/$riptrackname.$suffix[$c]");
               }
               elsif($coder[$c] == 6) {
                  rename("$sepdir[$c]/$riptrackname\_enc.$suffix[$c]",
                         "$sepdir[$c]/$riptrackname.$suffix[$c]");
                  if(-r "$sepdir[$c]/$riptrackname\_enc.wvc") {
                     rename("$sepdir[$c]/$riptrackname\_enc.wvc",
                            "$sepdir[$c]/$riptrackname.wvc");
                  }
               }
               else {
                  rename("$sepdir[$c]/$riptrackname.$suffix[$c]_enc",
                         "$sepdir[$c]/$riptrackname.$suffix[$c]");
               }
               # Add special mp3 tags.
               if(@mp3tags && $coder[$c] == 0) {
                  mp3_tags("$sepdir[$c]/$riptrackname.$suffix[$c]") if($mp3tags[0] ne "");
               }
               # Add coverart if it is a mp3 or ogg.
               if($coder[$c] == 0 && $coverart[$c] == 1 && -f "$coverpath" && -s "$coverpath") {
                  mp3_cover("$sepdir[$c]/$riptrackname.$suffix[$c]", "$coverpath");
               }
               elsif($coder[$c] == 1 && $coverart[$c] == 1 && -f "$coverpath" && -s "$coverpath") {
                  ogg_cover("$sepdir[$c]/$riptrackname.$suffix[$c]", "$coverpath");
               }
               if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
                  open(ENCLOG, ">>$wavdir/enc.log");
                  print ENCLOG "Encoding of " .
                               "\"$riptrackname.$suffix[$c]\" " .
                               "complete.\n" if($verbose >= 1);
                  close(ENCLOG);
               }
               else {
                  print "Encoding of \"$riptrackname.$suffix[$c]\" " .
                        "complete.\n" if($verbose >= 1);
               }
            }
            else {
               print "Encoder $encodername failed on $tracklist[$_ - 1]\n",
                     "of disc in device $cddev.\n",
                     "Error message says: $?\n";
               $failflag = 1;
               if($multi == 1) {
                  # Print error message to file srXY.Z.txt, checktrack
                  # will grep for string "encoder failed" and kill the
                  # CD immediately!
                  open(SRTF,">>$logfile.$riptrackno.txt")
                     or print "Can not append to file ",
                              "\"$logfile.$riptrackno.txt\"!\n";
                  print SRTF "\nencoder failed on $tracklist[$_ - 1] ";
                  print SRTF "in device $cddev, error $? !";
                  close(SRTF);
                  # Create on the fly error message in log-directory.
                  my $devnam = $cddev;
                  $devnam =~ s/.*dev.//;
                  open(ERO, ">>$outputdir/failed.log")
                     or print "Can not append to file ",
                              "\"$outputdir/failed.log\"!\n";
                  print ERO "$artist;$album;$genre;$categ;$cddbid;";
                  print ERO "$devnam;$hostnam; Encoder failure!\n";
                  close(ERO);
                  # Wait to be terminated by checktrack.
                  sleep 360;
               }
            }
            sleep 1;
         }
         # Copy the cdcue file (once) to the directory of the encoded
         # files.
         if($cdcue > 0) {
            my $cue_suffix = $suffix[$c];
            $cue_suffix =~ tr/a-z/A-Z/;
            open(CUE, ">$sepdir[$c]/$album.cue")
               or print "Can not append to file ",
                        "\"$sepdir[$c]/$album.cue\"!\n";
            foreach (@cuelines) {
               chomp;
               s/\.wav/.$suffix[$c]/;
               s/\sWAVE/ $cue_suffix/;
               print CUE "$_\n";
            }
            close(CUE);
         }
      }
      # Calculate time in seconds when encoding ended and total time
      # encoder needed.
      my $endsec = sprintf("%3d", sub {$_[1]*60+$_[0]}->(localtime));
      $endsec += 60 while($endsec <= $starts);
      $totalencs = $totalencs + $endsec - $starts;
      # Delete the wav if not wanted.
      unlink("$wavdir/$riptrackname.wav")
         if($delwav == 1 && $sshflag == 0);

      # Write the playlist file. This is somehow tricky, if ghost songs
      # may appear. To ensure the files in the right order, introduce
      # placeholders for possible ghost songs.
      # The problem is that the secondlist with the true track lengths
      # will only be updated when the last track has been encoded (the
      # last track except ghost songs). But we need the true length
      # right now. So, if $ghost == 1, check for the ghost.log file at
      # any track.
      # TODO:
      # An other buggy behaviour: if the last encoder of a list fails,
      # failflag will prevent writing playlist files, although encoding
      # was successful for all other encoders (but the last one).
      # Would it be better to write the playlist file in any case?
      if($failflag == 0 && $playlist >= 1) {
         # Ghost songs follow after the last track, but $ghostflag was
         # set to 1 just before last track is encoded. Therefore set
         # $ghostflag to 2 after the last track has been done and
         # inserted in the playlist file as a regular file (below),
         # and insert sound files as ghost songs only when $ghostflag is
         # 2. If only the last song has been split into chunks and
         # the counter increased, continue to insert as regular file.
         if($ghostflag == 2) {
            print PLST "GS$_:#EXTINF:$secondlist[$ghostcn - 1],",
                        "$tracktag\n"
               if($hiddenflag == 0);
            print PLST "GS$_:#EXTINF:$secondlist[$ghostcn],$tracktag\n"
               if($hiddenflag == 1);
            print PLST "GS$_:Sepdir/$riptrackname.suffix\n"
               if($playlist == 1);
            print PLST "GS$_:$riptrackname.suffix\n" if($playlist == 2);
         }
         else {
            if($ghost == 1 && -r "$wavdir/ghost.log") {
               open(GHOST, "<$wavdir/ghost.log")
                  or print "Can not read file ghost.log!\n";
               my @errlines = <GHOST>;
               close(GHOST);
               my @seclines = grep(s/^Array secondlist: //, @errlines);
               @secondlist = split(/ /, $seclines[$#seclines]);
               chomp($_) foreach(@secondlist);
            }
            print PLST "#EXTINF:$secondlist[$_ - 1],$tracktag\n"
               if($hiddenflag == 0);
            print PLST "#EXTINF:$secondlist[$_],$tracktag\n"
               if($hiddenflag == 1);
            print PLST "Sepdir/$riptrackname.suffix\n"
               if($playlist == 1);
            print PLST "$riptrackname.suffix\n" if($playlist == 2);
            print PLST "Add Ghost Song $_ Here.\n"
               if($ghost == 1 || $ghostflag == 1);
         }
      }
      last if($cdcue > 0);
   }

   print "\n" if($verbose > 2);
   # Only add albumgain and md5sum calculation if all tracks are done,
   # this might not be the case when running with more than one thread
   # or using remote process. In the later case, we need to add coverart
   # and album gain before calculating md5sums, so: move all this stuff
   # to del_erlog!

   # Tell the mother process the encoding time.
   open(ERR, ">>$wavdir/error.log")
      or print "Can not append to file error.log!\n";
   print ERR "Encoding needed $totalencs seconds!\n";
   print ERR "md5: $_\n" foreach(@md5tracks);
   close(ERR);
   close(PLST);
   exit unless($normalize == 1 or $cdcue > 0);
}
########################################################################
#
# Finish the M3U file used by players such as Amarok, Noatun, X11Amp...
#
sub create_m3u {
   my $playfile;
   my @mp3s = ();

   my $album = clean_all($album_utf8);
   my $artist = clean_all($artist_utf8);
   $album = clean_name($album);
   $artist = clean_name($artist);
   $album = clean_chars($album) if($chars);
   $artist = clean_chars($artist) if($chars);

   $playfile = "$artist" . " - " . "$album" . ".m3u";
   $playfile =~ s/ /_/g if($underscore == 1);
   if($limit_flag == 255) {
      $playfile = substr($playfile, 0, 240);
   }

   # Prevent warnings in some rare cases if no tracks have been ripped.
   return unless(-r "$wavdir/$playfile");

   open(PLST, "<$wavdir/$playfile")
      or print "Can not open file $wavdir/$playfile!\n";
   my @playlines = <PLST>;
   close(PLST);
   my @ghosts = grep(/^GS\d+:/, @playlines);

   unlink("$wavdir/$playfile");

   my @playlist = ();
   foreach (@playlines) {
      next if($_ =~ /^GS\d+:/ || $_ =~ /^$/);
      $_ =~ s/^Add Ghost Song (\d+) Here.$/$1/;
      chomp $_;
      if($_ =~ /^\d+$/) {
         foreach my $ghostsong (@ghosts) {
            if($ghostsong =~ s/^GS$_\://) { # Why not as a 1-liner?
               $ghostsong =~ s/^GS$_\://;
               chomp $ghostsong;
               push @playlist, $ghostsong;
            }
         }
      }
      else {
         push @playlist, $_;
      }
   }

   my $nplayfile;
   for(my $c = 0; $c <= $#coder; $c++) {
      my @mp3s = @playlist;
      $_ =~ s/\.suffix$/.$suffix[$c]/i foreach (@mp3s);
      $_ =~ s/^Sepdir/$sepdir[$c]/ foreach (@mp3s);
      # Extension of playlist-file only needed when more than one
      # encoder selected. Using separate dirs, this would not be
      # necessary, but who says we use them? We keep the extension.
      if($#coder != 0) {
         $nplayfile = $playfile;
         $nplayfile = change_case($nplayfile);
         $nplayfile =~ s/\.m3u$/ - $suffix[$c].m3u/
            if($underscore == 0);
         $nplayfile =~ s/\.m3u$/_-_$suffix[$c].m3u/
            if($underscore == 1);
         open(PLST, ">$sepdir[$c]/$nplayfile") or
            print "Can't open $sepdir[$c]/$nplayfile! $!\n";
      }
      else {
         $nplayfile = $playfile;
         open(PLST, ">$sepdir[$c]/$nplayfile") or
            print "Can't open $sepdir[$c]/$nplayfile! $!\n";
      }
      print PLST "$_\n" foreach(@mp3s);
      close(PLST);
      chmod oct($fpermission), "$sepdir[$c]/$nplayfile"
         if($fpermission);
   }
   # Recreate the wav-playlist if wavs aren't deleted.
   if($wav == 1) {
      my @mp3s = @playlist;
      $_ =~ s/\.suffix$/\.wav/i foreach (@mp3s);
      $_ =~ s/^Sepdir/$wavdir/ foreach (@mp3s);
      $nplayfile = $playfile;
      $nplayfile = change_case($nplayfile);
      $nplayfile =~ s/\.m3u$/ - wav\.m3u/
         if($underscore == 0);
      $nplayfile =~ s/\.m3u$/_-_wav\.m3u/
         if($underscore == 1);
      open(PLST, ">$wavdir/$nplayfile") or
         print "Can't open $wavdir/$nplayfile! $!\n";
      print PLST "$_\n" foreach(@mp3s);
      close(PLST);
      chmod oct($fpermission), "$wavdir/$nplayfile"
         if($fpermission);
   }
}
########################################################################
#
# Create a default or manual track list.
#
sub create_deftrack {
# Let operator chose to use default names or enter them manually.
# Do not ask if we come form CDDB submission, i.e. index == 0,
# or if $interaction == 0, then $index == 1.
   my ($i, $j, $index) = (0,1,@_);
   my ($album, $artist);

   my $tracks = substr($cddbid, 6);
   $tracks = hex($tracks);

   $album = clean_all($album_utf8) if(defined $cd{title});
   $artist = clean_all($artist_utf8) if(defined $cd{artist});

   # Preselect answer if no interaction requested.
   $index = 1 if($interaction == 0);

   while($index !~ /^[0-1]$/ ) {
      print "\nThis CD shall be labeled with:\n\n";
      print "1: Default Album, Artist and Tracknames\n\n";
      print "0: Manual input\n\nChoose [0-1]: (0) ";
      $index = <STDIN>;
      chomp $index;
      $index = 0 unless($index);
      print "\n";
   }
   # Create default tracklist and cd-hash.
   # NOTE: here we define an additional key: revision, which does not
   # exist if %cd is filled by CDDB_get. If this key exists we know
   # that it is a new entry.
   if($index == 1) {
      $artist = "Unknown Artist";
      $album = "Unknown Album";
      %cd = (
         artist => $artist,
         title => $album,
         cat => $categ,
         genre => $genre,
         id => $cddbid,
         revision => 0,
         year => $year,
      );
      while($i < $tracks) {
         $j = $i + 1;
         $j = "0" . $j if($j < 10);
         $cd{track}[$i] = "Track " . "$j";
         ++$i;
      }
      $cddbsubmission = 0;
   }
   # Create manual tracklist.
   elsif($index == 0) {
      # In case of CDDB resubmission
      if(defined $cd{artist}) {
         print "\n   Artist ($artist): ";
      }
      # In case of manual CDDB entry.
      else {
         print "\n   Artist : ";
      }
      $artist = <STDIN>;
      chomp $artist;
      # If CDDB entry confirmed, take it.
      if(defined $cd{artist} && $artist eq "") {
         $artist = $artist_utf8;
      }
      # If CDDB entry CHANGED, submission OK.
      elsif(defined $cd{artist} && $artist ne "") {
         $cddbsubmission = 1;
         $cd{artist} = $artist;
         $artist_utf8 = $artist;
      }
      if($artist eq "") {
         $artist = "Unknown Artist";
         $cddbsubmission = 0;
      }
      if(defined $cd{title}) {
         print "\n   Album ($album): ";
      }
      else {
         print "\n   Album : ";
      }
      $album = <STDIN>;
      chomp $album;
      while($year !~ /^\d{4}$/) {
         if(defined $cd{year}) {
            print "\n   Year ($year): ";
         }
         else {
            print "\n   year : ";
         }
         $year = <STDIN>;
         chomp $year;
         last if($year eq "");
      }
      # If CDDB entry confirmed, take it.
      if(defined $cd{title} && $album eq "") {
         $album = $album_utf8;
      }
      # If CDDB entry CHANGED, submission OK.
      elsif(defined $cd{title} && $album ne "") {
         $cddbsubmission = 1;
         $cd{title} = $album;
         $album_utf8 = $album;
      }
      if($album eq "") {
         $album = "Unknown Album";
         $cddbsubmission = 0;
      }
      %cd = (
         artist => $artist,
         title => $album,
         cat => $categ,
         genre => $genre,
         id => $cddbid,
         revision => 0,
         year => $year,
      ) unless(defined $cd{title});
      print "\n";
      $i = 1;
      while($i <= $tracks) {
         if(defined $cd{track}[$i-1]) {
            printf("   Track %02d (%s): ", $i, $tracktags[$i-1]);
         }
         else {
            printf("   Track %02d: ", $i);
         }
         my $tracktag = <STDIN>;
         chomp $tracktag;
         $tracktag = clean_all($tracktag);
         my $track = $tracktag;
         $track = clean_name($track);
         $track = clean_chars($track) if($chars);
         $track = change_case($track);
         $track =~ s/ /_/g if($underscore == 1);
         # If CDDB entry confirmed, take and replace it in tracklist.
         if(defined $cd{track}[$i-1] && $track ne "") {
            splice @tracklist, $i-1, 1, $track;
            splice @tracktags, $i-1, 1, $tracktag;
            $cddbsubmission = 1;
         }
         elsif(!$cd{track}[$i-1] && $track eq "") {
            $track = "Track " . sprintf("%02d", $i);
            $cddbsubmission = 0;
         }
         # Fill the "empty" array @{$cd{track}}.
         push(@{$cd{track}}, "$track");
         $i++;
      }
      print "\n";
   }
   else {
      # I don't like die, but I don't like if-loops without else.
      # This should not happen because of previous while-loop!
      die "Choose 0 or 1!\n\n";
   }
   return;
}
########################################################################
#
# Read the CD and generate a TOC with DiscID, track frames and total
# length. Then prepare CDDB-submission with entries from @tracklist.
#
sub pre_subm {
   my($check,$i,$ans,$genreno,$line,$oldcat,$subject) = (0,0);

   my $tracks = $#framelist;
   my $totals = int($framelist[$#framelist] / 75);

   my $album = clean_all($album_utf8);
   my $artist = clean_all($artist_utf8);

   my $revision = get_rev() unless($cd{discid});
   if($revision) {
      # TODO: if submission fails, set revision back.
      $revision++;
      print "Revision is set to $revision.\n" if($verbose > 4);
   }
   elsif(defined $cd{revsision}) {
      $revision = $cd{revision};
   }
   else {
      $revision = 0;
   }

   # Check for CDDB ID vs CD ID problems.
   if($cddbid ne $cd{id} && defined $cd{id}) {
      print "\nObsolet warning: CDID ($cddbid) is not identical to ";
      print "CDDB entry ($cd{id})!";
      print "\nYou might get a collision error. Try anyway!\n";
      $revision = 0;
   }
   # Questioning to change CDDB entries and ask to fill missing fields.
   if(defined $cd{year} && $year ne "") {
      $year = get_answ("year",$year);
   }
   if(!$year) {
      while($year !~ /^\d{4}$| / || !$year ) {
      print "\nPlease enter the year (or none): ";
      $year = <STDIN>;
      chomp $year;
      $cd{year} = $year;
      last if(!$year);
      $cddbsubmission = 1;
      }
   }
   if($cd{year}) {
      $cddbsubmission = 1 unless($year eq $cd{year});
   }
   else {
      $cddbsubmission = 1;
   }
   # Ask if CDDB category shall be changed and check if done;
   # $categ will be an empty string if user wants to change it.
   $oldcat = $categ;
   if($cd{cat} && $categ) {
      $categ = get_answ("CDDB category",$categ);
   }

   my @categ = ();
   my @categories = (
      "blues",  "classical", "country", "data",
      "folk",   "jazz",      "misc",    "newage",
      "reggae", "rock",      "soundtrack"
   );

   # If data is from musicbrainz, don't ask to check category and simply
   # prepare a CD-DB file with category musicbrainz. User will have to
   # find a unused category and submit the entry manually.
   if(!$categ && !$cd{discid} && $submission != 0) {
      print "Shall Ripit check for available categories?",
               " [y/n] (y) ";
      $ans = <STDIN>;
      chomp $ans;
      if($ans eq "") {
         $ans = "y";
      }
      if($ans =~ /^y/) {
         print "\n\nAvailable categories:\n";
         foreach (@categories) {
            my $templines = "";
            my $source = "http://www.freedb.org/freedb/" .
                          $_ . "/" . $cddbid;
            $templines = LWP::Simple::get($source);
            # Question: what is wrong that I need to put a \n in the print
            # command to force perl to print right away, and not to print
            # the whole bunch only when the foreach-loop is done???
            if($templines) {
               push @categ, $_;
            }
            else {
               print "   $_\n"
            }
         }
         if($categ[10]) {
            print "\nAll 11 categories are used, bad luck!";
            print "\nSave the file locally with --archive!\n";
            print "\nUse one of the following categories:";
            print "\nblues, classical, country, data, folk";
            print "\njazz, misc, newage, reggae, rock, soundtrack\n";
            $cddbsubmission = 0;
         }

         # Check if the $categ variable is correct.
         while($categ !~ m/^blues$|^classical$|^country$|^data$|^folk$|
                          |^jazz$|^newage$|^reggae$|^rock$|^soundtrack$|
                          |^misc$/ ) {
            print "\nPlease choose one of the available CDDB categories: "
               if($categ[10]);
            print "\nPlease choose one of the categories: "
               unless($categ[10]);
            $categ = <STDIN>;
            chomp $categ;
         }
         $cddbsubmission = 1 unless($categ eq $cd{cat});
      }
   }
   elsif($cd{discid}) {
      $categ = "musicbrainz";
   }
   # If one changes category for a new submission, set Revision to 0.
   if($oldcat ne $categ && defined $cd{cat}) {
      $revision = 0;
   }
   # Remind the user if genre is not ID3v2 compliant even if Lame is
   # not used! Reason: There should be no garbage genres in the DB.
   # If Lame is used, genre has already been checked!
   if($lameflag != 1 && defined $genre) {
      ($genre,$genreno) = check_genre($genre);
      $cddbsubmission = 1 unless($genre eq $cd{'genre'});
   }
   # Do not to ask if genre had been passed from command line.
   unless($pgenre) {
      $genre = get_answ("genre",$genre);
   }
   unless($genre) {
      print "\nPlease enter a valid CDDB genre (or none): ";
      $genre = <STDIN>;
      chomp $genre;
      $cd{genre} = $genre;
      # Allow to submit no genre! Else check it!
      if($genre) {
         $genre =~ s/[\015]//g;
         ($genre,$genreno) = check_genre($genre);
      }
   }
   $cddbsubmission = 1 unless($genre eq $cd{'genre'});
   my $dtitle = $artist . " / " . $album;
   substr($dtitle, 230, 0, "\nDTITLE=") if(length($dtitle) > 250);
   substr($dtitle, 460, 0, "\nDTITLE=") if(length($dtitle) > 500);

   # Start writing the CDDB submission.
   open(TOC, ">$homedir/cddb.toc")
      or print "Can not write to cddb.toc $!\n";
   print TOC "# xmcd CD database generated by RipIT\n#\n",
             "# Track frame offsets:\n";
   $i = 0;
   foreach (@framelist) {
      print TOC "# $_\n" if($i < $#framelist);
      $i++;
   }
   print TOC "#\n# Disc length: $totals seconds\n#\n";
   if(!$cd{discid} && $archive == 1) {
      my $source = "http://www.freedb.org/freedb/" . $categ . "/" . $cddbid;
      print "Will try to get <$source>.\n";
      my $templines = LWP::Simple::get($source);
      my @templines = split(/\n/, $templines);
      chomp($revision = join('', grep(s/^\s*#\sRevision:\s(\d+)/$1/, @templines)));
      $revision++ if($revision =~ /^\d+/);
      $revision = 0 unless($revision =~ /^\d+/);
      print "\nRevision number set to $revision.\n" if($verbose >= 4);
   }
   print TOC "# Revision: $revision\n";
   my $time = sprintf("%02d:%02d", sub {$_[2], $_[1]}->(localtime));
   my $date = sprintf("%04d-%02d-%02d",
      sub {$_[5]+1900, $_[4]+1, $_[3]}->(localtime));
   $date = $date . " at " . $time;
   print TOC "# Submitted via: RipIT $version ";
   print TOC "www.suwald.com/ripit/ripit.html on $date\n";
   print TOC "#\n";
   print TOC "DISCID=$cddbid\n";
   print TOC "DTITLE=$dtitle\n";
   print TOC "DYEAR=$year\n";
   if(defined $genre) {
      print TOC "DGENRE=$genre\n";
   }
   elsif($genre eq "" && defined $categ) {
      print TOC "DGENRE=$categ\n";
   }
   $i = 0;
   foreach (@tracktags) {
      substr($_, 230, 0, "\nTTITLE$i=") if(length($_) > 250);
      substr($_, 460, 0, "\nTTITLE$i=") if(length($_) > 500);
      print TOC "TTITLE$i=$_\n";
      ++$i;
   }

   my @comment = extract_comm;
   my $commentest = "@comment";
   if($commentest) {
      $ans = "x";
      $check = 0;
      print "Confirm (Enter), delete or edit each comment line ";
      print "(c/d/e)!\n";
      foreach (@comment) {
         chomp($_);
         s/\n//g;
         next if($_ eq "");
         while($ans !~ /^c|^d|^e/) {
            print "$_ (c/d/e): ";
            $ans = <STDIN>;
            chomp $ans;
            if($ans eq "") {
               $ans = "c";
            }
         }
         if($ans =~ /^c/ || $ans eq "") {
            print TOC "EXTD=$_\\n\n";
            $check = 1;
         }
         elsif($ans =~ /^e/) {
            print "Enter a different line: \n";
            my $ans = <STDIN>;
            chomp $ans;
            substr($ans, 230, 0, "\nEXTD=") if(length($ans) > 250);
            substr($ans, 460, 0, "\nEXTD=") if(length($ans) > 500);
            print TOC "EXTD=$ans\\n\n";
            $cddbsubmission = 1;
            $check = 1;
         }
         else {
            # Don't print the line.
            $cddbsubmission = 1;
         }
         $ans = "x";
      }
      $line = "a";
      while(defined $line) {
         print "Do you want to add a line? (Enter for none or type!): ";
         $line = <STDIN>;
         chomp $line;
         $cddbsubmission = 1 if($line ne "");
         last if(!$line);
         substr($line, 230, 0, "\nEXTD=") if(length($line) > 250);
         substr($line, 460, 0, "\nEXTD=") if(length($line) > 500);
         print TOC "EXTD=$line\\n\n";
         $check = 1;
      }
      # If all lines have been deleted, add an empty EXTD line!
      if($check == 0) {
         print TOC "EXTD=\n";
      }
   }
   # If there are no comments, ask to add some.
   elsif(!$comment[0]) {
      $line = "a";
      my $linecn = 0;
      while(defined $line) {
         print "Please enter a comment line (or none): ";
         $line = <STDIN>;
         chomp $line;
         $cddbsubmission = 1 if($line ne "");
         substr($line, 230, 0, "\nEXTD=") if(length($line) > 250);
         substr($line, 460, 0, "\nEXTD=") if(length($line) > 500);
         print TOC "EXTD=$line\n" if($linecn == 0);
         print TOC "EXTD=\\n$line\n" if($linecn != 0);
         $linecn++;
         # This line has to be written, so break the
         # while loop here and not before, as above.
         last if(!$line);
      }
   }
   else {
      print TOC "EXTD=\n";
   }

   # Extract the track comment lines EXTT.
   my @trackcom = grep(/^EXTT\d+=/, @{$cd{raw}});
   @trackcom = grep(s/^EXTT\d+=//, @trackcom);
   foreach (@trackcom) {
      chomp($_);
      s/\n//g;
      s/[\015]//g;
   }
   $ans = get_answ('Track comment','existing ones');
   if($ans eq "") {
      $i = 0;
      while($i < $tracks) {
         my $track;
         if($trackcom[$i]) {
            printf("   Track comment %02d (%s):", $i+1, $trackcom[$i]);
         }
         else {
            printf("   Track comment %02d: ", $i+1);
         }
         $track = <STDIN>;
         chomp $track;
         substr($track, 230, 0, "\nEXTT$i=") if(length($track) > 250);
         substr($track, 460, 0, "\nEXTT$i=") if(length($track) > 500);

         # If CDDB entry confirmed, take and replace it in tracklist.
         if(defined $trackcom[$i] && $track eq "") {
            print TOC "EXTT$i=$trackcom[$i]\n";
         }
         elsif(defined $trackcom[$i] && $track ne "") {
            print TOC "EXTT$i=$track\n";
            $cddbsubmission = 1;
         }
         elsif($track ne "") {
            print TOC "EXTT$i=$track\n";
            $cddbsubmission = 1;
         }
         else {
            print TOC "EXTT$i=\n";
         }
         $i++;
      }
   }
   elsif(@trackcom) {
      $i = 0;
      foreach (@tracklist) {
         print TOC "EXTT$i=$trackcom[$i]\n";
         ++$i;
      }
   }
   else {
      $i = 0;
      foreach (@tracklist) {
         print TOC "EXTT$i=\n";
         ++$i;
      }
   }

   # Extract the playorder line.
   my @playorder = grep(/^PLAYORDER=/, @{$cd{raw}});
   @playorder = grep(s/^PLAYORDER=//, @playorder);
   if(@playorder) {
      my $playorder = $playorder[0];
      chomp $playorder;
      print TOC "PLAYORDER=$playorder\n";
   }
   else {
      print TOC "PLAYORDER=\n";
   }
   close(TOC);
   # Copy the *edited* CDDB file if variable set to the ~/.cddb/
   # directory.
   if($archive == 1 && $cddbsubmission != 2) {
      log_system("mkdir -m 0755 -p \"$homedir/.cddb/$categ\"")
         or print
         "Can not create directory \"$homedir/.cddb/$categ\": $!\n";
      log_system(
         "cp \"$homedir/cddb.toc\" \"$homedir/.cddb/$categ/$cddbid\""
         )
         or print
         "Can not copy cddb.toc to directory ",
         "\"$homedir/.cddb/$categ/$cddbid\": $!\n";
      print "Saved file $cddbid in \"$homedir/.cddb/$categ/\"";
   }
   print "\n";
   # If no connection to the internet do not submit.
   if($submission == 0) {
      $cddbsubmission = 0;
   }
   # If we came from MB do not submit.
   elsif($cd{discid}) {
      $cddbsubmission = 0;
   }

   if($cddbsubmission == 1) {
      my $ans = "x";
      while($ans !~ /^y$|^n$/) {
         print "Do you really want to submit your data to freeDB.org?",
               " [y/n] (y) ";
         $ans = <STDIN>;
         chomp $ans;
         if($ans eq "") {
            $ans = "y";
         }
      }
      if($ans =~ /^y/) {
         $cddbsubmission = 1;
      }
      else{
         $cddbsubmission = 0;
      }
   }
   if($cddbsubmission == 1) {
      while($mailad !~ /.@.+[.]./) {
         print "\nReady for submission, enter a valid return ";
         print "e-mail address: ";
         $mailad = <STDIN>;
         chomp $mailad;
      }

      open TOC, "cat \"$homedir/cddb.toc\" |"
         or die "Can not open file $homedir/cddb.toc $!\n";
      my @lines = <TOC>;
      close(TOC);

      $subject = "cddb " . $categ . " " . $cddbid;
      open(MAIL, "|/usr/sbin/sendmail -t -r $mailad")
         or print "/usr/sbin/sendmail not installed? $!\n";

      # Generate the mail-header and add the toc-lines.
      print MAIL "From: $mailad\n";
      print MAIL "To: freedb-submit\@freedb.org\n";
#      print MAIL "To: test-submit\@freedb.org\n";
      print MAIL "Subject: $subject\n";
      print MAIL "MIME-Version: 1.0\n";
      print MAIL "Content-Type: text/plain; charset=$charset\n";
      foreach (@lines) {
         print MAIL $_;
      }
      close(MAIL);
      print "Mail exit status not zero: $?" if($?);
      print "CDDB entry submitted.\n\n" unless($?);
      unlink("$homedir/cddb.toc");
   }
   elsif($cddbsubmission == 2) {
      print "\n CDDB entry created and saved in \$HOME, but not send, ";
      print "because no changes";
      print "\n were made! Please edit and send it manually to ";
      print "freedb-submit\@freedb.org";
      print "\n with subject: cddb $categ $cddbid\n\n";
      sleep (4);
   }
   else {
      print "\n CDDB entry saved in your home directory, but not send,";
      print "\n please edit it and send it manually to:";
      print "\n freedb-submit\@freedb.org with subject:";
      print "\n cddb $categ $cddbid\n\n";
   }
}
########################################################################
#
# Check if genre is correct.
#
sub check_genre {
   my $genre = $_[0];
   my $genreno = "";
   my $genrenoflag = 1;

   $genre = "  " if($genre eq "");

   # If Lame is not used, don't die if ID3v2-tag is not compliant.
   if($lameflag == 0) {
      unless(log_system(
         "lame --genre-list 2>&1 | grep -i \" $genre\$\" > /dev/null ")) {
         print "Genre $genre is not ID3v2 compliant!\n"
            if($verbose >= 1);
         print "Continuing anyway!\n\n" if($verbose >= 1);
         chomp($genreno = "not ID3v2 compliant!\n");
      }
      return ($genre,$genreno);
   }

   # If Lame is not installed, don't loop for ever.
   if($lameflag == -1) {
      chomp($genreno = "Unknown.\n");
      return ($genre,$genreno);
   }

   # Check if (similar) genre exists. Enter a new one with interaction,
   # or take the default one.
   while(!log_system(
      "lame --genre-list 2>&1 | grep -i \"$genre\" > /dev/null ")) {
      print "Genre $genre is not ID3v2 compliant!\n" if($verbose >= 1);
      if($interaction == 1) {
         print "Use \"lame --genre-list\" to get a list!\n";
         print "\nPlease enter a valid CDDB genre (or none): ";
         $genre = <STDIN>;
         chomp $genre;
         $cd{genre} = $genre;
      }
      else {
         print "Genre \"Other\" will be used instead!\n"
            if($verbose >= 1);
         $genre = "12 Other";
      }
   }

   if($genre eq "") {
      return;
   }
   elsif($genre =~ /^\d+$/) {
      chomp($genre = `lame --genre-list 2>&1 | grep -i \' $genre \'`);
   }
   else {
      # First we want to be sure that the genre from the DB, which might
      # be "wrong", e.g. wave (instead of Darkwave or New Wave) or synth
      # instead of Synthpop, will be correct. Put the DB genre to ogenre
      # and get a new right-spelled genre... Note, we might get several
      # possibilities, e.g. genre is Pop, then we get a bunch of
      # "pop-like" genres!
      # There will be a line break, if multiple possibilities found.
      my $ogenre = $genre;
      chomp($genre = `lame --genre-list 2>&1 | grep -i \'$genre\'`);
      # Second we want THE original genre, if it precisely exists.
      chomp(my $testgenre = `lame --genre-list 2>&1 | grep -i \'\^... $ogenre\$\'`);
      $genre = $testgenre if($testgenre);
      # If we still have several genres:
      # Either let the operator choose, or if no interaction, take
      # default genre: "12 Other".
      if($genre =~ /\n/ && $interaction == 1) {
         print "More than one genre possibility found!\n";
         my @list = split(/\n/,$genre);
         my ($i,$j) = (0,1);
         while($i > $#list+1 || $i == 0) {
            # TODO: Here we should add the possibility to choose none!
            # Or perhaps to go back and choose something completely
            # different.
            foreach (@list) {
               printf(" %2d: $_ \n",$j);
               $j++;
            }
            $j--;
            print "\nChoose [1-$j]: ";
            $i = <STDIN>;
            chomp $i;
            $j = 1;
         }
         chomp($genre = $list[$i-1]);
      }
      # OK, no interaction! Take the first or default genre!
      elsif($genre =~ /\n/ && $interaction != 1 && $lameflag == 1) {
         $genre = "12 Other" if($genre eq "");
         $genre =~ s/\n.*//;
      }
      # OK, the genre is not Lame compliant, and we do not care about,
      # because Lame is not used. Set the genre-number-flag to 0 to
      # prevent genre-number-extracting at the end of the subroutine.
      elsif($lameflag != 1) {
         $genre = $ogenre;
         $genrenoflag = 0;
      }
      chomp $genre;
   }

   # Extract genre-number.
   if($genre ne "" && $genrenoflag == 1) {
      $genre =~ s/^\s*//;
      my @genre = split(/ /, $genre);
      $genreno = shift(@genre);
      $genre = "@genre";
   }
   return ($genre,$genreno);
}
########################################################################
#
# Check mirrors. Need to be tested from time to time, which ones are up.
#
#  http://at.freedb.org:80/~cddb/cddb.cgi working
#  http://au.freedb.org:80/~cddb/cddb.cgi not working
#  http://ca.freedb.org:80/~cddb/cddb.cgi working
#  http://ca2.freedb.org:80/~cddb/cddb.cgi working
#  http://de.freedb.org:80/~cddb/cddb.cgi working
#  http://es.freedb.org:80/~cddb/cddb.cgi working
#  http://fi.freedb.org:80/~cddb/cddb.cgi working
#  http://freedb.freedb.org:80/~cddb/cddb.cgi not working
#  http://ru.freedb.org:80/~cddb/cddb.cgi working
#  http://uk.freedb.org:80/~cddb/cddb.cgi working
#  http://us.freedb.org:80/~cddb/cddb.cgi not working
#
#
sub check_host {
#   while($mirror !~ m/^freedb$|^at$|^au$|^ca$|^es$|^fi$|
#                     |^fr$|^jp$|^jp2$|^ru$|^uk$|^uk2$|^us$/) {
   while($mirror !~ m/^freedb$|^at$|^au$|^bg$|^ca$|^es$|^fi$|
                     |^lu$|^no$|^uk$|^us$/) {
      print "host mirror ($mirror) not valid!\nenter freedb, ",
            "at, au, ca, es, fi, fr, jp, jp2, ru, uk, uk2 or us: ";
      $mirror = <STDIN>;
      chomp $mirror;
   }
}
########################################################################
#
# Answer to question.
#
sub get_answ {
   my $ans = "x";
   while($ans !~ /^y|^n/) {
      print "Do you want to enter a different ".$_[0]." than ".$_[1];
      print "? [y/n], (n): ";
      $ans = <STDIN>;
      chomp $ans;
      if($ans eq "") {
         $ans = "n";
      }
   }
   if($ans =~ /^y/) {
      return "";
   }
   return $_[1];
}
########################################################################
#
# Check quality passed from command line for lame, oggenc, flac, faac.
#
sub check_quality {
   #
   # Prevent warnings.
   @pquality = defined unless(@pquality);
   #
   # Remember, if the quality is defined via -q/--quality switch
   # on the command line, the array consists of a comma separated
   # string in the first entry only!
   #
   if($pquality[0] =~ /\d/) {
      # Why this joining and splitting? Because the whole string is in
      # $quality[0]! But why joining? Because we can come from CLI! In
      # this case we need to make it identical to the way it comes from
      # config file, i.e. as comma separated string in the first entry.
      @quality = split(/,/, join(',', @pquality));
   }
   elsif("@quality" eq "5 3 5 100 0 5") {
      return;
   }
   # If no coder-array has been passed, we do not know to which encoder
   # each quality-entry belongs to. NOTE, we've not yet read the config.
   # So we need to read the config file to check if there is a unusual
   # order of encoders. In this way, this subroutine will ask the
   # correct questions and not mess up the encoders if qualities are
   # wrong, supposing the operator is aware about an unusual order!
   if(!@pcoder && -r "$ripdir") {
      open(CONF, "$ripdir");
      my @conflines = <CONF>;
      close(CONF);
      @pcoder = grep(s/^coder=//, @conflines) unless(@pcoder);
      chomp @pcoder;
      if($pcoder[0] =~ /^\d/) {
         @coder = split(/,/, join(',',@pcoder));
      }
   }

   # Actually check only those qualities needed, i.e. for chosen
   # encoders.
   # NOTE again: the $qualame etc. variables hold the string needed for
   # the config, it might be a comma separated string. When passing
   # commands, we should not use them, but things like "$quality[$c]"
   # instead!
   my $corrflag = 0;
   $qualame = "";
   $qualoggenc = "";
   $quaflac = "";
   $quafaac = "";
   $quamp4als = "";
   $quamuse = "";
   for(my $c=0; $c<=$#coder; $c++) {
      if($coder[$c] == 0 && !defined($quality[$c])) {
         $quality[$c] = 5; # prevent warnings.
      }
      elsif($coder[$c] == 0 && $quality[$c] ne "off") {
         $quality[$c] = 5 unless($quality[$c] =~ /\d/);
         while($quality[$c] > 9) {
            print "\nThe quality $quality[$c] is not valid for Lame!",
                  "\nPlease enter a different quality (0 = best),",
                  " [0-9]: ";
            $quality[$c] = <STDIN>;
            chomp $quality[$c];
         }
         $qualame .= ";" . $quality[$c];
      }
      elsif($coder[$c] == 0 && $quality[$c] eq "off") {
         $qualame .= ";" . $quality[$c];
      }
      # Done with lame, do the other encoders.
      if($coder[$c] == 1 && !defined($quality[$c])) {
         $quality[$c] = 3; # prevent warnings.
      }
      elsif($coder[$c] == 1 && $quality[$c] ne "off") {
         $quality[$c] = 3 unless($quality[$c] =~ /\d/);
         while($quality[$c] > 10 || $quality[$c] == 0) {
            print "\nThe quality $quality[$c] is not valid for Oggenc!",
                  "\nPlease enter a different quality (10 = best),",
                  " [1-10]: ";
            $quality[$c] = <STDIN>;
            chomp $quality[$c];
         }
         $qualoggenc .= "," . $quality[$c];
      }
      elsif($coder[$c] == 1 && $quality[$c] eq "off") {
         $qualoggenc .= "," . $quality[$c];
      }
      if($coder[$c] == 2 && !defined($quality[$c])) {
         $quality[$c] = 5; # prevent warnings.
      }
      elsif($coder[$c] == 2 && $quality[$c] ne "off") {
         $quality[$c] = 5 unless($quality[$c] =~ /\d/);
         while($quality[$c] > 8) {
            print "\nThe compression level $quality[$c] is not valid ",
                  "for Flac!",
                  "\nPlease enter a different compression level ",
                  "(0 = lowest), [0-8]: ";
            $quality[$c] = <STDIN>;
            chomp $quality[$c];
         }
         $quaflac = $quaflac . "," . $quality[$c];
      }
      elsif($coder[$c] == 2 && $quality[$c] eq "off") {
         $quaflac .= "," . $quality[$c];
      }
      if($coder[$c] == 3 && !defined($quality[$c])) {
         $quality[$c] = 100; # prevent warnings.
      }
      elsif($coder[$c] == 3 && $quality[$c] ne "off") {
         $quality[$c] = 100 unless($quality[$c] =~ /\d/);
         while($quality[$c] > 500 || $quality[$c] < 10) {
            print "\nThe quality $quality[$c] is not valid for Faac!",
                  "\nPlease enter a different quality (500 = max), ",
                  "[10-500]: ";
            $quality[$c] = <STDIN>;
            chomp $quality[$c];
         }
         $quafaac .= "," . $quality[$c];
      }
      elsif($coder[$c] == 3 && $quality[$c] eq "off") {
         $quafaac .= "," . $quality[$c];
      }
      if($coder[$c] == 4 && !defined($quality[$c])) {
         $quality[$c] = 0; # prevent warnings.
      }
      elsif($coder[$c] == 4 && $quality[$c] ne "off") {
         $quality[$c] = 0 unless($quality[$c] =~ /\d/);
         # Any info about mp4als "qualities", i. e. compression levels?
         $quamp4als .= "," . $quality[$c];
      }
      elsif($coder[$c] == 4 && $quality[$c] eq "off") {
         $quamp4als .= "," . $quality[$c];
      }
      if($coder[$c] == 5 && !defined($quality[$c])) {
         $quality[$c] = 5; # prevent warnings.
      }
      elsif($coder[$c] == 5 && $quality[$c] ne "off") {
         $quality[$c] = 5 unless($quality[$c] =~ /\d/);
         while($quality[$c] > 10 || $quality[$c] < 0) {
            print "\nThe quality $quality[$c] is not valid for $musenc!",
                  "\nPlease enter a different quality (10 = max), ",
                  "[0-10]: ";
            $quality[$c] = <STDIN>;
            chomp $quality[$c];
         }
         $quamuse .= "," . $quality[$c];
      }
      elsif($coder[$c] == 5 && $quality[$c] eq "off") {
         $quamuse .= "," . $quality[$c];
      }
      if($coder[$c] == 6 && !defined($quality[$c])) {
         $quality[$c] = " "; # prevent warnings.
      }
      if($coder[$c] == 7 && !defined($quality[$c])) {
         $quality[$c] = " "; # prevent warnings.
      }
   }
   $qualame =~ s/^,//;
   $qualoggenc =~ s/^,//;
   $quaflac =~ s/^,//;
   $quafaac =~ s/^,//;
   $quamuse =~ s/^,//;
   # Small problem if only option --savenew is used, with no other
   # option at all. Then, qualame has default value (because Lame is
   # default encoder), but all other qualities are empty!
   $qualoggenc = 3 unless($qualoggenc);
   $quaflac = 5 unless($quaflac);
   $quafaac = 100 unless($quafaac);
   $quamp4als = 0 unless($quamp4als);
   $quamuse = 5 unless($quamuse);
   # NOTE: corrections have been done on quality array, not pquality.
   # If pquality was passed, we need to apply corrections and save it
   # the same way as if it had been passed on command line.
   if($pquality[0]) {
      my $pquality = join(',', @quality);
      $pquality =~ s/(,\s)*$//;
      @pquality = ();
      $pquality[0] = $pquality;
   }
}
########################################################################
#
# Check bitrate for Lame only if vbr is wanted.
#
sub check_vbrmode {
   while($vbrmode ne "new" && $vbrmode ne "old") {
      print "\nFor vbr using Lame choose *new* or *old*! (new): ";
      $vbrmode = <STDIN>;
      chomp $vbrmode;
      $vbrmode = "new" if($vbrmode eq "");
   }
}
########################################################################
#
# Check preset for Lame only.
#
sub lame_preset {
   if($vbrmode eq "new") {
      $preset = "fast " . $preset;
   }
}
########################################################################
#
# Check if there is an other than $cddev which has a CD if no --device
# option was given.
#
sub check_cddev {
   # Try to get a list of possible CD devices.
   open(DEV, "/etc/fstab");
   my @dev = <DEV>;
   close(DEV);
   @dev = grep(/^\s*\/dev/, @dev);
   @dev = grep(!/^\s*\/dev\/[f|h]d/, @dev);
   @dev = grep(!/sd/, @dev);
   my @devlist = ();
   foreach (@dev) {
      my @line = split(/\s/, $_);
      chomp $line[0];
      push(@devlist, $line[0]) unless($line[0] =~ /by-id/);
   }
   # First check some default addresses.
   if(open(CD, "$cddev")) {
      $cddev = $cddev;
      close(CD);
   }
   elsif(open(CD, "/dev/cdrecorder")) {
      $cddev = "/dev/cdrecorder";
      close(CD);
   }
   elsif(open(CD, "/dev/dvd")) {
      $cddev = "/dev/dvd";
      close(CD);
   }
   elsif(open(CD, "/dev/sr0")) {
      $cddev = "/dev/sr0";
      close(CD);
   }
   elsif(open(CD, "/dev/sr1")) {
      $cddev = "/dev/sr1";
      close(CD);
   }
   else {
      foreach (@devlist) {
         if(open(CD, "$_")) {
            $cddev = $_;
            chomp $cddev;
            close(CD);
         }
         else {
            $cddev = "";
         }
      }
   }
   # On a notebook, the tray can't be closed automatically!
   # Print error message and retry detection.
   if($cddev eq "") {
      print "Is there a CD and the tray of the device closed?\n";
      print "Pausing 12 seconds.\n";
      sleep(12);
      foreach (@devlist) {
         if(open(CD, "$_")) {
            $cddev = $_;
            chomp $cddev;
            close(CD);
         }
      }
   }
   if($cddev eq "") {
      print "Could not detect CD device! The default /dev/cdrom ";
      print "device will be used.\n";
      $cddev = "/dev/cdrom";
   }
   return;
}
########################################################################
#
# Check bitrate if bitrate is not zero.
#
sub check_bitrate {
   while($bitrate !~ m/^32$|^40$|^48$|^56$|^64$|^80$|^96$|^112$|^128$|
                     |^160$|^192$|^224$|^256$|^320$|^off$/) {
      print "\nBitrate should be one of the following numbers or ";
      print "\"off\"! Please Enter";
      print "\n32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, ";
      print "256 or 320: (128) \n";
      $bitrate = <STDIN>;
      chomp $bitrate;
      if($bitrate eq "") {
         $bitrate = 128;
      }
   }
}
########################################################################
#
# Check protocol level for CDDB query.
#
sub check_proto {
   while($proto > 6) {
      print "Protocol level for CDDB query should be less-equal 6!\n";
      print "Enter an other value for protocol level (6): ";
      $proto =  <STDIN>;
      chomp $proto;
      $proto = 6 if($proto eq "");
   }
}
########################################################################
#
# Check and clean the coder array.
#
sub check_coder {

   # Reset $lameflag set by past invocation of check_coder() except if
   # lame is not installed ($lameflag == -1).
   $lameflag = 0 if($lameflag > 0);

   # Create encoder array if passed or read from config file.
   # Remember, if we come from reading the config file, the array
   # consists of a comma separated string in the first entry only!
   if(@pcoder) {
      @coder = split(/,/, join(',', @pcoder));
   }
   else {
      # This can happen because this subroutine is called before config
      # file is read! So @pcoder can be empty and @coder will be filled
      # with default value for Lame. Do we need this?
      @coder = split(/,/, join(',', @coder));
   }

   my @ffmpegsuf = ();
   if($ffmpegsuffix) {
      @ffmpegsuf = split(/,/, $ffmpegsuffix);
   }

   # Check if there is an entry > 7.
   for(my $c = 0; $c <= $#coder; $c++) {
      if($coder[$c] > 7) {
         die "Encoder number $coder[$c] does not yet exist, ",
             "please enter\n0 for Lame, 1 for Oggenc, 2 for Flac ",
             "3 for Faac,\n 4 for mp4als, 5 for Musepack, ",
             "6 for Wavpack or 7 for ffmpeg!\n\n";
         # TODO: splice that entry out, don't die!
      }
      $lameflag = 1 if($coder[$c] == 0);
      $oggflag = 1 if($coder[$c] == 1);
      $wvpflag = 1 if($coder[$c] == 6);
      $suffix[$c] = "mp3" if($coder[$c] == 0);
      $suffix[$c] = "ogg" if($coder[$c] == 1);
      $suffix[$c] = "flac" if($coder[$c] == 2);
      $suffix[$c] = "m4a" if($coder[$c] == 3);
      $suffix[$c] = "m4b" if($coder[$c] == 3 && $book == 1);
      $suffix[$c] = "als" if($coder[$c] == 4);
      $suffix[$c] = "mpc" if($coder[$c] == 5);
      $suffix[$c] = "wv" if($coder[$c] == 6);
      if($coder[$c] == 7) {
         $suffix[$c] = shift @ffmpegsuf;
      }
   }
   # Use comma separated string to write the encoder array to the
   # config file!
   $wcoder = join(',', @coder);
}
########################################################################
#
# Over or re-write the config file (depends on option savenew or save).
#
# New options step 10: Add description of new option to config file.
#
sub save_config {
   $confdir = "$homedir/.ripit" if($confdir eq "");
   log_system("mkdir -m 0755 -p $confdir")
      or die "Can not create directory $confdir: $!\n";
   # Remember: $ripdir is the full path including config file name.
   rename("$confdir/$confname","$confdir/$confname.old")
      if(-r "$confdir/$confname");
   open(CONF, "> $confdir/$confname")
      or die "Can not write to $confdir/$confname: $!\n";
   print CONF "
#####
#
# RipIT $version configuration file.
#
# For further information on ripit configuration / parameters
# and examples see the manpage or the README provided with ripit
# or type ripit --help .


#####
#
# Ripping device & path.
#

# cddevice: Define ripping device if other than /dev/cdrom.
# Default: /dev/cdrom

cddevice=$cddev

# scsidevice: Device name for special devices if the non ripping
# commands should be executed on a different device node. This might
# be useful for some old SCSI devices. If not set the cddevice will
# be used.
# Example: /dev/sr18
# Default: not set

scsidevice=$scsi_cddev

# output: Path for audio files. If not set, \$HOME will be used.
# Default: not set

output=$outputdir

# directory permissions: Permissions for directories.
# Default: 0755

dpermission=$dpermission

# file permissions: Permissions for sound and log files.
# If not set, uses the default system settings.
# Default: not set

fpermission=$fpermission


#####
#
# Ripping options.
#

# ripper: select CD ripper
# 0 - dagrab
# 1 - cdparanoia
# 2 - cdda2wav
# 3 - tosha
# 4 - cdd
# Default: cdparanoia

ripper=$ripper

# ripopt: User definable options for the CD ripper.
# Default: not set

ripopt=$ripopt

# span: Rip only part of a single track or the merged track-interval.
# Possible values: any in the format hh:mm:ss.ff-hh:mm:ss.ff
# Example: rip first 30s of each track: 0-30
# Default: not set

span=$span

# paranoia: Turn \"paranoia\" on or off for dagrab and cdparanoia.
# Possible values: 0 - no paranoia, 1 - use paranoia
#                  2 - switch paranoia off if ripping fails on one
#                      track and retry this track without paranoia
# Default: 1 - use paranoia

paranoia=$parano

# ghost: Analyze the wavs for possible gaps, split the wav into
# chunks of sound and delete blank tracks.
# Possible values: 0 - off, 1 - on
# Default: off

ghost=$ghost

# prepend: Enlarge the the chunk of sound by a number of
# seconds at the beginning (if possible).
# Possible values: any positive number and zero; precision in
# tenths of seconds. Be aware of low numbers, especially when
# using option cdcue.
# Default: 2.0

prepend=$prepend

# extend: Enlarge the the chunk of sound by a number of
# seconds at the end (if possible).
# Possible values: any positive number and zero; precision in
# tenths of seconds. Be aware of low numbers.
# Default: 2.0

extend=$extend

# resume: Resume a previously started session.
# Possible values: 0 - off, 1 - on
# Default: off

resume=$resume

# overwrite: Default behaviour of Ripit is not to overwrite existing
# directories, a suffix will be added if directory name exists.
# Use option overwrite to prevent this and either overwrite a previous
# rip or force Ripit to quit or even eject the disc. If ejection is
# chosen, the disc will be ejected even if option eject has not been
# switched on.
# Possible values: n - off, y - on,
#                  q - quit, e - quit and force ejection
# Default: off

overwrite=$overwrite


#####
#
# Encoding options
#

# encode: Encode the wavs.
# Possible values: 0 - off, 1 - on
# Default: on

encode=$encode

# coder: Select encoders for audio files:
# 0 - Lame (mp3)
# 1 - Oggenc (ogg)
# 2 - Flac (flac)
# 3 - Faac (m4a)
# 4 - mp4als (als or mp4)
# 5 - Musepack (mpc)
# 6 - Wavpack (wv)
# 7 - ffmpeg
# Multiple encoders can be selected by giving a comma separated list
# Example: coder=0,0,1,2 encodes CD twice to mp3, ogg and flac files
# Default: Lame

coder=$wcoder

###
#
# lame (mp3) encoder options
#

# qualame: Sets audio quality for lame encoder in cbr (lame-option -q)
# and vbr (lame-option -V) mode, comma separated list if encoder is
# used several times.
# Possible values: 0...9, off
# 0: highest quality
# 9: lowest quality
# Can be set to \"off\" if all options are passed to --lameopt.
# Example: qualame=off,off
# Note: default value is the same for cbr and vbr,
# although vbr-default should be 4.
# Default: 5

qualame=$qualame

# lameopt: Additional options for lame encoder,
# use a comma separated list if encoder is used several times.
# Example: lameopt=-b 128,--preset extreme
# Default: not set

lameopt=$lameopt

# vbrmode: Enable variable bitrate for lame encoder.
# Possible values: \"old\" or \"new\"
# Default: not set

vbrmode=$vbrmode

# bitrate: Sets bitrate for lame encoder.
# Possible values: 32...320, off
# Should be set to \"off\" if vbr is used
# Default: 128

bitrate=$bitrate

# maxrate: Sets maximum bitrate for lame (when using vbr) and oggenc.
# Possible values: 0 - off, 32...320
# Default: 0

maxrate=$maxrate

# preset: Use lame presets. To set the \"fast\" switch, use --vbrmode new.
# Possible values: medium, standard, extreme, insane
#
# medium: 160kbps
# standard: 192kbps
# extreme: 256kbps
# insane: 320kbps
#
# Default: not set

preset=$wpreset

###
#
# oggenc (ogg) encoder options
#

# qualoggenc: Sets audio quality for oggenc.
# Possible values: 1..10, off
# 1: lowest quality
# 10: highest quality
# Can be set to \"off\"
# Default: 3

qualoggenc=$qualoggenc

# oggencopt: Additional options for oggenc,
# use a comma separated list if encoder is used several times.
# Default: not set

oggencopt=$oggencopt

###
#
# flac (lossless) encoder options
#

# quaflac: Sets audio compression for flac encoder
# Possible values: 0...8, off
# 0: lowest compression
# 8: highest compression
# Can be set to \"off\"
# Default: 5

quaflac=$quaflac

# flacopt: Additional options for flac encoder,
# use a comma separated list if encoder is used several times.
# Example of single encoder:
# flacopt=--padding=8212 --replay-gain
# Example of multiple encoder:
# flacopt=--padding=8212 --replay-gain,--padding=8212
# Note: If using the --replay-gain option the padding option
# is recommended, otherwise all padding might be lost.
# Default: not set

flacopt=$flacopt

###
#
# faac (m4a) encoder options
#

# quafaac: Sets audio quality for faac encoder
# Possible values: 10...500, off
# 10: lowest quality
# 500: highest quality
# Can be set to \"off\"
# Default: 100

quafaac=$quafaac

# faacopt: Additional options for faac encoder,
# comma separated list if encoder is used several times.
# Default: not set

faacopt=$faacopt

###
#
# mp4als (als or mp4) encoder options
#

# quamp4als: Set audio compression level for mp4als.
# Note: Options that influence compression and speed
# should be used in the mp4als options below.
# Default: 0

quamp4als=$quamp4als

# mp4alsopt: Additional options for mp4als encoder,
# comma separated list if encoder is used several times.
# Example: -MP4 to allow tagging, mandatory.
# Example: -a -o30 for faster speed.
# Default: not set

mp4alsopt=$mp4alsopt

###
#
# Musepack (mpc) encoder options
#

# musenc: The encoder name on the command line
# Possible values: any
# Example: musenc=mppenc for older versions
# Default: mpcenc

musenc=$musenc

# quamuse: Sets audio quality for Musepack encoder
# Possible values: 0...10, off
# 0: lowest quality
# 10: highest quality
# Can be set to \"off\"
# Default: 5

quamuse=$quamuse

# museopt: Additional options for Musepack encoder,
# use a comma separated list if encoder is used several times.
# Default: not set

museopt=$museopt

###
#
# Wavpack (wv) encoder options
#

# wavpacopt: Additional options for Wavpack encoder,
# use a comma separated list if encoder is used several times.
# Example: -b320chy
# Default: -y

wavpacopt=$wavpacopt

###
#
#ffmpeg encoder options
#

# ffmpegopt: Additional options for ffmpeg,
# use a comma separated list if encoder is used several times.
# Example if ffmpeg is used twice: -acodec alac,-acodec wmav2
# Default: off

ffmpegopt=$ffmpegopt

# ffmpegsuffix: Suffix to be used for ffmpeg,
# use a comma separated list if encoder is used several times.
# Example if ffmpeg is used twice: m4a,wma
# Default: off

ffmpegsuffix=$ffmpegsuffix


#####
#
# Trackname and directory template
#

# dirtemplate: Template for directory structure
# The template can be created using any legal
# character, including slashes (/) for multi-level
# directory-trees, and the following variables:
# \$album
# \$artist
# \$iletter
# \$genre
# \$quality
# \$suffix
# \$trackname
# \$tracknum
# \$year
# \$trackno
#
# The variable \$iletter is the initial letter of
# the artist variable, the \$quality is the quality
# according to the encoding format defined by \$suffix.
# The variable \$quality reflects the encoder options,
# not the arguments of option --quality which may be set
# to off. The variable \$trackno is the total number of tracks
# of the release.
#
# dirtemplate is an array, for each encoder a different
# dirtemplate may be defined (i. e. for each encoder state
# a line starting with dirtemplate=...).
#
# Example:
# dirtemplate=\"\$suffix/hard_path/\$iletter/\$artist/\$year - \$album\"
#
# The double quotes (\") are mandatory!
# Default: \"\$artist - \$album\"
\n";
   print CONF "dirtemplate=$_\n" foreach(@dirtemplate);
   print CONF "
# tracktemplate: Template for track names
# \"tracktemplate\" is used similarly to \"dirtemplate\"
# Default:  \"\$tracknum \$trackname\"

tracktemplate=$tracktemplate

# trackoffset: Add an offset to the track counter (\$tracknum)
# Possible values: any integer
# Default: 0

trackoffset=$trackoffset

# infolog: Log certain operations to file
# (e.g. system calls, creation of dirs/files)
# Possible values: filename (full path, no ~ here!)
# Default: not set

infolog=$infolog

# lowercase: Convert filenames to lowercase
# Possible values: 0 - off, 1 - on
# Default: off

lowercase=$lowercase

# uppercasefirst: Convert filenames and tags to uppercase first,
# not recommended. To be used on the command line only if CDDB entry
# is in uppercase.
# Possible values: 0 - off, 1 - on
# Default: off

uppercasefirst=$uppercasefirst

# underscore: Replace blanks in filenames with underscores
# Possible values: 0 - off, 1 - on
# Default: off

underscore=$underscore

# chars: Exclude special characters in file names and path.
# Note: following characters will always be purged:
#  ; > < \" and \\015 .
# Side note: if calling this option on the command line without
# argument, following characters will be purged:  |\\:*?\$  plus
# blanks and periods at beginning and end of file names and directories.
# This is identical to the word NTFS passed as argument to the command
# line or stated here in the config file. The word HFS will purge colons
# only plus blanks and periods at beginning of file names and
# directories.
#
# No need to escape the special characters here in the config file.
# Possible values: HFS, NTFS, none, any (?)
# Default: not set

chars=$chars

# playlist: Create m3u playlist with or without the full path
# in the filename.
# Possible values: 0 - off,
#                  1 - on with full path
#                  2 - on with no path (filename only)
# Default: on (with full path)

playlist=$playlist


#####
#
# Audio file tagging
#

# year-tag: State a year (mp3, m4a) or a date (ogg, flac) tag.
# Possible values: integer
# Default: not set

year=$year

# comment-tag: State a comment (mp3, m4a, mpc) or a
# description (ogg, flac) tag. To write the cddbid used for freedb
# or the MusicBrainz discid into the comment, use the word \"cddbid\"
# or \"discid\".
# Possible values: discid, cddbid or any string
# Default: not set

comment=$commentag

# mp3tags: Additional tags for mp3 not passed by the encoder.
# Example: Force a unofficial compilation frame when using within
# a certain player: TCMP=1
# Note: option is an array, for each additional frame/tag to be added
# state the option once.
# Possible values: none, any
# Default: not set
\n";
   if(@mp3tags) {
      foreach(@mp3tags) {
         print CONF "mp3tags=$_\n";
      }
   }
   else {
      print CONF "mp3tags=\n";
   }
   print CONF "
# utftag: Use Lame-tags in UTF-8 or convert them
# (but not the filenames) from Unicode to ISO8859-1.
# Use when your mp3-audio player doesn't support Unicode tags.
# Recommended with Lame.
# Possible values: 0 - off, 1 - on
# Default: on

utftag=$utftag

# coverart: Add cover image to metadata of encoded file if possible.
# Note: The cover must be available when encoding starts, one might
# want to use option --precmd to execute a script for downloading and
# preparing a cover. Argument is a list in same order as encoders with
# values 0 (no coverart) or 1 (add coverart) for each encoder.
# Example: 1,0,0,1
# Possible values: 0 - off, 1 - on
# Default: off

coverart=$coverart

# coverpath: Path where the cover can be found.
# Example: ../thumb.png
# Possible values: string, none
# Default: none

coverpath=$coverpath

# copycover: Copy a cover (or any other file) to all
# directories containing encoded files. Useful e.g. when using Amarok.
# Example: \"\$wavdir/cover.jpg\"
# Possible values: none - off, absolute path to image
# Default: off

copycover=$copycover

# vatag: Analyze tracknames for \"various artists\" style and split
# the metadata in case one of the delimiters (colon, hyphen, slash or
# parenthesis) are found. Use unpair numbers for the scheme
# \"artist ? tracktitle\" and pair numbers in the opposite case.
# The artist will be compared to the argument of option --vastring
# (see below). If the artist must be like vastring and each track have a
# delimiter, use 1 (2), if the artist must be like vastring while only
# some tracks contain the delimiter, use 3 (4), if no restrictions
# apply for the artist but all tracknames must have a delimiter, use
# 5 (6) and finally, if only a few tracks contain a delimiter to be
# used as splitting point, set vatag to 7 (8).
# Example: 5
# Possible values: 0 - off, 1, 2, 3, 4, 5, 6, 7, 8
# Default: off

vatag=$vatag

# vastring: the string (regular expression) that defines the
# \"various artists\" style
# Example: Varios|VA
# Possible values: string, none
# Default: \\bVA\\b|Variou*s|Various\\sArtists

vastring=$vastring

# mp3gain: Add album gain tags to mp3 files using the appropriate
# command with options and arguments but without infiles.
# Example: mp3gain -a -c -q -s i
# Default: not set

mp3gain=$mp3gain

# vorbgain: Add album gain tags to ogg files using the appropriate
# command with options and arguments but without infiles.
# Example: vorbisgain -a -q
# Default: not set

vorbgain=$vorbgain

# flacgain: Add album gain tags to flac files using the appropriate
# command with options and arguments but without infiles.
# Example: metaflac --add-replay-gain
# Default: not set

flacgain=$flacgain

# aacgain: Add album gain tags to mp4 or m4a files using the appropriate
# command with options and arguments but without infiles.
# Example: aacgain -a -c -q
# Default: not set

aacgain=$aacgain

# mpcgain: Add album gain tags to mpc files using the appropriate
# command with options and arguments but without infiles.
# Example: mpcgain
# Default: not set

mpcgain=$mpcgain

# wvgain: Add album gain tags to wv files using the appropriate
# command with options and arguments but without infiles.
# Example: wvgain -a -q
# Default: not set

wvgain=$wvgain


#####
#
# CDDB options
#

# mb: Access MusicBrainz DB via WebService::MusicBrainz module instead
# of the http protocol (see below).
# Possible values: 0 - off, 1 - on
# Default: off

mb=$mb

# CDDBHOST: Specifies the CDDB server
# Possible values: freedb.org, freedb2.org or musicbrainz.org
# Note: Full name of the server used is \$mirror.\$CDDBHOST, except for
# freedb2.org (no mirror) and musicbrainz.org has freedb as default
# mirror.
# E.g. default server is freedb.freedb.org
# Default: freedb.org

CDDBHOST=$CDDB_HOST

# mirror: Selects freedb mirror
# Possible values: \"freedb\" or any freedb mirrors
# See www.freedb.org for mirror list
# Note: Full name of the server used is \$mirror.\$CDDBHOST
# E.g., default server is freedb.freedb.org
# Default: freedb

mirror=$mirror

# transfer: Set transfer mode for cddb queries
# Possible values: cddb, http
# Note: CDDB servers freedb2.org and musicbrainz.org may need transfer
# mode http.
# Default: cddb

transfer=$transfer

# proto: Set CDDP protocol level
# Possible values: 5, 6
# Protocol level 6 supports Unicode (UTF-8)
# Default: 6

proto=$proto

# proxy: Address of http-proxy, if needed.
# Default: not set

proxy=$proxy

# mailad: Mail address for cddb submissions.
# Possible values: Valid user email address for submitting cddb entries
# Default: not set

mailad=$mailad

# archive: Read and save cddb data on local machine.
# Possible values: 0 - off, 1 - on
# Default: off

archive=$archive

# submission: Submit new or edited cddb entries to freeCDDB.
# Possible values: 0 - off, 1 - on
# Default: on

submission=$submission

# interaction: Turns on or off user interaction in cddb dialog and
# everywhere else.
# Possible values: 0 - off, 1 - on
# Default: on

interaction=$interaction

# isrc: detect track iscrs using icedax and submit them to Musicbrainz
# if login info is provided. Please check if the device in use is
# able to read correct ISRCs and submit them if found.
# Possible values: 0 - off, 1 - on
# Default: off

isrc=$isrc

# mbname: login name to Musicbrainz.org
# Possible values: string
# Default: not set

mbname=$mbname

# mbpass: password to Musicbrainz.org
# Possible values: string
# Default: not set

mbpass=$mbpass


#####
#
# LCD options
#

# lcd: Use lcdproc to display status on LCD
# Possible values: 0 - off, 1 - on
# Default: off

lcd=$lcd

# lcdhost: Specify the lcdproc host
# Default: localhost

lcdhost=$lcdhost

# lcdport: Specify port number for $lcdhost
# Default: 13666

lcdport=$lcdport


#####
#
# Distributed ripping options
#

# sshlist: Comma separated list of remote machines ripit shall use
# for encoding. The output path must be the same for all machines.
# Specify the login (login\@machine) only if not the
# same for the remote machine. Else just state the
# machine names.
# Default: not set

sshlist=$wsshlist

# scp: Copy files to encode to the remote machine.
# Use if the fs can not be accessed on the remote machines
# Possible values: 0 - off, 1 - on
# Default: off

scp=$scp

# local: Turn off encoding on local machine, e.g. use only remote
# machines.
# Possible values: 0 - off, 1 - on
# Example: local=0 (off) turns off encoding on the
# local machine
# Default: on

local=$local


#####
#
# Misc. options
#

# verbosity: Run silent (do not output comments, status etc.) (0), with
# minimal (1), normal without encoder msgs (2), normal (3), verbose (4)
# or extremely verbose (5).
# Possible values: 0...5
# Default: 3 - normal

verbose=$verbose

# eject: Eject cd after finishing encoding.
# Possible values: 0 - off, 1 - on
# Default: off

eject=$eject

# ejectcmd: Command used to eject and close CD tray.
# Possible values: string
# Example: /usr/sbin/cdcontrol for FreeBSD
# Default: eject

ejectcmd=$ejectcmd

# ejectopt: Options to command used to eject or close CD.
# Possible values: string or \"{cddev}\" to design the CD
# device.
# Note: Don't use options -t / close or eject,
#       RipIT knows when to eject or load the tray
# Default: {cddev}

ejectopt=$ejectopt

# quitnodb: Give up CD if no CDDB entry found.
# Useful if option --loop or --nointeraction are on.
# Default behaviour is to let operator enter data or to use default
# artist, album and track names.
# Possible values: 0 - off, 1 - on
# Default: off

quitnodb=$quitnodb

# execmd: Execute a command when done with ripping. Quote the command
# if needed.
# Note: The same variables as in the dirtemplate can be used. When
# using MusicBrainz one might want to use \$cd{asin} to get the ASIN
# if available.
# Possible values: none - off, string - on
# Example: execmd=\"add_db -a \\\"\$artist\\\" -r \\\"\$album\\\"\"
# Default: off

execmd=$execmd

# precmd: Execute a command before starting to rip. Quote the command
# if needed.
# Note: The same variables as in the dirtemplate can be used. When
# using MusicBrainz one might want to use \$cd{asin} to get the ASIN
# if available.
# Possible values: none - off, string - on
# Example: precmd=\"get_cover -a \\\"\$artist\\\" -r \\\"\$album\\\" -o \\\"\$wavdir\\\" -t \\\"\$trackno\\\"\"
# Default: off

precmd=$precmd

# book: Create an audiobook, i. e. merge all tracks into one single
# file, option --ghost will be switched off and file suffix will be
# m4b. Make sure to use encoder faac, ripit will not check that.
# A chapter file will be written for chapter marks.
# Possible values: 0 - off, 1 - on
# Default: off

book=$book

# loop: Continue with a new CD when the previous one is done.
# Option --eject will be forced. To start ripping process immediately
# after ejection of previous disc, use experimental argument 2. Ripit
# will restart as child process, one might see the prompt and it will
# be necessary to manually terminate the process! Use with caution!
# Possible values: 0 - off, 1 - on, 2 - immediate restart, experimental
# Default: off

loop=$loop

# halt: Powers off machine after finishing encoding.
# Possible values: 0 - off, 1 - on
# Default: off

halt=$halt

# nice: Sets \"nice\" value for the encoding process.
# Possible values: 0..19 for normal users,
#                  -20..19 for user \"root\"
# Default: 0

nice=$nice

# nicerip: Sets \"nice\" value for the ripping process.
# Possible values: 0..19 for normal users,
#                  -20..19 for user \"root\"
# Default: 0

nicerip=$nicerip

# threads: Comma separated list of numbers giving maximum
# of allowed encoder processes to run at the same time
# (on each machine when using sshlist).
# Possible values: comma separated integers
# Default: 1

threads=$wthreads

# md5sum: Create file with md5sums for each type of sound files.
# Possible values: 0 - off, 1 - on
# Default: off

md5sum=$md5sum

# wav: Don't delete wave-files after encoding.
# Possible values: 0 - off, 1 - on
# Default: off

wav=$wav

# normalize: Normalizes the wave-files to a given dB-value
# (default: -12dB)
# See http://normalize.nongnu.org for details.
# Possible values: 0 - off, 1 - on
# Default: off

normalize=$normalize

# normcmd: Command to be used to normalize.
# Possible values: string
# Example: normalize-audio (when using Debian)
# Default: normalize

normcmd=$normcmd

# normopt: Options to pass to normalize
# Possible values: -a -nndB   : Normalize to -nn dB, default is -12dB,
#                  Value range: All values <= 0dB
#                  Example    : normalize -a -20dB *.wav
#                  -b         : Batch mode - loudness differences
#                               between individual tracks of a CD are
#                               maintained
#                  -m         : Mix mode - all track are normalized to
#                               the same loudness
#                  -v         : Verbose operation
#                  -q         : Quiet operation
# For further options see normalize documentation.
# Default: -b
# The -v option will be added by default according to RipITs verbosity

normopt=$normopt

# cdtoc: Create a toc file to burn the wavs with
# cd-text using cdrdao or cdrecord (in dao mode).
# Possible values: 0 - off, 1 - on
# Default: off

cdtoc=$cdtoc

# inf: Create inf files to burn the wavs with
# cd-text using wodim or cdrecord (in dao mode).
# Possible values: 0 - off, 1 - on
# Default: off

inf=$inf

# cdcue: Create a cue file to burn the wavs with cd-text.
# Possible values: 0 - off, 1 - on, 2 - on (experimental fallback)
# Note: Use value 2 only if for whatever reason value 1 should fail.
# Default: off

cdcue=$cdcue
\n";
   close(CONF);
}
########################################################################
#
# Read the config file, take the parameters only if NOT yet defined!
#
# New options step 11: Read the new options from config file. Replicate
# one of the 2-liners starting with chomp.
#
sub read_config {
   $ripdir = $confdir . "/" . $confname if($confdir ne "");
   # Fallback:
   $ripdir = $homedir . "/.ripit/config" unless(-r "$ripdir");
   $ripdir = "/etc/ripit/config" unless(-r "$ripdir");
   if(-r "$ripdir") {
      open(CONF, "$ripdir") or
      print "Can not read config file in $ripdir: $!\n";
      my @conflines = <CONF>;
      close(CONF);
      my @confver = grep(s/^# RipIT //, @conflines);
      @confver = split(/ /, $confver[0]) if($confver[0] =~ /^\d/);
      my $confver = $confver[0] if($confver[0] =~ /^\d/);
      $confver = 0 unless($confver);
      chomp $confver;
      if($version ne $confver && $savepara == 0) {
         $verbose = 3 if($verbose <= 1);
         print "\nPlease update your config-file with option --save";
         print "\nto ensure correct settings! Pausing 3 seconds!\n\n";
         # TODO:
         # In older configs the option var was either set to zero or
         # one. In the case one wanted to replace characters, ignore
         # for simplicity! User should update! This line and the elsif
         # part below will go away with version 3.8.0.
         grep(s/^chars=[01]\s*$/chars=/, @conflines);
         sleep(3);
      }
      elsif($version ne $confver) {
         grep(s/^chars=[01]\s*$/chars=/, @conflines);
      }
      chomp($archive = join(' ', grep(s/^archive=//, @conflines)))
         unless defined $parchive;
      chomp($bitrate = join(' ', grep(s/^bitrate=//, @conflines)))
         unless($pbitrate);
      chomp($book = join(' ', grep(s/^book=//, @conflines)))
         unless($pbook);
      chomp($maxrate = join(' ', grep(s/^maxrate=//, @conflines)))
         unless($pmaxrate);
      chomp($cddev = join(' ', grep(s/^cddevice=//, @conflines)))
         unless($pcddev);
      chomp($scsi_cddev = join(' ', grep(s/^scsidevice=//, @conflines)))
         unless($pscsi_cddev);
      chomp($cdtoc = join('', grep(s/^cdtoc=//, @conflines)))
         unless($pcdtoc);
      chomp($cdcue = join('', grep(s/^cdcue=//, @conflines)))
         unless($pcdcue);
      chomp($chars = join('', grep(s/^chars=//, @conflines)))
         if($chars eq "XX");
      chomp($commentag = join('', grep(s/^comment=//, @conflines)))
         unless($pcommentag);
      chomp($CDDB_HOST = join('', grep(s/^CDDBHOST=//, @conflines)))
         unless($PCDDB_HOST);
      @pcoder = grep(s/^coder=//, @conflines) unless(@pcoder);
      # NOTE: all coders are in array entry $pcoder[0]!
      # NOTE: we have to fill the wcoder (w=write) variable!
      $wcoder = $pcoder[0] if(@pcoder);
      chomp $wcoder;
      @dirtemplate = grep(s/^dirtemplate=//, @conflines)
         unless($pdirtemplate[0]);
      chomp $_ foreach(@dirtemplate);
      chomp($dpermission = join('', grep(s/^dpermission=//, @conflines)))
         unless($pdpermission);
      chomp($eject = join('', grep(s/^eject=//, @conflines)))
         unless defined $peject;
      chomp($ejectcmd = join('', grep(s/^ejectcmd=//, @conflines)))
         unless defined $pejectcmd;
      chomp($ejectopt = join('', grep(s/^ejectopt=//, @conflines)))
         unless defined $pejectopt;
      chomp($encode = join('', grep(s/^encode=//, @conflines)))
         unless defined $pencode;
      chomp($extend = join('', grep(s/^extend=//, @conflines)))
         unless defined $pextend;
      chomp($execmd = join('', grep(s/^execmd=//, @conflines)))
         unless defined $pexecmd;
      chomp($precmd = join('', grep(s/^precmd=//, @conflines)))
         unless defined $pprecmd;
      chomp($fpermission = join('', grep(s/^fpermission=//, @conflines)))
         unless($pfpermission);
      chomp($ghost = join('', grep(s/^ghost=//, @conflines)))
         unless defined $pghost;
      chomp($halt = join('', grep(s/^halt=//, @conflines)))
         unless($phalt);
      chomp($inf = join('', grep(s/^inf=//, @conflines)))
         unless($pinf);
      chomp($infolog = join('', grep(s/^infolog=//, @conflines)))
         unless($pinfolog);
      chomp($interaction = join('', grep(s/^interaction=//, @conflines)))
         unless defined $pinteraction;
      chomp($isrc = join('', grep(s/^isrc=//, @conflines)))
         unless defined $pisrc;
      chomp($lcd = join('', grep(s/^lcd=//, @conflines)))
         unless defined $plcd;
      chomp($lcdhost = join('', grep(s/^lcdhost=//, @conflines)))
         unless($plcdhost);
      chomp($lcdport = join('', grep(s/^lcdport=//, @conflines)))
         unless($plcdport);
      chomp($local = join('', grep(s/^local=//, @conflines)))
         unless defined $plocal;
      chomp($loop = join('', grep(s/^loop=//, @conflines)))
         unless defined $ploop;
      chomp($lowercase = join('', grep(s/^lowercase=//, @conflines)))
         unless defined $plowercase;
      chomp($uppercasefirst = join('', grep(s/^uppercasefirst=//, @conflines)))
         unless defined $puppercasefirst;
      chomp($mailad = join('', grep(s/^mailad=//, @conflines)))
         unless($pmailad);
      chomp($mb = join('', grep(s/^mb=//, @conflines)))
         unless defined $pmb;
      chomp($mbname = join('', grep(s/^mbname=//, @conflines)))
         unless defined $pmbname;
      chomp($mbpass = join('', grep(s/^mbpass=//, @conflines)))
         unless defined $pmbpass;
      chomp($md5sum = join('', grep(s/^md5sum=//, @conflines)))
         unless($pmd5sum);
      chomp($mirror = join('', grep(s/^mirror=//, @conflines)))
         unless($pmirror);
      @mp3tags = grep(s/^mp3tags=//, @conflines)
         unless($pmp3tags[0]);
      chomp $_ foreach(@mp3tags);
      chomp($musenc = join('', grep(s/^musenc=//, @conflines)))
         unless($pmusenc);
      chomp($normalize = join('', grep(s/^normalize=//, @conflines)))
         unless defined $pnormalize;
      chomp($normcmd = join('', grep(s/^normcmd=//, @conflines)))
         unless($pnormcmd);
      chomp($normopt = join('', grep(s/^normopt=//, @conflines)))
         unless($pnormopt);
      chomp($nice = join('', grep(s/^nice=//, @conflines)))
         unless defined $pnice;
      chomp($nicerip = join('', grep(s/^nicerip=//, @conflines)))
         unless defined $pnicerip;
      chomp($outputdir = join('', grep(s/^output=//, @conflines)))
         unless($poutputdir);
      chomp($overwrite = join('', grep(s/^overwrite=//, @conflines)))
         unless($poverwrite);
      chomp($parano = join('', grep(s/^paranoia=//, @conflines)))
         unless defined $pparano;
      chomp($playlist = join('', grep(s/^playlist=//, @conflines)))
         unless defined $pplaylist;
      chomp($prepend = join('', grep(s/^prepend=//, @conflines)))
         unless defined $pprepend;
      chomp($preset = join('', grep(s/^preset=//, @conflines)))
         unless($ppreset);
      # NOTE: we have to fill the w_RITE_preset variable!
      $wpreset = $preset unless($ppreset);
      chomp $preset;
      chomp $wpreset;
      chomp($proto = join('', grep(s/^proto=//, @conflines)))
         unless($pproto);
      chomp($proxy = join('', grep(s/^proxy=//, @conflines)))
         unless($pproxy);
      my @quafaac = grep(s/^quafaac=//, @conflines) unless($pquality[0]);
      chomp($quafaac = $quafaac[0]) unless($pquality[0]);
      my @quaflac = grep(s/^quaflac=//, @conflines) unless($pquality[0]);
      chomp($quaflac = $quaflac[0]) unless($pquality[0]);
      my @qualame = grep(s/^qualame=//, @conflines) unless($pquality[0]);
      chomp($qualame = $qualame[0]) unless($pquality[0]);
      my @qualoggenc = grep(s/^qualoggenc=//, @conflines)
         unless($pquality[0]);
      chomp($qualoggenc = $qualoggenc[0]) unless($pquality[0]);
      my @quamp4als = grep(s/^quamp4als=//, @conflines)
         unless($pquality[0]);
      chomp($quamp4als = $quamp4als[0]) unless($pquality[0]);
      my @quamuse = grep(s/^quamuse=//, @conflines)
         unless($pquality[0]);
      chomp($quamuse = $quamuse[0]) unless($pquality[0]);
      # I don't really like this. I don't like the variables qualame etc
      # too and wanted to get rid of them. Not possible anymore. We need
      # them because they hold a comma separated string necessary to
      # write to the config file...
      unless($pquality[0]) {
         @qualame = split(/,/, $qualame);
         @qualoggenc = split(/,/, $qualoggenc);
         @quaflac = split(/,/, $quaflac);
         @quafaac = split(/,/, $quafaac);
         @quamp4als = split(/,/, $quamp4als);
         @quamuse = split(/,/, $quamuse);
         @coder = split(/,/, join(',',@pcoder));
         for(my $c=0; $c<=$#coder; $c++) {
            if($coder[$c] == 0) {
               $quality[$c] = $qualame[0];
               shift(@qualame);
            }
            if($coder[$c] == 1) {
               $quality[$c] = $qualoggenc[0];
               shift(@qualoggenc);
            }
            if($coder[$c] == 2) {
               $quality[$c] = $quaflac[0];
               shift(@quaflac);
            }
            if($coder[$c] == 3) {
               $quality[$c] = $quafaac[0];
               shift(@quafaac);
            }
            if($coder[$c] == 4) {
               $quality[$c] = $quamp4als[0];
               shift(@quamp4als);
            }
            if($coder[$c] == 5) {
               $quality[$c] = $quamuse[0];
               shift(@quamuse);
            }
         }
      }
      chomp($faacopt = join('', grep(s/^faacopt=//, @conflines)))
         unless($pfaacopt);
      chomp($flacopt = join('', grep(s/^flacopt=//, @conflines)))
         unless($pflacopt);
      chomp($lameopt = join('', grep(s/^lameopt=//, @conflines)))
         unless($plameopt);
      chomp($mp4alsopt = join('', grep(s/^mp4alsopt=//, @conflines)))
         unless($pmp4alsopt);
      chomp($museopt = join('', grep(s/^museopt=//, @conflines)))
         unless($pmuseopt);
      chomp($oggencopt = join('', grep(s/^oggencopt=//, @conflines)))
         unless($poggencopt);
      chomp($wavpacopt = join('', grep(s/^wavpacopt=//, @conflines)))
         unless($pwavpacopt);
      chomp($aacgain = join('', grep(s/^aacgain=//, @conflines)))
         unless($paacgain);
      chomp($flacgain = join('', grep(s/^flacgain=//, @conflines)))
         unless($pflacgain);
      chomp($mp3gain = join('', grep(s/^mp3gain=//, @conflines)))
         unless($pmp3gain);
      chomp($mpcgain = join('', grep(s/^mpcgain=//, @conflines)))
         unless($pmpcgain);
      chomp($vorbgain = join('', grep(s/^vorbgain=//, @conflines)))
         unless($pvorbgain);
      chomp($wvgain = join('', grep(s/^wvgain=//, @conflines)))
         unless($pwvgain);
      chomp($ffmpegopt = join('', grep(s/^ffmpegopt=//, @conflines)))
         unless($pffmpegopt);
      chomp($ffmpegsuffix = join('', grep(s/^ffmpegsuffix=//, @conflines)))
         unless($pffmpegsuffix);
      chomp($coverart = join('', grep(s/^coverart=//, @conflines)))
         unless($pcoverart);
      chomp($coverpath = join('', grep(s/^coverpath=//, @conflines)))
         unless($pcoverpath);
      chomp($copycover = join('', grep(s/^copycover=//, @conflines)))
         unless($pcopycover);
      chomp($quitnodb = join('', grep(s/^quitnodb=//, @conflines)))
         unless defined $pquitnodb;
      chomp($ripper = join('', grep(s/^ripper=//, @conflines)))
         unless defined $pripper;
      chomp($resume = join('', grep(s/^resume=//, @conflines)))
         unless defined $presume;
      chomp($ripopt = join('', grep(s/^ripopt=//, @conflines)))
         unless defined $pripopt;
      my @clist = grep(s/^threads=//, @conflines) unless($pthreads[0]);
      chomp @clist;
      # NOTE: all threads numbers are in array entry $clist[0]!
      @threads = split(/,/, join(',',@clist));
      my @rlist = grep(s/^sshlist=//, @conflines) unless($psshlist[0]);
      chomp @rlist;
      # NOTE: all machine names are in array entry $rlist[0]!
      @sshlist = split(/,/, join(',',@rlist));
      chomp($scp = join('', grep(s/^scp=//, @conflines)))
         unless defined $pscp;
      chomp($span = join('', grep(s/^span=//, @conflines)))
         unless defined $pspan;
      chomp($submission = join('', grep(s/^submission=//, @conflines)))
         unless defined $psubmission;
      chomp($transfer = join('', grep(s/^transfer=//, @conflines)))
         unless($ptransfer);
      chomp($tracktemplate = join('', grep(s/^tracktemplate=//, @conflines)))
         unless($ptracktemplate);
      chomp($trackoffset = join('', grep(s/^trackoffset=//, @conflines)))
         unless($ptrackoffset);
      chomp($underscore = join('', grep(s/^underscore=//, @conflines)))
         unless defined $punderscore;
      chomp($utftag = join('', grep(s/^utftag=//, @conflines)))
         unless defined $putftag;
      chomp($vatag = join('', grep(s/^vatag=//, @conflines)))
         unless defined $pvatag;
      chomp($vastring = join('', grep(s/^vastring=//, @conflines)))
         unless defined $pvastring;
      chomp($vbrmode = join('', grep(s/^vbrmode=//, @conflines)))
         unless($pvbrmode);
      chomp($year = join('', grep(s/^year=//, @conflines)))
         unless($pyear);
      chomp($wav = join('', grep(s/^wav=//, @conflines)))
         unless defined $pwav;
   }
   else {
      print "\nNo config file found! Use option --save to create one.\n"
         if($verbose >= 2);
   }
}
########################################################################
#
# Encode to utf-8 with UTF8 flag.
#
sub UTF8_encoding {
   my $string = shift;
   # We are still at point zero: is it Latin-1 or UTF-8? Let's decode
   # without fear, it will work because no wide chars are in!
   my @c_points = unpack("C0U*", "$string");
   my $d_string = decode_utf8($string, Encode::FB_QUIET);
   Encode::from_to($d_string, 'utf8', 'UTF-8');
   my @d_points = unpack("C0U*", "$d_string");
   # This is one possible test, compare the number of bytes. If
   # a wide character is in, then there are less bytes after
   # Encode::from_to. E.g. the string DÅµ will become Dŵr, but
   # Ärger will be �ger and by chance has the same number of
   # bytes -- and unicode points look like: 65533 114 103 101 114.
   print "@c_points\n==\n@d_points.\n" if($verbose >= 5);
   # In album - artist part we have == !
#   $string = $d_string unless(@c_points == @d_points);
   # In track part we have > ! Why?
   $string = $d_string unless(@c_points > @d_points);
   return($string);
}
########################################################################
#
# Change encoding of tags back to iso-8859-1. Again: this is only needed
# when using lame to create mp3s. Tagging works for all other
# encoders and encodings.
#
# Test CDs where option --noutf should work:
# Bang Bang:  Je t'aime...     10: Sacré cœur
# Distain!:   [Li:quíd]:        3: Summer 84
# Enya:       The Celts:       10: Triad: St. Patrick Cú Chulainn Oisin
# Enya:       The Celts:       14: Dan y Dŵr
# Röyksopp:   Junior:           5: Röyksopp Forever
# Žofka:      Bad Girls:        1: Woho
#
sub back_encoding {
   my $string = shift;
   my $utf_string = $string;
   if(utf8::is_utf8($string)) {
      print "The \$string is already in utf8, do nothing!\n"
      if($verbose >= 5);
   }
   else {
      $utf_string = Encode::decode('UTF-8', $utf_string, Encode::FB_QUIET);
   }
   my @utf_points = unpack("U0U*", "$utf_string"); # Perl 5.10
   print "\nutf_points:\n@utf_points\n" if($verbose >= 5);
   my $latinflag = 0;
   my $wideflag = 0;
   foreach (@utf_points) {
      $wideflag = 1 if($_ > 255);
      $latinflag++ if($_ > 128 && $_ < 256);
   }

   # It works with Röyksopp archive and freeCDDB entry.
   my @char_points = unpack("U0U*", "$string");


   @char_points = @utf_points if($wideflag == 1);

   return $string if($string eq "");
   my $decoded = "";
   foreach (@char_points) {
      if($_ > 255) {
         print "\"Wide\" char detected: $_.\n" if($verbose >= 5);
         use Unicode::UCD 'charinfo';
         my $charinfo = charinfo(sprintf("0x%X", $_));
         my $letter = $charinfo->{name};
         print "The charinfo is <$letter>.\n" if($verbose >= 5);
         my $smallflag = 0;
         $smallflag = 1 if($letter =~ /SMALL\sLETTER/);
         $smallflag = 1 if($letter =~ /SMALL\sLIGATURE/);
         $letter =~ s/^.*LETTER\s(\w+)\s.*/$1/;
         $letter =~ s/^.*LIGATURE\s(\w+)(\.|\s)*.*/$1/;
         $letter = "\L$letter" if($smallflag == 1);
         # Rather do nothing than print rubbish (string with words):
         $letter = $_ if($letter =~ /\s/);
         print "New letter will be: $letter.\n" if($verbose >= 5);
         $decoded .= $letter;
      }
      else {
         $decoded .= chr($_);
      }
   }

   if($cd{discid}) {
      # Special condition for MB data. Please do not ask why.
      if($wideflag == 0 && $latinflag == 0) {
      # Original.
#         Encode::from_to($decoded, 'utf-8', 'iso-8859-15');
      # But we come here in every case because we want the discid to be
      # present (in comment tags), but come from archive, not from MB.
         Encode::from_to($decoded, 'UTF8', 'iso-8859-15');
      }
      elsif($wideflag == 0) {
         Encode::from_to($decoded, 'utf-8', 'ISO-8859-15');
      }
   }
   elsif($wideflag == 0) {
      Encode::from_to($decoded, 'utf-8', 'ISO-8859-15');
   }
   return $decoded;
}
########################################################################
#
# Check the preset options.
#
sub check_preset {
   if($preset !~ /^\d/) {
      while($preset !~ /^insane$|^extreme$|^standard$|^medium$/) {
         print "\nPreset should be one of the following words! Please";
         print " Enter \ninsane (320\@CBR), extreme (256), standard";
         print " (192) or medium (160), (standard): ";
         $preset = <STDIN>;
         chomp $preset;
         if($preset eq "") {
            $preset = "standard";
         }
      }
   }
   else {
      while($preset !~ m/^32$|^40$|^48$|^56$|^64$|^80$|^96$|^112$|^128$|
                        |^160$|^192$|^224$|^256$|^320$/) {
         print "\nPreset should be one of the following numbers!",
               " Please Enter \n32, 40, 48, 56, 64, 80, 96, 112, 128,",
               " 160, 192, 224, 256 or 320, (128):\n";
         $preset = <STDIN>;
         chomp $preset;
         if($preset eq "") {
            $preset = 128;
         }
      }
   }
   $preset = "medium" if($preset =~ /\d+/ && $preset == 160);
   $preset = "standard" if($preset =~ /\d+/ && $preset == 192);
   $preset = "extreme" if($preset =~ /\d+/ && $preset == 256);
   $preset = "insane" if($preset =~ /\d+/ && $preset == 320);
   $wpreset = $preset;
}
########################################################################
#
# Check sshlist of remote machines and create a hash.
#
sub check_sshlist {
   if(@psshlist) {
      @sshlist = split(/,/, join(',', @psshlist));
   }
   if(@pthreads) {
      @threads = split(/,/, join(',', @pthreads));
   }
   $wthreads = join(',', @threads);
   if(@sshlist || $threads[0] > 1) {
      $sshflag = 1;
      $wsshlist = join(',', @sshlist);
      # Create a hash with all machines and the number of encoding
      # processes each machine is able to handle.
      $sshlist{'local'} = $threads[0] if($local == 1);
      my $threadscn = 1;
      foreach (@sshlist) {
         $threads[$threadscn] = 1 unless($threads[$threadscn]);
         $sshlist{$_} = $threads[$threadscn];
         $threadscn++;
      }
   }
   else {
      $sshflag = 0;
   }
}
########################################################################
#
# Dispatcher for encoding on remote machines. If there are no .lock
# files, a ssh command will be passed, else the dispatcher waits until
# an already passed ssh command terminates and removes the lock file.
# The dispatcher checks all machines all 6 seconds until a machine is
# available. If option --scp is used, the dispatcher will not start an
# other job while copying. In this situation, it looks like nothing
# would happen, but it's only during scp.
#
sub enc_ssh {
   my $machine;
   my @codwav = ();
   my $delwav = $_[0];
   my $enccom = $_[1];
   my $ripnam = $_[2];
   my $sepdir = $_[3];
   my $suffix = $_[4];
   my $old_wavdir = $wavdir;
   my $old_sepdir = $sepdir;
   my $old_ripnam = $ripnam;
   my $esc_name;
   my $esc_dir;
   my $threadscn;

   $sshflag = 2;
   while ($sshflag == 2) {
      # Start on the local machine first.
      $threadscn = 1;
      for($threadscn = 1; $threadscn <= $threads[0]; $threadscn++) {
         if(! -r "$wavdir/local.lock_$threadscn") {
            if($local == 1) {
               $sshflag = 1;
               $machine = "local";
               push @codwav, "$ripnam";
            }
         }
         last if($sshflag == 1);
      }
      last if($sshflag == 1);
      $threadscn = 1;
      foreach $_ (keys %sshlist) {
         $machine = $_; # Why this?
         for($threadscn = 1; $threadscn <= $sshlist{$_}; $threadscn++) {
            if(! -r "$wavdir/$machine.lock_$threadscn") {
               $sshflag = 1;
            }
            # Prepare array @codwav with all tracknames in, which are
            # still in progress, i. e. either being ripped or encoded.
            else {
               open(LOCK, "$wavdir/$machine.lock_$threadscn");
               my @locklines = <LOCK>;
               close(LOCK);
               if($locklines[0]) {
                  chomp(my $locklines = $locklines[0]);
                  # Push trackname into array only if not yet present.
                  my @presence = grep(/$locklines/, @codwav);
                  my $presence = $presence[0];
                  push @codwav, "$locklines" if(!$presence);
               }
            }
            last if($sshflag == 1);
         }
         last if($sshflag == 1);
      }
      last if($sshflag == 1);
      sleep 3;
   }

   if(-r "$wavdir/enc.log" && $verbose >= 3) {
      open(ENCLOG, ">>$wavdir/enc.log");
      print ENCLOG "...on machine $machine.\n"
         if($#threads > 1 || $machine !~ /^local$/);
      print ENCLOG "Executing scp command to $machine.\n"
         if($scp && $machine !~ /^local$/);
      close(ENCLOG);
   }
   elsif($verbose >= 3) {
      print "...on machine $machine.\n"
         if($#threads > 1 || $machine !~ /^local$/);
      print ENCLOG "Executing scp command to $machine.\n"
         if($scp && $machine !~ /^local$/);
   }
   open(LOCKF, ">$wavdir/$machine.lock_$threadscn");
   print LOCKF "$sepdir/$ripnam.$suffix\n";
   close(LOCKF);

   # We need more quotes for the commands (faac,flac,lame,ogg)
   # passed to the remote machine. NOTE: But now pay attention
   # to single quotes in tags. Quote them outside of single quotes!
   # TODO: Please tell me how to quote leading periods, thanks!!!
   if($machine !~ /^local$/) {
      $enccom =~ s/'/'\\''/g;
      $enccom = "ssh " . $machine . " '" . $enccom . "'";
      if($scp) {
         # *Create* the directory:
         # Quote the double quotes with a backslash when using ssh!
         $sepdir = esc_char($sepdir);
         $wavdir = esc_char($wavdir);
         log_info("new-outputdir: $sepdir on $machine created.");
         log_system("ssh $machine mkdir -p \\\"$sepdir\\\"");
         log_info("new-outputdir: $wavdir on $machine created.");
         log_system("ssh $machine mkdir -p \\\"$wavdir\\\"");
         # *Copy* the File:
         # Don't overwrite destination file, it will confuse running
         # encoders! Do it the hard way! First get all lock-file-names
         # of that machine. There will be at least one, created above!
         opendir(LOCK, "$old_wavdir") or
            print "Can not read in $old_wavdir: $!\n";
         my @boxes = grep {/^$machine/i} readdir(LOCK);
         close(LOCK);
         my $wavflag = 0;
         # Open each lock-file, read the content, increase counter if
         # the same wavname is found. Again: it will be found at least
         # once.
         foreach(@boxes) {
            open(LOCKF, "$old_wavdir/$_") or
               print "Can't open $old_wavdir/$_: $!\n";
            my @content = <LOCKF>;
            close(LOCKF);
            $wavflag++ if("@content" =~ /$ripnam/);
         }
         $ripnam = esc_char($ripnam);
         log_system("scp $wavdir/$ripnam.wav \\
           $machine:\"$wavdir/$ripnam.wav\" > /dev/null 2>&1")
           if($wavflag <= 1);
      }
   }
   else {
      # On the local machine escape at least the dollar sign.
      $ripnam =~ s/\$/\\\$/g;
      $sepdir =~ s/\$/\\\$/g;
   }
   $enccom = $enccom . " > /dev/null";
   # Because Lame comes with the "Can't get "TERM" environment string"
   # error message, I decided to switch off all error output. This is
   # not good, if ssh errors appear, then RipIT may hang with a message
   # "Checking for lock files". If this happens, switch to verbosity 4
   # or higher and look what's going on.
   $enccom = $enccom . " 2> /dev/null" if($verbose <= 3);
   if($machine !~ /^local$/ && $scp) {
      if($suffix eq "mpc") {
         $enccom = $enccom . " && \\
            scp $machine:\"$sepdir/$ripnam\_enc.$suffix\" \\
            $sepdir/$ripnam.$suffix > /dev/null 2>&1 && \\
            ssh $machine rm \"$sepdir/$ripnam\_enc.$suffix\" ";
      }
      elsif($suffix eq "wv") {
         $enccom = $enccom . " && \\
            scp $machine:\"$sepdir/$ripnam\_enc.$suffix\" \\
            $sepdir/$ripnam.$suffix > /dev/null 2>&1 && \\
            ssh $machine rm \"$sepdir/$ripnam\_enc.$suffix\" ";
            # TODO:
            # Copy correction file! Not yet supported.
      }
      else {
         $enccom = $enccom . " && \\
            scp $machine:\"$sepdir/$ripnam.$suffix\_enc\" \\
            $sepdir/$ripnam.$suffix > /dev/null 2>&1 && \\
            ssh $machine rm \"$sepdir/$ripnam.$suffix\_enc\" ";
      }
   }
   if($suffix eq "mpc") {
      $enccom = $enccom . " && \\
                mv \"$sepdir/$ripnam\_enc.$suffix\" \\
                \"$sepdir/$ripnam.$suffix\""
         if($machine eq "local" || ($machine !~ /^local$/ && !$scp));
   }
   elsif($suffix eq "wv") {
      $enccom = $enccom . " && \\
                mv \"$sepdir/$ripnam\_enc.$suffix\" \\
                \"$sepdir/$ripnam.$suffix\""
         if($machine eq "local" || ($machine !~ /^local$/ && !$scp));
         # TODO:
         # Copy correction file! Not yet supported.
   }
   else {
      $enccom = $enccom . " && \\
                mv \"$sepdir/$ripnam.$suffix\_enc\" \\
                \"$sepdir/$ripnam.$suffix\""
         if($machine eq "local" || ($machine !~ /^local$/ && !$scp));
   }
   $enccom = $enccom . " && \\
             rm \"$old_wavdir/$machine.lock_$threadscn\" &";

   # A huge hack only not to interfere with the ripper output.
   if($verbose >= 4) {
      my $ripmsg = "The audio CD ripper reports: all done!";
      my $ripcomplete = 0;
      if(-r "$wavdir/error.log") {
         open(ERR, "$wavdir/error.log")
            or print "Can not open file error.log!\n";
         my @errlines = <ERR>;
         close(ERR);
         my @ripcomplete = grep(/^$ripmsg/, @errlines);
         $ripcomplete = 1 if(@ripcomplete);
         if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
            open(ENCLOG, ">>$wavdir/enc.log");
            print ENCLOG "\n\nExecuting command on machine $machine",
                         " and trying to encode \n$ripnam.$suffix\_enc.\n";
            close(ENCLOG);
         }
         else {
            print "\nExecuting command on machine $machine and tring",
                  " to encode \n$ripnam.$suffix\_enc.\n";
         }
      }
      else {
         print "\nExecuting command on machine $machine and tring",
               " to encode \n$ripnam.$suffix\_enc.\n";
      }
   }
   log_system("$enccom");
   sleep 2; # Don't mess up with possible error-msgs from remote hosts.

   $wavdir = $old_wavdir;
   $sepdir = $old_sepdir;
   $ripnam = $old_ripnam;
   # Delete the wav only if all encodings of this track are done!
   # When the (last) encoding of a track started, its name is pushed
   # into the array @delname. Then the first (oldest) entry of the same
   # array (@delname) will be compared to the @codwav array. If this
   # entry is still present in the codewav-array, nothing happens, else
   # the wav file will be deleted and the trackname shifted out of the
   # @delname.
   if($delwav == 1) {
      push @delname, "$ripnam";
      my $delflag = 0;
      while ($delflag == 0) {
         my $delname = $delname[0];
         my @delwav = grep(/$delname/, @codwav);
         if(!$delwav[0] && $#delname > 1) {
            unlink("$wavdir/$delname.wav");
            log_info("File $wavdir/$delname.wav deleted.");
            shift(@delname);
            # Prevent endless loop if array is empty.
            $delflag = 1 if(!$delwav[0]);
         }
         else {
            $delflag = 1;
         }
      }
   }
}
########################################################################
#
# Delete wavs if sshlist was used. TODO: Improve code for following
# situation: if no .lock files are found, but the encoder did not yet
# finish, don't delete the wavs. Do it only after 3*4 seconds timeout
# with no .lock file.
#
sub del_wav {
   my $waitflag = 1;
   sleep 3;
   printf "\n%02d:%02d:%02d: ",
      sub {$_[2], $_[1], $_[0]}->(localtime) if($verbose > 1);
   print "Checking for remaining lock files.\n" if($verbose > 1);
   while ($waitflag <= 3) {
      sleep 3;
      opendir(DIR, "$wavdir");
      my @locks = readdir(DIR);
      closedir(DIR);
      @locks = grep { /\.lock_\d+$/ } @locks;
      $waitflag++ if(! @locks);
      $waitflag = 0 if(@locks);
   }
   if($wav == 0) {
      printf "\n%02d:%02d:%02d: ",
         sub {$_[2], $_[1], $_[0]}->(localtime) if($verbose > 1);
      print "Deleting the wavs...\n" if($verbose > 1);
      opendir(DIR, "$wavdir");
      my @wavs = readdir(DIR);
      closedir(DIR);
      @wavs = grep { /\.wav$/ } @wavs;
      foreach (@wavs) {
         unlink("$wavdir/$_");
         log_info("File $wavdir/$_ deleted.");
      }
   }
   if($scp) {
      foreach my $machine (keys %sshlist) {
         next if($machine =~ /local/);
         foreach my $deldir (@sepdir, $wavdir) {
            my $dd = $deldir;
            $dd = esc_char($dd);
            log_system("ssh $machine rm \"$dd/*.wav\" 2> /dev/null");
            log_system("ssh $machine rmdir -p \"$dd\" 2> /dev/null");
         }
      }
   }
}
########################################################################
#
# LCDproc subroutines, all credits to Max Kaesbauer. For comments and
# questions contact max [dot] kaesbauer [at] gmail [dot] com.
#

# print

sub plcd {
   my ($data) = @_;
   print $lcdproc $data."\n";
   my $res = <$lcdproc>;
}

# update

sub ulcd {
   if($lcdoline1 ne $lcdline1) {
      $lcdoline1 = $lcdline1;
      plcd("widget_set ripitlcd line1 1 2 {$lcdline1}");
       }
   if($lcdoline2 ne $lcdline2) {
      $lcdoline2 = $lcdline2;
      plcd("widget_set ripitlcd line2 1 3 {$lcdline2}");
   }
   if($lcdoline3 ne $lcdline3) {
      $lcdoline3 = $lcdline3;
      plcd("widget_set ripitlcd line3 1 4 {$lcdline3}");
   }
}

# init

sub init_lcd {
   $lcdproc = IO::Socket::INET->new(
      Proto     => "tcp",
      PeerAddr  => $lcdhost,
      PeerPort  => $lcdport,
   ) || die "Can not connect to LCDproc port\n";
   $lcdproc->autoflush(1);
   sleep 1;

   print $lcdproc "Hello\n";
   my @lcd_specs = split(/ /,<$lcdproc>);
   my %screen;

   $screen{wid} = $lcd_specs[7];
   $screen{hgt} = $lcd_specs[9];
   $screen{cellwid} = $lcd_specs[11];
   $screen{cellhgt} = $lcd_specs[13];

   $screen{pixwid} = $screen{wid}*$screen{cellwid};
   $screen{pixhgt} = $screen{hgt}*$screen{cellhgt};

   fcntl($lcdproc, F_SETFL, O_NONBLOCK);

   plcd("client_set name {ripit.pl}");
   plcd("screen_add ripitlcd");
   plcd("screen_set ripitlcd name {ripitlcd}");

   plcd("widget_add ripitlcd title title");
   plcd("widget_set ripitlcd title {RipIT $version}");

   plcd("widget_add ripitlcd line1 string");
   plcd("widget_add ripitlcd line2 string");
   plcd("widget_add ripitlcd line3 string");
}
########################################################################
#
# Read the CDDB on the local machine.
#
sub read_entry {
   my ($album, $artist, $trackno, $asin, $discid, $barcode,
       $language, $reldate);
   my $logfile = $_[0];
   open(LOG, "<$logfile") || print "Can't open $logfile\n";
   my @cddblines = <LOG>;
   close(LOG);
   %cd = ();
   # Note that long lines may be split into several lines
   # all starting with the same keyword, e.g. DTITLE.
   if($_[1] eq "musicbrainz" or $multi == 1) {
      chomp($artist = join('', grep(s/^artist:\s//i, @cddblines)));
      chomp($album = join('', grep(s/^album:\s//i, @cddblines)));
      chomp($categ = join('', grep(s/^category:\s//i, @cddblines)));
      chomp($genre = join('', grep(s/^genre:\s//i, @cddblines)));
      chomp($year = join('', grep(s/^year:\s//i, @cddblines)));
      chomp($cddbid = join('', grep(s/^cddbid:\s//i, @cddblines)));
      chomp($discid = join('', grep(s/^discid:\s//i, @cddblines)));
      chomp($asin = join('', grep(s/^asin:\s//i, @cddblines)));
      chomp($barcode = join('', grep(s/^barcode:\s//i, @cddblines)));
      chomp($language = join('', grep(s/^language:\s//i, @cddblines)));
      chomp($reldate = join('', grep(s/^reldate:\s//i, @cddblines)));
      chomp($trackno = join('', grep(s/^trackno:\s//i, @cddblines)));
      $trackno = $_[2] unless($trackno);
   }
   else {
      $cd{raw} = \@cddblines;
      chomp($artist = join(' / ', grep(s/^DTITLE=//g, @cddblines)));
      $artist =~ s/[\015]//g;
      $artist =~ s/\n\s\/\s//g;
      # Artist is just the first part before first occurrence of
      # the slash (/), album gets all the rest!
      my @disctitle = split(/\s\/\s/, $artist);
      $artist = shift(@disctitle);
      $album = join(' / ', @disctitle);
      chomp $artist;
      chomp $album;
      $categ = $_[1];
      unless($genre) {
         chomp($genre = join('', grep(s/^DGENRE=//, @cddblines)));
         $genre =~ s/[\015]//g;
      }
      unless($year) {
         chomp($year = join('', grep(s/^DYEAR=//, @cddblines)));
         $year =~ s/[\015]//g;
      }
      unless($discid) {
         chomp($discid = join('', grep(s/^MBID=//, @cddblines)));
         $discid =~ s/[\015]//g;
      }
      $trackno = $_[2];
   }
   $cd{artist} = $artist;
   $cd{title} = $album;
   $cd{cat} = $categ;
   $cd{genre} = $genre;
   $cd{id} = $cddbid;
   $cd{discid} = $discid;
   $cd{asin} = $asin;
   $cd{year} = $year;
   $cd{barcode} = $barcode;
   $cd{language} = $language;
   $cd{reldate} = $reldate;

   my $i = 1;
   my $j = 0;
   while($i <= $trackno) {
      my @track = ();
      @track = grep(s/^TTITLE$j=//, @cddblines) if($multi == 0);
      @track = grep(s/^track\s$i:\s//i, @cddblines)
         if($_[1] eq "musicbrainz" or $multi == 1);
      my $track = join(' ', @track);
      $track =~ s/[\015]//g;
      $track =~ s/\n\s\/\s//g;
      chomp $track;
      $cd{track}[$j] = $track;
      $i++;
      $j++;
   }
}
########################################################################
#
# Delete error.log if there is no track-comment in!
#
sub del_erlog {
   if(-r "$wavdir/error.log") {
      open(ERR, "$wavdir/error.log")
        or print "Fatal: $wavdir/error.log disappeared!\n";
      my @errlines = <ERR>;
      close(ERR);
      # Add missing coverart to files previously not done because of
      # option thread or sshlist.
      my @md5tracks = grep(s/^md5: //, @errlines) if($md5sum == 1);
      if(@md5tracks) {
         foreach (@md5tracks) {
            my ($sepdir, $donetrack) = split(/;#;/, $_);
            chomp $donetrack;
            # Add special mp3 tags.
            if(@mp3tags && $donetrack =~ /mp3$/) {
               mp3_tags("$sepdir/$donetrack") if($mp3tags[0] ne "");
            }
            # Add coverart if it is a mp3 or ogg.
            if($donetrack =~ /mp3$/ && -f "$coverpath" && -s "$coverpath") {
               mp3_cover("$sepdir/$donetrack", "$coverpath");
            }
            elsif($donetrack =~ /ogg$/ && -f "$coverpath" && -s "$coverpath") {
               ogg_cover("$sepdir/$donetrack", "$coverpath");
            }
         }
      }
      # Add album-gain once all files are present.
      for(my $c = 0; $c <= $#coder; $c++) {
         printf "\n%02d:%02d:%02d: ",
            sub {$_[2], $_[1], $_[0]}->(localtime) if($verbose > 2);
         print "Starting with album gain detection for $suffix[$c]-files.\n"
         if($verbose > 2);
         if($mp3gain && $suffix[$c] =~ /mp3/) {
            log_system("$mp3gain \"$sepdir[$c]/\"*.$suffix[$c]");
         }
         elsif($vorbgain && $suffix[$c] =~ /ogg/) {
            log_system("$vorbgain \"$sepdir[$c]/\"*.$suffix[$c]");
         }
         elsif($flacgain && $suffix[$c] =~ /flac/) {
            log_system("$flacgain \"$sepdir[$c]/\"*.$suffix[$c]");
         }
         elsif($aacgain && $suffix[$c] =~ /m4a|mp4/) {
            log_system("$aacgain \"$sepdir[$c]/\"*.$suffix[$c]");
         }
         elsif($mpcgain && $suffix[$c] =~ /mpc/) {
            log_system("$mpcgain \"$sepdir[$c]/\"*.$suffix[$c]");
         }
         elsif($wvgain && $suffix[$c] =~ /wv/) {
            log_system("$wvgain \"$sepdir[$c]/\"*.$suffix[$c]");
         }
         else {
            print "\nNo album gain command found for $suffix[$c].\n"
            if($verbose > 5);
         }
      }
      # Now, once all tagging is done, continue with md5sum calculation.
      printf "\n\n%02d:%02d:%02d: ",
         sub {$_[2], $_[1], $_[0]}->(localtime)
         if($verbose > 2 && $md5sum == 1);
      print "Starting with md5sum calculation.\n"
      if($verbose > 2 && $md5sum == 1);
      # Final sound file stuff
      my $album = clean_all($album_utf8);
      my $riptrackname;
      foreach (@tracksel) {
         $riptrackname = get_trackname($_, $tracklist[$_ - 1]);
         $riptrackname = get_trackname($_, $tracklist[$_])
            if($hiddenflag == 1);
         $riptrackname = $album if($book == 1 or $cdcue > 0);
         for(my $c = 0; $c <= $#coder; $c++) {
            chmod oct($fpermission),
               "$sepdir[$c]/$riptrackname.$suffix[$c]"
               if($fpermission);
            # Generate md5sum of files.
            if($md5sum == 1) {
               if(-r "$sepdir[$c]/$riptrackname.$suffix[$c]") {
                  md5_sum("$sepdir[$c]",
                     "$riptrackname.$suffix[$c]", 1);
               }
            }
         }
         last if($cdcue > 0);
      }


      # Change file permissions for md5 files.
      if($fpermission && $md5sum == 1){
         foreach(@sepdir, $wavdir) {
            opendir(MD5, "$_") or print "Can not read in $_: $!\n";
            my @md5files = grep {/\.md5$/i} readdir(MD5);
            close(MD5);
            # Security check: if encoder not installed, but directory
            # created, then no md5sum-file will be found and the
            # directory instead of the file gets the permissions.
            next unless($md5files[0]);
            if($_ eq $wavdir) {
               chmod oct($fpermission), "$_/$md5files[0]" if($wav == 1);
            }
            else {
               chmod oct($fpermission), "$_/$md5files[0]";
            }
         }
      }
      chmod oct($fpermission), "$wavdir/cd.toc" if($fpermission);
      my @ulink = grep(/^Track /, @errlines);
      if(!@ulink && $multi == 0) {
         unlink("$wavdir/error.log");
      }
      elsif($fpermission) {
         chmod oct($fpermission), "$wavdir/error.log";
      }
      if($ghost == 1&& -r "$wavdir/ghost.log") {
         unlink("$wavdir/ghost.log");
      }
      if($wav == 0 && $wavdir ne $homedir) {
         # I don't like the -p option.
         log_system("rmdir -p \"$wavdir\" 2> /dev/null");
      }
   }
}
########################################################################
#
# Escape special characters when using scp.
#
sub esc_char {
   $_[0] =~ s/\(/\\\(/g;
   $_[0] =~ s/\)/\\\)/g;
   $_[0] =~ s/\[/\\\[/g;
   $_[0] =~ s/\]/\\\]/g;
   $_[0] =~ s/\&/\\\&/g;
   $_[0] =~ s/\!/\\\!/g;
   $_[0] =~ s/\?/\\\?/g;
   $_[0] =~ s/\'/\\\'/g;
   $_[0] =~ s/\$/\\\$/g;
   $_[0] =~ s/ /\\ /g;
   return $_[0];
}
########################################################################
#
# Calculate how much time ripping and encoding needed.
#
sub cal_times {
   my $encend = `date \'+%R\'`;
   chomp $encend;
   # Read times from the file $wavdir/error.log.
   open(ERR, "$wavdir/error.log")
      or print "Can't calculate time, $wavdir/error.log disappeared!\n";
   my @errlines = <ERR>;
   close(ERR);
   my @enctime = grep(s/^Encoding needed //, @errlines);
   my @ripstart = grep(s/^Ripping started: //, @errlines);
   my @ripend = grep(s/^Ripping ended: //, @errlines);
   chomp(my $blanktrks = join(', ', grep(s/^Blankflag = //, @errlines)));
   chomp(my $ghostrks = join(', ', grep(s/^Ghostflag = //, @errlines)));
   chomp(my $splitrks = join(', ', grep(s/^Splitflag = //, @errlines)));
   $blanktrks =~ s/\n//g;
   $ghostrks =~ s/\n//g;
   $splitrks =~ s/\n//g;
   $blanktrks =~ s/,\s(\d+)$/ and $1/g;
   $ghostrks =~ s/,\s(\d+)$/ and $1/g;
   $splitrks =~ s/,\s(\d+)$/ and $1/g;

   @ripstart = split(/:/, $ripstart[0]);
   @ripend = split(/:/, $ripend[0]);
   $ripend[0] += 24 if($ripend[0] < $ripstart[0]);
   my $riptime = ($ripend[0] * 60 + $ripend[1]) -
                 ($ripstart[0] * 60 + $ripstart[1]);

   my $enctime = "@enctime";
   chomp $enctime;
   if($encode == 1) {
      @enctime = split(/ /, $enctime);
      $enctime[0] = 0 unless(@enctime);
      $enctime = int($enctime[0]/60);
   }
   else {
      $enctime = 0;
   }
   return ($riptime,$enctime,$encend,$blanktrks,$ghostrks,$splitrks);
}
########################################################################
#
# Thanks to mjb: log info to file.
#
sub log_info {
   if(!defined($infolog)) { return; }
   elsif($infolog eq "") { return; }
   open(SYSLOG, ">>$infolog") or
   print "Can't open info log file <$infolog>.\n";
   print SYSLOG "@_\n";
   close(SYSLOG);
}
########################################################################
#
# Thanks to mjb and Stefan Wartens improvements:
# log_system used throughout in place of system() calls.
#
sub log_system {
   my $P_command = shift;
   if($verbose > 3) {
      # A huge hack only not to interfer with the ripper output.
      if($P_command =~ /faac|flac|lame|machine|mpc|mp4als|oggenc/ &&
         $P_command !~ /cdparanoia|cdda2wav|dagrab|icedax|vorbiscomment/) {
         my $ripmsg = "The audio CD ripper reports: all done!";
         my $ripcomplete = 0;
         if(-r "$wavdir/error.log") {
            open(ERR, "$wavdir/error.log")
               or print "Can not open file error.log!\n";
            my @errlines = <ERR>;
            close(ERR);
            my @ripcomplete = grep(/^$ripmsg/, @errlines);
            $ripcomplete = 1 if(@ripcomplete);
            if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
               open(ENCLOG, ">>$wavdir/enc.log");
               print ENCLOG "\n$P_command\n\n";
               close(ENCLOG);
            }
            else {
               print "system: $P_command\n\n";
            }
         }
      }
      else {
         print "system: $P_command\n\n";
      }
   }

   # Log the system command to logfile unless it's the coverart command
   # for vorbiscomment with the whole binary picture data.
   log_info("system: $P_command") unless($P_command =~ /vorbiscomment/);

   # Start a watch process to check progress of ripped tracks.
   if($parano == 2 && $P_command =~ /^cdparano/
                   && $P_command !~ /-Z/
                   && $P_command !~ /-V/) {
      my $pid = 0;
      # This is probably dangerous, very dangerous because of zombies...
      $SIG{CHLD} = 'IGNORE';
      unless($pid = fork) {
         exec($P_command);
         exit;
      }
      # ... but we check and wait for $pid to finish in subroutine.
      my $result = check_ripper($P_command, $pid);
      waitpid($pid, 0);
      $SIG{CHLD} = 'DEFAULT';
      return $result;
   }
   else {
      system($P_command);
   }

   # system() returns several pieces of information about the launched
   # subprocess squeezed into a 16-bit integer:
   #
   #     7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0
   #   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   #   |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  [ $? ]
   #   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   #    \_____________________/ \/ \__________________/
   #          exit code        core    signal number
   #
   # To get the exit code, use        ($? >> 8)
   # To get the signal number, use    ($? & 127)
   # To get the dumped core flag, use ($? & 128)

   # Subprocess has been executed successfully.
   return 1 if $? == 0;

   # Subprocess was killed by SIGINT (CTRL-C). Exit RipIT.
   die "\n\nRipit caught a SIGINT.\n" if(( $? & 127) == 2);

   # Subprocess could not be executed or failed.
   return 0;
}
########################################################################
#
# Special characters in cd.toc file won't be written correctly by
# cdrdao, so change them to octal!
#
# Thanks to pancho horrillo:
# http://perldoc.perl.org/perluniintro.html#Displaying-Unicode-As-Text
#
sub oct_char {
   $_[0] = join '',
               map { $_ > 191
                     ? sprintf '\%o', $_
                     : chr $_
               } unpack("C0U*", "$_[0]");
}
########################################################################
#
# Check if there is a CD in the CD device. If a CD is present, start
# process. If not, wait until operator inserts a CD, i.e. come back!
# Problem: when used with option loop, the CD already done should not
# be reread again. In this case, don't close the tray automatically.
#
sub cd_present {
   sysopen(CD, $scsi_cddev, O_RDONLY | O_NONBLOCK) or return;
   my $os = `uname -s`;
   my $CDROMREADTOCHDR = 0x5305;            # Linux
   if($os eq "SunOS") {
      $CDROMREADTOCHDR = 0x49b;
   }
   elsif($os =~ /BSD/i) {
      $CDROMREADTOCHDR = 0x40046304;
   }
   my $tochdr = "";
   my $err = ioctl(CD, $CDROMREADTOCHDR, $tochdr);
   close(CD);
   return $err;
}
########################################################################
#
# A hack to reinitialize global variables before starting a new loop.
#
sub init_var {
   $categ            = "";
   $cddbid           = 0;
   @framelist        = ();
   @secondlist       = ();
   @tracklist        = ();
   @tracktags        = ();
   @seltrack         = ();
   @tracksel         = ();
   %cd               = ();
   $cddbsubmission   = 2;
   $hiddenflag       = 0;
   $wavdir           = "";
   @sepdir           = ();
}
########################################################################
#
# Get the revision number of the CDDB entry.
#
sub get_rev {
   my @revision = grep(/^\#\sRevision:\s/, @{$cd{raw}});
   my $revision = join('', grep(s/^\#\sRevision:\s//, @revision));
   chomp $revision if($revision);
   return $revision;
}
########################################################################
#
# Change case to lowercase and uppercase first if wanted. Test command:
# perl -e '$string="gLabber (live/mix:radio edit [perl re-mix])"; $string=~s/(\w+)/\u\L$1/g; print "\nString is: $string\n"'
#
sub change_case {
#   use encoding "utf8"; # This will break every single non ascii char!
   if($lowercase == 1 or $uppercasefirst == 1) {
      $_[0] = lc($_[0]);
      $_[0] =~ tr/[ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ]/[àáâãäåæçèéêëìíîï]/;
      $_[0] =~ tr/[ÐÑÒÓÔÕÖØÙÚÛÜÝÞ]/[ðñòóôõöøùúûüýþ]/;
   }
   if($uppercasefirst == 1) {
      # s/(\w+)/\u\L$1/g; # Does not work with non ascii chars...
      my @words = split(/ /, $_[0]);
      foreach (@words) {
         s/(\w+)/\u\L$1/g; # Ensure ucfirst within brackets etc.
         $_ = "\u$_";
         $_ =~ tr/^[àáâãäåæçèéêëìíîï]/[ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏ]/;
         $_ =~ tr/^[ðñòóôõöøùúûüýþ]/[ÐÑÒÓÔÕÖØÙÚÛÜÝÞ]/;
      }
      $_[0] = join(' ', @words);
   }
   return $_[0];
}
########################################################################
#
# Strip dodgey chars I. This will be done for file names and tags.
#
# TODO: Do we really have to erase all of them? Maybe we should keep
# some for the tags...
#
sub clean_all {
   $_[0] =~ s/[;><"\015]//g;
   $_[0] =~ s/\`/\'/g;
   $_[0] =~ s/´/\'/g;
   $_[0] =~ s/\s+/ /g;
   return $_[0];
}
########################################################################
#
# Strip dodgey chars II. This will only be done for file names.
#
sub clean_name {
   $_[0] =~ s/[*]//g;
   $_[0] =~ s/\// - /g;
   $_[0] =~ s/\s+/ /g;
   return $_[0];
}
########################################################################
#
# Strip dodgey chars III. This will optionally be done for file names
# and paths. Remember the default chars to be erased are:   |\:*?$  plus
# blanks and periods at begin and end of file names and directories.
# But the ending periods problem is not done here, because
# it should only affect directories, not files which have a suffix
# anyway! See subroutine create_dirs!
#
sub clean_chars {
   # Delete beginning blanks in directory names.
   $_[0] =~ s,/\s+,/,g if($chars =~ /NTFS/);
   # Delete beginning blanks in file names.
   $_[0] =~ s/^\s+//g if($chars =~ /NTFS/);
   # Delete beginning periods in directory names.
   $_[0] =~ s,/\.+,/,g if($chars =~ /HFS|NTFS/);
   # Delete beginning perods in file names.
   $_[0] =~ s/^\.+//g if($chars =~ /HFS|NTFS/);
   my $purged_chars = $chars;
   $purged_chars = ":" if($chars =~ /HFS/);
   $purged_chars = "[|\\\\:*?\$]" if($chars =~ /NTFS/);
   $_[0] =~ s/$purged_chars//g;
   $_[0] =~ s/\s+/ /g;
   $_[0] =~ s/\s$//;
   return $_[0];
}
########################################################################
#
# Put all chars in brackets and escape some.
#
sub check_chars {
   $chars =~ s/\\/\\\\/;
   $chars =~ s/-/\\-/;
   $chars =~ s/]/\\]/;
   $chars =~ s/\s/\\s/;
   $chars = "[" . $chars . "]" unless($chars =~ /HFS|NTFS|off/);
}
########################################################################
#
# Extract the CDDB comment lines starting with EXTD=.
# NOTE: Each EXTD line my have \n's, but two EXTD lines do NOT
# mean that there's a linebreak in between! So, what we have to do
# is, put all comment lines into a string and split the string
# according to the explicitly \n's (i.e. use \\n).and add a \n at the
# end of each line!
#
sub extract_comm {
   my @comment = grep(/^EXTD=/, @{$cd{raw}});
   @comment = grep(s/^EXTD=//, @comment);
   my $line = "@comment";
   $line =~ s/[\015]//g;
   @comment = split(/\\n/, $line);
   foreach (@comment) {
      chomp $_;
      $_ =~ s/^\s+//g;
   }
   return (@comment);
}
########################################################################
#
# Display a help page and exit.
#
# New options step 12: Add a short explanation for option --help and do
# not forget to update the manpage ripit.1 to be checked with option -l.
#
sub print_help {
   print <<END

SYNOPSIS:
   ripit [options]

OPTIONS:
 [track_selection]
              If not specified, all tracks will be ripped. Type a number
              or a selection of tracks using numbers separated by commas
              or hyphens, default: not set.
 -I, --span number-number
              Give a span or interval to rip only a part of the track.
              The cdparanoia notation is used in the format hh:mm:ss.ff
              without brackets. The hyphen is mandatory.
 --merge ordered list of comma separated intervals
              Place a hyphen or a + between first and last tracknumber
              to be merged, default: not set.
 -o, --outputdir directory
              Where the sound should go. If not set, \$HOME will be used.
              Default: not set.
 --dpermission number
              Define directory permissions, default: 0755.
 --fpermission number
              Define permissions of sound and log files, default: not
              set, i.e. depending on the system settings.
 -d, --device cddevice
              Path of audio CD device, default: /dev/cdrom.
 --scsidevice cddevice
              Devicename for a different device node of cddevice
              where non ripping commands shall be executed.
 -r, --ripper number
              0 dagrab, 1 cdparanoia, 2 cdda2wav, 3 tosha, 4 cdd,
              default: 1.
 --ripopt options
              Additional options for specific ripper. Default: not set.
 --nicerip value
              Set niceness of ripping process, default: 0.
 -Z, --disable-paranoia [number]
              When using dagrab, the number of retries will be set to 3,
              with cdparanoia this option is equal to the -Z option of
              cdparanoia. Usefull for faster ripping but not recommended.
              Use no argument or 1 to swith paranoia off or 2 if failed
              tracks should be done again without paranoia (only one
              retry). Default: off (i.e. paranoia on).
 -G, --ghost  Analyze wav and split into possible chunks of sound or try
              to trim lead-in/out. This may override option merge!
              Delete blank tracks if only silence ("zero bytes") are
              found. Experimental! Default: off.
 --extend seconds
              Enlarge splitted chunk by number of seconds if possible,
              or track may be trimmed if value is small (e.g. 0.2), use
              with caution! default: 2.0.
 --prepend seconds
              Enlarge splitted chunk by number of seconds if possible,
              or track may be trimmed if value is small (e.g. 0.2), use
              with caution! Default: 2.0.
 -c, --coder encoder
              0 Lame, 1 Oggenc, 2 Flac,  3 Faac, 4 mp4als, 5 Musepack,
              6 Wavpack, 7 ffmpge, a comma separated list or use -c for
              each encoder.
              The same encoder may be stated more than once. Adapt
              --dirtemplate in this case, see below. Default: 0.
 --musenc name
              Pass the command line name of Musepack encoder, e. g.
              mppenc. Default: mpcenc.
 --faacopt Faac-options
              Pass other options to the encoder,  quote them with double
              quotes if needed; comma separated list if same enocder
              is used more than once. Default: not set.
 --flacopt    Flac-options
              Same as above.
 --lameopt    Lame-options
              Same as above.
 --museopt    Musepack-options
              Same as above.
 --mp4alsopt  mp4als-options
              Same as above.
 --oggencopt  Oggenc-options
              Same as above.
 --wavpacopt  Wavpack-options
              Same as above.
 --ffmpegopt  ffmpeg-options
              Same as above.
 --ffmpegsuffix suffix
              Suffix of the choosen encoder in ffmpeg, a comma sparated
              list; default: not set.
 -q, --quality quality
              A comma separated list of values or the word \"off\", passed
              in the same order as the list of encoders! If no encoders
              passed, follow the order of the config file! No quality
              for wavpack and ffmpeg, use options instead. Default
              5,3,5,100,0,5.
 -v, --vbrmode mode
              Variable bitrate, only used with Lame, mode is new or old,
              see Lame manpage.  The Lame-option quality will be changed
              to -V instead of -q if vbr-mode is used; default: not set.
 -b, --bitrate rate
              Encode \"mp3\" at this bitrate for Lame. If option --vbrmode
              used, bitrate is equal to the -b option, so one might want
              to set it \"off\", default: 128.
 -B, --maxrate rate
              maxrate (Bitrate) for Lame using --vbrmode is equal to the
              -B option for Lame or the -M option for Oggenc,
              default: 0.
 -S, --preset mode
              Use the preset switch when encoding with Lame. With option
              --vbrmode new --preset fast will be used. Default: off.
 -W, --chars [list]
              Exclude special characters and  (ending!)  periods in file
              names and path. The argument is optional. Following
              characters will be erased, if no argument is stated:
              |\\:*?\$  else only ending periods and all passed ones.
              Default: off.
 --comment comment
              Specify a comment tag (mp3, m4a), or a description tag for
              (ogg, flac). To write the cddbid used for freedb
              or the MusicBrainz discid into the comment, use the word
              \"cddbid\" or \"discid\". Default: not set.
 -g, --genre genre
              Specify (and  override CDDB)  genre,  must be a valid ID3-
              genre name  if using Lame, can (but shouldn't) be anything
              if using other encoders, default: not set.
 -y, --year year
              Specify (and override CDDB) year tag (mp3, m4a), or a date
              tag (ogg, flac), default: not set.
 -D, --dirtemplate '\" foo \$parameters \"'
              Use single and double quotes to pass the parameters of the
              templates. More than one --dirtemplate may be stated, or
              use variables \$quality and \$suffix. See manpage for more
              info. Default: '\"\$artist - \$album\"'
 -T, --tracktemplate '\"foo \$parameters\"'
              See above. Only one tracktemplate can be stated. Default:
              '"\$tracknum \$trackname"'.
--trackoffset number
              Use an offset to be added to \$tracknum, default 0.
--coverart list
              Add coverart to the sound files. Comma separated list
              according to option coder with values 0 (no) or 1 (yes),
              default 0.
--coverpath path
              Path to the coverart picture to be included in the
              metadata of the sound files, default: not set.
--copycover path
              Copy an image (may also be any other file) to all
              directories containing encoded files.
              Value: absolute path to file. Default: not set.
--mp3tags FRAME=Tag
              Additional frames to be added to the mp3 file if encoder
              does not support the frame or if some unofficial FRAMEs
              shall be used. More than one --mp3tags can be used if
              several tags shall be added. Default: not set.
--vatag number
              Analyze tracknames for "various artists" style and split
              the metadata in case one of the delimiters (colon, hyphen,
              slash or parenthesis) are found. Use unpair numbers for
              the scheme "artist ? tracktitle" and pair numbers in the
              opposite case. Default: not set.
--vastring string
              A string (regular expression) that defines the "various
              artists" style, default: \bVA\b|Variou*s|Various\\sArtists
 --flacgain   Flacgain command with options but no filenames, e.g.
              metflac.
 --mp3gain    mp3gain command with options but no filenames.
 --mpcgain    mpdgain command with options but no filenames.
 --aacgain    aacgain command with options but no filenames.
 --vorbgain   vorbisgain command with options but no filenames.
 --wvgain     wvgain command with options but no filenames.
 --sshlist list
              Comma separated list of remote machines where RipIT should
              encode. Default: not set.
 --scp        If the filesystem can not be accessed on the remote
              machines, copy the wavs to the remote machines,
              default: off.
 --local      Only used with option --sshlist; if all encodings shall be
              done on remote machines, use --nolocal, default: on.
 --mb         Use musicbrainz instead of freedb, default: off.
 --mbname login
              Give MB login name to submitt ISRCs to the database. Saved
              in plain when using a config, default not set.
 --mbpass password
              Give MB password to submitt ISRCs to the database. Saved
              in plain when using a config, default not set.
 --isrc number
              Enable ISRC detection and submission to MB (1 yes, 0 no);
              default: 0
 -C, --cddbserver server
              CDDB server, default freedb.org. Note, the full address is
              \"mirror\".freedb.org, i. e. default is freedb.freedb.org.
 -m, --mirror mirror
              Choose \"freedb\" or one of the possible freedb
              mirrors, default: freedb.
 -L, --protocol level
              CDDB protocol level for CDDB query. Level 6 supports UTF-8
              and level 5 not. Default: 6
 -P, --proxy address
              The http proxy to use when accessing the cddb server.  The
              CDDB protocol must be http! Default: not set.
 -t, --transfer mode
              Transfer mode, cddb or http, will set default port to 8880
              or 80 (for http), default: cddb.
 -n, --nice value
              Set niceness of encoding process, default: not set.
 -a, --archive
              Read and save CDDB files in  \$HOME/.cddb/\"category\"
              directory. Default: off.
 -e, --eject  Ejects the CD when finished, default off.
 --ejectcmd cmd
              Command to use for ejecting CD (see --eject), default:
              eject.
 --ejectopt options
              Arguments to the ejecting CD command (see --ejectcmd),
              default: path of CD device.
 --halt       Powers off  the machine when finished if the configuration
              supports it, default: off.
 -s, --submission
              Specify --nosubmission if the computer is  offline and the
              created file cddb.toc shall be saved in the home directory
              instead of being submitted. With option  --archive it will
              also be saved in the \$HOME/.cddb directory. Default: on.
 -M, --mail address
              Users return email address, needed for submitting an entry
              to freedb.org. Default: not set.
 -p, --playlist number
              Create a m3u playlist file with full paths in filenames.
              For filenames without paths use --playlist 2. To prevent
              playlist creation, use: --playlist 0. Default: 1 - on.
 -i, --interaction
              Specify --nointeraction if ripit shall take the first CDDB
              entry found and rip without any questioning. Default: on.
 --lcd        Use lcdproc to display status, default: on.
 --lcdhost    Specify the lcdproc host, default: localhost.
 --lcdport    Specify the lcdport, default: 13666.
 --infolog file
              Log operations (system calls,  file/directory creation) to
              file, given with full path; default: not set.
 -l, --lowercase
              Lowercase filenames, default: off.
 -u, --underscore
              Use underscores _ instead of spaces in filenames, default:
              off.
 --uppercasefirst
              Uppercase first characters of each word in filenames and
              tags, default: off.
 -U, --utftag If negated decodes Lame-tags to ISO8859-1. Default: off.
 --rip        Rip the CD, to be used as --norip if wavs are present.
              Default: on.
 --encode     Prevent encoding (generate only wavs) with --noencode.
              Default: on.
 -w, --wav    Keep the wav files after encoding instead of deleting them
              default: off.
 -N, --normalize
              Normalizes the wav-files to a given dB-value (default:
              -12dB). Default: off.
 --normcmd    Command to use for normalizing, default: normalize.
 -z, --normopt
              Options to pass to normalize. For further options see
              normalize documentation (http://normalize.nongnu.org).
              Keeping the default value of -12dB is recommended.
              Default: -b. Option v will be set according to verbosity.
 --cdtoc n
              n=1: Create a toc file to burn the wavs with cd-text using
              cdrdao or cdrecord (in dao mode), default: 0 - off.
 --cdcue n
              n=1: Create a cue file to burn the wavs with cd-text,
              default: 0 - off.
 --inf n
              n=1: Creat inf files for each track to burn the wavs with
              cd-text using wodim or cdrecord (in dao mode),
              default: 0 - off.
 -h, --help   Print this and exit.
 -V, --version
              Print version and exit.
 -x, --verbose number
              Run silent (0), with minimal (1), normal without encoder
              messages (2), normal (3), verbose (4) or extremely verbose
              (5). Default 3
 --config     Read parameters from config file or specify  --noconfig to
              prevent reading it; default: on.
 --save       Add parameters passed on command line to config file. This
              options does not  overwrite other  settings.  An  existing
              config file will be saved as config.old. Default: off.
 --savenew    Save all parameters passed on command line to a new config
              file, backup an existing file to config.old. Default: off.
 -A, --book number
              Create an audiobook, i. e. merge all tracks into one sinlge
              file, option --ghost will be switched off and file suffix
              will be m4b. Make sure to use encoder faac. A chapter file
              will be written for chapter marks. Default: off
 --loop number
              Continue to ripp and encode as soon as the previous CD has
              finished. This option forces ejection of the CD. Set
              number to 2 for immediate restart of ripping process,
              experimental. Default off.
 --quitnodb value
              Give up CD if no CDDB entry found.
              Possible values: 0 - off, 1 - on, default: off
 --resume     Resume a previously started session. Default: not set.
 - O, --overwrite argument
              Overwrite existing rip (y), quit if directory exists (q)
              or force ejection of disc if directory exists (e). Default
              off (n), do not overwrite existing directories, use a
              directory name with a suffix instead.
 --md5sum     Create a MD5-sum file for each type of sound files.
              Default: not set.
 --threads number
              Comma separated list of numbers giving maximum of allowed
              encoders to run at the same time, default: 1.
 --execmd command
              State a command to be executed when ripit finshed. Make
              sure to escape the command if needed. Default: not set.
 --precmd command
              State a command to be executed when ripping starts. Make
              sure to escape the command if needed. Default: not set.


SEE ALSO
       ripit(1), cdparanoia(1), lame(1), oggenc(1), flac(1),
       normalize(1), cdda2wav(1).

AUTHORS
       RipIT is now maintained by Felix Suwald, please send bugs, wishes
       comments to ripit _[at]_ suwald _[dot]_ com. For bugs, wishes and
       comments about lcdproc, contact max.kaesbauer [at] gmail[dot]com.
       Former maintainer:  Mads Martin Joergensen;  RipIT was originally
       developed by Simon Quinn.

DATE
       14 July 2010

END
}
########################################################################
#
# Display available options and exit!
#
# New options step 13: Add the new options to the short help/error msg.
#
sub print_usage {
   print <<END

Usage:
ripit [--device|d cd-device] [--scsidevice path] [--outputdir|o path]
      [--dirtemplate '\"\$parameters\"'] [--chars|W [list]]
      [--tracktemplate '\"\$parameters\"'] [--trackoffset number]
      [--dpermission number] [--fpermission number]
      [--overwrite argument] [--resume|R]
      [--rip] [--ripper|r cdripper] [--ripopt ripper-options]
      [--nicerip number] [--disable-paranoia|Z] [--wav|w]
      [--ghost|G] [--extend seconds] [--prepend seconds]
      [--quitnodb value] [--encode] [--coder|c encoders] [--musenc cmd]
      [--faacopt options] [--flacopt options] [--oggencopt options]
      [--lameopt options] [--mp4alsopt options] [--museopt options]
      [--wavpacopt options] [--ffmpegopt options] [--ffmpegsuffix suffix]
      [--quality qualities-list] [--bitrate|b rate]
      [--maxrate|B rate] [--vbrmode|v old or new] [--preset|S mode]
      [--vatag number] [--vastring string or regular expression]
      [--comment id3-comment] [--genre|g genre-tag] [--year|y year-tag]
      [--mp3gain| cmd options] [--vorbgain| cmd options]
      [--flacgain| cmd options] [--aacgain| cmd options]
      [--mpcgain| cmd options] [--wvgain| cmd options]
      [--utftag|U] [--lowercase|l] [--underscore|u] [--uppercasefirst]
      [--coverart list] [--coverpath path] [--copycover path]
      [--mp3tags frame-tag=string] [--proxy|P path] [--mb]
      [--mbname MB-login] [--mbpass MB-password] [--isrc number]
      [--cddbserver|C server] [--mirror|m mirror] [--protocol|L level]
      [--transfer|t cddb or http] [--submission|s] [--mail|M address]
      [--eject|e] [--ejectcmd command] [--ejectopt options for command]
      [--lcd] [--lcdhost host] [--lcdport port]
      [--config] [--save] [--savenew]
      [--sshlist remote hosts] [--local] [--scp] [--threads numbers]
      [--archive|a] [--playlist|p number] [--infolog path] [--md5sum]
      [--cdtoc number] [--inf number] [--cdcue number]
      [--loop number] [--verbose|x number]
      [--normalize|N] [--normcmd] [--normopt|z options]
      [--interaction|i] [--nice|n adjustment] [--halt]
      [--help|h] [--version|V] [--precmd cmd] [--execmd|X cmd]
      [--book|A number] [--merge list] [--span|I span] [track_selection]


For general usage information see the manpage or type:
       ripit --help | less
or try to run
       ripit
without any options.

END
}
########################################################################
#
# Define the tracks to be skipped, change the passed values of the form
# 2+3,5-7 to an array @skip with 3,6,7. Note that we use the untouched
# variable $pmerge to determine the tracks to be skipped.
# In the same time, the intervals have to be tested if valid.
#
sub skip_tracks {
   my @merge = split(/,/, $pmerge);
   foreach (@merge) {
      # Split each interval into a BeginEndArray.
      my @bea = split(/-|\+/, $_);
      my $i = $bea[0] + 1;
      # Missing separator in command line argument?
      if($#bea > 1) {
         print "\nStrange interval in argument of option merge ($_)!",
               "\nIs there a comma missing?\n\n";
         exit;
      }
      # Operator forgot to give last track or wanted the whole CD to be
      # merged. But don't add zeros if we come here from the initial
      # argument check when the CD-data is still unknown.
      if($#tracklist >= 0) {
         $pmerge .= $#tracklist + 1 unless($bea[1]);
         $bea[1] = $#tracklist + 1 unless($bea[1]);
      }
      # Track number larger than number of tracks on CD?
      if($#tracklist > 0) {
         if($bea[0] > $#tracklist + 1 || $bea[1] > $#tracklist + 1) {
            print "\nWrong interval in argument of option merge ($_)!",
                  "\nHigher track number than tracks on CD?\n\n";
            exit;
         }
      }
      while($i <= $bea[$#bea]) {
         push(@skip, $i);
         $i++;
      }
   }
   return(@skip);
}
########################################################################
#
# Read the header of the wav file yet still called $trn.rip.
#
sub get_wavhead {
   my $trn = shift;
   my $prn = shift;
   open(IN, "< $wavdir/$trn") or print "Can't open $trn: $!\n";
   binmode(IN);
   my $H = {};
   $H->{header_size} = 44;
   my $wavheader;
   print "Can not read full WAV header!\n"
      if($H->{header_size} != read(IN, $wavheader, $H->{header_size}));
   close(IN);

   # Unpack the wav header and fill all values into a hashref.
   ($H->{RIFF_header},     $H->{file_size_8},      $H->{WAV_header},
    $H->{FMT_header},      $H->{WAV_chunk_size},   $H->{WAV_type},
    $H->{channels},        $H->{sample_rate},      $H->{byte_per_sec},
    $H->{block_align},     $H->{bit_per_sample},   $H->{data_header},
    $H->{data_size}
   ) = unpack("a4Va4a4VvvVVvva4V", $wavheader);

   $H->{sample_size} = ($H->{channels} * $H->{bit_per_sample})>>3;
   if($verbose >= 4 && $prn == 1) {
      print "\nThe wav header has following entries:\n";
      print "$_ \t -> $H->{$_} \n" foreach (keys %$H);
      print "\n";
   }
   return($wavheader, $H);
}
########################################################################
#
# Analyze the wav for chunks and gaps. Fill an array @times with two
# blank separated numbers in each entry. These two numbers are the
# time in seconds of the starting point of sound and the duration of
# that chunk. This is important because this number will be used to seek
# to that point of sound from the beginning of the file, not form the
# end point of the previous cycle. For each chunk we start to seek from
# zero; this is not a large time loss, seeking is fast.
#
# There were weeks of testing to manage Audio::FindChunks-0.03, gave up!
# The behaviour seems inconsistent. For example: ripping all tracks of
# the CD: Lamb - What Sound gave *no* gaps. When ripping only the last
# track, gaps were immediately detected.
# First, changing the sec_per_chunk value gave better results, but
# suddenly no gaps at all were found. The threshold stayed at zero.
# So then I introduced a loop where the sec_per_chunk increases from
# 0.1 - 0.8 in steps of 0.1, and in the same time, the threshold from
# 0.1 in steps of 0.2 only if the resulting threshold is less than 100.
# You say that this is ugly? No, it is extremely ugly. And all this
# because there might be a caching problem in Audio::FindChunks-0.03?
# Then, testing on a 64bit machine was a drawback, no gaps at all.
#
# So I gave up this sophisticated and "fully documented" PM, and coded a
# few lines to solve the problem. This code might not be useful to
# split manually recorded vinyl, but the results for ripped CDs are
# much more precise than with the PM. Of course, I can test only on a
# limited range of CDs, and I have no classical or Death-Metal to test.
# But for the following CDs (and hundreds of CDs with no gaps -->
# thousands of tracks and not one was erroneously split) this
# snippet works. (See below for explanation!)
#
#
# Testreport (CDs with correctly split ghost songs):
#
# OK: 2raumwohnung: in wirklich: 11
# OK: A Camp: Colonia: 12
# OK: Archive: Londonium: 13
# OK: Archive: Take My Head: 10
# OK: Aromabar: 1!: 15 (2 ghost songs!)
# OK: Autour de Lucie: L'échappée belle: 11
# OK: Camille: Sac des filles: 11
# OK: Camille: Le fil: 15 (Ghost song without zero-gap... not splitted!)
# OK: Cibelle: Cibelle: 11
# OK: Dining Rooms: Experiments In Ambient Soul: 13
# OK: Distain!: [li:quíd]: 11
# OK: Falco: Out of the Dark: 9+1 renamed
# OK: Helena: Née dans la nature: 11
# OK: Imogen Heap: Ellipse (disc 2): Half Life (instr.):13+1 reanmed
# OK: Jay-Jay Johanson: Antenna: 10
# OK: Laika: Sound Of The Satellites: 12
# OK: Lamb: Debut: 10
# OK: Lamb: Fear Of Fours: 00 Hidden Track
# OK: Lamb: What Sound: 10
# OK: Little Boots: Hands: 12+1 renamed
# OK: Lunik: Preparing To Leave: 11
# OK: Lunik: Weather: 11
# OK: Mãozinha: Aerosferas: 11
# OK: Massive Attack: 100th Window: 09
# OK: Moby: Last Night : 14
# OK: Moloko: Do You Like My Tight Sweater?: 17+1
# OK: Olive: Trickle: 12
# OK: Rightous Men: Disconnected: 11
# OK: Samia Farah: Samia Farah: 12
# OK: Saint Etienne: Heart Failed [In The Back Of A Taxi] (CD1): 03
# OK: Stereo Total: Musique Automatique 15
# OK: Yoshinori Sunahara: Pan Am: 09
#
# Deleted blank tracks:
#
# OK: 22 Pistepirkko: Ralley of Love: 0 (hidden track - copy protection)
# OK: NPG: New Power Soul: all blank tracks
# OK: Dave Matthews Band: Under the Table and Dreaming: all blank tracks
#
#
sub get_chunks {
   my ($tcn, $trn) = @_;
   my @times = ();
   $trn = $trn . ".rip";
   my ($wavheader, $H) = get_wavhead("$trn", 0);

# How do I analyze the chunks? I calculate a threshold value called
# $thresh of the actual chunk by summing up its binary values - perl is
# so cool! Then this value is used to calculate a kind of mean value
# with the previous one --> $deltathre, but only at every 5th value, to
# cancel short fluctuations. If the actual $thresh value lies
# within a small range compared to the deltathre, a weight (counter)
# will be increased and deltathre will be raised to cancel (not so)
# short peak changes (not only) at the end of a track (gap).
# Silence is defined as (not so) significant drop of the $thresh value
# compared to the $deltathre one. Use an upper cut-off value $maxthresh
# (70% of maximum value) to allow deltathre to grow (quickly) in the
# beginning but prevent to bailing out immediately. During the track, a
# weight will help to prevent the same. If the silence lasts more than
# 4 seconds, the detected startsound and duration values will be pushed
# into the @times array. In version 3.7.0 additionally a $trimcn is
# introduced, to enable RipIT to trim tracks at beginning and end. This
# can now be done, if the --extend and --prepend options are set to 0,
# not recommended. If the lead-in/out and gaps are really zero, the
# $trimcn will correct the values pushed into @times which correspond to
# the time points where volume is below the thresh value, but not yet
# zero. With these values a --prepend or --extend of 0 would cut off a
# few fractions of seconds. This may still happen, if the lead-in/out
# and/or gap is not really zero. How should RipIT know about silence?
# If lead-in/out and gaps are zero, $trimcn will slightly enlarge the
# chunks of sound and trimming should not cut away sound, hopefully.
# As far as I understand, the unpack function returns the number of bits
# set -- in a bit vector.  Using this value, I stress that we deal with
# bits and not bytes for the variables $thresh and $maxthresh. Therefore
# $maxthresh is multiplied by 8!

   my $bindata;
   my $bytecn = 0;
   my $silencecn = 0;
   my $chunkcn = 0;
   my $chunksize = 0.1; # Chunk size in seconds.
   my $chunkbyte = $H->{byte_per_sec} * $chunksize;
   my $chunklen = 0;
   my $leadinflag = 0;
   my $startsnd = 0;
   my $soundflag = 0;
   my $deltathre = $H->{byte_per_sec} * $chunksize;
   my $totalthre = 0;
   my $trimcn = 0;
   my $weight = 1;
   my $maxthresh = $deltathre * 8 * 0.7;

   open(IN, "< $wavdir/$trn") or print "Can't open $trn: $!\n";
   binmode(IN);
   seek(IN, 44, 0);
   while(read(IN, $bindata, $chunkbyte)) {
      $chunkcn++;
      my $thresh = unpack('%32b*', $bindata);
      $totalthre += $thresh / 1000 if($thresh < $maxthresh * 1.1 );
      $weight++
         if($thresh > 0.8 * $deltathre && $thresh < 1.1 * $deltathre);
      $deltathre = ($deltathre * $weight + $thresh) / (1 + $weight)
         if($thresh > 0.8 * $deltathre && $thresh < $maxthresh &&
            $chunkcn =~ /[05]$/);
      # According to the $thresh value, decide whether it is sound or
      # not.
      # The if-condition itself is a little more tricky. We have to
      # force this condition at beginning, even if there is no silence!
      # Why this? If there is a lead-in with immediate sound but very
      # short interruptions, the switch of $soundflag = 1 will be the
      # reason that the startsnd will increase, although it shouldn't,
      # it should stay at 0.0, but will become 0.1 or similar in this
      # case! In this way, if the interuptions are short (< 4s) nothing
      # will happen, and the fact that $startsnd will not set back to
      # zero until a true gap will be found, $startsnd will not be
      # recalculated in the else-part.
      if($thresh < 0.8 * $deltathre || $bytecn == 0) {
         $silencecn += $chunkbyte;
         # If thesh is zero, use an other counter to calculate more
         # precise values.
         $trimcn += $chunkbyte if($thresh == 0);
         $leadinflag = 1 if($thresh == 0 && $bytecn == 0);
         # If the gap is 4 seconds long, save values in array @times, or
         # to detect lead-ins shorter than 4s, set the $soundflag to 1.
         if($silencecn == $H->{byte_per_sec} * 4 ||
            $bytecn < $H->{byte_per_sec} * 4) {
            $chunklen = ($bytecn - $silencecn) / $H->{byte_per_sec};
            # Otherwise:
            $chunklen = ($bytecn - $trimcn) / $H->{byte_per_sec}
               if($trimcn < $silencecn && $trimcn > 0);
            $chunklen -= $startsnd;
            # The chunk of sound must be longer than 4.0 seconds!
            if($chunklen < 4) {
               $chunklen = 0;
            }
            else {
               push(@times, "$startsnd $chunklen");
               # Prevent re-entering a duplicate last entry outside of
               # the loop.
               $startsnd = 0;
            }
            # Chunk of sound has been detected. Doing this here and not
            # just above where $starsnd is set to zero, will enable
            # detection of short lead-ins!
            $soundflag = 1;
            # From now on we are in silence!
            # Set $trimcn to $silencecn to detect another difference
            # at the end of the gap, if the gap consists of zeros.
            $trimcn = $silencecn if($bytecn > $H->{byte_per_sec} * 4);
         }
         # We will stay in this condition, until...
      }
      else {
         # ... sound is detected (again)!
         # If we get here the first time, save the $startsound time.
         if($soundflag == 1 && $startsnd == 0) {
            if($trimcn < $silencecn &&
               $trimcn > (0.8 * $silencecn)) {
               $startsnd = ($bytecn - $silencecn + $trimcn) /
                            $H->{byte_per_sec};
            }
            elsif($startsnd == 0) {
               $startsnd = $bytecn / $H->{byte_per_sec};
            }
            $soundflag = 0;
         }
         $trimcn = 0;
         $silencecn = 0;
      }
      $bytecn += $chunkbyte;
   }
   # Calculations for the last (only) chunk of sound.
   $chunklen = ($bytecn - $silencecn) / $H->{byte_per_sec};
   # Otherwise (slightly different condition than above):
   $chunklen = ($bytecn - $trimcn) / $H->{byte_per_sec}
      if($trimcn < $silencecn);
   $chunklen -= $startsnd;
   push(@times, "$startsnd $chunklen") unless($startsnd == 0);
   push(@times, "$startsnd $chunklen") unless(@times);
   $times[0] =~ s/^0.1/0/ if($startsnd == 0.1 && $leadinflag == 0);

   my $tracklen = int(($framelist[$tcn] - $framelist[$tcn - 1]) / 7.5);
   $tracklen = int($framelist[$tcn] / 7.5) if($tcn == 0);
   $tracklen /= 10;

   # I don't like it, but it might be OK to delete very short tracks
   # if their content is blank.
   if(-s "$wavdir/$trn" < 200000 && $totalthre >= 200) {
      $chunkcn = 0;
      $totalthre = 0;
      open(IN, "< $wavdir/$trn") or print "Can't open $trn: $!\n";
      binmode(IN);
      seek(IN, 44, 0);
      while(read(IN, $bindata, 2)) {
         $chunkcn++;
         my $thresh = unpack('%32b*', $bindata);
         $thresh = 0 if($thresh >= 14);
         $totalthre += $thresh;
      }
      $totalthre = $totalthre * 4 / $chunkcn;
   }

   if($totalthre < 200) {
      unlink("$wavdir/$trn") or print "Can't delete $trn: $!\n";
      if($verbose >= 1) {
         print "\n\nRipIT found blank track $trn\n",
               "and decided to delete it.\n\n";
      }
      open(ERO,">>$wavdir/error.log")
         or print "Can not append to file $wavdir/error.log\"!\n";
      print ERO "Blankflag = $tcn\nTrack $tcn on CD failed!\n";
      close(ERO);
      log_info("blank track deleted: $wavdir/$trn");
      $times[0] = "blank";
      return(@times);
   }

   if($verbose >= 2) {
      printf "\n%02d:%02d:%02d: ",
         sub {$_[2], $_[1], $_[0]}->(localtime);
      print "RipIT found following chunks for track\n",
            "$trn (${tracklen}s long):\nstart duration (in seconds)\n";
      log_info("\nRipIT found following chunks for track:");
      log_info("$trn (${tracklen}s long):\nstart duration (in seconds)");
      foreach(@times) {
         my @interval = split(/ /, $_);
         printf("%5.1f %9.1f\n", $interval[0], $interval[1]);
         log_info("@interval");
      }
   }
   return(@times);
}
########################################################################
#
# Split the wav into chunks of sound and rename all of them to
# "Ghost Song $counter.wav".
#
sub split_chunks {
   my ($tcn, $trn, $cdtocn, @times) = @_;
   my $album = clean_all($album_utf8);
   $album = clean_name($album);
   $album = clean_chars($album) if($chars);
   $album =~ s/ /_/g if($underscore == 1);
   my $bindata;
   my ($wavheader, $H) = get_wavhead("$trn.rip", 1);
   my $chunksize = 0.1; # Chunk size in seconds.
   my $chunkbyte = $H->{byte_per_sec} * $chunksize;
   my $chunkcn = 0;
   # Save the tracklength of the original track to be compared with the
   # chunks of sound.
   my $tracklen = int($H->{data_size} / $H->{byte_per_sec} * 10);
   $tracklen /= 10;
   # Let the other processes know, if the track has been shorten or not.
   my $shorten = 0;

   my $times_cn = 0;
   foreach(@times) {
      # Remember: each entry of @times has the form: "start duration"
      # where start is the beginning of sound in seconds, and duration
      # the time in seconds.
      my @interval = split(/ /, $_);
      if($interval[0] >= $prepend) {
         $interval[0] -= $prepend;
         $interval[1] += $prepend;
      }
      else{
         $interval[1] += $interval[0];
         $interval[0] = 0;
      }
      # Extend the interval, this might result in a too long interval.
      $interval[1] += $extend;
      # Don't allow too long end-times, this can happen with the above
      # extend command.
      if($interval[0] + $interval[1] > $tracklen) {
         $interval[1] = $tracklen - $interval[0];
      }
      # Don't split if interval is larger than tracklength from cdtoc.
      # Use a threshold of $extend + $prepend. Reasonable?
      if(($tracklen - $extend - $prepend) <= $interval[1] ||
          $interval[1] < 3) {
         print "Track $tcn not splitted.\n\n" if($verbose >= 1);
         log_info("Track $tcn not splitted.");

         # Merge track into album-track if $cdcue == 1.
         merge_wav($trn, $chunkbyte, $album) if($cdcue == 1);
         return($shorten, @times);
      }

      # Update the times array.
      $times[$times_cn] = "$interval[0] $interval[1]";
      $times_cn++;
      # Modify the @times array.
      $_ = "$interval[0] $interval[1]";

      # Use array @secondlist to save new track lengths to allow the
      # ripper (!) process to write correct playlist files. The array
      # will be printed to ghost.log for encoder process in the "next"
      # subroutine called rename_chunks, see below.
      if($chunkcn == 0) {
         $secondlist[$tcn - 1] = int($interval[1]) if($hiddenflag == 0);
         $secondlist[$tcn] = int($interval[1]) if($hiddenflag == 1);
      }
      else {
         push(@secondlist, int($interval[1]));
      }

      # Print info message about what is going on (only once):
      if($verbose >= 2 && $chunkcn == 0) {
         print "Splitting \"$trn\" into " . ($#times + 1) . " chunk";
         print ".\n" if($#times == 0);
         print "s.\n" unless($#times == 0);
      }
      if($chunkcn == 0) {
         log_info("Splitting \"$trn\" into " . ($#times + 1) . " chunk.")
            if($#times == 0);
         log_info("Splitting \"$trn\" into " . ($#times + 1) . " chunks.")
            unless($#times == 0);
      }
      if($verbose >= 4) {
         print "\n\nUsing these values for chunk $chunkcn:\n";
         printf("%5.1f %5.1f\n", $interval[0], $interval[1]);
      }
      log_info("\nUsing these values for chunk $chunkcn:");
      log_info("@interval");

      # Prepare the filename for output.
      my $outr = "Ghost Song $chunkcn";
      $outr = get_trackname($tcn, $outr) . ".rip";

      if($cdcue == 1) {
         my $file_size = -s "$wavdir/$album.wav";
         print "\nAppending $outr to album.wav, yet $file_size B large",
               ".\n" if($verbose > 4);
         log_info("\nAppending $outr to album.wav, yet $file_size B large.\n");
         open(OUT, ">> $wavdir/$album.wav")
            or print "Can not append to file ",
                     "\"$wavdir/$album.wav\"!\n";
      }
      else {
         open(OUT, "> $wavdir/$outr");
      }
      binmode(OUT);

      # From now on count in bytes instead of seconds.
      $interval[0] = $interval[0] * $H->{byte_per_sec} + 44;
      $interval[1] = $interval[1] * $H->{byte_per_sec};

      # Edit header according to size of the chunk.
      $H->{data_size} = $interval[1];
      $H->{file_size_8} = $H->{data_size} + 36;
      substr($wavheader, 4, 4) = pack("V", $H->{file_size_8});
      substr($wavheader, 40, 4) = pack("V", $H->{data_size});

      # This is nuts, don't know why this happens, but the chunk sizes
      # in the RIFF header are sometimes one byte smaller leading to an
      # unpaired number. This causes flac to fail on splitted tracks!
      # So let's do it the ugly way: add 1 byte and rewrite the header.
      # What goes wrong in the above substr command or elsewhere?
      # If someone finds out, please let me know!
      my $loopcn = 0;
      # Initialization:
      ($H->{RIFF_header},  $H->{file_size_8},     $H->{WAV_header},
       $H->{FMT_header},   $H->{WAV_chunk_size},  $H->{WAV_type},
       $H->{channels},     $H->{sample_rate},     $H->{byte_per_sec},
       $H->{block_align},  $H->{bit_per_sample},  $H->{data_header},
       $H->{data_size}
      ) = unpack("a4Va4a4VvvVVvva4V", $wavheader);

      while($loopcn < 10 and $H->{data_size} ne $interval[1]) {
         if($verbose >= 5) {
            print "\nFatal error, unpair chunk sizes detected\n",
                  "in new header of ghost track part $chunkcn:\n",
                  "\$H->{data_size} is $H->{data_size} ",
                  "instead of chunk length = $interval[1]!\n",
                  "The new wav header has following entries:\n";
            print "$_ \t -> $H->{$_} \n" foreach(keys %$H);
            print "\n";
         }
         log_info("\nFatal error, unpair chunk sizes detected\n",
               "in new header of ghost track part $chunkcn:\n",
               "\$H->{data_size} is $H->{data_size} ",
               "instead of chunk length = $interval[1]!\n",
               "The new wav header has following entries:");
         log_info("$_ \t -> $H->{$_}") foreach(keys %$H);
         log_info("\n");

         $H->{data_size} = 2 * $interval[1] - $H->{data_size};
#         $H->{data_size} = $interval[1] + 1;
         $H->{file_size_8} = $H->{data_size} + 36;

         substr($wavheader, 4, 4) = pack("V", $H->{file_size_8});
         substr($wavheader, 40, 4) = pack("V", $H->{data_size});

         ($H->{RIFF_header}, $H->{file_size_8},    $H->{WAV_header},
          $H->{FMT_header},  $H->{WAV_chunk_size}, $H->{WAV_type},
          $H->{channels},    $H->{sample_rate},    $H->{byte_per_sec},
          $H->{block_align}, $H->{bit_per_sample}, $H->{data_header},
          $H->{data_size}
         ) = unpack("a4Va4a4VvvVVvva4V", $wavheader);

         $loopcn++;
      }

      if($loopcn >= 9 && $verbose >= 3) {
         print "\nMajor problem writing the wav header.";
         log_info("\nMajor problem writing the wav header.");
         if($wcoder =~ /2/) {
            print "\nWon't split this track because Flac will fail.";
            log_info("\nWon't split this track because Flac will fail.");
            # Reset the @times array.
            @interval = (0, $tracklen);
            @times = ("0 $tracklen");
            if($chunkcn == 0) {
               $secondlist[$tcn - 1] = $interval[1] if($hiddenflag == 0);
               $secondlist[$tcn] = $interval[1] if($hiddenflag == 1);
            }
            else {
               pop(@secondlist);
            }
         }
         else {
            print "\nTrying to continue anyway.\n";
            log_info("\nTrying to continue anyway.\n");
         }
      }
      return($shorten, @times) if($loopcn >= 9 && $wcoder =~ /2/);

      syswrite(OUT, $wavheader, 44) if($cdcue == 0);
      log_info("The length of data is $interval[1].");
      log_info("The final wav header has following entries:");
      log_info("$_ \t -> $H->{$_}") foreach(keys %$H);
      log_info("\n");
      if($verbose >= 5) {
         print "The length of data is $interval[1].\nThe final wav",
               "header of chunk $chunkcn has following entries:\n";
         print "$_ \t -> $H->{$_} \n" foreach (keys %$H);
         print "\n";
      }

      # Seek from beginning of file to start of sound of chunk.
      open(IN, "< $wavdir/$trn.rip") or
      print "Can't open $trn.rip: $!\n";
      binmode(IN);
      print "Seeking to: ${interval[0]}B, starting from 0B.\n"
         if($verbose >= 4);
      log_info("Seeking to: ${interval[0]}B, starting from 0B.");
      seek(IN, $interval[0], 0) or
         print "\nCould not seek in file IN: $!\n";

      # I don't know if it is good to read so many bytes a time, but it
      # is faster than reading byte by byte.
      my $start_snd = $interval[0];
      $interval[1] = $interval[1] + $interval[0];
      while(read(IN, $bindata, $chunkbyte) &&
            $interval[0] < $interval[1] - 1) {
         $interval[0] += $chunkbyte;
         # Before we write the data, check it, because it happens that
         # seek does not seek to a pair number, starting to read an
         # unpair (right-channel) byte. In this case, the wav will sound
         # like pure noise, and adding or deleting a single byte right
         # after the header will heal the wav.
         # The amount of data in the read $bindata seems OK, only the
         # position is wrong.
         my $pos = tell(IN);
         if($pos !~ /[02468]$/) {
            print "After chunkbyte = <$chunkbyte> reached pos <$pos>.\n"
               if($verbose >= 5);
            log_info("After chunkbyte = <$chunkbyte> reached pos <$pos>.\n");
            # Move one byte!
            read(IN, my $dummybyte, 1);
            $pos = tell(IN);
            print "After 1 byte read reached pos <$pos>.\n"
               if($verbose >= 5);
         }
         print OUT $bindata;
      }
      print "This chunk should be ", $interval[0] - $start_snd,
            "B large.\n" if($verbose >= 5);
      log_info("This chunk should be ", $interval[0] - $start_snd,
               "B large.");
      log_info("Remember, steps in the size of $chunkbyte B are used.");
      close(OUT);
      write_wavhead("$wavdir/$album.wav") if($cdcue == 1);
      $chunkcn++;
   }
   close(IN);
   open(ERO,">>$wavdir/error.log")
      or print "Can not append to file ",
               "\"$wavdir/error.log\"!\n";
   if($#times == 0) {
      print "Track $tcn successfully trimmed.\n\n" if($verbose >= 1);
      log_info("Track $tcn successfully trimmed.\n\n");
      print ERO "Splitflag = $tcn\n";
      $shorten = 1;
   }
   else {
      print "Track $tcn successfully splitted.\n\n" if($verbose >= 1);
      log_info("Track $tcn successfully splitted.\n\n");
      print ERO "Ghostflag = $tcn\n";
      $shorten = 1;
   }
   close(ERO);
   return($shorten, @times);
}
########################################################################
#
# Rename the chunks called "XY Ghost Song $chunkcn" to the appropriate
# file name according to the track-template.
#
sub rename_chunks {

   # The ripper uses a copy of the initial @seltrack array, called
   # @tracksel. Ghost songs will be added in @seltrack, but not in array
   # @tracksel. This means: the ripper will behave as usual, and not
   # care about additional songs. Note that splitted songs are of course
   # already ripped, so we do not need to notify the ripper about ghost
   # songs.

   # If there is only one chunk, this chunk gets the true trackname.
   # If there are more than one chunk, the first chunk gets the true
   # track name; this might be wrong, but who knows? If there are only
   # two chunks, the second will get the suffix Ghost Song without a
   # counter. If the track name holds a slash, the track name will be
   # splitted, the first part will be used for the actual track, the
   # second part for the ghost song. If there are more than two chunks,
   # a counter will be added.

   # Another problem is with data tracks. Then the track-counter will
   # not increase for ghost songs, as we expect for ghost songs that
   # appear in the last track, sad. (See below!)

   my ($tcn, $trn, $cdtocn, $cue_point, $shorten, $artistag, $trt, @times) = @_;
   my $chunkcn = 0;
   my $ghostflag = 0;
   my $outr = "Ghost Song $chunkcn";
   $outr = get_trackname($tcn, $outr) . ".rip";
   # The first track must be renamed to the *.rip file because the
   # ripper will rename it to wav!
   rename("$wavdir/$outr", "$wavdir/$trn.rip");

   # Writing the toc file in case $cdtoc == 1.
   if($cdtoc == 1) {
         my $cdtocartis = $artistag;
         oct_char($cdtocartis);
         my $cdtoctitle = $trt;
         $cdtoctitle = clean_name($cdtoctitle);
         oct_char($cdtoctitle);
         open(CDTOC, ">>$wavdir/cd.toc")
            or print "Can not append to file \"$wavdir/cd.toc\"!\n";
         print CDTOC "\n//Track $cdtocn:\nTRACK AUDIO\n";
         print CDTOC "TWO_CHANNEL_AUDIO\nCD_TEXT {LANGUAGE 0 {\n\t\t";
         print CDTOC "TITLE \"$cdtoctitle\"\n\t\t";
         print CDTOC "PERFORMER \"$cdtocartis\"\n\t}\n}\n";
         print CDTOC "FILE \"$trn.wav\" 0\n";
         close(CDTOC);
   }
   # Writing the cue file in case $cdcue == 1.
   if($cdcue > 0) {
      my $points = chapter_length($cue_point);
      $points =~ /\.\d+$/;
      my $cuetrackno = sprintf("%02d", $cdtocn);
      open(CDCUE ,">>$wavdir/cd.cue")
         or print "Can not append to file \"$wavdir/cd.cue\"!\n";
      print CDCUE "TRACK $cuetrackno AUDIO\n",
                  "   TITLE \"$trn\"\n",
                  "   PERFORMER \"$artistag\"\n",
                  "   INDEX 01 $points\n";
      close(CDCUE);
   }
   # Calculate length of track for next cue point.
   my $interval = shift(@times);
   my ($start, $chunk_length) = split(/ /, $interval);
   $chunk_length *= 75;
   if($shorten == 0) {
      $chunk_length = $framelist[$tcn] - $framelist[$tcn - 1] if($hiddenflag == 0);
      $chunk_length = $framelist[$tcn] - $framelist[$tcn] if($hiddenflag == 1);
   }
   $cue_point += $chunk_length;

   # If only one chunk has been trimmed, we are done, array @times is
   # empty now.
   # If there are two or more chunks, proceed and hack all necessary
   # arrays needed for the encoder. They will be written in the
   # ghost.log file. Note that with only one ghost song no counter is
   # needed in the filename. The suffix can now be wav instead of rip.
   # TODO: check if final trackname already exists.
   # So, the trackname of a new ghost song shall have the same leading
   # tracknumber to identify its origin, except if it comes from
   # the last track, then the leading number may increase! Define a new
   # ghost counter $gcn.
   my $gcn = $tcn;
   $ghostflag = 1 if($tcn == $#framelist);
   $ghostflag = 1 if($hiddenflag == 1 && $tcn == $#framelist - 1);
   $gcn++ if($ghostflag == 1);
   $chunkcn++;
   foreach (@times) {
      my $trt = $tracktags[$tcn - 1];
      $trt = $tracktags[$tcn] if($hiddenflag == 1);
      # Some tracks with ghost songs contain the ghost song name after
      # a slash.
      my @ghostnames = split(/\//, $trt);
      if($ghostnames[$chunkcn]) {
         $trt = $ghostnames[$chunkcn];
         $trt =~ s/^\s+|\s+$//;
         # We need to update the track-arrays as the first track will
         # get only its name without the ghost songs name.
         if($chunkcn == 1) {
            my $prev_trt = $ghostnames[0];
            $prev_trt =~ s/^\s+|\s+$//;
            $tracktags[$#tracktags] = $prev_trt;
            my $prev_trn = $prev_trt;
            $prev_trn = clean_all($prev_trn);
            $prev_trn = clean_name($prev_trn);
            $prev_trn = clean_chars($prev_trn) if($chars);
            $prev_trn = change_case($prev_trn);
            $prev_trn =~ s/ /_/g if($underscore == 1);
            $tracklist[$#tracklist] = $prev_trn;
            # The cdtoc needs to be hacked too.
            if($cdtoc == 1) {
               open(CDTOC, "<$wavdir/cd.toc")
                  or print "Can not read file cd.toc!\n";
               my @toclines = <CDTOC>;
               close(CDTOC);
               open(CDTOC, ">$wavdir/cd.toc")
                  or print "Can not append to file \"$wavdir/cd.toc\"!\n";
               foreach (@toclines) {
                  last if(/\/\/Track\s$cdtocn:/);
                  print CDTOC $_;
               }
               my $cdtocartis = $artistag;
               oct_char($cdtocartis);
               my $cdtoctitle = $prev_trt;
               $cdtoctitle = clean_name($cdtoctitle);
               oct_char($cdtoctitle);
               $prev_trn = get_trackname($tcn, $prev_trn);
               print CDTOC "\n//Track $cdtocn:\nTRACK AUDIO\n";
               print CDTOC "TWO_CHANNEL_AUDIO\nCD_TEXT {LANGUAGE 0 {\n\t\t";
               print CDTOC "TITLE \"$cdtoctitle\"\n\t\t";
               print CDTOC "PERFORMER \"$cdtocartis\"\n\t}\n}\n";
               print CDTOC "FILE \"$prev_trn.wav\" 0\n";
               close(CDTOC);
            }
            # The cdcue needs to be hacked too, because the track length
            # is different if the track has been splitted.
            if($cdcue > 0) {
               open(CDCUE, "<$wavdir/cd.cue")
                  or print "Can not read file cd.cue!\n";
               my @cuelines = <CDCUE>;
               close(CDCUE);
               open(CDCUE, ">$wavdir/cd.cue")
                  or print "Can not append to file \"$wavdir/cd.cue\"!\n";
               my $cuetrackno = sprintf("%02d", $cdtocn);
               my $track_flag = 0;
               foreach (@cuelines) {
                  if($track_flag == 1) {
                     print "   TITLE \"$prev_trt\"\n";
                     print CDCUE "   TITLE \"$prev_trt\"\n";
                     $track_flag = 0;
                  }
                  else {
                     print $_;
                     print CDCUE $_;
                  }
                  $track_flag = 1 if(/^TRACK\s$cuetrackno\sAUDIO/);
               }
               close(CDCUE);
            }
         }
      }
      else {
         # The name for the tags will be with originating track name as
         # prefix.
         $trt = $trt . " - Ghost Song" if($#times == 0);
         $trt = $trt . " - Ghost Song $chunkcn" if($#times > 0);
      }
      # The actual track name will be slightly different.
      $trn = $trt;
      $trn = clean_all($trn);
      $trn = clean_name($trn);
      $trn = clean_chars($trn) if($chars);
      $trn = change_case($trn);
      $trn =~ s/ /_/g if($underscore == 1);
      push(@seltrack, $gcn);
      push(@tracklist, $trn);
      push(@tracktags, "$trt");
      # Remember: $outr is the output track name of the splitted wav.
      $outr = "Ghost Song $chunkcn";
      $outr = get_trackname($tcn, $outr) . ".rip";
      $trn = get_trackname($gcn, $trn);
      rename("$wavdir/$outr", "$wavdir/$trn.wav");
      md5_sum("$wavdir", "$trn.wav", 0) if($md5sum == 1 && $wav == 1);
      if($cdtoc == 1 || $cdcue > 0) {
         $cdtocn++;
      }
      if($cdtoc == 1) {
         my $cdtocartis = $artistag;
         oct_char($cdtocartis);
         my $cdtoctitle = $trt;
         $cdtoctitle = clean_name($cdtoctitle);
         oct_char($cdtoctitle);
         open(CDTOC, ">>$wavdir/cd.toc")
            or print "Can not append to file \"$wavdir/cd.toc\"!\n";
         print CDTOC "\n//Track $cdtocn:\nTRACK AUDIO\n";
         print CDTOC "TWO_CHANNEL_AUDIO\nCD_TEXT {LANGUAGE 0 {\n\t\t";
         print CDTOC "TITLE \"$cdtoctitle\"\n\t\t";
         print CDTOC "PERFORMER \"$cdtocartis\"\n\t}\n}\n";
         print CDTOC "FILE \"$trn.wav\" 0\n";
         close(CDTOC);
      }
      if($cdcue > 0) {
         my $points = chapter_length($cue_point);
         $points =~ /\.\d+$/;
         my $cuetrackno = sprintf("%02d", $cdtocn);
         open(CDCUE ,">>$wavdir/cd.cue")
            or print "Can not append to file \"$wavdir/cd.cue\"!\n";
         print CDCUE "TRACK $cuetrackno AUDIO\n",
                     "   TITLE \"$trt\"\n",
                     "   PERFORMER \"$artistag\"\n",
                     "   INDEX 01 $points\n";
         close(CDCUE);
      }
      # Calculate length of track for next cue point.
      my ($start, $chunk_length) = split(/ /, $_);
      $cue_point += $chunk_length * 75;
      print "\nNext cue_point based on interval: $cue_point.\n",
            "This is: ", chapter_length($cue_point), ".\n"
         if($verbose > 5);
      $gcn++ if($ghostflag == 1);
      $chunkcn++;
   }
   print "\n\n" if($verbose >= 2);
   log_info("\n");

   # Is there another way to communicate with the encoder process (child
   # precess) than writing log files?
   open(GHOST, ">$wavdir/ghost.log")
      or print "Can not append to file ghost.log!\n";
   print GHOST "Array seltrack: @seltrack\n";
   print GHOST "Array secondlist: @secondlist\n";
   print GHOST "Array tracklist: $_\n" foreach(@tracklist);
   print GHOST "Array tracktags: $_\n" foreach(@tracktags);
   close(GHOST);
   return($cdtocn, $cue_point);
}
########################################################################
#
# Check if the necessary modules are available.
#
sub init_mod {
   print "\n" if($verbose >= 1);

   # We need to know if coverart is added to mp3 or ogg because those
   # encoders can't handle picture tags. The pictures are added after
   # encoding process using an additional module.
   # Create a coverart array supposing its exactly in the same order as
   # encoder.
   my $mp3art = 0;
   my $oggart = 0;
   my $wvpart = 0;
   if($coverart && ($lameflag == 1 || $oggflag == 1 || $wvpflag == 1)) {
      my @coverart = split(/,/, $coverart);
      for(my $c = 0; $c <= $#coder; $c++) {
         $mp3art = 1 if($coder[$c] == 0 && $coverart[$c] > 0);
         $oggart = 1 if($coder[$c] == 1 && $coverart[$c] > 0);
         $wvpart = 1 if($coder[$c] == 6 && $coverart[$c] > 0);
      }
   }

   eval { require CDDB_get };
   if($@) {
      print "\nPerl module CDDB_get not found. Needed for",
            "\nchecking the CD-ID and retrieving the CDDB",
            "\nentry from freeDB.org!",
            "\nPlease install CDDB_get from your closest",
            "\nCPAN mirror before trying again.",
            "\nInstall by hand or e.g. type as root:",
            "\nperl -MCPAN -e 'install CDDB_get'\n\n";
      exit 0;
   }
   $@ = ();
   eval { require LWP::Simple };
   if($@) {
      print "\nPerl module LWP::Simple not found. Needed for",
            "\nchecking free categories before submitting CDDB",
            "\nentries to freeDB.org!",
            "\nPlease install LWP::Simple and dependencies ",
            "\nfrom your closest CPAN mirror or submission will fail.",
            "\nInstall by hand or e.g. type as root:",
            "\nperl -MCPAN -e 'install LWP::Simple'\n\n";
      sleep 2;
   }
   $@ = ();
   eval { require Digest::MD5 } if($md5sum == 1);
   if($@) {
      print "\nPlease install Digest::MD5 and dependencies",
            "\nfrom your closest CPAN mirror before trying again with",
            "\noption --md5sum. Install by hand or e.g. type as root:",
            "\nperl -MCPAN -e 'install Digest::MD5'\n\n";
      exit 0;
   }
   $@ = ();
   eval { require Unicode::UCD } if($utftag == 0);
   if($@) {
      print "\nPlease install Unicode::UCD and dependencies",
            "\nfrom your closest CPAN mirror before trying again with",
            "\noption --noutftag. Install by hand or e.g. type as root:",
            "\nperl -MCPAN -e 'install Unicode::UCD'\n\n";
      exit 0;
   }
   $@ = ();
   eval { require MP3::Tag } if($mp3art == 1 && $lameflag == 1);
   if($@) {
      print "\nPlease install MP3::Tag and dependencies",
            "\nfrom your closest CPAN mirror before trying again with",
            "\noption --coverart. Install by hand or e.g. type as root:",
            "\nperl -MCPAN -e 'install MP3::Tag'\n\n";
      exit 0;
   }
   $@ = ();
   eval { require MIME::Base64 } if($oggart == 1 && $oggflag == 1);
   if($@) {
      print "\nPlease install MIME::Base64 and dependencies",
            "\nfrom your closest CPAN mirror before trying again with",
            "\noption --coverart. Install by hand or e.g. type as root:",
            "\nperl -MCPAN -e 'install MIME::Base64'\n\n";
      exit 0;
   }

   eval { require WebService::MusicBrainz::Release } if($mb == 1);
   if($@) {
      print "\nPlease install WebService::MusicBrainz and dependencies",
            "\nfrom your closest CPAN mirror before trying again with",
            "\noption --mb. Install by hand because using (as root:",
            "\nperl -MCPAN -e 'install WebService::MusicBrainz'",
            "\nmigth fail.\n\n";
      exit 0;
   }

   eval { require MusicBrainz::DiscID } if($mb == 1);
   if($@) {
      print "\nPlease install MusicBrainz::DiscID and dependencies",
            "\nfrom your closest CPAN mirror; e.g. type as root:",
            "\nperl -MCPAN -e 'install MusicBrainz::DiscID'\n\n";
#      exit 0;
   }

   if($wvpart == 1) {
      open(WAVPAK, "wavpack 2>&1|");
      my @response = <WAVPAK>;
      close(WAVPAK);
      chomp(my $wvpver = join('', grep(s/.*Linux\sVersion\s//, @response)));
      $wvpver =~ s/(\d+\.\d).*/$1/;
      if($wvpver <= 4.5) {
         print "\n\nWarning:\nThere is a newer version of wavpack ",
               "with coverart support.\nThis version of wavpack does ",
               "not write binary-tags.\n\n" if($verbose > 0);
      }
   }

   if($multi == 1) {
      eval "use Color::Output";
      if($@) {print "\nColor::Output not installed!\n"};
      eval "Color::Output::Init";
   }

   print "\n\n" if($verbose >= 1);
}
########################################################################
#
# Check if lame is installed.
#
sub check_enc {
   my ($enc, $suf) = @_;
   unless(log_system("$enc --version > /dev/null 2>&1")) {
      $enc = "\u$enc";
      if(!@pcoder && "@coder" =~ /0/ || "@pcoder" =~ /0/) {
         print "\n$enc not found (needed to encode $suf)!",
               "\nUse oggenc instead (to generate ogg)?\n";
         my $ans = "x";
         while($ans !~ /^[yn]$/i) {
            print "Do you want to try oggenc? [y/n] (y) ";
            $ans = <STDIN>;
            chomp $ans;
            $ans = "y" if($ans eq "");
         }
         if($ans eq "y") {
            my $coders = "@coder";
            my $pcoders = "@pcoder";
            if($coders !~ /1/) {
               $coders =~ s/0/1/g if($enc =~ /Lame/);
               $coders =~ s/3/1/g if($enc =~ /Faac/);
            }
            else {
               $coders =~ s/0//g if($enc =~ /Lame/);
               $coders =~ s/3//g if($enc =~ /Faac/);
            }
            if($pcoders !~ /1/) {
               $pcoders =~ s/0/1/g if($enc =~ /Lame/);
               $pcoders =~ s/3/1/g if($enc =~ /Faac/);;
            }
            else {
               $pcoders =~ s/0//g if($enc =~ /Lame/);
               $pcoders =~ s/3//g if($enc =~ /Faac/);
            }
            $lameflag = -1;
            @coder = split(/ /, $coders);
            @pcoder = split(/ /, $pcoders);
         }
         else {
            print "\n",
                  "Install $enc or choose another encoder with option",
                  "\n",
                  "-c 1 for oggenc, -c 2 for flac, -c 3 for faac,",
                  "\n",
                  "-c 4 for mp4als, -c 5 for Musepack,",
                  "\n",
                  "-c 6 for Wavpack or -c 7 for ffmpeg.",
                  "\n\n",
                  "Type ripit --help or check the manpage for info.",
                  "\n\n";
            exit;
         }
      }
      else {
         $lameflag = -1;
      }
   }
}
########################################################################
#
# Create MD5sum file of sound files.
#
sub md5_sum {
   my $sepdir = shift;
   my $filename = shift;
   my $ripcomplete = shift;
   my $suffix = $filename;
   $suffix =~ s/^.*\.//;
   chomp($filename);
   chomp($suffix);

   # What name should the md5 file get?
   my @paths = split(/\//, $sepdir);
   my $md5file =  $paths[$#paths] . " - " . $suffix . ".md5";
   $md5file =~ s/ /_/g if($underscore == 1);

   return unless(-r "$sepdir/$filename");

   open(SND, "< $sepdir/$filename") or
      print "Can not open $sepdir/$filename: $!\n";
   binmode(SND);
   if($verbose >= 4) {
      if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
         open(ENCLOG, ">>$wavdir/enc.log");
         print ENCLOG "\n\nCalculating MD5-sum for $filename...";
         close(ENCLOG);
      }
      else {
         print "\nCalculating MD5-sum for $filename...";
      }
   }
   my $md5 = Digest::MD5->new->addfile(*SND)->hexdigest;
   close(SND);
   if($verbose >= 4) {
      if(-r "$wavdir/enc.log" && $ripcomplete == 0) {
         open(ENCLOG, ">>$wavdir/enc.log");
         print ENCLOG "\nThe MD5-sum for $filename is: $md5.\n\n";
         close(ENCLOG);
      }
      else {
         print "\nThe MD5-sum for $filename is: $md5.\n";
      }
   }
   open(MD5SUM,">>$sepdir/$md5file")
      or print "Can not append to file \"$sepdir/$md5file\"!\n";
   print MD5SUM "$md5 *$filename\n";
   close(MD5SUM);
}
########################################################################
#
# Sort the options and fill the globopt array according to the encoder.
# Remember, the list of options for one encoder stated several times is
# separated by commas. The *opt arrays below will have only one
# entry, if the corresponding encoder has been stated only once. If one
# needs to find globopt in the code, search for "$globopt[" and not for
# @globopt.
#
sub check_options {
   my @flacopt = split(/,/, $flacopt);
   my @lameopt = split(/,/, $lameopt);
   my @oggencopt = split(/,/, $oggencopt);
   my @faacopt = split(/,/, $faacopt);
   my @mp4alsopt = split(/,/, $mp4alsopt);
   my @museopt = split(/,/, $museopt);
   my @wavpacopt = split(/,/, $wavpacopt);
   my @ffmpegopt = split(/,/, $ffmpegopt);
   $faacopt[0] = " " unless($faacopt[0]);
   $flacopt[0] = " " unless($flacopt[0]);
   $lameopt[0] = " " unless($lameopt[0]);
   $mp4alsopt[0] = " " unless($mp4alsopt[0]);
   $museopt[0] = " " unless($museopt[0]);
   $oggencopt[0] = " " unless($oggencopt[0]);
   $wavpacopt[0] = " " unless($wavpacopt[0]);
   $ffmpegopt[0] = " " unless($ffmpegopt[0]);
   for(my $c=0; $c<=$#coder; $c++) {
      if($coder[$c] == 0) {
         if($preset) {
            $lameopt[0] .= " --preset $preset";
         }
         else {
            $lameopt[0] .= " --vbr-$vbrmode" if($vbrmode);
            $lameopt[0] .= " -b $bitrate" if($bitrate ne "off");
            $lameopt[0] .= " -B $maxrate" if($maxrate != 0);
            $lameopt[0] .= " -V $quality[$c]"
               if($qualame ne "off" && $vbrmode);
            $lameopt[0] .= " -q $quality[$c]"
               if($quality[$c] ne "off" && !$vbrmode);
         }
         # Nice output of Lame-encoder messages.
         if($quality[$c] eq "off" && $lameopt[0] =~ /\s*-q\s\d\s*/) {
            $quality[$c] = $lameopt[0];
            $quality[$c] =~ s/^.*-q\s(\d).*$/$1/;
         }
         $lameopt[0] =~ s/^\s*//;
         push(@globopt, $lameopt[0]);
         shift(@lameopt);
      }
      elsif($coder[$c] == 1) {
         $oggencopt[0] .= " -q $quality[$c]" if($quality[$c] ne "off");
         $oggencopt[0] .= " -M $maxrate" if($maxrate != 0);
         $oggencopt[0] =~ s/^\s*//;
         push(@globopt, $oggencopt[0]);
         shift(@oggencopt);
      }
      elsif($coder[$c] == 2) {
         $flacopt[0] .= " -$quality[$c]" if($quality[$c] ne "off");
         $flacopt[0] =~ s/^\s*//;
         push(@globopt, $flacopt[0]);
         shift(@flacopt);
      }
      elsif($coder[$c] == 3) {
         $faacopt[0] .= " -q $quality[$c]" if($quality[$c] ne "off");
         $faacopt[0] =~ s/^\s*//;
         push(@globopt, $faacopt[0]);
         shift(@faacopt);
      }
      elsif($coder[$c] == 4) {
         $mp4alsopt[0] .= " -q $quality[$c]" if($quality[$c] ne "off");
         $mp4alsopt[0] =~ s/^\s*//;
         push(@globopt, $mp4alsopt[0]);
         shift(@mp4alsopt);
      }
      elsif($coder[$c] == 5) {
         $museopt[0] .= " --quality $quality[$c]" if($quality[$c] ne "off");
         $museopt[0] =~ s/^\s*//;
         push(@globopt, $museopt[0]);
         shift(@museopt);
      }
      elsif($coder[$c] == 6) {
         push(@globopt, $wavpacopt[0]);
         shift(@wavpacopt);
      }
      elsif($coder[$c] == 7) {
         push(@globopt, $ffmpegopt[0]);
         shift(@ffmpegopt);
      }
   }
}
########################################################################
#
# Check ripper (cdparanoia) and calculate a timeout according to track
# length.
#
sub check_ripper {
   my $P_command = shift;
   my $pid = shift;
   my @commands = split(/ /, $P_command);
   my $riptrackno = $commands[3];
   # Remember, $riptrackno might hold an span (interval) format.
   $riptrackno =~ s/\[.*$//;
   $riptrackno =~ s/-.*$//;
   # The $P_command is slightly different in case of hidden tracks.
   # Prevent warning when $riptrackno holds the device path instead of
   # the hidden track number.
   $riptrackno = 0 if($hiddenflag == 1 && $riptrackno !~ /^\d+$/);
   my $tlength = $secondlist[$riptrackno - 1];
   $tlength = $secondlist[$riptrackno] if($hiddenflag == 1);
   $tlength = int(exp(- $tlength / 2000) * ($tlength + 20));
   my $cn = 0;
   while(kill 0, $pid) {
      if($cn > $tlength) {
         unless(kill 9, $pid) {
            warn "\nProcess $pid already finished!\n";
         }
         return 0;
      }
      sleep 3;
      $cn += 3;
   }
   return 1;
}
########################################################################
#
# Check distribution.
#
sub check_distro {
   $distro = "debian" if(-f "/etc/debian_version");
}
########################################################################
#
# Get discid and number of tracks of inserted disc.
#
sub get_cddbid {
   CDDB_get->import( qw( get_cddb get_discids ) );
   my $cd = get_discids($scsi_cddev);
   my ($id, $tracks, $toc) = ($cd->[0], $cd->[1], $cd->[2]);
   $cddbid = sprintf("%08x", $id);
   my $totaltime = sprintf("%02d:%02d",$toc->[$tracks]->{min},$toc->[$tracks]->{sec});
   return($cddbid, $tracks, $totaltime);
}
########################################################################
#
# Analyze string build from CDDB data for latin and wide chars.
#
sub check_encoding {
   my $char_string = shift;
   my $utf_string = $char_string;
   my $latinflag = 0;
   my $wideflag = 0;
   my $utf_latinflag = 0;
   my $utf_wideflag = 0;

   if($cd{discid}) {
      # We do nothing for the moment.
      # $char_string = decode("iso-8859-15", $char_string);
   }
   else {
      $utf_string = Encode::decode('UTF-8', $utf_string, Encode::FB_QUIET);
   }

   my @char_points = ();
   my @utf_points = ();

   # Prevent warning:
   # Malformed UTF-8 character (unexpected non-continuation byte 0x74,
   # immediately after start byte 0xe1) in unpack.
   if(!utf8::is_utf8($char_string)) {
      @char_points = unpack("C0U*", "$char_string");
   }
   # @utf_points = unpack("C0U*", "$datb"); # Perl 5.8
   @utf_points = unpack("U0U*", "$utf_string"); # Perl 5.10


   foreach (@char_points) {
#      print "$_ " if($verbose >= 5);
      $latinflag++ if($_ > 128 && $_ < 256);
      $wideflag++ if($_ > 255);
   }

   foreach (@utf_points) {
#      print "$_ " if($verbose >= 5);
      $utf_latinflag++ if($_ > 128 && $_ < 256);
      $utf_wideflag++ if($_ > 255);
   }

   return($latinflag, $wideflag, $utf_latinflag, $utf_wideflag);
}
########################################################################
#
# Transform length of span in seconds. Argument has hh:mm:ss.ff format.
#
sub span_length {
   my $time = shift;
   my @time = split(/:/, $time);
   my $factor = 60;
   $time = pop(@time);
   # Cut off frames (sectors).
   my $frames = 0;
   ($time, $frames) = split(/\./, $time) if($time =~ /\./);
   # Round the value of frames.
   $time++ if($frames > 37);
   while ($time[0]) {
      $time += pop(@time) * $factor;
      $factor += 60;
   }
   return($time);
}
########################################################################
#
# Transform length of span from frames to hh:mm:ss.ff format.
# Thanks to perlmonks.
#
sub chapter_length {
   my $f = shift;

   my $s = int($f / 75);

   return sprintf("%s%02d", "00:00:", $s) if($s < 60);

   my $m = $s / 60;
   $s = $s % 60;
   return sprintf("%s%02d:%02d", "00:", $m, $s) if($m < 60);

   my $h = $m / 60;
   $m %= 60;
   return sprintf("%02d:%02d:%02d", $h, $m, $s) if($h < 24);

   my $d = $h / 24;
   $h %= 24;
   return sprintf("%d:%02d:%02d:%02d", $d, $h, $m, $s);
}
########################################################################
#
# Finish process.
#
sub finish_process {

   if($sshflag == 1) {
      del_wav();
   }
   else {
      wait;
   }

   if($playlist >= 1 && $encode == 1) {
      create_m3u();
   }

   my ($riptime, $enctime, $encend, $blanktrks, $ghostrks, $splitrks)
      = cal_times();
   del_erlog();

   if(-r "$wavdir/error.log" && $blanktrks eq "") {
      if($verbose >= 1) {
         print "\nCD may NOT be complete! Check the error.log \n",
               "in $wavdir!\n";
      }
      elsif($verbose >= 3) {
         print "\nRipping needed $riptime min and encoding needed ",
               "$enctime min.\n\n";
      }
   }
   else {
      if($verbose >= 1) {
         if($ghost == 1) {
            if($blanktrks) {
               print "\nCD may NOT be complete! Check the error.log \n",
                    "in $wavdir!\n";
               print "Blank track deleted: $blanktrks!\n"
                   if($blanktrks !~ /and/);
               print "Blank tracks deleted: $blanktrks!\n"
                   if($blanktrks =~ /and/);
            }
            else {
               printf "\n%02d:%02d:%02d: ",
                  sub {$_[2], $_[1], $_[0]}->(localtime);
               print "All complete!\n";
            }
            if($ghostrks) {
               print "Ghost song found in track $ghostrks!\n"
                   if($ghostrks !~ /and/);
               print "Ghost songs found in tracks $ghostrks!\n"
                   if($ghostrks =~ /and/);
            }
            else {
               print "No ghost songs found!\n";
            }
            if($splitrks) {
               print "Track $splitrks trimmed!\n"
                  if($splitrks !~ /and/);
               print "Tracks $splitrks trimmed!\n"
                  if($splitrks =~ /and/);
            }
            else {
              print "No tracks trimmed!\n" unless($splitrks);
            }
         }
         else {
            print "\nAll complete!\n";
         }
         print "Ripping needed $riptime min and ";
         print "encoding needed $enctime min.\n\n";
      }
   }

   log_info("\nRipping needed $riptime minutes.");
   log_info("Encoding needed $enctime minutes.");

   if($lcd == 1) {                 # lcdproc
      $lcdline1 = " ";
      $lcdline2 = "   RipIT finished   ";
      $lcdline3 = " ";
      ulcd();
      close($lcdproc) or print "close: $!";
   }

   if($multi == 1) {
      open(SRXY,">>$logfile")
         or print "Can not append to file \"$logfile\"!\n";
      print SRXY "\nEncoding   ended: $encend";
      print SRXY "\nRipping  needed: $riptime min.";
      print SRXY "\nEncoding needed: $enctime min.";
      print SRXY "\nGhost song(s) found in tracks $ghostrks!\n"
         if($ghostrks && $ghost == 1);
      print SRXY "\nTrack(s) $splitrks trimmed!\n"
         if($splitrks && $ghost == 1);
      print SRXY "\nTrack(s) $blanktrks deleted!\n"
         if($blanktrks && $ghost == 1);
      close(SRXY);
      my $cddevno = $cddev;
      $cddevno =~ s/\/dev\///;
      open(SUCC,">>$outputdir/done.log")
         or print "Can not append to file \"$outputdir/succes.log\"!\n";
      print SUCC "$cd{artist};$cd{title};$genre;$categ;$cddbid;";
      print SUCC "$cddevno;$hostnam;$riptime;$enctime\n";
      close(SUCC);
      $cddev =~ s/\/dev\//device /;
      $cddev = $cddev . " " unless($cddev =~ /\d\d$/);
      my $time = sprintf("%02d:%02d", sub {$_[2], $_[1]}->(localtime));
      cprint("\x037Encoding done  $time in $cddev with:\x030");
      cprint("\x037\n$cd{artist} - $cd{title}.\x030");
      cprint("\x033\nGhost song(s) found in tracks $ghostrks!\x030")
         if($ghostrks =~ /1/ && $ghost == 1);
      cprint("\x033\nTrack(s) $splitrks trimmed!\x030")
         if($splitrks =~ /1/ && $ghost == 1);
      cprint("\x033\nTrack(s) $blanktrks deleted!\x030")
         if($blanktrks =~ /1/ && $ghost == 1);
   }

   if($execmd) {
      $execmd =~ s/\$/\\\$/g;
      print "Will execute command \"$execmd\".\n" if($verbose >= 3);
      log_system("$execmd");
   }

   if($halt == 1 && $verbose >= 1) {
      print "\nShutdown...\n";
      log_system("shutdown -h now");
   }

   log_info("*" x 72, "\n");
   print "\n";
   print "Please insert a new CD!\n\n" if($loop == 2);
   return;
}
########################################################################
#
# Write inf files for each track.
#
sub write_inf {
   my $wavdir = shift;
   my $riptrackname = shift;
   my $artistag = shift;
   my $albumtag = shift;
   my $tracktag = shift;
   my $riptrackno = shift;
   my $nofghosts = shift;
   my $trackstart = shift;

   $nofghosts = $nofghosts - $riptrackno + 1;
   my $ripstart = sprintf("%02d:%02d:%02d",
                          sub {$_[2], $_[1], $_[0]}->(localtime));
   my $date = sprintf("%04d-%02d-%02d",
      sub {$_[5]+1900, $_[4]+1, $_[3]}->(localtime));

   while ($nofghosts > 0) {
      open(INF,">$wavdir/$riptrackname.inf");
      print INF "# Wave-info file created by ripit $version on ",
                "$date at $ripstart.\n# To burn the wav files use e.g.",
                " command:\n# wodim dev=/dev/scd0 -v -eject -pad -dao ",
                "-useinfo -text *.wav\n#\n";
      print INF "CDINDEX_DISCID=\t'$cd{discid}'\n" if($cd{discid});
      print INF "CDDB_DISCID=\t$cddbid\n#\n";
      if($va_flag == 1) {
         print INF "Albumperformer=\t'$artist_utf8'\n";
      }
      else {
         print INF "Albumperformer=\t'$artistag'\n";
      }
      print INF "Performer=\t'$artistag'\n";
      print INF "Albumtitle=\t'$albumtag'\n";
      print INF "Tracktitle=\t'$tracktag'\n";
      print INF "Tracknumber=\t$riptrackno\n";
      print INF "Trackstart=\t$trackstart\n";
      my $length = -s "$wavdir/$riptrackname.wav";
      $length = int(($length - 44) / 2352);
      print INF "# track length in sectors (1/75 seconds each), rest samples\n";
      print INF "Tracklength=\t'",  $length, ", 0'\n";
      $trackstart += $length;
      print INF "Pre-emphasis=\tno\n";
      print INF "Channels=\t2\n";
      print INF "Endianess=\tlittle\n";
      print INF "# index list\n";
      print INF "Index=\t0\n";
      print INF "Index0=\t-1\n";
      close(INF);
      $nofghosts--;
      if($nofghosts > 0) {
         my $gcn = $seltrack[$#seltrack - $nofghosts];
         my $trn = $tracklist[$gcn];
         $tracktag = $tracktags[$gcn];
         $riptrackname = get_trackname($gcn + 1, $trn);
         $riptrackno++;
      }
   }
   return($trackstart);
}
########################################################################
#
# Write coverart to mp3 files.
#
sub mp3_cover {
   my($snd_file, $coverpath) = @_;
   my $mp3 = MP3::Tag->new($snd_file);
   $mp3->get_tags;
   my $id3v2 = exists $mp3->{'ID3v2'}
         ? $mp3->{'ID3v2'}
         : $mp3->new_tag('ID3v2');
   my $type = $coverpath;
   $type =~ s/.*\.(gif|jpg|jpeg|png)$/$1/;
   $type =~ s/jpeg/jpg/;

   print "Adding a $type to $snd_file.\n\n" if($verbose > 4);
   log_info("\nAdding a $type to $snd_file.");
   # Read coverart into $data.
   open(PIC, "< $coverpath" )
      or print "Cannot open file $coverpath: $!\n\n";
   binmode(PIC);
   my $data = do { local($/); <PIC> };
   $id3v2->add_frame('APIC', "image/$type", 3, 'Cover Image', $data);
   $id3v2->write_tag;
   close(PIC);
   $mp3->close;
   return;
}
########################################################################
#
# Write special tags to mp3 files.
#
sub mp3_tags {
   my($snd_file) = shift;
   my $mp3 = MP3::Tag->new($snd_file);
   $mp3->get_tags;
   my $id3v2 = exists $mp3->{'ID3v2'}
         ? $mp3->{'ID3v2'}
         : $mp3->new_tag('ID3v2');
   foreach (@mp3tags) {
      my ($frame, $content) = split(/=/, $_);
      $id3v2->add_frame($frame, $content);
      log_info("\nAdding $frame=$content to $snd_file.");
   }
   $id3v2->write_tag;
   $mp3->close;
   return;
}
########################################################################
#
# Write coverart to ogg files.
#
sub ogg_cover {
   use MIME::Base64 qw(encode_base64);
   my($snd_file, $coverpath) = @_;
   my $type = $coverpath;
   $type =~ s/.*\.(gif|jpg|png)$/$1/;

   open(PIC, "$coverpath")
      or print "Cannot open file $coverpath: $!\n\n";
   my $data = do { local($/); encode_base64(<PIC>, '') };
   close(PIC);

   print "Adding a $type to $snd_file.\n\n" if($verbose > 4);
   log_info("\nAdding a $type to $snd_file.");
#   print "\n\nvorbiscomment -a $snd_file -t COVERARTMIME=image/$type -t COVERART=$data\n\n";
   log_system("vorbiscomment -a \"$snd_file\" -t COVERARTMIME=image/$type -t COVERART=$data");
   return;
}
########################################################################
#
# Write the CDDB entry to ~/.cddb/category if there is not already
# an entry present.
#
sub write_cddb {
    chomp($categ = $cd{cat});
    log_system("mkdir -m 0755 -p $homedir/.cddb/$categ/")
       or print "Can not create directory $homedir/.cddb/$categ: $!\n";
    $cddbid =~ s/,.*$// if($cddbid =~ /,/);
    if(! -f "$homedir/.cddb/$categ/$cddbid") {
       open(TOC, "> $homedir/.cddb/$categ/$cddbid")
           or print "Can not write to $homedir/.cddb/$categ/$cddbid: $!\n";
       foreach (@{$cd{raw}}) {
           print TOC $_;
       }
    }
    close TOC;
    $archive = 0;
}
########################################################################
#
# Merge the wav files if $cdcue == 1.
#
sub merge_wav {
   my ($trn, $chunkbyte, $album) = @_;
   open(IN, "< $wavdir/$trn.rip") or
   print "Can't open $trn.rip: $!\n";
   binmode(IN);
   # Only skip the header in case the base file already exists.
   if(-r "$wavdir/$album.wav") {
      seek(IN, 44, 0) or
         print "\nCould not seek beyond header in file IN: $!\n";
   }
   open(OUT, ">> $wavdir/$album.wav");
   binmode(OUT);

   # I don't know if it is good to read so many bytes a time, but it
   # is faster than reading byte by byte.
   while(read(IN, my $bindata, $chunkbyte)) {
      print OUT $bindata;
   }
   close(IN);
   close(OUT);

   # Rewrite the header of the merged file $album.wav.
   write_wavhead("$wavdir/$album.wav");

   return;
}
########################################################################
#
# Rewrite the wav header.
#
sub write_wavhead {
   my $file = shift;
   if(!sysopen(WAV, $file, O_RDWR | O_CREAT, 0755)) {
      print "\nCan not to open $file: $!\n";
      return;
   }
   my $buffer;
   my $nread = sysread(WAV, $buffer, 44);
   if($nread != 44 || length($buffer) != 44) {
      print "\nWAV-header length problem in file $file.\n";
      close(WAV);
      return;
   }

   my $main_template = "a4 V a4 a4 V a16 a4 V";
   my($riff_header, $file_size_8, $wav_header, $fmt_header,
      $fmt_length, $fmt_data,$data_header,$data_size) =
      unpack($main_template, $buffer);
   if($verbose > 5) {
      print "RIFF chunk descriptor is: $riff_header\n",
            "RIFF chunk length is:     $file_size_8\n",
            "WAVE format is:           $wav_header\n",
            "FMT subchunk is:          $fmt_header\n",
            "FMT subchunk length is:   $fmt_length\n";
   }
   my $file_length = -s "$file";
   $file_size_8 = $file_length - 8;
   $data_size = $file_length - 44;
   $buffer = pack($main_template, $riff_header, $file_size_8,
                  $wav_header, $fmt_header, $fmt_length, $fmt_data,
                  $data_header, $data_size);
   sysseek(WAV, 0, 0);
   syswrite(WAV, $buffer, length($buffer));
   close(WAV);
   return;
}
########################################################################
#
# Check all tracks for VA-style.
#
sub check_va {
   my $prt_msg = shift;
   my $delim = "";
   my $delim_colon = 0;
   my $delim_hyphen = 0;
   my $delim_slash = 0;
   my $delim_parenthesis = 0;
   my $n = 0;
   # Don't use @tracktags because operator might not want to rip the
   # whole CD. VA-style detection will fail if number of selected tracks
   # are compared to the total number of tracks!
   foreach (@seltrack) {
      my $tn = $_ - 1;
      $delim_colon++ if($tracktags[$tn] =~ /:/);
      $delim_hyphen++ if($tracktags[$tn] =~ /-/);
      $delim_slash++ if($tracktags[$tn] =~ /\//);
      $delim_parenthesis++ if($tracktags[$tn] =~ /\(.*\)/);
      $n++;
   }

   my $artist = clean_all($cd{artist});

   if($vatag >= 1 and $artist =~ /$vastring/i and
     ($delim_colon == $n or $delim_hyphen == $n or
      $delim_slash == $n or $delim_parenthesis == $n)) {
      $va_flag = 1;
      print "\nVarious Artists CDDB detected, track artist will be ",
            "used for each track tag.\n"
            if($verbose > 2 and $prt_msg == 1);
   }
   elsif($vatag >= 3 and $artist =~ /$vastring/i and
     ($delim_colon > 0 or $delim_hyphen > 0 or $delim_slash > 0 or
      $delim_parenthesis > 0)) {
      $va_flag = 1;
      print "\nVarious Artists CDDB detected, track artist will be ",
            "used for some track tags.\n"
            if($verbose > 2 and $prt_msg == 1);
   }
   elsif($vatag >= 5 and
     ($delim_colon == $n or $delim_hyphen == $n or
      $delim_slash == $n or $delim_parenthesis == $n)) {
      $va_flag = 1;
      print "\nMultiple artists data detected, track artist will be",
            " used for each track tag.\n"
            if($verbose > 2 and $prt_msg == 1);
   }
   elsif($vatag >= 7 and
     ($delim_colon > 0 or $delim_hyphen > 0 or $delim_slash > 0 or
      $delim_parenthesis > 0)) {
      $va_flag = 1;
      print "\nMultiple artists data detected, track artist will be",
            " used for some track tags.\n"
            if($verbose > 2 and $prt_msg == 1);
   }
   else {
      $va_flag = 0 unless($va_flag == 2);
      print "\nNo Various Artists DB detected, album artist will be",
            " used for each track tag.\n"
            if($verbose > 2 and $va_flag == 0 and $prt_msg == 1);
   }
   print "\n" if($verbose > 2 and $prt_msg == 1);
   return($delim) if($va_flag == 0);

   if($va_flag == 2) {
      $va_flag = 1;
      $delim = "/";
   }
   # Give slashes highest priority and set default to slashes too.
   if($delim_slash >= $delim_colon and
      $delim_slash >= $delim_hyphen and
      $delim_slash >= $delim_parenthesis) {
      $delim = "/";
   }
   elsif($delim_colon >= $delim_hyphen and
      $delim_colon >= $delim_parenthesis and
      $delim_colon >= $delim_slash) {
      $delim = ":";
   }
   elsif($delim_hyphen >= $delim_colon and
         $delim_hyphen >= $delim_slash and
         $delim_hyphen >= $delim_parenthesis) {
      $delim = "-";
   }
   elsif($delim_parenthesis >= $delim_colon and
         $delim_parenthesis >= $delim_slash and
         $delim_parenthesis >= $delim_hyphen) {
      $delim = "(";
   }
   else {
      $delim = "/";
   }
   return($delim);
}
########################################################################
#
# Copy image file from destination path to directories of encoded sound
# files.
#
sub copy_cover {
   for(my $c=0; $c<=$#coder; $c++) {
      copy("$copycover", "$sepdir[$c]")
      or print "Copying file to $sepdir[$c] failed: $!\n";
   }
}
########################################################################
#
# Check album cover in path variable copycover.
#
sub check_cover {
   my $ans;
   unless(-s $copycover) {
      while($copycover !~ /^[yn]$/i) {
         print "\nImage file $copycover is not a valid file. Continue? [y/n] (y) ";
         $ans = <STDIN>;
         chomp $ans;
         $ans = "y" if($ans eq "");
         last if($ans =~ /y/i);
         die "Aborting\n" if($ans =~ /n/i);
      }
   }
}
########################################################################
#
# Read in ISRCs using Icedax and submit them if detected using code from
# Nicholas Humfrey <njh@aelius.com>.
#
sub get_isrcs {
   print "\nReading ISRC..." if($verbose > 2);
   my $icedax = `which icedax`;
   chomp($icedax);
   if($mbname ne "" and $mbpass ne "" and $icedax ne "") {
      my $mcn = undef;
      @isrcs = ();
      open(ICEDAX, "icedax -D $scsi_cddev -g -H -J -Q -v trackid 2>&1 |")
      or print "\nFailed to run icedax command: $!\n";
      while(<ICEDAX>) {
         chomp;
         if(/T:\s+(\d+)\s+ISRC:\s+([A-Z]{2}-?\w{3}-?\d{2}-?\d{5})$/) {
            my ($num, $isrc) = ($1-1, uc($2));
            $isrc =~ s/\W//g;
            $isrcs[$num] = $isrc;
         }
         elsif(/Media catalog number: (.+)/i) {
            $mcn = $1;
         }
      }
      close(ICEDAX);

      my $diflag = 1; # Suppose all ISRCs found are different.
      # Now preparing ISRC data array to post to MB server.
      my @isrcdata = ();
      print "MCN: " . $mcn . "\n" if ($mcn && $verbose > 3);
      for(my $i = 0; $i < scalar(@isrcs); $i++) {
         my $isrcno = $isrcs[$i];
         my $trackid = $idata[$i];
         next unless($trackid);
         if(defined $isrcno && $isrcno ne '' && $isrcno !~ /^0+$/) {
            printf("\nTrack %2.2d: %s %s", $i + 1, $isrcno || '', $trackid)
            if($verbose > 3);
            push(@isrcdata, "isrc=" . $trackid . '%20' . $isrcno);
         }
         # Test if subsequent (all) ISRCs are different.
         if($i > 0) {
            $isrcno = $i unless($isrcno);
            $diflag = 0 if($isrcs[$i-1] && $isrcno eq $isrcs[$i-1]);
         }
      }
      print "\n\n" if($verbose > 3);

      # Check that we have something to submit
      if(scalar(@isrcdata) < 1) {
         print "\nNo valid ISRCs to submit." if($verbose > 2);
         sleep 1;
      }
      elsif($diflag == 0) {
         print "\nIdentical ISRCs for different tracks detected.",
               "\nNo submission in this case.\n" if($verbose > 2);
         sleep 1;
      }
      else {
         # Send to Musicbrainz.
         if($mbname ne "" and $mbpass ne "") {
            my $ua = LWP::UserAgent->new;
            $ua->timeout(10);
            $ua->env_proxy;
            $ua->credentials( 'musicbrainz.org:80', 'musicbrainz.org', "$mbname", "$mbpass" );

            my $request = HTTP::Request->new( 'POST', 'http://musicbrainz.org/ws/1/track/' );
            $request->content(join('&', @isrcdata));
            $request->content_type('application/x-www-form-urlencoded');

            my $response = $ua->request($request);
            print "\nISRC submission to MB " . $response->status_line. "\n" if($verbose > 2);
         }
         else {
            print "\nNo ISRC submission to MB.\n" if($verbose > 2);
         }
      }
   }
   return;
}
