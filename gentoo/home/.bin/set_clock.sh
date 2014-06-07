#!/bin/bash

# -------------------------------------------------------------------
#
# Shell program to set the system and hardware clock.
#
# Copyright 2002, William Shotts <bshotts@users.sourceforge.net>.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version. 
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# This software is part of the LinuxCommand.org project, a site for
# Linux education and advocacy devoted to helping users of legacy
# operating systems migrate into the future.
#
# You may contact the LinuxCommand.org project at:
#
#   http://www.linuxcommand.org
#
# Description:
#
# This program sets the system and hardware clocks with time
# obtained from an external network source (default: time server
# at NIST).  The user may optionally select if the hardware clock
# is using local (default) or universal time.
#
# NOTE: You must be the superuser to run this script.
#
# Usage:
#
#   set_clock [ -h | --help ] [-u]
#
# Options:
#
#   -h, --help  Display this help message and exit.
#   -u              Set hardware clock to Universal Time (GMT)
#
#
# Revision History:
#
# 02/17/2002  File created by new_script ver. 2.1.0
#
# $Id$
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Constants
# -------------------------------------------------------------------

  PROGNAME=$(basename $0)
  VERSION="1.0.1"
  TIMEHOST1=time.fu-berlin.de
  TIMEHOST2=nist1-ny.WiTime.net


# -------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------


function clean_up
{

# -----------------------------------------------------------------------
# Function to remove temporary files and other housekeeping
#   No arguments
# -----------------------------------------------------------------------

  # Nothing needed
  return
}


function error_exit
{

# -----------------------------------------------------------------------
# Function for exit due to fatal program error
#   Accepts 1 argument:
#     string containing descriptive error message
# -----------------------------------------------------------------------


  echo "${PROGNAME}: ${1:-"Unknown Error"}" >&2
  clean_up
  exit 1
}


function graceful_exit
{

# -----------------------------------------------------------------------
# Function called for a graceful exit
#   No arguments
# -----------------------------------------------------------------------

  clean_up
  exit
}


function signal_exit
{

# -----------------------------------------------------------------------
# Function to handle termination signals
#   Accepts 1 argument:
#     signal_spec
# -----------------------------------------------------------------------

  case $1 in
    INT)  echo "$PROGNAME: Program aborted by user" >&2
      clean_up
      exit
      ;;
    TERM) echo "$PROGNAME: Program terminated" >&2
      clean_up
      exit
      ;;
    *)  error_exit "$PROGNAME: Terminating on unknown signal"
      ;;
  esac
}


function usage
{

# -----------------------------------------------------------------------
# Function to display usage message (does not exit)
#   No arguments
# -----------------------------------------------------------------------

  echo "Usage: ${PROGNAME} [-h | --help] [-u]"
}


function helptext
{

# -----------------------------------------------------------------------
# Function to display help message for program
#   No arguments
# -----------------------------------------------------------------------

  local tab=$(echo -en "\t\t")

  cat <<- -EOF-

  ${PROGNAME} ver. ${VERSION}
  This is a program to set the system and hardware clock.

  $(usage)

  Options:

  -h, --help  Display this help message and exit.
  -u              Set hardware clock to Universal Time (GMT)

  NOTE: You must be the superuser to run this script.
-EOF-
}


function root_check
{
# -----------------------------------------------------------------------
# Function to check if user is root
#   No arguments
# -----------------------------------------------------------------------

  if [ "$(id | sed 's/uid=\([0-9]*\).*/\1/')" != "0" ]; then
    error_exit "You must be the superuser to run this script."
  fi
}


# -------------------------------------------------------------------
# Program starts here
# -------------------------------------------------------------------

##### Initialization And Setup #####

# Set file creation mask so that all files are created with 600 permissions.

umask 066
root_check

# Trap TERM, HUP, and INT signals and properly exit

trap "signal_exit TERM" TERM HUP
trap "signal_exit INT"  INT


##### Command Line Processing #####

if [ "$1" = "--help" ]; then
  helptext
  graceful_exit
fi

universal_time=

while getopts ":hu" opt; do
  case $opt in
    u ) universal_time="--utc" ;;
    h ) helptext
      graceful_exit ;;
    * ) usage
      clean_up
      exit 1
  esac
done


##### Main Logic #####

# Set system time from time server

if ! rdate -s ${TIMEHOST1} 2> /dev/null ; then
  if ! rdate -s ${TIMEHOST2} 2> /dev/null ; then
    error_exit "could not connect to time servers"
  fi
fi

# Set hardware clock with system time

/sbin/hwclock $universal_time --systohc

graceful_exit

