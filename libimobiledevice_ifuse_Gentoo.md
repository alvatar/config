# Why this document?

I upgraded my iPhone 5s to iOS 10 and could no longer retrieve photos from it.  This was unacceptable for me so I worked at achieving retrieving my photos.  This document is my story (on Ubuntu 16.04).

The solution is to compile [libimobiledevice and ifuse from source](https://github.com/libimobiledevice).

# Audience

Who is this guide intended for?

Compiling software is what I would consider an advanced Linux skill.  If you're not interested in tinkering with your operating system then this guide may not be for you.  There's a lot of concepts I don't explain or gloss over with brevity to keep these instructions brief (compared to explaining everything).

While I did my best to think of beginners when creating this guide; This guide is not for the faint of heart.  If you've not compiled software before then I recommend you practicing inside of [VirtualBox](https://www.virtualbox.org/) before attempting this on your real system.  Follow this guide at your own risk because I can't make any guarantees based on unknown individual skill level.

# Support

This solution works for (posted via comments):

* Archlinux
* Debian GNU/Linux 8 - 64 bits
* Linux Mint 18
* Linux Mint 18.1 Serena
* Ubuntu 16.04

This solution does **not work** for the following:

* Ubuntu 14.04

On `Ubuntu 16.04`, I have personally used this method to connect to:

* iOS 10
* iOS 10.1
* iOS 10.2
* iOS 10.3.1
* iOS 10.3.3

Hardware: I have personally successfully used this method on an iPhone 5S.  The following is a list of hardware successes from other users (via comments below).

* iPhone 5C
* iPhone 6
* iPhone 6 Plus

If you get this working on a flavor that I don't list, then please post a comment and I will update this support section.

# Setup environment

Don't forget to set up your environment before building.  I typically build and install packages to my local user at `$HOME/usr`.

```bash
sudo apt-get install -y build-essential git
```

Here's a peek at my `.bashrc` settings:

```bash
[ ! -d "$HOME/usr/src" ] && mkdir -p "$HOME/usr/src"
export PKG_CONFIG_PATH="${HOME}/usr/lib/pkgconfig:${PKG_CONFIG_PATH}"
export CPATH="${HOME}/usr/include:${CPATH}"

export MANPATH="${HOME}/usr/share/man:${MANPATH}"

export PATH="${HOME}/usr/bin:${PATH}"
export LD_LIBRARY_PATH="${HOME}/usr/lib:${LD_LIBRARY_PATH}"
```

Notes:
* Important! `PATH` and `LD_LIBRARY_PATH` is important because it is the runtime of `libimobiledevice` and `ifuse` to fix mounting iOS 10 devices.
* `MANPATH` is only used when looking up man pages so it's optional (I recommend it).
* `PKG_CONFIG_PATH` and `CPATH` is used at compile time to resolve dependencies.

# Build libimobiledevice and ifuse from HEAD

### Building on other platforms and OSes (reported in user comments)

* Mint 18.2 - https://gist.github.com/samrocketman/70dff6ebb18004fc37dc5e33c259a0fc#gistcomment-2161041
* Mac OS X
  - https://gist.github.com/samrocketman/70dff6ebb18004fc37dc5e33c259a0fc#gistcomment-2116098
  - https://gist.github.com/samrocketman/70dff6ebb18004fc37dc5e33c259a0fc#gistcomment-2140745 

Jump to [Clone and Build](#clone-and-build).

### Building on Ubuntu 16.04

Install development packages discovered through trial and error.

```
sudo apt-get install automake libtool pkg-config libplist-dev libplist++-dev python-dev libssl-dev libusb-1.0-0-dev libfuse-dev
```

### Clone and Build

Clone the sources.

```bash
cd ~/usr/src
for x in libusbmuxd usbmuxd libimobiledevice ifuse; do git clone https://github.com/libimobiledevice/${x}.git;done
```

Now build in order (the order matters):

1. [libplist](https://github.com/libimobiledevice/libplist) (not required on Ubuntu 16.04)
2. libusbmuxd
3. libimobiledevice
4. usbmuxd
5. ifuse

If you have a system package installed which is in the above list then I recommend uninstalling it.

##### Build libusbmuxd

```bash
cd ~/usr/src/libusbmuxd
./autogen.sh --prefix="$HOME/usr"
make && make install
```

##### Build libimobiledevice

```bash
cd ~/usr/src/libimobiledevice
./autogen.sh --prefix="$HOME/usr"
make && make install
```

##### Build usbmuxd

Unfortunately, `sudo make install` is required because it needs to write to `/lib/udev/rules.d` and `/lib/systemd/system`.

```bash
cd ~/usr/src/usbmuxd
./autogen.sh --prefix="$HOME/usr"
make && sudo make install
```

##### Build ifuse

```bash
cd ~/usr/src/ifuse
./autogen.sh --prefix="$HOME/usr"
make && make install
```

# Connect iPhone

Create a mount point and verify the paths of the tools before executing.

```bash
$ mkdir -p ~/usr/mnt

$ type -p ifuse
/home/sam/usr/bin/ifuse

$ type -p idevicepair
/home/sam/usr/bin/idevicepair
```

Now attempt to mount using ifuse.

```
$ idevicepair pair
SUCCESS: Paired with device 37b633350ab83dc815a6a97dcd6d327b12c41968

$ ifuse ~/usr/mnt/

$ ls ~/usr/mnt/
AirFair  Books  CloudAssets  DCIM  Downloads  FactoryLogs  iTunes_Control  MediaAnalysis  PhotoData  Photos  PhotoStreamsData  PublicStaging  Purchases  Radio  Recordings  Safari  Vibrations
```

When you're finished.  Unmount `~/usr/mnt` using `fusermount`.  For example,

```
fusermount -u ~/usr/mnt
```

# Browsing Photos

The mounted iPhone has an empty `Photos` folder.  Strangely, this has no photos.  Instead, look in the `DCIM` folder for photos.

I have only copied photos _off of my iPhone_ as a backup.  I have not tried copying photos to my iPhone.

# Browsing Music

It is a [known issue music sync no longer works][music] in later versions of iOS with libimobiledevice.  Since Apple changed some things (outlined in linked issues), music sync hasn't worked since as early as iOS 6 and still doesn't work.

[music]: https://github.com/search?q=org%3Alibimobiledevice+music&type=Issues


-------------------------------------

(Added extra)

# For Gentoo

Need to run this daemon:

```
sudo sbin/usbmuxd -f -v
```

Mount VLC:

```
ifuse --documents org.videolan.vlc-ios  ~/usr/mnt/
```

https://gist.github.com/samrocketman/70dff6ebb18004fc37dc5e33c259a0fc
