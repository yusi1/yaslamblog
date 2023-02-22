---
layout: post
author: Yusef Aslam
tags: funtoo, linux
toc: true
---

Documenting the steps I took to install Funtoo Linux.

# First notes

-   Created single partition in `/dev/sda1` inside the vm (will be mounted at `/`)
-   Mounted partition at `/mnt` outside of the vm, by mounted the vm HDD onto the
    host `/mnt` directory

# Second notes

-   Unpacked gnome stage3 archive with
    `tar --numeric-owner --xattrs --xattrs-include='*' -xpf <stage3>.tar.xz`
    and them extra options to preserve what the root fs needs.

## IMPORTANT procedures before chrooting

-   FIRST MAKE SURE THAT YOU ARE IN THE MOUNT DIRECTORY (i.e funtoo root, `/mnt`)
-   Mounted `/proc` from host to funtoo root with:

    ```shell
    mount --rbind /proc proc
    ```

-   Mounted `/sys` from host to funtoo root with:

    ```shell
    mount --rbind /sys sys
    ```

-   Mount `/dev` from host to funtoo root with:

    ```shell
    mount --rbind /dev dev
    ```

> **<u>FOR NETWORKING</u>** you have to copy the `/etc/resolv.conf` file from the host to the Funtoo rootfs in `/etc`, in order to be able to access the internet.


## [FURTHER ON IN THESE NOTES I&rsquo;M IN THE CHROOT UNLESS I SAY OTHERWISE]

-   Chrooted into the install outside the vm with (inside the mount directory) `chroot . /bin/su --login`, the reason to use `/bin/su` is because it sets useful environment variables that are needed.
-   Setted prompt with `export PS1="(chroot) ${PS1}"`
-   Ran `ego sync` to sync the Funtoo portage repository

# Third notes

-   Created `/etc/fstab` with

    ```conf
    /dev/sda1	/	ext4	noatime		0 1
    ```
(since this vm will only have a single root partition, nothing else)

-   NOTE: Use UUID&rsquo;s in fstab when installing on a second hard drive on baremetal, Funtoo wouldn&rsquo;t boot without using UUID&rsquo;s in fstab because grub doesn&rsquo;t know which hard drive is the second or first because names must be assigned dynamically like for example `/dev/sda` could be the second hard drive while `/dev/sdb` could be the first, and grub doesn&rsquo;t know, so it is better to use UUID&rsquo;s in fstab since you are hardcoding exactly which partition is what specific UUID.

-   Created timezone symlink to `/etc/localtime` with

    ```shell
    ln -s /usr/share/zoneinfo/Europe/London /etc/localtime
    ```
-   Changed hostname from &ldquo;localhost&rdquo; to &ldquo;funtux&rdquo; in `/etc/conf.d/hostname`

    ```conf
    hostname="funtux"
    ```
-   Changed keymap from &ldquo;us&rdquo; to &ldquo;uk&rdquo; in `/etc/conf.d/keymaps`

    ```conf
    keymap="uk"
    ```
-   Didn&rsquo;t change anything in `/etc/conf.d/hwclock` since it is a vm.

## BUG I RAN INTO

-   The Funtoo wiki suggests to update the system before doing first boot
    because the &ldquo;bindist&rdquo; use flag is enabled by default and can cause problems
    if the system is not updated before first boot, so I ran
    `emerge -auDN @world` to update the system. (TAKES A WHILE)

### [ERROR THAT I RAN INTO &#x2013; HAPPENED LAST TIME ASWELL]

-   I ran into an error which stopped me from doing the install further:

    > ERROR: sys-apps/coreutils-9.1::core-kit failed (prepare phase):
     > patch -p1  failed with   /var/tmp/portage/sys-apps/coreutils-9.1/files/coreutils-9.0-fix-chmod-symlink-exit.patch

-   There&rsquo;s a bug report in the official Funtoo bug tracker for this, further proves that I did nothing wrong in the install process: <https://bugs.funtoo.org/plugins/servlet/mobile#issue/FL-9709>
    -   Related: <https://bugs.funtoo.org/plugins/servlet/mobile#issue/FL-9704>

### FIX FOR THIS BUG

-   Turns out I did nothing wrong in the install process, the friendly people in the Funtoo Discord said to just exclude the coreutils package when updating for the time being, great, makes sense too!

    ```shell
    emerge -avuDN @world --exclude coreutils
    ```

# Steps that need to be done inside of the VM


## Fourth notes

-   Installed the latest `linux-firmware` package with

    ```shell
    emerge -av linux-firmware
    ```
-   Installed the `grub` package with

    ```shell 
    emerge -av grub
    ```
-   Install `grub` into the boot directory (should be a partition on an actual system) with (mostly this part specifically needs to be done inside the VM)

    ```shell
    grub-install --target=i386-pc --no-floppy /dev/sda
    ```
-   Updated the `grub` configuration to detect the `initrd` and `vmlinuz` images in the boot directory with

    ```shell
    ego boot update
    ```
    >   Instead of `grub-mkconfig -o /boot/grub/grub.cfg` or in some distros `update-grub`, `ego` is Funtoo&rsquo;s system management tool designed for these kinds of tasks.

# Fifth notes - NEARLY READY TO BOOT!

-   Enabled the dhcp service to make sure networking is enabled on first boot with
    
    ```shell
    rc-update add dhcpcd default
    ```
-   Set the root password with `passwd` since we are already on the root user (to ensure we can login on first boot).

-   Added user `yaslam` with the correct groups with
    
    ```shell
    useradd -m yaslam -G wheel,audio,video,plugdev,portage
    ```
-   Set the password for the new `yaslam` user with
    
    ```shell
    passwd yaslam
    ```

## Needed for sudoing

-   Installed the `sudo` package since it isn&rsquo;t installed by default with
   
    ```shell
    emerge sudo
    ```
-   Enabled users in the `wheel` group to gain root privileges with `sudo`, doing `sudoedit /etc/sudoers` first and then uncommenting this line
    
    ```shell
    %wheel ALL=(ALL:ALL) ALL
    ```

## STEP THAT IS NEEDED FOR VMWARE VM - Installed Vmware-tools

-   Install `vmware-tools` in the VM for graphical acceleration in VMware virtual machines in Xorg with
    
    ```shell
    emerge xf86-video-vmware
    ```

## OPTIONAL - Install an entropy generator for enhanced randomness in generating random numbers (good for security)

-   Install the `haveged` package with
    
    ```shell
    emerge haveged
    ```
-   Enable the `haveged` service with
    
    ```shell
    rc-update add heveged default
    ```
    
    and now it will automatically enhance the random number generator that is in the Linux kernel.

# Sixth notes - FINAL STEPS

-   Exit the chroot shell with
    
    ```shell
    exit
    ```
-   Unmount the Funtoo root partition (in my case `/mnt`) with
    
    ```shell
    umount -lR /mnt
    ```
-   And then if you were in the live-cd you would have to reboot take out the USB and then reboot with
    
    ```shell
    reboot
    ```
    ... and then grub should appear and you will be in your new distro!

